(ns batbridge.pipeline
  "Implements a simple pipelined processor. Makes no attempt to
  provide out-of-order or superscalar excutuion. Branch prediction and
  hazard conditions are dealt with as minimally as possible."
  (:require [clojure.core.typed :as t]
            ; [clojure.tools.logging :refer [info]]
            [batbridge.isa :as i]))


;; annotate clojure.core fns to behave as I intend
;;------------------------------------------------------------------------------
(t/ann ^:no-check clojure.core/assoc-in
       (Fn [i/Processor '[i/Storage t/AnyInteger] t/AnyInteger -> i/Processor]))


(t/ann ^:no-check clojure.core/update-in
       (Fn [i/Processor '[i/Storage t/AnyInteger]
            (Fn [t/AnyInteger -> t/AnyInteger]) -> i/Processor]))

(t/def-alias Meta
  (HMap :mandatory {:fetch-pc t/AnyInteger
                    :fetched? boolean}))

(t/ann ^:no-check clojure.core/meta
       (Fn [i/Instruction -> Meta]))

(t/ann ^:no-check clojure.core/with-meta
       (Fn [i/InstructionVec Meta -> i/InstructionVec]
           [i/InstructionMap Meta -> i/InstructionMap]
           [i/CommitCommand Meta -> i/CommitCommand]))


;; holdover hack kept for niceties
;;------------------------------------------------------------------------------
(t/defn> ^:no-check get-memory
  :- t/AnyInteger
  [p    :- i/Processor
   addr :- t/AnyInteger]
  (get-in p [:memory addr]))

(t/defn> ^:no-check get-icode
  :- i/InstructionVec
  [p    :- i/Processor
   addr :- t/AnyInteger]
  (get-in p [:memory addr]))

(t/defn> ^:no-check get-register
  :- t/AnyInteger
  [p   :- i/Processor
   reg :- t/AnyInteger]
  (get-in p [:registers reg] 0))


;; processor implementation
;;------------------------------------------------------------------------------
(t/defn> fetch
  "Implements the fetch stage"
  :- i/Processor
  [processor :- i/Processor]
  (let [pc (get-register processor 31)
        icode (or (get-icode processor pc) i/vec-no-op)]
    (println (format "[fetch    ] pulling from address %d : %s" pc icode))
    (-> processor
        (assoc-in [:registers 31] (+ pc 4))
        (assoc :fetch (with-meta icode
                        {:fetch-pc pc
                         :fetched? true})))))


(t/defn> ^:no-check in-flight
  :- (t/Set i/InstructionParam)
  [processor :- i/Processor]
  (set
   [(get-in processor [:decode :dst])
    (when (= :registers (get-in processor [:execute 0]))
      (get-in processor [:execute 1]))]))


(t/defn> decode
  "Dummy decode which translates a vector literal to an icode map. Not really
  important now, but it'll be nice later."
  :- i/Processor
  [processor :- i/Processor]
  (let [icode (get processor :fetch i/vec-no-op)
        decoded {:icode (nth icode 0 :add)
                 :dst   (get i/register-symbol-map (nth icode 1 0) 0)
                 :srca  (get i/register-symbol-map (nth icode 2 0) 0)
                 :srcb  (get i/register-symbol-map (nth icode 3 0) 0)
                 :lit   (nth icode 4 0)}
        decoded (with-meta decoded (meta icode))]
    (if (and (i/fetched? icode)
             (and (i/fetched? (:decode processor))
                  (or (contains? (in-flight processor) (:srca decoded))
                      (contains? (in-flight processor) (:srcb decoded)))))
      (do (println (format "[decode   ] data hazard detected! setting pc to %d..."
                           (:fetch-pc (meta icode))))
          (-> processor
              (assoc :decode i/map-no-op)
              (assoc-in [:registers 31] (:fetch-pc (meta icode)))))
      (do (println "[decode   ] no data hazard.. decoding.. done.")
          (assoc processor :decode decoded)))))


(t/defn> register->val
  :- t/AnyInteger
  [reg       :- i/InstructionParam
   processor :- i/Processor]
  (case reg
    (:r_ZERO 0) 0
    (:r_IMM 30) (get (get processor :decode {}) :lit 0)
    (:r_PC 31)  (get-register processor 31)
    (-> processor
        :registers
        (get reg 0))))


(t/ann ^:no-check opcode->fn
       (t/Map t/AnyInteger
              (Fn [t/AnyInteger t/AnyInteger i/Processor
                   -> t/AnyInteger])))
(def opcode->fn
  {:add (fn [x y _] (+ x y))
   :sub (fn [x y _] (- x y))
   })


(t/defn> execute
  :- i/Processor
  [processor :- i/Processor]
  (let [icode (get processor :decode i/map-no-op)
        srca  (register->val (get icode :srca 0) processor)
        srcb  (register->val (get icode :srcb 0) processor)
        dst   (get i/register-symbol-map (get icode :dst 0) 0)
        opcode (get icode :icode :add)]

    (when-not (i/fetched? icode)
      (println "[execute  ] processing no-op..."))

    (as-> (case opcode
            (:add :sub)
              (do (println "[execute  ] processing registered op...")
                  [:registers dst ((get opcode->fn opcode (constantly 0))
                                   srca srcb processor)])

            (:ld)
              (do (println "[execute  ] processing load op...")
                  [:load (+ srca (* 4 srcb)) dst])

            (:st)
              (do (println "[execute  ] processing store op...")
                  [:store (+ srca (* 4 srcb)) dst])

            (do (println "[execute  ] sending halt...") [:halt]))
          cmd
        (with-meta cmd (meta icode))
        (assoc processor :execute cmd))))


(t/defn> writeback
  :- i/Processor
  [processor :- i/Processor]
  (let [directive (get processor :execute
                       (with-meta
                         [:registers 0 0]
                         {:fetched? false
                          :fetch-pc -1}))]
    (when-not (i/fetched? directive)
      (println "[writeback] processing no-op..."))

    (cond ;; special case to stop the show
          (= :halt (first directive))
            (do (println "[writeback] halting...")
                (assoc processor :halted true))

          ;; special case for printing
          (and (= :registers (first directive))
               (= 0 (second directive)))
            (if-not (zero? (nth directive 2 0))
              (do (print (char (nth directive 2 0)))
                  processor)
              processor)

          ;; ;; special case for setting the PC
          ;; ;;
          ;; ;; The pipeline has to be flushed in this implementation
          ;; ;; because we don't know (and can't know) if the in-flight
          ;; ;; instructions are on the right execution path.
          ;; (and (= :registers (first directive))
          ;;      (= 31 (second directive)))
          ;;   (do (println "[writeback] Setting PC and flushing the pipeline...")
          ;;     (-> processor
          ;;         (assoc-in [:registers 31] (second directive))
          ;;         (dissoc :fetch :decode :execute :writeback)))

          (= (nth directive 0) :registers)
            (do (println "[writeback] processing register store...")
              (assoc-in processor [:registers (nth directive 1 0)]
                        (nth directive 2 0)))

          true
            (do (println (format "[writeback] ERROR: UNMATCHED COMMAND\n\t%s"
                                 directive))
                processor))))


(t/defn> step
  "Sequentially applies each phase of execution to a single processor
  state, returning an updated state representing a single cycle of
  computation's effects on the processor."
  :- i/Processor
  [state :- i/Processor]
  (-> state
      (writeback)
      (execute)
      (decode)
      (fetch)))

(t/ann ^:no-check run [i/Processor -> i/Processor])
(defn run [state & {:keys [limit]
                    :as kwargs
                    :or {limit 50}}]
  (loop [state state
         i limit]
    (if-not (or (i/halted? state)
                (zero? limit))
      (let [ns (step state)]
        (println "---------------------------------------------------")
        (recur ns (dec i)))
      state)))

(t/ann ^:no-check -main [(t/Map t/AnyInteger i/InstructionVec) -> i/Processor])
(defn -main
  "Creates a processor state with a map \"instructions\" from
  4-aligned addresses to instruction representations, and executes
  this state starting from memory address 0."
  [instructions]
  (run (i/seq->state instructions)))

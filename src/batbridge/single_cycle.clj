(ns batbridge.single-cycle
  "Implements a simple in-order processor. Makes no attempt to provide
  out-of-order or superscalar excutuion. Will serve as a benchmark for per-cycle
  operation performance and to allow correctness comparison between multiple
  differing processor implementations."
  (:require [clojure.tools.logging :refer [error info]]
            [clojure.core.typed :as t]
            [batbridge.isa :as i]))

(t/ann ^:no-check clojure.core/assoc-in
       (Fn [i/Processor '[i/Storage t/AnyInteger] t/AnyInteger -> i/Processor]))

(t/ann ^:no-check clojure.core/update-in
       (Fn [i/Processor '[i/Storage t/AnyInteger] 
            (Fn [t/AnyInteger -> t/AnyInteger]) -> i/Processor]))

(t/ann ^:no-check get-instruction [i/Processor t/AnyInteger -> i/InstructionVec])
(defn get-instruction [p addr]
  (get-in p [:memory addr]))

(t/ann ^:no-check get-memory [i/Processor t/AnyInteger -> t/AnyInteger])
(defn get-memory [p addr]
  (get-in p [:memory addr] 0))

(t/ann ^:no-check get-register [i/Processor t/AnyInteger -> t/AnyInteger])
(defn get-register [p reg]
  (get-in p [:register reg] 0))

(t/ann fetch [i/Processor -> i/Processor])
(defn fetch [processor]
  (let [pc (get-register processor 31)
        icode (get-instruction processor pc)]
    (-> processor
        (update-in [:registers 31] (t/fn> [x :- t/AnyInteger] (+ x 4)))
        (assoc :fetch icode))))

(t/ann decode [i/Processor -> i/Processor])
(defn decode
  "Dummy decode which translates a vector literal to an icode map. Not really
  important now, but it'll be nice later."
  [processor]
  (let [icode (:fetch processor)]
    (assoc processor :decode
           {:icode (nth icode 0 :add)
            :dst   (get i/register-symbol-map (nth icode 1 0) 0)
            :srca  (get i/register-symbol-map (nth icode 2 0) 0)
            :srcb  (get i/register-symbol-map (nth icode 3 0) 0)
            :lit   (nth icode 4 0)})))

(t/ann register->val [i/InstructionParam i/Processor -> t/AnyInteger])
(defn register->val [reg processor]
  (case reg
    (:r_ZERO 0) 0
    (:r_IMM 30) (get (get processor :decode {}) :lit 0)
    (:r_PC 31)  (get-register processor 31)
    (-> processor 
        :registers
        (get reg 0))))

(t/ann opcode->fn (t/Map t/Keyword 
                       (Fn [t/AnyInteger t/AnyInteger i/Processor -> t/AnyInteger])))
(def opcode->fn
  {:add (t/fn> [x :- t/AnyInteger
                y :- t/AnyInteger
                _ :- i/Processor] 
               (+ x y))

   :sub (t/fn> [x :- t/AnyInteger
                y :- t/AnyInteger
                _ :- i/Processor] 
               (- x y))

   :ld  (t/fn> [x :- t/AnyInteger
                y :- t/AnyInteger
                p :- i/Processor] 
               (get-memory p (+ x (* 4 y))))})

(t/ann execute [i/Processor -> i/Processor])
(defn execute [processor]
  (let [icode (:decode processor)
        srca  (register->val (get icode :srca 0) processor)
        srcb  (register->val (get icode :srcb 0) processor)
        dst   (get i/register-symbol-map (get icode :dst 0) 0)
        icode (get icode :icode :add)]
    (assoc processor :execute
           (case icode
             (:add :sub :ld)
               [:registers dst ((get opcode->fn icode (constantly 0))
                                      srca srcb processor)]

             (:st)
               [:memory (+ srca (* 4 srcb)) dst]

             :else
               :halt))))

(t/ann writeback [i/Processor -> i/Processor])
(defn writeback [processor]
  (let [directive (:execute processor)]
    (cond ;; special case to stop the show
          (= :halt directive)
            (assoc processor :halted true)
          
          ;; special case for printing
          (and (= :registers (first directive))
               (= 0 (second directive)))
            (do (print (char (nth directive 2 0)))
                processor)

          true
          (assoc-in processor [(nth directive 0 :registers)
                               (nth directive 1 0)]
                 (nth directive 2 0)))))

(t/ann halted? [i/Processor -> boolean])
(defn halted? [state]
  (or (:halted state) false))

(t/ann step [i/Processor -> i/Processor])
(defn step
  "Sequentially applies each phase of execution to a single processor
  state, returning an updated state representing a single cycle of
  computation's effects on the processor."
  [state]
  (-> state
      (fetch)
      (decode)
      (execute)
      (writeback)))

(t/ann ^:no-check -main [(t/Map t/AnyInteger i/InstructionVec) -> i/Processor])
(defn -main
  "Creates a processor state with a map \"instructions\" from
  4-aligned addresses to instruction representations, and executes
  this state starting from memory address 0."
  [instructions]
  (t/loop> [state :- i/Processor
            {:memory instructions
             :registers {31 0}}]
    (if-not (halted? state)
      (recur (step state))
      state)))

(t/ann seq->instrs [(t/Seq i/InstructionVec) -> (t/Map t/AnyInteger i/InstructionVec)])
(defn seq->instrs [seq]
  (zipmap (range 0 (* 4 (count seq)) 4)
          seq))

(t/ann bprn [String -> (t/Seq i/InstructionVec)])
(defn bprn [str]
  (map (t/fn> [x :- Character] [:add :r_ZERO :r_IMM 0 (int x)])
       str))

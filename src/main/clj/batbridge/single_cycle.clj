(ns batbridge.single-cycle
  "Implements a simple in-order processor. Makes no attempt to provide
  out-of-order or superscalar excutuion. Will serve as a benchmark for per-cycle
  operation performance and to allow correctness comparison between multiple
  differing processor implementations."
  (:require [batbridge
             [common :as common]
             [isa :as isa]]
            [clojure.core.match :refer [match]]
            [taoensso.timbre :refer [info warn debug]]))

(def fetch-default
  {:blob isa/vec-no-op
   :npc  -1
   :pc   -1})

(defn zdec [n]
  (max 0 ((fnil dec 0) n)))

(defn fetch
  "Accesses the value of the PC register, fetches the instruction
  there from memory and sets the :fetch value key in the
  state. Increments the PC by the instruction width. Returns an
  updated state."

  [processor]
  (if-not (common/halted? processor)
    (let [pc    (common/register->val processor 31)
          icode (common/get-memory processor pc)
          npc   (+ pc 4)]
      (cond
        (common/stalled? processor)
        ,,(do (info "[fetch    ] Stalled!" (:fetch/stall processor))
              (update processor :fetch/stall zdec))

        :else
        ,,(do (info "[fetch    ]" pc "->" icode)
              (-> processor
                  (common/write-register 31 npc)
                  (assoc :fetch/result
                         {:blob icode
                          :pc    npc
                          :npc   (+ npc 4)})))))
    processor))

(defn- queue [coll]
  (into clojure.lang.PersistentQueue/EMPTY coll))

(defn- queue? [obj]
  (instance? clojure.lang.PersistentQueue obj))

(defn- next-op
  "Function from a processor to the next operation which the processor
  needs to perform, the new macro queue and the number of new ops."
  [processor]
  {:post [(vector? %)
          (let [[icode queue new] %]
            (and (map? icode)
                 (queue? queue)
                 (number? new)
                 (<= 0 new)))]}
  (let [;; constants
        argfn         (juxt :d :a :b :i)

        ;; destructure arguments
        ops           (:decode/ops processor (queue []))

        {:keys [blob npc pc]
         :as   fetch} (:fetch/result processor fetch-default)

        ;; main logic
        ;; --------------------
        ;; Decode that operation and figure out if it is a
        ;; macro (macro-fn will be nil if it isn't)
        di            (isa/decode-instr blob)
        _             (assert (map? di) (pr-str di))
        _             (assert (keyword? (:icode di)) (pr-str di))

        ;; If the processor is stalled and we have ops in the queue,
        ;; then we take the first op from the queue. Otherwise we take
        ;; the op from fetch.
        [blob ops]   (if (and (common/stalled? processor)
                              (not (empty? ops)))
                       [(peek ops) (pop ops)]
                       [di         ops])]

    (if-let [macro-fn (get isa/opcode->macro (:icode di))]
      ;; If we have a macro-fn, use it to compute the list of new
      ;; operations we need to perform.
      (let [new-ops (when macro-fn
                      (->> (argfn di)
                           (apply macro-fn)
                           (map isa/decode-instr)))

            ;; Note that these operations must occur before anything
            ;; already in the ops queue.
            ops     (-> (queue [])
                        (into (rest new-ops))
                        (into ops))

            ;; Choose the icode we're going to use. Either it'll be the
            ;; first of the new icodes because we're about to stall some,
            ;; or it'll be the icode we had to begin with
            icode   (-> new-ops
                        first
                        isa/decode-instr)]

        [icode ops (count new-ops)])

      ;; Otherwise there are no new opcodes, don't do anything more.
      [blob ops 0])))

(def decode-default
  (merge isa/map-no-op
         {:pc -1 :npc -1}))

(defn decode
  [processor]
  (if-not (common/halted? processor)
    (let [[di queue n :as r] (next-op processor)
          {:keys [pc npc]
           :or   {pc  -1
                  npc -1}}   (:fetch/result processor fetch-default)]
      (info "[decode   ]" (common/fmt-instr di))
      (-> processor
          (update :fetch/stall (fnil + 0) n)
          (assoc :fetch/result
                 ,,fetch-default

                 :decode/ops
                 ,,queue

                 :decode/result
                 ,,(merge di {:pc pc :npc npc}))))
    processor))

(def execute-default
  [:registers 30 0])

(defn execute
  "Indexes into the opcode->fn map to fetch the implementation of the
  last decoded instruction. Decodes the parameter values, and applies
  the implementation function to the parameters. Sets the :execute key
  with a state update directive which can use. Returns the updated
  processor state."

  [processor]
  (if-not (common/halted? processor)
    (let [{:keys [icode a b d i pc npc]
           :or   {pc    -1
                  npc   -1}
           :as   decode} (get processor
                              :decode/result
                              decode-default)
          srca           (common/register->val processor a pc i)
          srcb           (common/register->val processor b pc i)
          t              ((juxt :icode :d :a :b :i) decode)]
      (when-not (= [:add 30 30 30 0] t)
        (debug "[execute  ]" t "args" [pc i srca srcb d]))
      (as-> icode v
        (or (get isa/opcode->fn v)
            (throw (Exception. (pr-str decode))))
        (v processor pc i srca srcb d)
        (common/upgrade-writeback-command v)
        (assoc v
               :pc pc
               :npc npc)
        (assoc processor
               :execute/result v)))
    processor))

(defn ^:dynamic branch [processor address]
  (common/write-register processor 31 (common/normalize-address address)))

(defn writeback
  "Pulls a writeback directive out of the processor state, and
  performs the indicated update on the processor state. Update command
  have been restructured and are now maps
  {:dst (U :registers :halt :memory) :addr Int :val Int}."

  [processor]
  (if-not (common/halted? processor)
    (let [directive              (get processor :execute/result isa/writeback-no-op)
          {:keys [dst addr val]} directive
          t                      [dst addr val]]
      (when-not (= t [:registers 30 0])
        (debug "[writeback]" t))

      (match [dst addr val]
        [:registers 30 0]
        ,,processor

        [:halt _ _]
        ,,(assoc processor :halted true)

        [:registers 29 val]
        ,,(do (when-not (zero? val)
                (print (format "0x%X" (char val))))
              processor)

        [:registers 30 val]
        ,,(do (when-not (zero? val)
                (print (char val)))
              processor)

        ;; special case for branching as we must flush the pipeline
        [:registers 31 val]
        ,,(branch processor val)

        [:registers r val]
        ,,(common/write-register processor r val)

        [:memory addr val]
        ,,(common/write-memory processor addr val)))
    processor))

(defn step
  "Sequentially applies each phase of execution to a single processor
  state, returning an updated state representing a single cycle of
  computation's effects on the processor."

  [state]
  (-> state
      fetch
      decode
      execute
      writeback))

(defn -main
  "Steps a processor state until such time as it becomes marked as
  'halted'. Makes no attempt to construct or validate a processor
  state, or handle errors therein."

  [state]
  (loop [state state]
    (if-not (common/halted? state)
      (recur (step state))
      state)))

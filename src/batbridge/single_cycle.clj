(ns batbridge.single-cycle
  "Implements a simple in-order processor. Makes no attempt to provide
  out-of-order or superscalar excutuion. Will serve as a benchmark for per-cycle
  operation performance and to allow correctness comparison between multiple
  differing processor implementations."
  (:require [clojure.core.match :refer [match]]
            [batbridge
             [common :as common]
             [isa :as isa]]
            [taoensso.timbre :refer [info warn]]))

(defn fetch
  "Accesses the value of the PC register, fetches the instruction
  there from memory and sets the :fetch value key in the
  state. Increments the PC by the instruction width. Returns an
  updated state."

  [processor]
  (let [pc    (common/register->val processor 31)
        icode (common/get-memory processor pc)
        npc   (+ pc 4)]
    (info "[fetch    ]" pc "->" icode)
    (cond
      (common/halted? processor)
      ,,(assoc processor
               :result/fetch
               {:icode isa/vec-no-op
                :npc   -1
                :pc    -1})

      (common/stalled? processor)
      ,,processor

      :else
      ,,(-> processor
            (assoc-in [:registers 31] npc)
            (assoc :result/fetch
                   {:icode icode
                    :npc   (+ npc 4)
                    :pc    npc})))))

(defn- queue [coll]
  (into clojure.lang.PersistentQueue/EMPTY coll))

(defn- next-op
  "Function from a processor to the next operation which the processor
  needs to perform, the new macro queue and the number of new ops."
  [processor]
  {:post [(or (number? (first %))
              (vector? (first %)))
          (instance? clojure.lang.PersistentQueue (second %))
          (number? (last %))]}
  (let [;; constants
        noop        {:icode isa/vec-no-op
                     :pc    -1}
        argfn       (juxt :d :a :b :i)

        ;; destructure arguments
        fetch       (:result/fetch processor)
        decode      (:decode processor)
        ops         (:ops processor (queue []))
        icode       (:icode fetch isa/vec-no-op)

        ;; main logic

        ;; If the processor is stalled and we have ops in the queue,
        ;; then we take the first op from the queue. Otherwise we take
        ;; the op from fetch.
        [icode ops] (if (and (common/stalled? processor) ops)
                      [(peek ops) (pop ops)]
                      [icode      ops])

        ;; Decode that operation and figure out if it is a
        ;; macro (macro-fn will be nil if it isn't)
        di          (isa/decode-instr icode)]

    (if-let [macro-fn (get isa/opcode->macro (:icode di))]
      ;; If we have a macro-fn, use it to compute the list of new
      ;; operations we need to perform.
      (let [new-ops (when macro-fn
                      (apply macro-fn (argfn di)))

            ;; Note that these operations must occur before anything
            ;; already in the ops queue.
            ops     (-> (queue [])
                        (into (rest new-ops))
                        (into ops))

            ;; Choose the icode we're going to use. Either it'll be the
            ;; first of the new icodes because we're about to stall some,
            ;; or it'll be the icode we had to begin with
            icode   (first new-ops)]
        [icode ops (count new-ops)])

      ;; Otherwise there are no new opcodes, don't do anything more.
      [icode ops 0])))

(defn decode
  [processor]
  (if-not (common/halted? processor)
    (let [[icode queue n]  (next-op processor)
          di               (isa/decode-instr icode)
          {:keys [pc npc]} (:result/fetch processor)]
      (info "[decode   ]" icode)
      (info "[decode   ]" (common/fmt-instr di))
      (-> processor
          (update :stall (fnil + 0) n)
          (assoc :ops queue)
          (assoc :result/decode (merge di {:pc pc :npc npc}))))
    processor))

(defn execute
  "Indexes into the opcode->fn map to fetch the implementation of the
  last decoded instruction. Decodes the parameter values, and applies
  the implementation function to the parameters. Sets the :execute key
  with a state update directive which can use. Returns the updated
  processor state."

  [processor]
  (let [{:keys [icode a b d i pc npc]
         :as   decode} (get processor :result/decode isa/map-no-op)
        srca           (common/register->val processor a pc i)
        srcb           (common/register->val processor b pc i)]
    (info "[execute  ]" decode)
    (println decode srca srcb)
    (as-> icode v
      (get isa/opcode->fn v)
      (v processor pc i srca srcb d)
      (common/upgrade-writeback-command v)
      (assoc v :pc pc)
      (assoc v :npc npc)
      (assoc processor :result/execute v))))

(defn writeback
  "Pulls a writeback directive out of the processor state, and
  performs the indicated update on the processor state. Update command
  have been restructured and are now maps
  {:dst (U :registers :halt :memory) :addr Int :val Int}."

  [processor]
  (let [directive (get processor :result/execute isa/writeback-no-op)
        {:keys [dst addr val]} directive]
    (info "[writeback]" directive)
    (match [dst addr val]
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
      ,,(assoc-in processor [:registers addr] val)

      [:registers r val]
      ,,(assoc-in processor [:registers r] val)

      [:memory addr val]
      ,,(common/write-memory processor addr val))))

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

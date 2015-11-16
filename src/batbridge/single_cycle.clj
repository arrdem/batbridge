(ns batbridge.single-cycle
  "Implements a simple in-order processor. Makes no attempt to provide
  out-of-order or superscalar excutuion. Will serve as a benchmark for per-cycle
  operation performance and to allow correctness comparison between multiple
  differing processor implementations."
  (:require [clojure.core.match :refer [match]]
            [batbridge
             ,,[common :as common]
             ,,[isa :as isa]]
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
    (if-not (:halted processor)
      (-> processor
          (assoc-in [:registers 31] npc)
          (assoc :fetch {:icode icode
                         :npc   -1
                         :pc    npc}))
      (-> processor
          (assoc :fetch {:icode [:hlt 0 0 0 0]
                         :npc   -1
                         :pc    npc})))))

(defn decode
  "Dummy decode which translates a vector literal to an icode map. Not
  really important now because there is no binary decoding to do, but
  it'll be nice later."

  [processor]
  (let [{:keys [icode pc npc] :as fetch}
        (get processor :fetch {:icode isa/vec-no-op
                               :pc    -1})]
    (info "[decode   ]" icode)
    (info "[decode   ]" (-> icode
                            isa/decode-instr
                            common/fmt-instr))
    (as-> icode v
      (isa/decode-instr v)
      (assoc v :pc pc)
      (assoc v :npc npc)
      (update processor :decode merge v))))

(defn execute
  "Indexes into the opcode->fn map to fetch the implementation of the
  last decoded instruction. Decodes the parameter values, and applies
  the implementation function to the parameters. Sets the :execute key
  with a state update directive which can use. Returns the updated
  processor state."

  [processor]
  (let [{:keys [icode a b d i pc npc]
         :as   decode} (get processor :decode isa/map-no-op)
        srca           (common/register->val processor a pc i)
        srcb           (common/register->val processor b pc i)]
    (info "[execute  ]" decode)
    (as-> icode v
      (get isa/opcode->fn v)
      (v processor pc i srca srcb d)
      (common/upgrade-writeback-command v)
      (assoc v :pc pc)
      (assoc v :npc npc)
      (assoc processor :execute v))))

(defn writeback
  "Pulls a writeback directive out of the processor state, and
  performs the indicated update on the processor state. Update command
  have been restructured and are now maps
  {:dst (U :registers :halt :memory) :addr Int :val Int}."

  [processor]
  (let [directive (get processor :execute isa/writeback-no-op)
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
      ,,(-> processor
            (assoc-in [:registers addr] val))

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

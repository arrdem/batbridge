(ns batbridge.single-cycle
  "Implements a simple in-order processor. Makes no attempt to provide
  out-of-order or superscalar excutuion. Will serve as a benchmark for per-cycle
  operation performance and to allow correctness comparison between multiple
  differing processor implementations."
  (:require [batbridge [common :as common]
                       [isa :as isa]]))


(defn fetch 
  "Accesses the value of the PC register, fetches the instruction
  there from memory and sets the :fetch value key in the
  state. Increments the PC by the instruction width. Returns an
  updated state."

  [processor]
  (let [pc (common/get-register processor 31)
        icode (common/get-memory processor pc)]
    (println "[fetch    ]" pc "->" icode)
    (-> processor
        (update-in [:registers 31] (fn [x] (+ x 4)))
        (assoc :fetch {:icode icode :pc pc}))))


(defn decode
  "Dummy decode which translates a vector literal to an icode map. Not
  really important now because there is no binary decoding to do, but
  it'll be nice later."  

  [processor]
  (let [{:keys [icode pc] :as fetch} 
            (get processor :fetch {:icode isa/vec-no-op
                                   :pc    -1})]
    (println "[decode   ]" icode)
    (as-> icode v 
          (isa/decode-instr v)
          (assoc v :pc pc)
          (assoc processor :decode v))))


(defn execute 
  "Indexes into the opcode->fn map to fetch the implementation of the
  last decoded instruction. Decodes the parameter values, and applies
  the implementation function to the parameters. Sets the :execute key
  with a state update directive which can use. Returns the updated
  processor state."

  [processor]
  (let [{:keys [icode srca srcb dst imm pc]}
        (get processor :decode
             {:icode :add   :dst 0
              :srca  0      :srcb 30
              :imm   0      :pc -1})
        srca  (common/register->val processor srca pc imm)
        srcb  (common/register->val processor srcb pc imm)]
    (println "[execute  ]" (:decode processor))
    (as-> icode v
          (get isa/opcode->fn v)
          (v srca srcb processor dst)
          (common/upgrade-writeback-command v)
          (assoc v :pc (get-in processor [:decode :pc]))
          (assoc processor :execute v))))


(defn writeback 
  "Pulls a writeback directive out of the processor state, and
  performs the indicated update on the processor state. Update command
  have been restructured and are now maps
  {:dst (U :registers :halt :memory) :addr Int :val Int}."  

  [processor]
  (let [directive (get processor :execute isa/writeback-no-op)
        {:keys [dst addr val]} directive]
    (println "[writeback]" directive)
    (cond ;; special case to stop the show
          (= :halt dst)
            (assoc processor :halted true)
          
          ;; special case for hex code printing
          (and (= :registers dst)
               (= 29 addr))
            (do (when-not (zero? val)
                  (print (format "0x%X" (char val))))
                processor)

          ;; special case for printing
          (and (= :registers dst)
               (= 30 addr))
            (do (when-not (zero? val)
                  (print (char val)))
                processor)
            
          ;; special case for branching as we must flush the pipeline
          (and (= :registers dst)
               (= 31 addr))
            (do (println "[writeback] flushing pipeline!")
                (-> processor
                    (dissoc :fetch)
                    (dissoc :decode)
                    (dissoc :execute)
                    (assoc-in [:registers addr] val)))

          true
            (assoc-in processor [dst addr] val))))


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

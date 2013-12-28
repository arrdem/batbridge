(ns batbridge.pipeline
  "Implements a simple pipelined processor. This processor makes no
  effort to perform branch prediction, instruction caching or memory
  caching. Correctness with respect to the pipeline and bubbles is
  ensured however."
  (:require [batbridge.isa :refer :all
                           :exclude [register->val]]))


;; Differences from the single cycle implementations.
;;
;; For the most part, the single cycle processor implementation is
;; preserved. However, the pipelined instruction model requires more
;; book keeping to track the PC value from which an instruction was
;; fetched as well as other metadata about the processor state. In
;; order to accomidate this metadata, the following changes to the
;; single cycle processor data representation have been made.
;;
;; :fetch is now a map {:pc Int, :icode Icode} [DONE]
;; :decode now has a :pc key
;; :execute is now a map {:cmd (U :registers :memory :halt),
;;                        :dst Int, :val Int}
;;
;; These changes provide the information required to correctly present
;; the PC value as specified by the ISA on a by-instruction basis as
;; well as to store the data which will be required for branch
;; prediction when the time comes to add such prediction.

;; FIXME:
;;    This opcode listing is not specification complete. The control
;;    instructions and a number of the data instructions are missing.
(def opcode->fn
  "Maps opcode names to implementing functions."
  {:add  (fn [x y _ dst] [:registers dst (+ x y)])
   :sub  (fn [x y _ dst] [:registers dst (- x y)])
   :mul  (fn [x y _ dst] [:registers dst (* x y)])
   :div  (fn [x y _ dst] [:registers dst (/ x y)])
   :mod  (fn [x y _ dst] [:registers dst (mod x y)])
   :shr  (fn [x y _ dst] [:registers dst (bit-shift-right x y)])
   :shl  (fn [x y _ dst] [:registers dst (bit-shift-left x y)])
   :ld   (fn [x y p dst] [:registers dst (get-memory p (+ x (* 4 y)))])
   :st   (fn [x y p dst] [:memory (+ x (* 4 y)) (get-register p dst)])
   :halt (fn [_ _ _ _]   [:halt nil nil])
   })


(defn get-instr
  "Special case of memory access which yields an addition no-op if
  there is no value at the target memory address."

  [processor addr]
  (get-in processor [:memory addr] [:add 0 0 30 0]))


(defn upgrade-writeback-command
  "Transforms an old vector writeback command into the new map
  structure, thus allowing for pc data to be preserved."
  
  [[dst addr v]]
  {:dst dst :addr addr :val v})


(defn register->val
  "Helper function to compute a value from either a keyword register
  alias or an integer register identifier. Returnes the value of
  accessing the identified register. Note that this differs from
  batbridge.isa/register->val in that it provides for yielding the
  address of the next instruction to be executed in a pipelined
  execution environment."

  [reg processor]
  (case reg
    (:r_PC   31) (+ (get-in processor [:decode :pc]) 4)
    (:r_ZERO 30) 0
    (:r_IMM  29) (get (get processor :decode {}) :lit 0)
    (-> processor :registers (get reg 0))))


(defn fetch 
  "Accesses the value of the PC register, fetches the instruction
  there from memory and sets the :fetch value key in the
  state. Increments the PC by the instruction width. Returns an
  updated state."

  [processor]
  (let [pc (get-register processor 31)
        icode (get-instr processor pc)]
    (assert (vector? icode))
    (println "[fetch    ]" pc "->" icode)
    (-> processor
        (update-in [:registers 31] (fn [x] (+ x 4)))
        (assoc :fetch {:icode icode :fpc pc}))))


(defn decode
  "Dummy decode which translates a vector literal to an icode map. Not
  really important now because there is no binary decoding to do, but
  it'll be nice later."  

  [processor]
  (let [icode (get-in processor [:fetch :icode] [:add 0 0 30 0])]
    (println "[decode   ]" icode)
    (assoc processor :decode
           {:icode (nth icode 0 :add)
            :dst   (get register-symbol-map (nth icode 1 0) 0)
            :srca  (get register-symbol-map (nth icode 2 0) 0)
            :srcb  (get register-symbol-map (nth icode 3 0) 0)
            :lit   (nth icode 4 0)
            :pc    (:pc (:fetch processor))})))


(defn execute 
  "Indexes into the opcode->fn map to fetch the implementation of the
  last decoded instruction. Decodes the parameter values, and applies
  the implementation function to the parameters. Sets the :execute key
  with a state update directive which can use. Returns the updated
  processor state."

  [processor]
  (let [{:keys [icode srca srcb dst lit pc]} 
        (get processor :decode
             {:icode :add   :dst 0
              :srca 0       :srcb 30
              :lit 0        :pc -1})
        srca  (register->val srca processor)
        srcb  (register->val srcb processor)]
    (println "[execute  ]" (:decode processor))
    (as-> icode v
          (get opcode->fn v (constantly :halt))
          (v srca srcb processor dst)
          (upgrade-writeback-command v)
          (assoc v :pc (get-in processor [:decode :pc]))
          (assoc processor :execute v))))


(defn writeback 
  "Pulls a writeback directive out of the processor state, and
  performs the indicated update on the processor state. Update command
  have been restructured and are now maps
  {:dst (U :registers :halt :memory) :addr Int :val Int}."  

  [processor]
  (let [directive (get processor :execute [:registers 30 0])
        {:keys [dst addr val]} directive]
    (println "[writeback]" directive)
    (cond ;; special case to stop the show
          (= :halt dst)
            (assoc processor :halted true)
          
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
  computation's effects on the processor. Because this is a pipelined
  design, we simply take the in-order steps and run them in the
  reverse order using the processor state as a set of latches for
  storing the state between clock 'cycles'."

  [state]
  (-> state
      writeback
      execute
      decode
      fetch))


(defn -main
  "Steps a processor state until such time as it becomes marked as
  'halted'. Makes no attempt to construct or validate a processor
  state, or handle errors therein." 

  [state]
  (loop [state state]
    (if-not (halted? state)
      (do (println "-------------------------------------------------")
          (recur (step state)))
      state)))

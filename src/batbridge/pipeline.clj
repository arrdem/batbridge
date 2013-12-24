(ns batbridge.pipeline
  "Implements a simple pipelined processor. This processor makes no
  effort to perform branch prediction, instruction caching or memory
  caching. Correctness with respect to the pipeline and bubbles is
  ensured however.")

;; Instructions are represented as being length five vectors of the
;; structure [icode dst param param lit]. Instructions are stored in
;; state memory as one would expect.

(defn seq->instrs [seq]
  (zipmap (range 0 (* 4 (count seq)) 4)
          seq))

(defn instrs->state [instrs]
  {:memory instrs
   :registers {31 0}})

;; The state structure will be represented as a map:
;; {:memory {<int> <val>}
;;  :registers {<int> <val>}
;;  :halted <boolean>
;; }

;; In addition, the :fetch, :decode, :execute and :writeback keys will
;; be used to store intermediate values used in computing the next
;; state of the processor.

(defn get-memory 
  "Accesses an address in a processor state's memory, returning the
  value there."
  [p addr]
  (get-in p [:memory addr] 0))

(defn get-instr
  "Accesses an address in a processor state's memory, returning the
  value there. Provides a sane default for fetching instructions"
  [p addr]
  (get-in p [:memory addr] [:add 0 0 0 0]))

(defn write-memory 
  "Writes the argument value into the processor state's memory,
  returning an updated processor state representation."
  [p addr v]
  (assoc-in p [:memory addr] v))

(defn get-register
  "Fetches a register value by register ID from a processor state,
  returning the value."
  [p reg]
  (get-in p [:registers reg] 0))

(defn halted? 
  "Predicate to test whether a processor state record is halted."
  [state]
  (or (:halted state) false))

(defn register->val 
  "Helper function to compute a value from either a keyword register
  alias or an integer register identifier. Returnes the value of
  accessing the identified register."
  [reg processor]
  (case reg
    (:r_ZERO 0) 0
    (:r_IMM 30) (get (get processor :decode {}) :lit 0)
    (:r_PC 31)  (get-register processor 31)
    (-> processor 
        :registers
        (get reg 0))))

(def register-symbol-map
  "Maps register abbreviations and IDs to register IDs."
  (-> (zipmap (range 30) (range 30))
      (assoc :r_PC 31)
      (assoc :r_IMM 30)
      (assoc :r_ZERO 0)))

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
   :halt (fn [_ _ _ _]   :halt)
   })


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
        (assoc :fetch icode))))


(defn decode
  "Dummy decode which translates a vector literal to an icode map. Not
  really important now because there is no binary decoding to do, but
  it'll be nice later."  
  [processor]
  (let [icode (get processor :fetch [:add 0 0 0 0])]
    (println "[decode   ]" icode)
    (assoc processor :decode
           {:icode (nth icode 0 :add)
            :dst   (get register-symbol-map (nth icode 1 0) 0)
            :srca  (get register-symbol-map (nth icode 2 0) 0)
            :srcb  (get register-symbol-map (nth icode 3 0) 0)
            :lit   (nth icode 4 0)})))


(defn execute 
  "Indexes into the opcode->fn map to fetch the implementation of the
  last decoded instruction. Decodes the parameter values, and applies
  the implementation function to the parameters. Sets the :execute key
  with a state update directive which can use. Returns the updated
  processor state."
  [processor]
  (let [icode (:decode processor)
        srca  (register->val (get icode :srca 0) processor)
        srcb  (register->val (get icode :srcb 0) processor)
        dst   (get icode :dst 0)
        icode (get icode :icode :add)]
    (println "[execute  ]" (:decode processor))
    (as-> icode v
          (get opcode->fn v (constantly :halt))
          (v srca srcb processor dst)
          (assoc processor :execute v))))


(defn writeback 
  "Pulls a writeback directive out of the processor state, and
  performs the indicated update on the processor state. Updates are
  either the value :halt, or vectors [:memory <addr> <val>] or
  [:registers <addr> <val>]. Returns an updated state."
  [processor]
  (let [directive (get processor :execute [:registers 0 0])]
    (println "[writeback]" directive)
    (cond ;; special case to stop the show
          (= :halt directive)
            (assoc processor :halted true)
          
          ;; special case for printing
          (and (= :registers (first directive))
               (= 0 (second directive)))
          (do (when-not (zero? (nth directive 2 0))
                (print (char (nth directive 2 0))))
                processor)

          ;; special case for branching as we must flush the pipeline
          (and (= :registers (first directive))
               (= 31 (second directive)))
            (do (println "[writeback] flushing pipeline!")
                (-> processor
                    (dissoc :fetch)
                    (dissoc :decode)
                    (dissoc :execute)
                    (assoc-in [(nth directive 0 :registers) (nth directive 1 0)]
                              (nth directive 2 0))))

          true
          (assoc-in processor [(nth directive 0 :registers)
                               (nth directive 1 0)]
                 (nth directive 2 0)))))


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

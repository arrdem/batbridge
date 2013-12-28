(ns batbridge.single-cycle
  "Implements a simple in-order processor. Makes no attempt to provide
  out-of-order or superscalar excutuion. Will serve as a benchmark for per-cycle
  operation performance and to allow correctness comparison between multiple
  differing processor implementations."
  (:require [batbridge [isa :refer :all]]))


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
        icode (get-memory processor pc)]
    (assert (vector? icode))
    (println "[fetch]" pc icode)
    (-> processor
        (update-in [:registers 31] (fn [x] (+ x 4)))
        (assoc :fetch icode))))


(defn decode
  "Dummy decode which translates a vector literal to an icode map. Not
  really important now because there is no binary decoding to do, but
  it'll be nice later."  

  [processor]
  (let [icode (:fetch processor)]
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
        dst   (get register-symbol-map (get icode :dst 0) 0)
        icode (get icode :icode :add)]
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
  (let [directive (:execute processor)]
    (cond ;; special case to stop the show
          (= :halt directive)
            (assoc processor :halted true)
          
          ;; special case for printing - char
          (and (= :registers (first directive))
               (= 30 (second directive)))
            (do (print (char (nth directive 2 0)))
                processor)

          ;; special case for printing - hex
          (and (= :registers (first directive))
               (= 29 (second directive)))
            (do (print (format "0x%X" (nth directive 2 0)))
                processor)

          true
          (assoc-in processor [(nth directive 0 :registers)
                               (nth directive 1 0)]
                 (nth directive 2 0)))))


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


(defn -main
  "Steps a processor state until such time as it becomes marked as
  'halted'. Makes no attempt to construct or validate a processor
  state, or handle errors therein." 

  [state]
  (loop [state state]
    (if-not (halted? state)
      (recur (step state))
      state)))

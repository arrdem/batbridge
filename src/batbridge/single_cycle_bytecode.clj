(ns batbridge.single-cycle-bytecode
  "Implements a simple in-order processor. Makes no attempt to provide
  out-of-order or superscalar excutuion. Will serve as a benchmark for
  per-cycle operation performance and to allow correctness comparison
  between multiple differing processor implementations. This
  implementation differes from the normal single cycle processor in
  that it uses a byte encoded instruction representation rather than a
  keyword/integer vector."
  (:require [batbridge [bytecode :refer :all]
                       [isa :refer :all]]))


;; The state structure will be represented as a map:
;; {:memory {<int> <val>}
;;  :registers {<int> <val>}
;;  :halted <boolean>
;; }
;;
;; In addition, the :fetch, :decode, :execute and :writeback keys will
;; be used to store intermediate values used in computing the next
;; state of the processor.


(def opcode->fn
  "Maps opcode codes to implementing functions. Mapping taken from
  doc/batbridge.md, in accordance with the batbridge ISA encoding
  specification."

  {
   0x0   (fn [_ _ _ _  ] :halt)
   0x10  (fn [x y p dst] [:registers dst (get-memory p (+ x (* 4 y)))])
   0x11  (fn [x y p dst] [:memory (+ x (* 4 y)) (get-register p dst)])

   0x20  (fn [x y p dst]
           (let [pc (get-register 31)]
             [:registers 31 (if (< x y) pc (+ pc 4))]))

   0x21  (fn [x y p dst]
           (let [pc (get-register 31)]
             [:registers 31 (if (<= x y) pc (+ pc 4))]))

   0x22 (fn [x y p dst]
           (let [pc (get-register 31)]
             [:registers 31 (if (= x y) pc (+ pc 4))]))

   0x23 (fn [x y p dst]
           (let [pc (get-register 31)]
             [:registers 31 (if (!= x y) pc (+ pc 4))]))

   0x30  (fn [x y _ dst] [:registers dst (+ x y)])
   0x31  (fn [x y _ dst] [:registers dst (- x y)])
   0x32  (fn [x y _ dst] [:registers dst (/ x y)])
   0x33  (fn [x y _ dst] [:registers dst (mod x y)])
   0x34  (fn [x y _ dst] [:registers dst (* x y)])
   0x35  (fn [x y _ dst] [:registers dst (bit-and x y)])
   0x36  (fn [x y _ dst] [:registers dst (bit-or x y)])
   0x37  (fn [x y _ dst] [:registers dst (bit-not (bit-and x y))])
   0x37  (fn [x y _ dst] [:registers dst (bit-xor x y)])
   0x3A  (fn [x y _ dst] [:registers dst (bit-shift-left x y)])
   0x3B  (fn [x y _ dst] [:registers dst (bit-shift-right x y)])
   })


;; Implement the processor state transforms
;;------------------------------------------------------------------------------
(defn fetch
  "Accesses the value of the PC register, fetches the instruction
  there from memory and sets the :fetch value key in the
  state. Increments the PC by the instruction width. Returns an
  updated state."

  [processor]
  (let [pc (get-register processor 31)
    icode (get-memory processor pc)]
    (println "[fetch]" pc icode)
    (-> processor
    (update-in [:registers 31] (fn [x] (+ x 4)))
    (assoc :fetch icode))))


(defn decode
  "Dummy decode which translates a vector literal to an icode map. Not
  really important now because there is no binary decoding to do, but
  it'll be nice later."

  [processor]
  (let [icode (get processor :fetch 0)
        m     (word->symbol-map icode)]
    (println "[decode]" m)
    (assoc processor :decode m)))


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
    (println "[execute]" (:decode processor))
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
    (println "[writeback]" directive)
    (cond
      ;; special case to stop the show
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
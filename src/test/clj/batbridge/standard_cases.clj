(ns batbridge.standard-cases
  "Defines a series of standard test cases for processors, being an
  input memory and a final register state with a bound on the number
  of cycles to run."
  (:require [batbridge
             [bytecode :as b]
             [common :as c]
             [isa :as i]]
            [clojure.test :as t]
            [taoensso.timbre :as timbre]
            [toothpick.assembler :refer [assemble]]
            [toothpick.isa.batbridge :refer [batbridge]]))

(defrecord ps-test [state predicate])

(defmacro deftest [sym opcodes predicate]
  `(let [state# (c/instrs->state ~opcodes)]
     (def ~sym (->ps-test state# ~predicate))
     (def ~(symbol (str (name sym) "-state")) state#)))

(defn run-test
  "Destructures and runs a given test record using the specified step
  function. This makes run-test parametric on what simulator is to be
  used in running the unstructions. Uses clojure.test/is for assertion
  checking."

  [test step bound]
  (timbre/with-log-level :warn
    (let [{:keys [state predicate]} test]
      ;; (println "[]" opcodes)
      ;; (println "[]" predicate)
      ;; (println "[]" bound)
      (t/is (predicate
             (loop [state state
                    bound bound]
               (if (or (c/halted? state)
                       (= bound 0))
                 state
                 (recur (step state)
                        (dec bound)))))))))


;; Test suite based on computing fibonacci numbers
;;------------------------------------------------------------------------------
(defn r:= [r n]
  [:add r 30 29 n])

(def fib-icodes
  [
   ;; init
   (r:= 0 14)
   (r:= 1 1)
   (r:= 2 0)

   ;; top
   [:ifeq 0 30 0]
   [:add 31 31 29 20] ; jump to the end
   [:sub 0 0 29 1]
   [:add 3 1 2 0] ; (+ fib₋₁ fib₋₂)
   [:add 2 1 30 0] ; fib₋₂ := fib₋₁
   [:add 1 3 30 0] ; fib₋₁ := r3 (subexpr)
   [:sub 31 31 29 28]

   ;; fall out
   [:hlt]
   ])

(deftest fib-test
  ;; This test computes fib(14) and stores the result to r2.
  ;; r0 is used to store fib(n-2)
  ;; r1 is used to store fib(n-1)
  ;; r2 is a scratch register used to store fib(n)
  ;; r3 is used to count down from 14 for loop control

  (c/seq->instrs fib-icodes)

  (fn [state]
    (t/is (= (c/register->val state 1) 610))
    (t/is (c/halted? state))
    true))

(deftest fib-byte-test
  ;; This test does the same thing as fib-test, but using byte encoded
  ;; instructions from Toothpick.
  (->> fib-icodes
       (assemble batbridge)
       (c/seq->instrs))
  
  (fn [state]
    (t/is (= (c/register->val state 1) 610))
    (t/is (c/halted? state))
    true))

(t/deftest fib-encoding-equality-test
  ;; Tests to make sure that round tripping through the assembler does
  ;; no damange to the instruction sequence for the fib test.
  (doseq [p (->> fib-icodes
                 (assemble batbridge)
                 (map b/word->symbol-map)
                 (map i/decode-instr)
                 (map c/fmt-instr)
                 (map vector fib-icodes))]
    (t/is (= (first p)
             (second p)))))

;; Test suite based on computing factorials
;;------------------------------------------------------------------------------
(def fact-icodes
  [
   [:add  0  30 29 1 ] ;; preload fact(0)
   [:add  1  30 29 10] ;; the loop bound

   ;; write the fact loop
   [:mul  0  0  1  0 ] ;; Multiply fact(n-1) and n
   [:sub  1  1  29 1 ] ;; dec the loop constant
   [:ifne    1  30 0 ] ;; test if we've zeroed the loop counter yet
   [:add  31 30 29 8 ] ;; if not jump to the top of the loop
   [:hlt             ] ;; otherwise halt
   ])

(deftest fact-test
  ;; This test computes fact(10) and stores the result to r0. The
  ;; control flow used is essentially the same as that in fib-test.
  ;; This is essentially the same as (reduce * (range 1 11)).

  (c/seq->instrs fact-icodes)

  (fn [state]
    (t/is (= (c/register->val state 0)
             3628800))                ;; fact(10)
    (t/is (c/halted? state))          ;; the processor should have halted correctly
    true))

(deftest fact-byte-test
  ;; This test computes fact(10) and stores the result to r0. The
  ;; control flow used is essentially the same as that in fib-test.
  ;; This is essentially the same as (reduce * (range 1 11)).

  (->> fact-icodes
       (assemble batbridge)
       (c/seq->instrs))

  (fn [state]
    (t/is (= (c/register->val state 0)
             3628800))                ;; fact(10)
    (t/is (c/halted? state))          ;; the processor should have halted correctly
    true))

(t/deftest fact-encoding-equality-test
  ;; Tests to make sure that round tripping through the assembler does
  ;; no damange to the instruction sequence for the fib test.
  (doseq [p (->> fact-icodes
                 (assemble batbridge)
                 (map b/word->symbol-map)
                 (map i/decode-instr)
                 (map c/fmt-instr)
                 (map vector fact-icodes))]
    (t/is (= (first p) (second p)))))

;; Test to make sure that ld/st works
;;--------------------------------------------------------------------------------
;; A modified factorial program which goes to memory every single time

;; r0, the base pointer, 0x100
;; r1, the offset pointer, 0x0 and incrementing
;; r2, the previous value as loaded from mem

(def ld-st-icodes
  [[:add  0  30  29 (- 0x100 4)]
   [:add  1  30  29 1 ]
   [:st   1  0   1  0 ] ;; write r0 -> base + offset

   ;; write the fact loop
   [:ld   2  0  1  0   ] ;; load r0+r1*4 -> r2
   [:mul  3  1  2  0   ] ;; Multiply fact(n-1) and n
   [:add  1  1  29 1   ] ;; increment the memory offset
   [:st   3  0  1  0   ] ;; store r3 -> r0+r1*4 where it will be read from into r2 next trip
   [:ifne    1  29 12  ] ;; test if we've zeroed the loop counter yet
   [:add  31 30 29 12  ] ;; if not jump to the top of the loop

   [:hlt               ] ;; otherwise halt
   ])

(defn- fact [i]
  (if (zero? i) 1
      (apply * (range 1 (inc i)))))

(deftest memory-fact-test
  (->> ld-st-icodes
       (assemble batbridge)
       (c/seq->instrs))

  (fn [state]
    (dotimes [i 10]
      (t/is (= (c/get-memory state (+ 256 (* 4 i)))
               (fact i))))
    true))

(def push-icodes
  [[:add  0  30  29 1000] ; preload r0 with a value
   [:add  28 0   0  0]    ; preload the stack pointer with a value
   [:push 0  28  0  0]    ; push
   [:hlt             ]])

(deftest push-test
  (->> push-icodes
       #_(assemble batbridge)
       (c/seq->instrs))

  (fn [state]
    (t/is (c/halted? state))
    (t/is (= (c/get-memory state 1996) 1000))
    (t/is (= (c/register->val state 28) 1996))
    true))

(def pop-icodes
  [[:add  0  30  29 1000] ; preload r0 with a value
   [:add  28 0   0  0]    ; preload the stack pointer with a value
   [:st   0  28  30 0]    ; write value to pop
   [:pop  1  28  0  0]    ; should pop value of 2k to r1
   [:hlt             ]])

(deftest pop-test
  (->> pop-icodes
       #_(assemble batbridge)
       (c/seq->instrs))

  (fn [state]
    (t/is (c/halted? state))
    (t/is (= (c/get-memory state 2000) 1000))
    (t/is (= (c/register->val state 28) 2004))
    true))

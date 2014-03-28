(ns batbridge.standard-cases
  "Defines a series of standard test cases for processors, being an
  input memory and a final register state with a bound on the number
  of cycles to run."

  (:require [batbridge [assembler :as a]
                       [common    :as c]
                       [isa       :as i]
                       [bytecode  :as b]]
            [toothpick.assembler :refer [assemble]]
            [toothpick.isa.batbridge :refer [batbridge]]
            [taoensso.timbre :as timbre]
            [clojure.test :as t]))


(defrecord ps-test [opcodes predicate])


(defmacro deftest [sym opcodes predicate]
  `(def ~sym (->ps-test ~opcodes ~predicate)))


(defn run-test
  "Destructures and runs a given test record using the specified step
  function. This makes run-test parametric on what simulator is to be
  used in running the unstructions. Uses clojure.test/is for assertion
  checking."

  [test step bound]
  (timbre/with-log-level :warn
    (let [{:keys [opcodes predicate]} test
          state (c/instrs->state opcodes)]
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
(def fib-icodes
  [;; load constants into registers
   [:add  0  30 29 0 ] ;; preload fib(0)
   [:add  1  30 29 1 ] ;; preload fib(1)
   [:add  3  30 29 14] ;; the loop bound

                       ;; write the fib loop
   [:add  2  1  0  0 ] ;; add fib(n-2) + fib(n-1), store to fib
   [:add  0  1  30 0 ] ;; move fib(n-1) down
   [:add  1  2  30 0 ] ;; move fib(n) down
   [:sub  3  3  29 1 ] ;; dec the loop constant
   [:ifne 0  3  30 0 ] ;; test if we've zeroed the loop counter yet
   [:add  31 30 29 12] ;; if not jump to the top of the loop
   [:hlt  0  0  0  0 ] ;; otherwise halt
   ])

(deftest fib-test
  ;; This test computes fib(15) and stores the result to r2.
  ;; r0 is used to store fib(n-2)
  ;; r1 is used to store fib(n-1)
  ;; r2 is a scratch register used to store fib(n)
  ;; r3 is used to count down from 15 for loop control

  (c/seq->instrs fib-icodes)

  (fn [state]
    (t/is (= (c/get-register state 2)  ;; fib 15 with th (0,1) as the base case
             610))
    (t/is (c/halted? state))           ;; the processor should have halted correctly
    true))

(deftest fib-byte-test
  ;; This test does the same thing as fib-test, but using byte encoded
  ;; instructions from Toothpick.
  (->> fib-icodes
       (assemble batbridge)
       (c/seq->instrs))
  
  (fn [state]
    (t/is (= (c/get-register state 2)  ;; fib 15 with th (0,1) as the base case
             610))
    (t/is (c/halted? state))           ;; the processor should have halted correctly
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
    (t/is (= (first p) (second p)))))


;; Test suite based on computing factorials
;;------------------------------------------------------------------------------
(def fact-icodes
  [
   [:add  0  30 29 1 ] ;; preload fact(0)
   [:add  1  30 29 10] ;; the loop bound

                       ;; write the fact loop
   [:mul  0  0  1  0 ] ;; Multiply fact(n-1) and n
   [:sub  1  1  29 1 ] ;; dec the loop constant
   [:ifne 0  1  30 0 ] ;; test if we've zeroed the loop counter yet
   [:add  31 30 29 8 ] ;; if not jump to the top of the loop
   [:hlt  0  0  0  0 ] ;; otherwise halt
   ])

(deftest fact-test
  ;; This test computes fact(10) and stores the result to r0. The
  ;; control flow used is essentially the same as that in fib-test.
  ;; This is essentially the same as (reduce * (range 1 11)).

  (c/seq->instrs fact-icodes)

  (fn [state]
    (t/is (= (c/get-register state 0)
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
    (t/is (= (c/get-register state 0)
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

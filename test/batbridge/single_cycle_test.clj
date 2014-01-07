(ns batbridge.single-cycle-test
  (:require [batbridge.single-cycle :as ss]
            [batbridge.standard-cases :as sc]
            [clojure.test :refer :all]))


(def bound 100)


(deftest fib-test
  ;; runs the fib test on the single cycle machine
  (sc/run-test sc/fib-test ss/step bound)
  (sc/run-test sc/fib-byte-test ss/step bound))

(deftest fact-test
  ;; runs the factorial test on the single cycle machine
  (sc/run-test sc/fact-test ss/step bound)
  (sc/run-test sc/fact-byte-test ss/step bound))

(ns batbridge.single-cycle-test
  (:require [batbridge.single-cycle :as ss]
            [batbridge.standard-cases :as sc]
            [clojure.test :refer :all]))


(deftest fib-test
  ;; runs the fib test on the single cycle machine
  (sc/run-test sc/fib-test ss/step))


(deftest fact-test
  ;; runs the factorial test on the single cycle machine
  (sc/run-test sc/fact-test ss/step))

(ns batbridge.pipeline-test
  (:require [batbridge.pipeline :as p]
            [batbridge.standard-cases :as sc]
            [clojure.test :refer :all]))


(deftest fib-test
  ;; runs the fib test on the single cycle machine
  (sc/run-test sc/fib-test p/step))


(deftest fact-test
  ;; runs the factorial test on the single cycle machine
  (sc/run-test sc/fact-test p/step))

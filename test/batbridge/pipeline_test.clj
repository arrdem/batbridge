(ns batbridge.pipeline-test
  (:require [batbridge.pipeline :as p]
            [batbridge.standard-cases :as sc]
            [clojure.test :refer :all]))


(def bound 300)


(deftest fib-test
  ;; runs the fib test on the single cycle machine
  (sc/run-test sc/fib-test p/step bound))


(deftest fact-test
  ;; runs the factorial test on the single cycle machine
  (sc/run-test sc/fact-test p/step bound))

(ns batbridge.predicted-pipeline-test
  (:require [batbridge.predicted-pipeline :as pp]
            [batbridge.standard-cases :as sc]
            [clojure.test :refer :all]))

(def bound 200)

(deftest fib-test
  ;; runs the fib test on the single cycle machine
  (sc/run-test sc/fib-test pp/step bound)
  (println "--------------------------------------------------------------------------------")
  (sc/run-test sc/fib-byte-test pp/step bound))

(deftest fact-test
  ;; runs the factorial test on the single cycle machine
  (sc/run-test sc/fact-test pp/step bound)
  (println "--------------------------------------------------------------------------------")
  (sc/run-test sc/fact-byte-test pp/step bound))

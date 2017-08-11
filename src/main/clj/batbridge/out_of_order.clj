(ns batbridge.out-of-order
  "Implements an out of order pipeline on the basis of the
  scoreboarding algorithm."
  (:require [batbridge
             [single-cycle :as ss]
             [pipeline :as p]
             [predicted-pipeline :as pp]
             [common :as common]]
            [taoensso.timbre :refer [info warn]]))

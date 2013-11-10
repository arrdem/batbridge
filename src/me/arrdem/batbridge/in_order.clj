(ns me.arrdem.batbridge.in-order
  "Implements a simple in-order processor. Makes no attempt to provide
  out-of-order or superscalar excutuion. Will serve as a benchmark for per-cycle
  operation performance and to allow correctness comparison between multiple
  differing processor implementations."
  (:gen-class)
  (:require [clojure.tools.logging :refer [error info]]))

;; Define the Processor structure, and sketch some functions to operate thereon
;;------------------------------------------------------------------------------
;; Processor:
;; {:registers -> {<name>  -> Integer}
;;  :memory    -> {Integer -> (U Instruction Integer)}
;; }
;;
;; Note that this is a Von Neuman architecture machine, with a single shared
;; memory for both instructions and data. This would play hell with our
;; simulator, were it not for the fact that we're going to cheat and use the
;; Toothpick assembler library to throw all the Batbridge simulators the same
;; standard bytecode.

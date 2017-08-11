(ns batbridge.vector
  "Implements a vector width 2 pipeline pair."
  (:require [batbridge
             [single-cycle :as ss]
             [pipeline :as p]
             [predicted-pipeline :as pp]
             [common :as common]]
            [taoensso.timbre :refer [info warn]]))


;; The state structure is a map of the form
;;
;; {:halted bool
;;  :registers {int int}
;;  :memory    {int int}}
;;
;; That may not be the precise model of memory but it's the one we'll
;; work with since the cache library I built out is fairly
;; transparent.
;;
;; In the pipelined processor, there was a single :stall count used to
;; track how long we needed to keep waiting before the next
;; instruction could be processed in the case of a data dependency
;; conflict.
;;
;; In the case of wanting to issue many instructions per cycle
;; however, we have to do a dance wherein first we fetch n-wide, then
;; we decode n-wide, then we have to check the data dependencies of
;; the decoded instructions and the in-flight instructions to ensure
;; that there aren't data dependency issues.
;;
;; To be specific for the sake of building an accurate simulator: An
;; instruction in decode/dependency check cannot read from any
;; register in the write set of the execute instructions, OR in the
;; write set of the OTHER instructions in the fetch block.
;;
;; For instance:
;; ADD 32 %0 %1 0
;; ADD %0 %0 %0 0
;; ADD %0 0  %1 0
;;
;; On a 2-wide machine the first two instructions cannot be executed
;; at once - they have a sequential data dependency. However the
;; second and third instructions are not mutually dependent and may be
;; issued together. However this statement of dependency cannot be
;; made until after we've already decoded the instructions. So our
;; pipeline winds up looking something like this:
;;
;; fetch %pc | fetch %pc+4
;;   decode  |  decode
;;   check dependencies
;;  execute  |  execute
;;      writeback
;;
;; This pipeline cannot go out of order. If we stall %pc, we must also
;; stall the other fetch unit in order to ensure effect sequencing.

(defn fetch
  "The defining characteristic of a vector processor is that it has
  several pipelines side by side, such that limited instruction level
  parallelism can be exploited between two pipelines.

  This means that each cycle two or more instructions are fetched and
  fed into a pair of parallel pipelines.

  The hard part is checking for data conflicts and stalling the
  pipeline which is ahead, or flushing both as appropriate."
  [state]
  )

(defn step
  "Sequentially applies each phase of execution to a single processor
  state, returning an updated state representing a single cycle of
  computation's effects on the processor."

  [state]
  (-> state
      fetch
      decode
      check-data-dependencies
      execute
      writeback))

(defn -main
  "Steps a processor state until such time as it becomes marked as
  'halted'. Makes no attempt to construct or validate a processor
  state, or handle errors therein."

  [state]
  (loop [state state]
    (if-not (common/halted? state)
      (recur (step state))
      state)))

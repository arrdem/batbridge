(ns batbridge.pipeline
  "Implements a simple pipelined processor. This processor makes no
  effort to perform branch prediction, instruction caching or memory
  caching. Correctness with respect to the pipeline and bubbles is
  ensured however."
  (:require [batbridge
             [isa :as isa]
             [common :as common]
             [single-cycle :as ss]]
            [taoensso.timbre :refer [info warn]]
            [clojure.set :as set]
            [clojure.core.match :refer [match]]))

;; There is an extra issue in the pipelined processor, one which I
;; completely forgot about until misbehavior in my test suite exposed
;; it from its lurkings.
;;
;; When it takes nonzero time for the "previous" instruction to
;; register changes in memory or in registers, it is very possible
;; that if the "subsequent" instruction depends on the change to be
;; made by the previous instruction one cannot simply pipeline all the
;; instructions together. Data changes made by previous instructions
;; must _if relevant_ be visible to subsequent instructions.
;;
;; We already have pipeline flushing on branching, now we need
;; pipeline stalling on dependency conflicts. That is, if the source
;; of the following instruction is the destination of the instruction
;; in decode, the instruction in decode must be delayed so that when
;; it executes the preceding change will be visible.
;;
;; In order to provide this behavior I will add a :stall key to the
;; pipelined processor's state. Every cycle, the :stall key will be
;; decremented _by step_, bounded at zero. If the stall key is
;; nonzero, then Fetch and Decode become the identity function. That
;; is they do no work until the value in Decode becomes
;; meaningful. The exception to this rule is a pipeline flush, which
;; zeros the stall counter and clears all stages.

(defn decode
  "Checks the stall counter, invoking ss/decode and checking for a
  pipeline hazzard in the result if zero otherwise returning the input
  state unmodified due to a pipeline stall."

  [processor]
  (let [next-processor     (ss/decode processor)
        {:keys [pc]
         :as   decode}     (:decode/result next-processor)
        execute            (:execute/result next-processor)
        {:keys [dst addr]} execute]
    (if (and (= :registers dst)
             (contains? (-> #{}
                            (into [(:a decode) (:b decode)])
                            (disj 30 29))
                        addr))
      (do (info "[decode   ] stalling the pipeline!")
          (-> processor
              (update :fetch/stall (fnil inc 0))
              (dissoc :decode/result
                      :fetch/result)
              (common/write-register 31 (- pc 4))))
      next-processor)))

(defn writeback
  "Pulls a writeback directive out of the processor state, and
  performs the indicated update on the processor state. Update command
  have been restructured and are now maps
  {:dst (U :registers :halt :memory) :addr Int :val Int}."

  [processor]
  (let [directive                  (get processor
                                        :execute/result
                                        [:registers 30 0])
        {:keys [dst addr val pc]} directive

        ;; Use the perfectly functional single cycle implementation of
        ;; all this stuff
        processor                  (ss/writeback processor)]
    (match [dst addr val]
      ;; Case of writing a the next PC back to the PC, in which case
      ;; we don't have to stall or do anything fancy because life is
      ;; good.
      [:registers 31 pc]
      ,,(do (info "[writeback] Next PC is" pc "not flushing pipeline")
            processor)

      ;; Case of writing a different value to the PC, this being a
      ;; branch and forcing pipeline stall.
      [:registers 31 _]
      ,,(do (warn "[writeback] Flushing pipeline!")
            (-> processor
                (dissoc :fetch/result
                        :decode/result)))

      :else
      ,,processor)))

(defn stall-dec
  "A dec which deals with a nil argument case, and has a floor value
  of 0."

  [{:keys [fetch/stall]
    :as processor
    :or {stall 0}}]
  (info "[stall-dec] decrementing stall count" stall)
  (update-in processor [:fetch/stall]
             (fn [nillable-value]
               (let [v (or nillable-value 0)]
                 (max 0 (dec v))))))

(defn step
  "Sequentially applies each phase of execution to a single processor
  state, returning an updated state representing a single cycle of
  computation's effects on the processor. Because this is a pipelined
  design, we simply take the in-order steps and run them in the
  reverse order using the processor state as a set of latches for
  storing the state between clock 'cycles'."

  [state]
  (-> state
      writeback
      ss/execute
      decode
      ss/fetch
      stall-dec))

(defn -main
  "Steps a processor state until such time as it becomes marked as
  'halted'. Makes no attempt to construct or validate a processor
  state, or handle errors therein."

  [state]
  (loop [state state]
    (if-not (common/halted? state)
      (recur (step state))
      state)))

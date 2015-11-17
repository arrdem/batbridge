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
            [clojure.set :as set]))

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
  (let [next-processor           (ss/decode processor)
        {:keys [decode execute]} next-processor
        {:keys [dst addr]}       execute]
    (if (and (= :registers dst)
             (contains? (-> #{}
                            (into [(:a decode) (:b decode)])
                            (disj 30 29))
                        addr))
      (do (info "[decode   ] stalling the pipeline!")
          (-> processor
              (update :stall (fnil inc 0))
              (dissoc :decode)))
      next-processor)))

(defn writeback
  "Pulls a writeback directive out of the processor state, and
  performs the indicated update on the processor state. Update command
  have been restructured and are now maps
  {:dst (U :registers :halt :memory) :addr Int :val Int}."

  [processor]
  (let [directive (get processor :execute [:registers 30 0])
        {:keys [dst addr val npc]} directive]
    (info "[writeback]" directive)
    (cond ;; special case to stop the show
      (= :halt dst)
      (assoc processor :halted true)

      ;; special case for hex code printing
      (and (= :registers dst)
           (= 29 addr))
      (do (when-not (zero? val)
            (print (format "0x%X" (char val))))
          processor)

      ;; special case for printing
      (and (= :registers dst)
           (= 30 addr))
      (do (when-not (zero? val)
            (print (char val)))
          processor)

      ;; special case for branching as we must flush the pipeline
      (and (= :registers dst)
           (= 31 addr)
           (not (= val npc))) ;; don't flush if we aren't changing
      ;; the next PC value. This means to
      ;; jumping to PC+4 does exactly
      ;; nothing as it should.
      (do (warn "[writeback] Flushing pipeline!")
          (-> processor
              (dissoc :fetch)
              (dissoc :decode)
              (dissoc :execute)
              (assoc-in [:registers addr] val)))

      true
      (assoc-in processor [dst addr] val))))

(defn stall-dec
  "A dec which deals with a nil argument case, and has a floor value
  of 0."

  [processor]
  (update-in processor [:stall]
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

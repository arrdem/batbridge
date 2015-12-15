(ns batbridge.predicted-pipeline
  "Implements a simple pipelined processor. This processor makes no
  effort to perform branch prediction, instruction caching or memory
  caching. Correctness with respect to the pipeline and bubbles is
  ensured however."
  (:require [batbridge
             [single-cycle :as ss]
             [pipeline :as p]
             [common :as common]
             [bytecode :refer [word->opcode]]]
            [taoensso.timbre :refer [info warn]]
            [amalloy.ring-buffer :refer [ring-buffer]]))

;; Build a simple branch predictor typeclass.
;; Several predictors are implemented as from McFarling[1]
;;
;; By convention, I will refer to the "taken" state if an instruction
;; branches to any address besides PC+4. "branching" to PC+4 is
;; considered to be not taken.
;;
;; ------------------------------------------------------------------------------
;; Here I will implement GShare as from [1]

(defn two-bit-counter [v op]
  (case op
    (:inc) (min (inc (or v 2)) 3)
    (:dec) (max (dec (or v 2)) 0)))

(defn bool->int [bool]
  (if bool 1 0))

(defn vec->bitv [bool-vec]
  (reduce (fn [c b] (bit-or (bit-shift-left c 1) (bool->int b)))
          0 bool-vec))

(defn train-pred [p addr hst res]
  (let [a [(bit-xor (bit-and 0x1FF addr)
                    (vec->bitv hst))]]
    (case res
      (:taken)     (update-in p a two-bit-counter :inc)
      (:not-taken) (update-in p a two-bit-counter :dec))))

(defn predict-pred [p addr hst]
  (>= (get p (bit-xor addr (vec->bitv hst)) 2) 2))

;; ------------------------------------------------------------------------------
;; [1] (http://www.hpl.hp.com/techreports/Compaq-DEC/WRL-TN-36.pdf)

(defn next-pc
  "Examines a processor state, determining the next value for the
  PC. Note that this relies on support from writeback to record PC
  value transitions when jumps are executed."

  [processor]
  (let [{:keys [jump-map pred hst]} (:predictor processor)
        pc                          (common/register->val processor 31)]
    (if (and (contains? jump-map pc)
             (predict-pred pred pc hst))
      (get jump-map pc)
      (+ 4 pc))))

;;--------------------------------------
;; Training API

(defn train-jump
  [processor this next]
  (let [{:keys [hst]} (:predictor processor)]
    (-> processor
        (update-in [:predictor :pred] train-pred this hst :taken)
        (update-in [:predictor :jump-map] assoc this next))))

(defn train-step
  [processor]
  (let [{:keys [hst]} (:predictor processor)
        this          (common/register->val processor 31)]
    (-> processor
        (update-in [:predictor :pred] train-pred this hst :not-taken))))

;;--------------------------------------
;; History API

(defn update-history
  [processor outcome]
  (update-in processor [:predictor :hst]
             (fn [hist]
               (into (ring-buffer 9)
                     (conj hist outcome)))))

(defn update-taken
  [processor]
  (update-history processor true))

(defn update-not-taken
  [processor]
  (update-history processor false))

;;------------------------------------------------------------------------------
;; Implement the bits of the processor that have to change

(defn branch? [{:keys [icode] :as result}]
  (or (when (number? icode)
        (not= 0 (bit-and (word->opcode icode) 0x20)))
      (when (vector? icode)
        (#{:ifeq :iflt :ifle :ifne} (first icode)))))

(defn fetch
  "Checks the stall counter, invoking ss/fetch if zero otherwise
  returning the input state unmodified due to a pipeline stall."

  [processor]
  (if (common/stalled? processor)
    processor
    (let [pc        (common/register->val processor 31)
          processor (ss/fetch processor)
          npc       (if (branch? (:fetch/result processor))
                      (next-pc processor)
                      (+ pc 4))]
      (info "[fetch    ]" pc "->" npc)
      (-> processor
          (assoc-in [:registers 31] npc)
          (assoc-in [:fetch :npc] npc)))))

(defn fixup-prediction [processor]
  (let [directive                     (get processor :execute/result [:registers 30 0])
        {:keys [dst val pc]} directive]
    (info "[retrain  ] changing a wrong prediction :(")
    (-> processor
        (update-taken)
        (train-jump (- pc 4) val))))

(defn correct-prediction [processor]
  (let [directive                     (get processor :execute/result [:registers 30 0])
        {:keys [dst npc pc]} directive]
    (if (and pc npc)
      (do (info "[train    ] training a correct prediction up!")
          (-> processor
              (update-taken)
              (train-jump (- pc 4) npc)))
      processor)))

(defn step
  "Sequentially applies each phase of execution to a single processor
  state, returning an updated state representing a single cycle of
  computation's effects on the processor. Because this is a pipelined
  design, we simply take the in-order steps and run them in the
  reverse order using the processor state as a set of latches for
  storing the state between clock 'cycles'."

  [state]
  (let [train (comp correct-prediction p/post-correct-prediction-hook)
        fix   (comp fixup-prediction p/post-incorrect-prediction-hook)]
    (binding [p/post-correct-prediction-hook   train
              p/post-incorrect-prediction-hook fix]
      (-> state
          p/writeback
          ss/execute
          p/decode
          fetch
          p/stall-dec))))

(defn -main
  "Steps a processor state until such time as it becomes marked as
  'halted'. Makes no attempt to construct or validate a processor
  state, or handle errors therein."

  [state]
  (loop [state state]
    (if-not (common/halted? state)
      (recur (step state))
      state)))

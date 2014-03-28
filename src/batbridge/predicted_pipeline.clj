(ns batbridge.predicted-pipeline
  "Implements a simple pipelined processor. This processor makes no
  effort to perform branch prediction, instruction caching or memory
  caching. Correctness with respect to the pipeline and bubbles is
  ensured however."
  (:require [batbridge [single-cycle :as ss]
                       [pipeline :as p]
                       [common :as common]]
            [taoensso.timbre :refer [info warn]]
            [amalloy.ring-buffer 
             :refer [ring-buffer]]))

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

(defn taken?
  "Examines a processor state, determining whether the state of the
  Writeback stage qualifies under my definition of taken vs. not
  taken." 

  [processor]
  (let [{:keys [dst addr val pc]} (:execute processor)]
    (cond (or (and (= dst :registers) 
                   (= addr 31) 
                   (= val pc))
              (not (= addr 31)))
            :not-taken

          (and (= dst :registers) 
               (= addr 31))
            :taken)))


(defn next-pc
  "Examines a processor state, determining the next value for the
  PC. Note that this relies on support from writeback to record PC
  value transitions when jumps are executed."
  
  [processor]
  (let [{:keys [jump-map pred hst]} (:predictor processor)
        pc                          (common/get-register processor 31)]
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
        this          (common/get-register processor 31)]
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
  (update-history processor 1))


(defn update-not-taken
  [processor]
  (update-history processor 0))


;;------------------------------------------------------------------------------
;; Implement the bits of the processor that have to change

(defn fetch
  "Checks the stall counter, invoking ss/fetch if zero otherwise
  returning the input state unmodified due to a pipeline stall."

  [processor]
  (if (p/stalled? processor) 
    processor
    (let [pc    (common/get-register processor 31)
          icode (common/get-memory processor pc)
          npc   (next-pc processor)]
      (info "[fetch    ]" pc "->" icode " npc:" npc)
      (-> processor
          (assoc-in [:registers 31] npc)
          (assoc :fetch {:icode icode 
                         :npc   npc
                         :pc    (+ pc 4)})))))


(defn writeback
  "Pulls a writeback directive out of the processor state, and
  performs the indicated update on the processor state. Update command
  have been restructured and are now maps
  {:dst (U :registers :halt :memory) :addr Int :val Int}."  

  [processor]
  (let [directive (get processor :execute [:registers 30 0])
        {:keys [dst addr val pc npc]} directive]
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
            (do (warn "[writeback] flushing pipeline!")
                (-> processor
                    (update-taken)
                    (train-jump (- pc 4) val)
                    (dissoc :fetch)
                    (dissoc :decode)
                    (dissoc :execute)
                    (assoc-in [:registers addr] val)))

          ;; special case for correctly predicted branching
          (and (= :registers dst)
               (= 31 addr)
               (= val npc)) ;; In this case we need to reinforce the
                            ;; branch prediction.
            (-> processor
                (update-taken)
                (train-jump (- pc 4) val))

          true
            (-> processor
                (update-not-taken)
                (assoc-in [dst addr] val)))))


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
      p/decode
      fetch
      p/stall-dec))


(defn -main
  "Steps a processor state until such time as it becomes marked as
  'halted'. Makes no attempt to construct or validate a processor
  state, or handle errors therein." 

  [state]
  (loop [state state]
    (if-not (common/halted? state)
      (recur (step state))
      state)))

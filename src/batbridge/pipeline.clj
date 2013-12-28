(ns batbridge.pipeline
  "Implements a simple pipelined processor. This processor makes no
  effort to perform branch prediction, instruction caching or memory
  caching. Correctness with respect to the pipeline and bubbles is
  ensured however."
  (:require [batbridge [isa :as isa]
                       [common :as common]
                       [single-cycle :as ss]]))


(defn writeback 
  "Pulls a writeback directive out of the processor state, and
  performs the indicated update on the processor state. Update command
  have been restructured and are now maps
  {:dst (U :registers :halt :memory) :addr Int :val Int}."  

  [processor]
  (let [directive (get processor :execute [:registers 30 0])
        {:keys [dst addr val]} directive]
    (println "[writeback]" directive)
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
               (= 31 addr))
            (do (println "[writeback] flushing pipeline!")
                (-> processor
                    (dissoc :fetch)
                    (dissoc :decode)
                    (dissoc :execute)
                    (assoc-in [:registers addr] val)))

          true
            (assoc-in processor [dst addr] val))))


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
      ss/decode
      ss/fetch))


(defn -main
  "Steps a processor state until such time as it becomes marked as
  'halted'. Makes no attempt to construct or validate a processor
  state, or handle errors therein." 

  [state]
  (loop [state state]
    (if-not (common/halted? state)
      (do (println "-------------------------------------------------")
          (recur (step state)))
      state)))

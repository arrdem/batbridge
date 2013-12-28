(ns batbridge.isa
  (:require [clojure.core.typed :as t]))

(defn register? 
  "Predicate to test whether or not the argument integer is a valid
  register identifier."

  [x]
  (and (number? x)
       (< x 32)
       (>= x 0)))

(defn seq->instrs
  "Translates a sequence of _vector_ instructions to a map of word IDs
   to instructions. Note that this _cannot_ be used for a bytecode
   interpreter because it approximates a byte buffer by aligning
   instructions at multiples of 4 and contains vector instructions
   rather than bytecodes."

  [seq] 
  (zipmap (range 0 (* 4 (count seq)) 4) seq))

(defn instrs->state 
  "Generates the minimum of a processor state required to invoke
  a (step) implementation successfully. Agnostic as to the details of
  the instructions value."  
  
  [instructions]
  {:memory (seq->instrs instructions)
   :registers {31 0}})

(defn halted? 
  "Common predicate for testing whether a processor state has become
  halted or not yet."

  [state]
  (or (:halted state) false))

(def register-symbol-map
  "Provides translation from the shorthand keyword symbols used to
  identify registers to their integer index IDs."

  (-> (zipmap (range 32) (range 32))
      (assoc :r_IMM  29)
      (assoc :r_ZERO 30)
      (assoc :r_PC   31)))

(def map-no-op
  "Map constant representing a no-op. Used in lieu of a fetched
  instruction by decode."

  {:icode :add
   :dst   :r_ZERO
   :srca  :r_ZERO
   :srcb  :r_ZERO
   :lit   0})

(def vec-no-op
  "Vector constant representing a no-op. May be used by fetch stages
  in lieu of a meaningful fetched value." 

  [:add :r_ZERO :r_ZERO :r_ZERO 0])

(def bytecode-no-op
  "Long constant representing the bytecode encoding of 
  [:add 0 30 0 0]. May be used by fetch stages in lieu of a true 
  fetched value."

  0xC000F000)

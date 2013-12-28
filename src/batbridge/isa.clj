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

;; Various processor components refactored out due to commonality
;;------------------------------------------------------------------------------
(defn get-memory
  "Accesses an address in a processor state's memory, returning the
  value there. Defaults to 0, so the processor will halt if it jumps
  into unset instructions."

  [p addr]
  (get-in p [:memory addr] 0))


(defn write-memory
  "Writes the argument value into the processor state's memory,
  returning an updated processor state representation."

  [p addr v]
  (assoc-in p [:memory addr] v))


(defn get-register
  "Fetches a register value by register ID from a processor state,
  returning the value."

  [p reg]
  (get-in p [:registers reg] 0))


(defn register->val
  "Helper function to compute a value from either a keyword register
  alias or an integer register identifier. Returnes the value of
  accessing the identified register."
  [reg processor]
  (case reg
    (:r_PC   31) (get-register processor 31)
    (:r_ZERO 30) 0
    (:r_IMM  29) (get (get processor :decode {}) :lit 0)
    (-> processor
    :registers
    (get reg 0))))

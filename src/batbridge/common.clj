(ns batbridge.common
  "Bits and pieces which were pulled out of various Batbridge
  simulators on the basis of constituting code repetition."
  (:require [batbridge.cache :refer [make-cache-higherarchy cache-get!
                                     cache-write!]]))


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
  (zipmap (range 0 (* 4 (count seq)) 4) 
          seq))


(defn instrs->state 
  "Generates the minimum of a processor state required to invoke
  a (step) implementation successfully. Agnostic as to the details of
  the instructions value."  
  
  [instructions]
  {:memory instructions
   :registers {31 0}})


(defn halted? 
  "Common predicate for testing whether a processor state has become
  halted or not yet."

  [state]
  (or (:halted state) false))


;; Various processor components refactored out due to commonality
;;------------------------------------------------------------------------------
(defn get-memory
  "Accesses an address in a processor state's memory, returning the
  value there. Defaults to 0, so the processor will halt if it jumps
  into unset instructions. Note that this function will behave badly
  if there is not an installed cache higherarchy."

  [p addr]
  (cache-get! (:memory p) addr))


(defn write-memory
  "Writes the argument value into the processor state's memory,
  returning an updated processor state representation. Note that this
  function will behave badly if there is not an installed cache
  higherarchy."

  [p addr v]
  (cache-write! (:memory p) addr v))


(defn get-register
  "Fetches a register value by register ID from a processor state,
  returning the value."

  [p reg]
  (get-in p [:registers reg] 0))     


(defn register->val
  "Helper function to compute a value from either a keyword register
  alias or an integer register identifier. Returnes the value of
  accessing the identified register."
  [processor reg pc imm]
  (case reg
    (:r_PC   31) pc
    (:r_ZERO 30) 0
    (:r_IMM  29) imm
    (get-register processor reg)))


(defn upgrade-writeback-command
  "Transforms an old vector writeback command into the new map
  structure, thus allowing for pc data to be preserved."
  
  [[dst addr v]]
  {:dst dst :addr addr :val v})


(defn fmt-instr
  "Formats a map instruction as a vector instruction. Intended for use
  in debugging / inspecting decoded word instructions."

  [{:keys [icode d a b i] :as  map-instr}]
  [icode d a b i])

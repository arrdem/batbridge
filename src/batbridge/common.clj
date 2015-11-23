(ns batbridge.common
  "Bits and pieces which were pulled out of various Batbridge
  simulators on the basis of constituting code repetition."
  (:require [batbridge.cache :refer [make-cache-hierarchy cache-get! cache-write!]]))

(defn register? 
  "Predicate to test whether or not the argument integer is a valid
  register identifier."

  [x]
  (and (number? x)
       (< x 32)
       (>= x 0)))

(defn halted? 
  "Common predicate for testing whether a processor state has become
  halted or not yet."

  [state]
  (:halted state false))

;; Various processor components refactored out due to commonality
;;------------------------------------------------------------------------------
(defn get-memory
  "Accesses an address in a processor state's memory, returning the
  value there. Defaults to 0, so the processor will halt if it jumps
  into unset instructions. Note that this function will behave badly
  if there is not an installed cache hierarchy."

  [p addr]
  (cache-get! (:memory p) addr))

(defn write-memory
  "Writes the argument value into the processor state's memory,
  returning an updated processor state representation. Note that this
  function will behave badly if there is not an installed cache
  hierarchy."

  [p addr v]
  ;; cache-write is side-effectful so first we do it
  (cache-write! (:memory p) addr v)
  ;; then we return the processor we had
  p)

(defn ^:private normalize-register [name]
  (case name
    (:r_PC)   31
    (:r_ZERO) 30
    (:r_IMM)  29
    name))

(defn ^:private get-register
  "Fetches a register value by register ID from a processor state,
  returning the value.

  UNLESS YOU REALLY KNOW WHAT YOU'RE DOING DON'T USE
  THIS. register->val should be preferred."

  [p reg]
  (let [reg (normalize-register reg)]
    (case reg
      (30) 0
      (get-in p [:registers reg] 0))))

(defn register->val
  "Helper function to compute a value from either a keyword register
  alias or an integer register identifier. Returnes the value of
  accessing the identified register."
  ([p r]
   ;; This case works fine unless you're trying to read from the immediate
   ;; register, in which case the caller fucked up and needs to provide that
   ;; value.
   {:pre [(not (#{:r_IMM 29} r))]}
   (get-register p r))

  ([processor reg pc imm]
   (case reg
     (:r_PC 31)   pc
     (:r_ZERO 30) 0
     (:r_IMM  29) imm
     (get-register processor reg))))

(defn upgrade-writeback-command
  "Transforms an old vector writeback command into the new map
  structure, thus allowing for pc data to be preserved."
  
  [[dst addr v]]
  {:dst dst :addr addr :val v})

(defn fmt-instr
  "Formats a map instruction as a vector instruction. Intended for use
  in debugging / inspecting decoded word instructions."

  [{:keys [icode d a b i] :as  map-instr}]
  (case icode
    (:ifeq :ifle :iflt :ifne)
    ,,[icode a b i]
    
    (:hlt)
    ,,[icode]

    ;; else
    ,,[icode d a b i]))

(defn make-processor
  "Builds a processor map by installing values in registers and the
  memory hierarchy. This function transforms what has been the
  traditional inline notation for a processor to a fully fledged cache
  hierarchy equiped structure." 

  [{:keys [regs memory]}]
  (let [initial-state 
        {:registers regs
         :memory (make-cache-hierarchy
                  [[1 128] [2 512] [4 2048] [16 Long/MAX_VALUE]])}]
    (doseq [[k v] memory]
      (cache-write! (:memory initial-state) k v))

    initial-state))

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
  the instructions value. All of main memory is initialized to zero,
  save those addresses specified in the instruction map. The PC is
  initialized to zero."
  
  [instructions]
  (make-processor
   {:memory instructions
    :registers {31 0}}))

(defn stalled?
  "Checks the stall counter, returning True if the stall counter is
  nonzero. A nil value is treated as zero."

  [processor]
  (not (zero? (:fetch/stall processor 0))))

(defn normalize-address
  "Rounds down to the nearest multiple of 4"
  [x] (bit-and x (bit-not 3)))

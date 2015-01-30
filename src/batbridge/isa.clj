(ns batbridge.isa
  "Define two structures for identifying and executing Batbridge
  opcodes which will be common to the various simulators. These
  structures were refactored out of several different simulators in
  the interests of making spec compliance easiser to maintain."

  (:require [batbridge [bytecode :refer [word->symbol-map]]
                       [common   :refer [get-register get-memory]]]))


;; No-op constants for the various stages
;;------------------------------------------------------------------------------
(def map-no-op
  "Map constant representing a no-op. Used in lieu of a fetched
  instruction by decode."

  {:icode :add
   :d     30
   :a     30
   :b     30
   :i     0
   :pc    -1})


(def vec-no-op
  "Vector constant representing a no-op. May be used by fetch stages
  in lieu of a meaningful fetched value." 

  [:add 30 30 30 0])


(def bytecode-no-op
  "Long constant representing the bytecode encoding of 
  [:add 30 30 30 0]. May be used by fetch stages in lieu of a true 
  fetched value."

  0xC3DEF000)


(def writeback-no-op
  "Writeback instruction which will do exactly nothing!"

  {:dst :registers :addr 30 :val 0 :pc -1})


;; The  common opcode implementation map
;;------------------------------------------------------------------------------
(def opcode->fn
  "Maps opcode names to implementing functions. These functions are
  generic with respect to processor state, and can therefor be re-used
  by all processors."

  {:hlt  (fn [_ _ _ _  ] [:halt nil nil])
   :ld   (fn [x y p dst] [:registers dst (get-memory p (+ x (* 4 y)))])
   :st   (fn [x y p dst] [:memory (+ x (* 4 y)) (get-register p dst)])

   :iflt (fn [x y p dst]
           (let [pc (get-register p 31)]
             [:registers 31 (if (< x y) pc (+ pc 4))]))

   :ifle (fn [x y p dst]
           (let [pc (get-register p 31)]
             [:registers 31 (if (<= x y) pc (+ pc 4))]))

   :ifeq (fn [x y p dst]
           (let [pc (get-register p 31)]
             [:registers 31 (if (= x y) pc (+ pc 4))]))

   :ifne (fn [x y p dst]
           (let [pc (get-in p [:decode :pc])]
             [:registers 31 (if-not (= x y) pc (+ pc 4))]))

   :add  (fn [x y _ dst] [:registers dst (+ x y)])
   :sub  (fn [x y _ dst] [:registers dst (- x y)])
   :div  (fn [x y _ dst] [:registers dst (/ x y)])
   :mod  (fn [x y _ dst] [:registers dst (mod x y)])
   :mul  (fn [x y _ dst] [:registers dst (* x y)])
   :and  (fn [x y _ dst] [:registers dst (bit-and x y)])
   :or   (fn [x y _ dst] [:registers dst (bit-or x y)])
   :nand (fn [x y _ dst] [:registers dst (bit-not (bit-and x y))])
   :xor  (fn [x y _ dst] [:registers dst (bit-xor x y)])
   :sl   (fn [x y _ dst] [:registers dst (bit-shift-left x y)])
   :sr   (fn [x y _ dst] [:registers dst (bit-shift-right x y)])})


(def bytecode->opcode
  "Maps bytecodes to their opcodes as per the spec."

  {0x00 :hlt
   0x10 :ld
   0x11 :st
   0x20 :iflt
   0x21 :ifle
   0x22 :ifeq
   0x23 :ifne
   0x30 :add
   0x31 :sub
   0x32 :div
   0x33 :mod
   0x34 :mul
   0x35 :and
   0x36 :or
   0x37 :nand
   0x38 :xor
   0x3A :sl
   0x3B :sr
   0x3C :sal
   0x3D :sar})


(def register-symbol-map
  "Provides translation from the shorthand keyword symbols used to
  identify registers to their integer index IDs."

  (-> (zipmap (range 32) (range 32))
      (assoc :r_IMM  29)
      (assoc :r_ZERO 30)
      (assoc :r_PC   31)))


;; The decode operation which is entirely common to all the processors
;;------------------------------------------------------------------------------
(defn vec->symbol-map
  "Pulls appart a vector instruction, building the symbolic assembler
  map which the Clojure simulators are designed to work with."
  [vec-instr]
  (case (first vec-instr)
    (:ifeq :ifle :iflt :ifne)
    ,,(zipmap [:icode :a :b :i] vec-instr)
    
    (:hlt)
    ,,(zipmap [:icode] vec-instr)

    ;; else
    ,,(zipmap [:icode :d :a :b :i] vec-instr)))


(defn normalize-icode
  "Does integer to symbol remapping for the icode of a decoded instr."
  [{:keys [icode] :as decode}]
  (assoc decode :icode (get bytecode->opcode icode icode)))


(defn normalize-registers
  "Does symbol to integer normalization for the register parameters of
  a decoded instruction."
  [{:keys [a b d] :as decode}]
  (-> decode
      (assoc :a (get register-symbol-map a a))
      (assoc :b (get register-symbol-map b b))
      (assoc :d (get register-symbol-map d d))))


(defn decode-instr
  "A somewhat more heavy duty decode. Determines whether the icode to
  be decoded is a vector or an integer, and performs the appropriate
  decoding to a map. Note that this version of decode does _not_
  perform value loading. As value loading becomes complicated in an
  out of order processor that task is left to the processor
  simulators."

  [icode-maybe]
  (when-not (nil? icode-maybe)
    (cond-> icode-maybe
            (vector?  icode-maybe) vec->symbol-map
            (integer? icode-maybe) word->symbol-map
            true                   normalize-icode
            true                   normalize-registers)))

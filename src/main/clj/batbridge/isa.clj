(ns batbridge.isa
  "Define two structures for identifying and executing Batbridge
  opcodes which will be common to the various simulators. These
  structures were refactored out of several different simulators in
  the interests of making spec compliance easiser to maintain."
  (:require [batbridge
             [bytecode :refer [word->symbol-map]]
             [common :refer [get-memory normalize-address register->val]]]
            [taoensso.timbre :refer [info]]))

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

  {:hlt  (fn [p pc i x y dst]
           [:halt nil nil])

   :ld   (fn [p pc i x y dst]
           (let [a (normalize-address (+ x (* 4 y)))
                 v (get-memory p a)]
             [:registers dst v]))

   :st   (fn [p pc i x y dst]
           (let [v   (register->val p dst pc i)
                 dst (normalize-address (+ x (* 4 y)))]
             [:memory dst v]))

   :iflt (fn [p pc i x y dst]
           (info "IFLT, pc" pc "testing" x y)
           [:registers 31 (if (< x y) pc (+ pc 4))])

   :ifle (fn [p pc i x y dst]
           (info "IFLE, pc" pc "testing" x y)
           [:registers 31 (if (<= x y) pc (+ pc 4))])

   :ifeq (fn [p pc i x y dst]
           (info "IFEQ, pc" pc "testing" x y)
           [:registers 31 (if (= x y) pc (+ pc 4))])

   :ifne (fn [p pc i x y dst]
           (info "IFNE, pc" pc "testing" x y)
           [:registers 31 (if (not= x y) pc (+ pc 4))])

   :add  (fn [p pc i x y dst]
           [:registers dst (+ x y)])
   
   :sub  (fn [p pc i x y dst]
           [:registers dst (- x y)])
   
   :div  (fn [p pc i x y dst]
           [:registers dst (/ x y)])
   
   :mod  (fn [p pc i x y dst]
           [:registers dst (mod x y)])
   
   :mul  (fn [p pc i x y dst]
           [:registers dst (* x y)])
   
   :and  (fn [p pc i x y dst]
           [:registers dst (bit-and x y)])
   
   :or   (fn [p pc i x y dst]
           [:registers dst (bit-or x y)])
   
   :nand (fn [p pc i x y dst]
           [:registers dst (bit-not (bit-and x y))])
   
   :xor  (fn [p pc i x y dst]
           [:registers dst (bit-xor x y)])
   
   :sl   (fn [p pc i x y dst]
           [:registers dst (bit-shift-left x y)])
   
   :sr   (fn [p pc i x y dst]
           [:registers dst (bit-shift-right x y)])})

(def opcode->macro
  {
   :push (fn [dst a b i]
           [[:sub a   a 28 4]
            [:st  dst a 30 0]])

   :pop (fn [dst a b i]
          [[:ld  dst a 30 0]
           [:add a   a 28 4]])
   })

(def bytecode->opcode
  "Maps bytecodes to their opcodes as per the spec."

  {
   0x00 :hlt
   
   ;; memory ops
   0x10 :ld
   0x11 :st
   0x12 :push
   0x13 :pop

   ;; control ops
   0x20 :iflt
   0x21 :ifle
   0x22 :ifeq
   0x23 :ifne

   ;; alu ops
   0x30 :add
   0x31 :sub
   0x32 :div
   0x33 :mod
   0x34 :mul
   0x35 :and
   0x36 :or
   0x37 :nand
   0x38 :xor
   ;; 0x39 undefined in v0
   0x3A :sl
   0x3B :sr
   0x3C :sal
   0x3D :sar
   ;; 0x3E undefined in v0
   ;; 0x3F undefined in v0
   })

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
    ,,(-> (zipmap [:icode :a :b :i] vec-instr)
          (assoc :d 0))
    
    (:hlt)
    ,,(merge (zipmap [:icode] vec-instr)
             {:a 0 :b 0 :d 0 :i 0})

    ;; else
    ,,(zipmap [:icode :d :a :b :i] vec-instr)))

(defn normalize-icode
  "Does integer to symbol remapping for the icode of a decoded instr."
  [{:keys [icode] :as decode}]
  {:pre [(map? decode)]}
  (update decode :icode #(get bytecode->opcode % %)))

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
      (integer? icode-maybe) word->symbol-map
      (vector?  icode-maybe) vec->symbol-map
      true                   normalize-icode
      true                   normalize-registers)))

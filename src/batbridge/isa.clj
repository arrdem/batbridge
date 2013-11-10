(ns batbridge.isa
  (:require [clojure.core.typed :as t]))

(t/def-alias Memory "A storage medium within a processor"
  (t/Map t/AnyInteger t/AnyInteger))

(t/def-alias Storage
  (U (Value :registers) (Value :memory)))

(t/def-alias InstructionParam "An instruction parameter"
  (U t/AnyInteger
     (Value :r_PC)
     (Value :r_IMM)
     (Value :r_ZERO)))

(t/def-alias InstructionVec "A symbolic vector instruction"
  '[t/Keyword InstructionParam InstructionParam InstructionParam t/AnyInteger])

(t/def-alias InstructionMap "A parsed map instruction"
  (HMap :mandatory {:icode (U t/Keyword t/AnyInteger)
                    :dst   InstructionParam
                    :srca  InstructionParam
                    :srcb  InstructionParam
                    :lit   t/AnyInteger}
        :complete? true))

(t/def-alias CommitCommand
  (U '[(Value :memory)   t/AnyInteger t/AnyInteger]
     '[(Value :registers) t/AnyInteger t/AnyInteger]
     (Value :halt)))

(t/def-alias Processor "A processor state"
  (HMap :mandatory {:registers Memory
                    :memory    Memory
                    :fetch   InstructionVec
                    :decode  InstructionMap
                    :execute CommitCommand
                    :halted  boolean}
        :complete? true))

(t/ann register? [t/AnyInteger -> boolean])
(defn register? [x]
  (and (number? x)
       (< x 32)
       (>= x 0)))

(t/defn> fetched?
  :- boolean
  [o :- (U InstructionVec InstructionMap)]
  (:fetched?
   (meta o)))

(t/defn> seq->instrs 
  :- (t/Map t/AnyInteger i/InstructionVec)
  [seq :- (t/Seq i/InstructionVec)]
  (zipmap (range 0 (* 4 (count seq)) 4)
          seq))

(t/defn> bprn 
  :- (t/Seq i/InstructionVec)
  [str :- String]
  (map (t/fn> [x :- Character] [:add :r_ZERO :r_IMM 0 (int x)])
       str))

(t/defn> halted? 
  :- boolean
  [state :- i/Processor]
  (or (:halted state) false))

; Registers
;-------------------------------------------------------------------------------
;; The PC 31 (0b11111)
;;   - reading it produces the PC of the next instruction
;;   - having it as a target causes a branch

;; The Zero register 30 (0b11110)
;;   - always read as 0
;;   - writing to it prints its value on the console (ASCII)

;; The Immediate value 29 (0b1101)
;;   - when read produces the 11 bit immediate field in the instruction
;;   - writing to it prints its value on the console (HEX)

;; Shortcut datastructure for mapping a register's numeric code to
;; it's human readable "assembler" representation.
(t/ann register-symbol-map (t/Map InstructionParam t/AnyInteger))
(def register-symbol-map
  (-> (t/ann-form (zipmap (range 30) (range 30))
                  (t/Map t/AnyInteger t/AnyInteger))
      (assoc :r_PC 31)
      (assoc :r_IMM 30)
      (assoc :r_ZERO 0)))

(t/ann map-no-op InstructionMap)
(def map-no-op
  (with-meta
    {:icode :add
     :dst   :r_ZERO
     :srca  :r_ZERO
     :srcb  :r_ZERO
     :lit   0}
    {:fetched? false}))

(t/ann vec-no-op InstructionVec)
(def vec-no-op
  (with-meta 
    [:add :r_ZERO :r_ZERO :r_ZERO 0]
    {:fetched? false}))

; Opcode decoding & encoding helpers
;-------------------------------------------------------------------------------
(t/ann word->opcode [t/AnyInteger -> t/AnyInteger])
(defn word->opcode
  "Pulls the opcode bits out of a word"
  [word]
  (-> word
      (bit-shift-right 26)
      (bit-and 0x3f)))

(t/ann word->dst [t/AnyInteger -> t/AnyInteger])
(defn word->dst 
  "Pulls the destination bits out of a word"
  [word]
  (-> word
      (bit-shift-right 21)
      (bit-and 0x1f)))

(t/ann word->srca [t/AnyInteger -> t/AnyInteger])
(defn word->srca 
  "Pulls the source A bits out of a word"
  [word]
  (-> word
      (bit-shift-right 16)
      (bit-and 0x1f)))

(t/ann word->srcb [t/AnyInteger -> t/AnyInteger])
(defn word->srcb 
  "Pulls the source B bits out of a word"
  [word]
  (-> word
      (bit-shift-right 11)
      (bit-and 0x1f)))

(t/ann word->lit [t/AnyInteger -> t/AnyInteger])
(defn word->lit 
  "Pulls the literal bits out of a word"
  [word]
  (bit-and word 0x7ff))

(t/ann word->symbol-map [t/AnyInteger -> InstructionMap])
(defn word->symbol-map
  "Pulls appart a word, building the symbolic assembler map which the
  Clojure simulators are designed to work with and which a human can
  reasonably debug."
  [word]
  {:icode (word->opcode word)
   :dst   (word->dst word)
   :srca  (word->srca word)
   :srcb  (word->srcb word)
   :lit   (word->lit word)})

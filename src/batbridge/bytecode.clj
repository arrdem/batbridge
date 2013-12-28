(ns batbridge.bytecode
  "Lays out the helper functions required to interact with BatBridge
  standard bytecode.")

;; Opcode decoding & encoding helpers
;;------------------------------------------------------------------------------
(defn word->opcode
  "Pulls the opcode bits out of a word"

  [word]
  (-> word
      (bit-shift-right 26)
      (bit-and 0x3f)))


(defn word->dst 
  "Pulls the destination bits out of a word"

  [word]
  (-> word
      (bit-shift-right 21)
      (bit-and 0x1f)))


(defn word->srca 
  "Pulls the source A bits out of a word"

  [word]
  (-> word
      (bit-shift-right 16)
      (bit-and 0x1f)))


(defn word->srcb 
  "Pulls the source B bits out of a word"

  [word]
  (-> word
      (bit-shift-right 11)
      (bit-and 0x1f)))


(defn word->lit 
  "Pulls the literal bits out of a word"

  [word]
  (bit-and word 0x7ff))


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


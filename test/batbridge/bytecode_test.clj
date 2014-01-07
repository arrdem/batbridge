(ns batbridge.bytecode-test
  (:require [batbridge.bytecode :as bytecode]
            [batbridge.assembler :as a]
            [clojure.test :refer :all]))


(deftest assembler-test
  ;; these tests ensure that the decoders in bytecode produces the
  ;; same results as the encoder
  (let [word (a/bb-fmt 0 1 2 3 4)]
    (are [expected got] (= expected got)
         (bytecode/word->opcode word) 0
         (bytecode/word->dst word)    1
         (bytecode/word->srca word)   2
         (bytecode/word->srcb word)   3
         (bytecode/word->lit  word)   4)

    ;; test that word->symbol-map behaves as expected
    (let [{:keys [a b icode d i]} 
          (bytecode/word->symbol-map word)]
      (are [expected got] (= expected got)
           icode 0
           d     1
           a     2 
           b     3
           i     4))))
  

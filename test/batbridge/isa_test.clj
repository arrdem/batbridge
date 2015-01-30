(ns batbridge.isa-test
  (:require [batbridge.isa :as isa]
            [clojure.test :refer :all]))

(deftest icode-normalization
  (are [expected got] (= expected got)
       (isa/decode-instr [:add 0 0 0 0])
       {:icode :add :d 0 :a 0 :b 0 :i 0}

       (isa/decode-instr [:add :r_ZERO :r_PC :r_ZERO 0])
       {:icode :add :d 30 :a 31 :b 30 :i 0}

       (isa/decode-instr 0xC3DEF000)
       {:icode :add :d 30 :a 30 :b 30 :i 0}))

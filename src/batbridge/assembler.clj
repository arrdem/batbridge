(ns batbridge.assembler
  "A tiny toolkit for building binary encoded Batbridge opcodes")


;; This code is reproduced from toothpick and is used to hack together
;; a simple bit formatting word assembler usable for testing bytecode
;; interpreters.
;;------------------------------------------------------------------------------
(defn- exp [b n]
  (reduce * 1 (repeat n b)))


(defn- bit-mask-n [n]
  (- (exp 2 n) 1))


(defn- bit-fmt
  "Takes a seq format parameter, followed by an equal number of format values
   with extra format values being ignored. Produces a bit vector by anding the
   n+1th argument expression value with a bitmask for m bits where m is the nth
   value in the first parameter.
   Eg. [30 2] 0 1 -> 0x1
       [30 2] 0 2 -> 0x2
       [30 2] 0 3 -> 0x3
       [1 31] 1 0 -> 0x80000000"

  [layout & args]
  (reduce (fn [vec [bits value]]
            (bit-or (bit-shift-left vec bits)
                    (bit-and value (bit-mask-n bits))))
          0 (map vector layout args)))


(def bb-fmt (partial bit-fmt [6 5 5 5 11]))

(ns batbridge.out-of-order
  "Implements an out of order pipeline on the basis of the
  scoreboarding algorithm."
  (:require [batbridge
             [single-cycle :as ss]
             [pipeline :as p]
             [predicted-pipeline :as pp]
             [common :as common]]
            [taoensso.timbre :refer [info warn]]
            [amalloy.ring-buffer
             :refer [ring-buffer]]))

;; Garbage collected map
;;--------------------------------------------------------------------
;; Used to keep track of what symbols are still referenced and what
;; aren't so that cleaning can be done.

(defn gc-clean
  "λ GCMap → GCMap

  Removes all keys with zero reference counts from the GC map"
  [m]
  (let [ks (keys m)]
    (reduce (fn [m k]
              (if (zero? (m k))
                (dissoc m k) m))
            m ks)))

(defn gc-add-ref
  "λ GCMap → _ → GCMap

  Adds 1 to the GC count of the K value, returning an updated GCMap
  structure."
  [m k]
  (update-in m [k] inc))

(defn gc-del-ref
  "λ GCMap → _ → GCMap

  Subtracts 1 from the GC count of the K value, returning an updated
  GCMap structure"
  [m k]
  (update-in m [k] dec))

(defn gc-insert
  "λ GCMap → (GCMap, k)

  Generates and installs a new key K with 0 refs, returning the
  updated map and the key k. The type of the key K is not guranteed."
  [m]
  (let [k (-> "register"
              gensym name keyword)]
    [(-> m
         (assoc k 0))
     k]))

;; Renaming table
;;--------------------------------------------------------------------
;; Used to rename architectural registers to garbage collected
;; "virtual" registers.
;;
;; The architecture of this datastructure is that each architectural
;; register maps to some virtual register in which a value is
;; currently stored defaulting to zero.
;;
;; { :refcount → GCMap
;;   :renaming → { k → v }
;; }

(defn initialize-renaming
  "λ nil → RenamingMap

  Builds an initial renaming structure mapping architectural registers
  to empty internal registers."
  []
  (let [[gc [& ks]]
        (reduce (fn [[m [& ks]] i]
                  (let [[m k] (gc-insert m)]
                    [m (conj ks [i k])]))
                [{} nil] (range 32))]
    {:refcount gc
     :renaming (into {} ks)}))

(defn get-register-renaming
  "λ RenamingMap → key → (RenamingMap, key)

  Looks a register renaming up out of the renaming table. This
  function does update the renaming table to indicate that the the
  renaming has an additional use. However it not side-effect the table
  to install the next renaming."

  [m r]
  (let [alias (get m r)]
    [(update-in m [:refcount] gc-add-ref alias) 
     alias]))

(defn make-register-renaming
  "λ RenamingMap → key → (RenamingMap, key)

  Creates a new renaming of the argument key, returning a pair
  reprensenting the updated renaming and the key representing the new
  renaming."
  [m k]
  (let [[gc k₀] 
        (-> (:refcount m)
            (gc-insert))]
    [(-> m
         (assoc :refcount gc)
         (assoc-in [:renaming k] k₀)) k₀]))

;; Instruction issuing
;;--------------------------------------------------------------------
;; Instructions in the pipeline constitute pending defs of values. No
;; instruction can be issued until all defs upon which it depends have
;; been resolved.
;;
;; Instructions in flight are assumed to have a :dst and :addr keys
;; which are either a register or a memory address.

(defn get-execute-instrs
  "λ Processor → (Seq Instruction)

  Pulls the in flight instructions out of the execute pipeline(s)."
  [processor]
  
  )

(defn in-flight
  "λ Processor → (Set Register)

  Given a processor state, computes the set of registers which will be
  written back to when the execute pipeline clears."
  [processor]
  (->> processor
       get-execute-instrs
       (map (fn [{:keys [dst addr]}]
              (when (= :registers :dst)
                addr)))
       (filter identity)))

(defn can-issue
  "λ (Set Register) → Instruction → bool

  Indicates whether the argument instruction can be issued or not
  given the set of values currently being computed."
  [in-flight instr]

  )

(defn to-issue
  "λ Processor → (Seq Instruction) → (Seq Instruction)

  Introspects the processor state and computes the set of instructions
  which can be issued this cycle from the instruction queue sequence."
  [processor queue]
  (let [active (in-flight processor)]
    )

  )

;; Processor implementation
;;--------------------------------------------------------------------

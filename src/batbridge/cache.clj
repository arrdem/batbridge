(ns batbridge.cache
  "Implements a simple nested LFU cache structure. Note that while
  accessing a cache element increases its cache score, no condition
  ever decreases the cache score save removal which zeros it."
  (:require [clojure.set :refer [map-invert]]))

;; caches are maps:
;;   {:map     (Atom {:store {<addres> <value>}
;;                    :meta  {<address> <meta>}
;;                    :size  <value>})
;;    :latency <value>
;;    :else    (U <cache> <None>)
;;   }

(defn ninc [x] (if x (inc x) 1))

(defn cache-hit!
  "Executes the cache metadata side-effects associated with hitting in
  a cache. In this case we just inc the metadata value for that
  particular key. Note that this operation has no meaningful return
  value and is entirely for side-effects upon the cache atom."

  [cache key]
  (-> (:map cache)
      (swap! update-in [:meta key] ninc)))


(defn cache-evict!
  "Executes the cache metadata side-effects associated with missing an
  element in a cache. In this particular implementation we select the
  minimum key/value pair in the metadata map and evict that key from
  both the metadata and the value maps. Note that this operation
  returns _no meaningful value_ and is entirely for side-effects upon
  the cache atom."

  [cache]
  (let [atom (:map cache)
        {:keys [store meta size]} @atom
        victim (->> (map-invert store)
                    (keys)
                    (apply min)
                    (get store))]
    (when (>= (count store) size)
      (do (swap! atom update-in [:store] dissoc victim)
          (swap! atom update-in [:meta]  dissoc victim)))))


(defn cache-install!
  "Installs a key/value pair in a cache, performing the appropriate
  side-effects. This function is entirely for side-effects and returns
  no meaningful value."
  [cache key v]
  (swap! (:map cache) update-in [:store] assoc key v)
  (swap! (:map cache) update-in [:meta]  assoc key 1))


(defn cache-get!
  "Recursively fetches a value from nested caches, incuring access
   latency as it scans the nested caches."
  
  [cache key]
  (Thread/sleep (get cache :latency 0)) ;; incur read latency
  (if (contains? (:store @(:map cache)) key)
      ;; cache hit case
      ;;-------------------------------------------------
      (do (cache-hit! cache key)
          (get (:store @(:map cache)) key))

      ;; cache miss case
      ;;-------------------------------------------------
      (let [v (if (:else cache)
                (cache-get! (:else cache) key)
                0)]
        (cache-install! cache key v)
        v)))

(defn cache-write!
  "Writing to the cache sucks. However for now it doesn't suck too
  hard because we can get away without flushing the cache to main
  memory. This means that a cache write is a simple update in place
  with a cache hit. Note that this function is entirely for
  side-effects and returns no meaningful value."
  
  [cache key v]
  (Thread/sleep (get cache :latency 0))
  (-> (:map cache)
      (swap! update-in [:store] assoc key v))
  (when (:else cache)
    (cache-write! (:else cache) key v)))


(defn make-cache-higherarchy 
  "Builds a cache higherarchy from a pairs [latency, size]. The result
  is a nested set of empty cache maps nested in pair order.

  Ex. a sequence [[1 8] [2 16]] would build a cache of latency 1 and
  size 8 with an else cache of latency 2 and size 16."
  
  [l-size-pairs]
  (->> l-size-pairs
       (map (fn [[l size]] 
              {:map (atom {:meta {} :store {} :size size})
               :latency l}))
       (reverse)
       (reduce (fn [prev c]
                 (assoc c :else prev)))))

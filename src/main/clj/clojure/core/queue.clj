(ns clojure.core.queue
  (:import [clojure.lang
            ,,PersistentQueue
            ,,ISeq]))

(defmethod print-method PersistentQueue [o ^java.io.Writer w]
  (.write w (format "#queue %s" (str (vec o)))))

(defn queue? [obj]
  (instance? PersistentQueue obj))

(defn queue ^PersistentQueue [coll]
  (into PersistentQueue/EMPTY coll))

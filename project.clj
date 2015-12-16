(defproject me.arrdem/batbridge "0.1.0"
  :description
  "A suite of processor simulators based on the Batbridge architecture"
  :url "http://www.github.com/arrdem/batbridge/"
  :license {:name "Eclipse Public License"
            :url "http://www.eclipse.org/legal/epl-v10.html"}
  :source-paths ["src/main/clj"]
  :test-paths   ["src/test/clj"]
  :dependencies [[org.clojure/clojure "1.7.0"]
                 [org.clojure/core.match "0.3.0-alpha4"]
                 [me.arrdem/toothpick "0.2.1"]
                 [amalloy/ring-buffer "1.0"]
                 [com.taoensso/timbre "3.1.6"]]
  :profiles {:dev {:source-paths ["src/dev/clj"]}})

(defproject me.arrdem.batbridge "0.1.0-SNAPSHOT"
  :description "FIXME: write description"
  :url "http://example.com/FIXME"
  :license {:name "Eclipse Public License"
            :url "http://www.eclipse.org/legal/epl-v10.html"}
  :dependencies [[org.clojure/clojure "1.5.1"]
                 [org.clojure/core.typed "0.2.17"]
                 [org.clojure/tools.logging "0.2.6"]]
  :core.typed {:check [batbridge.isa
                       batbridge.single-cycle]})

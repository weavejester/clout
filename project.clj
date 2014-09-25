(defproject clout "2.0.0-SNAPSHOT"
  :description "A HTTP route matching library"
  :url "https://github.com/weavejester/clout"
  :license {:name "Eclipse Public License"
            :url "http://www.eclipse.org/legal/epl-v10.html"}
  :dependencies [[org.clojure/clojure "1.3.0"]]
  :profiles
  {:dev {:jvm-opts ^:replace []
         :dependencies [[ring-mock "0.1.5"]
                        [criterium "0.4.2"]]}
   :1.4 {:dependencies [[org.clojure/clojure "1.4.0"]]}
   :1.5 {:dependencies [[org.clojure/clojure "1.5.1"]]}
   :1.6 {:dependencies [[org.clojure/clojure "1.6.0"]]}})

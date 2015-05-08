(defproject clout "2.1.2"
  :description "A HTTP route matching library"
  :url "https://github.com/weavejester/clout"
  :license {:name "Eclipse Public License"
            :url "http://www.eclipse.org/legal/epl-v10.html"}
  :dependencies [[org.clojure/clojure "1.5.1"]
                 [instaparse "1.4.0" :exclusions [org.clojure/clojure]]]
  :profiles
  {:dev {:jvm-opts ^:replace []
         :dependencies [[ring/ring-mock "0.2.0"]
                        [criterium "0.4.2"]]}
   :1.6 {:dependencies [[org.clojure/clojure "1.6.0"]]}
   :1.7 {:dependencies [[org.clojure/clojure "1.7.0-beta2"]]}})

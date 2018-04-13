(defproject clout "2.2.1"
  :description "A HTTP route matching library"
  :url "https://github.com/weavejester/clout"
  :license {:name "Eclipse Public License"
            :url  "http://www.eclipse.org/legal/epl-v10.html"}
  :dependencies [[org.clojure/clojure "1.7.0"]
                 [org.clojure/clojurescript "1.7.228" :scope "provided"]
                 [instaparse "1.4.8" :exclusions [org.clojure/clojure]]]
  :plugins [[lein-doo "0.1.7"]
            [lein-cljsbuild "1.1.4"]]
  :cljsbuild {:builds
              {:test
               {:source-paths ["src" "test"]
                :compiler     {:main          clout.test-runner
                               :output-dir    "target/out"
                               :output-to     "target/test/advanced.js"
                               :target        :nodejs
                               :optimizations :advanced}}}}
  :doo {:build "test"}
  :profiles
  {:dev {:jvm-opts     ^:replace []
         :dependencies [[ring/ring-mock "0.2.0"]
                        [criterium "0.4.2"]]}
   :1.7 {:dependencies [[org.clojure/clojure "1.7.0"]
                        [org.clojure/clojurescript "1.7.228"]]}
   :1.8 {:dependencies [[org.clojure/clojure "1.8.0"]
                        [org.clojure/clojurescript "1.8.51"]]}
   :1.9 {:dependencies [[org.clojure/clojure "1.9.0"]
                        [org.clojure/clojurescript "1.9.946"]]}})

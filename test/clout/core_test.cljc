(ns clout.core-test
  (:import #?@(:clj [[clojure.lang ExceptionInfo]
                     [java.util.regex PatternSyntaxException]]
               :cljs [[goog Uri]]))
  (:require #?@(:clj [[clojure.test :refer :all]
                      [ring.mock.request :refer [request]]]
               :cljs [[cljs.test :refer-macros [is are deftest testing use-fixtures]]])
            [clout.core :refer [route-matches route-compile]]))

#?(:cljs
   (defn request
     "Naive implementation of the Ring Mock Request in ClojureScript."
     [method uri]
     (let [uri    (Uri. uri)
           host   (if-not (empty? (.getDomain uri)) (.getDomain uri) "localhost")
           port   (.getPort uri)
           scheme (.getScheme uri)
           path   (js/encodeURI (.getPath uri))]
       {:server-port    (or port 80)
        :server-name    host
        :remote-addr    "localhost"
        :uri            (if (clojure.string/blank? path) "/" path)
        :query-string   (.getQuery uri)
        :scheme         (if-not (empty? scheme) (keyword scheme) :http)
        :request-method method
        :headers        {"host" (if port
                                  (str host ":" port)
                                  host)}})))

(deftest fixed-path
  (are [path] (route-matches path (request :get path))
    "/"
    "/foo"
    "/foo/bar"
    "/foo/bar.html"))

(deftest keyword-paths
  (are [path uri params] (= (route-matches path (request :get uri)) params)
    "/:x"       "/foo"     {:x "foo"}
    "/foo/:x"   "/foo/bar" {:x "bar"}
    "/a/b/:c"   "/a/b/c"   {:c "c"}
    "/:a/b/:c"  "/a/b/c"   {:a "a", :c "c"}))

(deftest keywords-match-extensions
  (are [path uri params] (= (route-matches path (request :get uri)) params)
    "/foo.:ext" "/foo.txt" {:ext "txt"}
    "/:x.:y"    "/foo.txt" {:x "foo", :y "txt"}))

(deftest hyphen-keywords
  (are [path uri params] (= (route-matches path (request :get uri)) params)
    "/:foo-bar" "/baz" {:foo-bar "baz"}
    "/:foo-"    "/baz" {:foo- "baz"}))

(deftest underscore-keywords
  (are [path uri params] (= (route-matches path (request :get uri)) params)
    "/:foo_bar" "/baz" {:foo_bar "baz"}
    "/:_foo"    "/baz" {:_foo "baz"}))

(deftest urlencoded-keywords
  (are [path uri params] (= (route-matches path (request :get uri)) params)
    "/:x" "/foo%20bar" {:x "foo%20bar"}
    "/:x" "/foo+bar"   {:x "foo+bar"}
    "/:x" "/foo%5Cbar" {:x "foo%5Cbar"}))

(deftest same-keyword-many-times
  (are [path uri params] (= (route-matches path (request :get uri)) params)
    "/:x/:x/:x" "/a/b/c" {:x ["a" "b" "c"]}
    "/:x/b/:x"  "/a/b/c" {:x ["a" "c"]}))

(deftest non-ascii-keywords
  (are [path uri params] (= (route-matches path (request :get uri)) params)
    "/:äñßOÔ"   "/abc"     {:äñßOÔ "abc"}
    "/:ÁäñßOÔ"  "/abc"     {:ÁäñßOÔ "abc"}
    "/:ä/:ش"    "/foo/bar" {:ä "foo" :ش "bar"}
    "/:ä/:ä"    "/foo/bar" {:ä ["foo" "bar"]}
    "/:Ä-ü"     "/baz"     {:Ä-ü "baz"}
    "/:Ä_ü"     "/baz"     {:Ä_ü "baz"}))

(deftest wildcard-paths
  (are [path uri params] (= (route-matches path (request :get uri)) params)
    "/*"     "/foo"         {:* "foo"}
    "/*"     "/foo.txt"     {:* "foo.txt"}
    "/*"     "/foo/bar"     {:* "foo/bar"}
    "/foo/*" "/foo/bar/baz" {:* "bar/baz"}
    "/a/*/d" "/a/b/c/d"     {:* "b/c"}))

(deftest escaped-chars
  (are [path uri params] (= (route-matches path (request :get uri)) params)
    "/\\:foo" "/foo"  nil
    "/\\:foo" "/:foo" {}))

(deftest inline-regexes
  (are [path uri params] (= (route-matches path (request :get uri)) params)
    "/:x{\\d+}"   "/foo" nil
    "/:x{\\d+}"   "/10"  {:x "10"}
    "/:x{\\d{2}}" "/2"   nil
    "/:x{\\d{2}}" "/20"  {:x "20"}
    "/:x{\\d}/b"  "/3/b" {:x "3"}
    "/:x{\\d}/b"  "/a/b" nil
    "/a/:x{\\d}"  "/a/4" {:x "4"}
    "/a/:x{\\d}"  "/a/b" nil))

(deftest compiled-routes
  (is (= (route-matches (route-compile "/foo/:id") (request :get "/foo/bar"))
         {:id "bar"})))

(deftest url-paths
  (is (route-matches
        "http://localhost/"
        {:scheme  :http
         :headers {"host" "localhost"}
         :uri     "/"}))
  (is (route-matches
       "//localhost/"
       {:scheme  :http
        :headers {"host" "localhost"}
        :uri     "/"}))
  (is (route-matches
       "//localhost/"
       {:scheme  :https
        :headers {"host" "localhost"}
        :uri     "/"})))

(deftest url-port-paths
  (let [req (request :get "http://localhost:8080/")]
    (is (route-matches "http://localhost:8080/" req))
    (is (not (route-matches "http://localhost:7070/" req)))))

(deftest unmatched-paths
  (is (nil? (route-matches "/foo" (request :get "/bar")))))

(deftest path-info-matches
  (is (route-matches "/bar" (-> (request :get "/foo/bar")
                                (assoc :path-info "/bar")))))

(deftest custom-matches
  (let [route (route-compile "/foo/:bar" {:bar #"\d+"})]
    (is (not (route-matches route (request :get "/foo/bar"))))
    (is (not (route-matches route (request :get "/foo/1x"))))
    (is (route-matches route (request :get "/foo/10")))))

(deftest unused-regex-keys
  (is (thrown? #?(:clj AssertionError :cljs js/Error)
               (route-compile "/:foo" {:foa #"\d+"})))
  (is (thrown? #?(:clj AssertionError :cljs js/Error)
               (route-compile "/:foo" {:foo #"\d+" :bar #".*"}))))

(deftest invalid-inline-patterns
  (is (thrown? ExceptionInfo (route-compile "/:foo{")))
  (is (thrown? ExceptionInfo (route-compile "/:foo{\\d{2}")))
  (is (thrown? #?(:clj PatternSyntaxException :cljs js/Error) 
               (route-compile "/:foo{[a-z}"))))

(deftest to-string-method
  (are [path regexs] (= (str (route-compile path regexs)) path)
    "/foo"            {}
    "/foo/:bar"       {}
    "/foo/:bar"       {:bar #"\d+"}
    "/foo/:bar{\\d+}" {}))

(ns clout.test.core
  (:use clojure.test
        clout.core))

(deftest fixed-path
  (are [path] (route-matches path {:uri path})
    "/"
    "/foo"
    "/foo/bar"
    "/foo/bar.html"))

(deftest keyword-paths
  (are [path uri params] (= (route-matches path {:uri uri}) params)
    "/:x"       "/foo"     {:x "foo"}
    "/foo/:x"   "/foo/bar" {:x "bar"}
    "/a/b/:c"   "/a/b/c"   {:c "c"}
    "/:a/b/:c"  "/a/b/c"   {:a "a", :c "c"}))

(deftest keywords-match-extensions
  (are [path uri params] (= (route-matches path {:uri uri}) params)
    "/foo.:ext" "/foo.txt" {:ext "txt"}
    "/:x.:y"    "/foo.txt" {:x "foo", :y "txt"}))

(deftest hyphen-keywords
  (are [path uri params] (= (route-matches path {:uri uri}) params)
    "/:foo-bar" "/baz" {:foo-bar "baz"}
    "/:foo-"    "/baz" {:foo- "baz"}))

(deftest underscore-keywords
  (are [path uri params] (= (route-matches path {:uri uri}) params)
    "/:foo_bar" "/baz" {:foo_bar "baz"}
    "/:_foo"    "/baz" {:_foo "baz"}))

(deftest urlencoded-keywords
  (are [path uri params] (= (route-matches path {:uri uri}) params)
    "/:x" "/foo%20bar" {:x "foo bar"}
    "/:x" "/foo+bar"   {:x "foo bar"}))

(deftest same-keyword-many-times
  (are [path uri params] (= (route-matches path {:uri uri}) params)
    "/:x/:x/:x" "/a/b/c" {:x ["a" "b" "c"]}
    "/:x/b/:x"  "/a/b/c" {:x ["a" "c"]}))

(deftest non-ascii-keywords
  (are [path uri params] (= (route-matches path {:uri uri}) params)
    "/:äñßOÔ"   "/abc"     {:äñßOÔ "abc"}
    "/:ÁäñßOÔ"  "/abc"     {:ÁäñßOÔ "abc"}
    "/:ä/:ش"    "/foo/bar" {:ä "foo" :ش "bar"}
    "/:ä/:ä"    "/foo/bar" {:ä ["foo" "bar"]}
    "/:Ä-ü"     "/baz"     {:Ä-ü "baz"}
    "/:Ä_ü"     "/baz"     {:Ä_ü "baz"}))

(deftest wildcard-paths
  (are [path uri params] (= (route-matches path {:uri uri}) params)
    "/*"     "/foo"         {:* "foo"}
    "/*"     "/foo.txt"     {:* "foo.txt"}
    "/*"     "/foo/bar"     {:* "foo/bar"}
    "/foo/*" "/foo/bar/baz" {:* "bar/baz"}
    "/a/*/d" "/a/b/c/d"     {:* "b/c"}))

(deftest compiled-routes
  (is (= (route-matches (route-compile "/foo/:id") {:uri "/foo/bar"})
         {:id "bar"})))

(deftest url-paths
  (is (route-matches
        "http://localhost/"
        {:scheme  :http
         :headers {"host" "localhost"}
         :uri     "/"})))

(deftest url-port-paths
  (let [request {:scheme  :http
                 :headers {"host" "localhost:8080"}
                 :uri     "/"}]
    (is (route-matches "http://localhost:8080/" request))
    (is (not (route-matches "http://localhost:7070/" request)))))

(deftest unmatched-paths
  (is (nil? (route-matches "/foo" {:uri "/bar"}))))

(deftest path-info-matches
  (is (route-matches "/bar" {:uri "/foo", :path-info "/bar"})))

(deftest custom-matches
  (let [route (route-compile "/foo/:bar" {:bar #"\d+"})]
    (is (not (route-matches route {:uri "/foo/bar"})))
    (is (not (route-matches route {:uri "/foo/1x"})))
    (is (route-matches route {:uri "/foo/10"}))))

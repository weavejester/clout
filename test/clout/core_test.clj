(ns clout.core-test
  (:use clojure.test
        clout.core))

(deftest fixed-path
  (are [path] (route-matches path path)
    "/"
    "/foo"
    "/foo/bar"
    "/foo/bar.html"))

(deftest nil-paths
  (is (route-matches "/" nil)))

(deftest keyword-paths
  (are [path uri params] (= (route-matches path uri) params)
    "/:x"       "/foo"     {"x" "foo"}
    "/foo/:x"   "/foo/bar" {"x" "bar"}
    "/a/b/:c"   "/a/b/c"   {"c" "c"}
    "/:a/b/:c"  "/a/b/c"   {"a" "a", "c" "c"}))

(deftest keywords-match-extensions
  (are [path uri params] (= (route-matches path uri) params)
    "/foo.:ext" "/foo.txt" {"ext" "txt"}
    "/:x.:y"    "/foo.txt" {"x" "foo", "y" "txt"}))

(deftest hyphen-keywords
  (are [path uri params] (= (route-matches path uri) params)
    "/:foo-bar" "/baz" {"foo-bar" "baz"}
    "/:foo-"    "/baz" {"foo-" "baz"}))

(deftest urlencoded-keywords
  (are [path uri params] (= (route-matches path uri) params)
    "/:x" "/foo%20bar" {"x" "foo bar"}
    "/:x" "/foo+bar"   {"x" "foo bar"}))

(deftest same-keyword-many-times
  (are [path uri params] (= (route-matches path uri) params)
    "/:x/:x/:x" "/a/b/c" {"x" ["a" "b" "c"]}
    "/:x/b/:x"  "/a/b/c" {"x" ["a" "c"]}))

(deftest wildcard-paths
  (are [path uri params] (= (route-matches path uri) params)
    "/*"     "/foo"         {"*" "foo"}
    "/*"     "/foo.txt"     {"*" "foo.txt"}
    "/*"     "/foo/bar"     {"*" "foo/bar"}
    "/foo/*" "/foo/bar/baz" {"*" "bar/baz"}
    "/a/*/d" "/a/b/c/d"     {"*" "b/c"}))

(deftest compiled-routes
  (is (= (route-matches (route-compile "/foo/:id") "/foo/bar")
         {"id" "bar"})))

(deftest url-paths
  (is (route-matches "http://localhost" "http://localhost")))

(deftest url-port-paths
  (is (route-matches "localhost:8080" "localhost:8080"))
  (is (not (route-matches "localhost:8080" "localhost:7070"))))

(deftest unmatched-paths
  (is (nil? (route-matches "/foo" "/bar"))))

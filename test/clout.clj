(ns test.clout
  (:use clojure.contrib.test-is)
  (:use clout))

(deftest fixed-path
  (are (route-matches _1 _1)
    "/"
    "/foo"
    "/foo/bar"
    "/foo/bar.html"))

(deftest nil-paths
  (is (route-matches "/" nil)))

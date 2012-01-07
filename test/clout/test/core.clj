(ns clout.test.core
  (:use clojure.test
        ring.mock.request
        clout.core))

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
    "/:x" "/foo%20bar" {:x "foo bar"}
    "/:x" "/foo+bar"   {:x "foo+bar"}
    "/:x" "/foo%5Cbar" {:x "foo\\bar"}))

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

(deftest utf8-routes
  (is (= (route-matches "/:x" (request :get "/gro%C3%9Fp%C3%B6sna"))
         {:x "großpösna"})))

(deftest wildcard-paths
  (are [path uri params] (= (route-matches path (request :get uri)) params)
    "/*"     "/foo"         {:* "foo"}
    "/*"     "/foo.txt"     {:* "foo.txt"}
    "/*"     "/foo/bar"     {:* "foo/bar"}
    "/foo/*" "/foo/bar/baz" {:* "bar/baz"}
    "/a/*/d" "/a/b/c/d"     {:* "b/c"}))

(deftest compiled-routes
  (is (= (route-matches (route-compile "/foo/:id")
                        (request :get "/foo/bar"))
         {:id "bar"})))

(deftest url-paths
  (is (route-matches
        "http://localhost/"
        {:scheme  :http
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

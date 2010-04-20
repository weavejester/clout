;; Copyright (c) James Reeves. All rights reserved.
;; The use and distribution terms for this software are covered by the Eclipse
;; Public License 1.0 (http://opensource.org/licenses/eclipse-1.0.php) which
;; can be found in the file epl-v10.html at the root of this distribution. By
;; using this software in any fashion, you are agreeing to be bound by the
;; terms of this license. You must not remove this notice, or any other, from
;; this software.

(ns clout.core
  "Library for parsing the Rails routes syntax."
  (:import java.util.Map
           java.net.URLDecoder))

;; Regular expression utilties

(defn- escape
  "Returns a string with each occurance of a character in
  chars escaped."
  [chars #^String string]
  (let [charset (set chars)]
    (apply str
      (mapcat
        #(if (contains? charset %) [\\ %] [%])
        string))))

(defn- re-escape
  "Escape all special regex chars in string."
  [string]
  (escape "\\.*+|?()[]{}$^" string))

(defn re-groups*
  "More consistant re-groups that always returns a vector of groups, even if
  there is only one group."
  [matcher]
  (for [i (range (.groupCount matcher))]
    (.group matcher (inc i))))

;; Lexer functions

(defn- lex-1
  "Lex one symbol from a string, and return the symbol and trailing source."
  [src clauses]
  (some
    (fn [[re action]]
      (let [matcher (re-matcher re src)]
        (if (.lookingAt matcher)
          [(if (fn? action) (action matcher) action)
           (.substring src (.end matcher))])))
    (partition 2 clauses)))

(defn- lex
  "Lex a string into tokens by matching against regexs and evaluating
   the matching associated function."
  [src & clauses]
  (loop [results []
         src     src
         clauses clauses]
    (if-let [[result src] (lex-1 src clauses)]
      (let [results (conj results result)]
        (if (= src "")
          results
          (recur results src clauses))))))

;; Compile route syntax

(defstruct route
  :absolute?
  :regex
  :keys)

(defn- make-route
  "Construct a route structure."
  [absolute? re keys]
  (with-meta 
    (struct route absolute? re keys)
    {:type ::compiled-route}))

(defn- absolute-url?
  "True if the path contains an absolute URL."
  [path]
  (boolean (re-matches #"https?://.*" path)))

(defn route-compile
  "Compile a path string using the routes syntax into a uri-matcher struct."
  ([path]
    (route-compile path {}))
  ([path regexs]
    (let [splat   #"\*"
          word    #":([A-Za-z][\w-]*)"
          literal #"(:[^A-Za-z*]|[^:*])+"
          word-group #(.group % 1)
          word-regex #(regexs (word-group %) "[^/.,;?]+")]
      (make-route
        (absolute-url? path)
        (re-pattern
          (apply str
            (lex path
              splat   "(.*?)"
              word    #(str "(" (word-regex %) ")")
              literal #(re-escape (.group %)))))
        (vec
          (remove nil?
            (lex path
              splat   "*"
              word    word-group
              literal nil)))))))

;; Parse URI with compiled route

(defn- assoc-vec
  "Associate a key with a value. If the key already exists in the map, create a
  vector of values."
  [m k v]
  (assoc m k
    (if-let [cur (m k)]
      (if (vector? cur)
        (conj cur v)
        [cur v])
      v)))

(defn- assoc-keys-with-groups
  "Create a hash-map from a series of regex match groups and a collection of
  keywords."
  [groups keys]
  (reduce
    (fn [m [k v]] (assoc-vec m k v))
    {}
    (map vector keys groups)))

(defn- urldecode
  "Encode a urlencoded string using the default encoding."
  [string]
  (URLDecoder/decode string))

(defn request-url
  "Return the complete URL for the request."
  [request]
  (str
    (name (:scheme request))
    "://"
    (get-in request [:headers "host"])
    (:uri request)))

(derive Map ::request)
(derive String ::request)

(derive String ::route)
(derive ::compiled-route ::route)

(defmulti route-matches
  "Match a route against an object. Returns the matched keywords of the route.
  e.g. (route-matches \"/product/:id\" \"/product/10\")
       -> {:id 10}"
  (fn [route x] [(type route) (type x)]))

(defmethod route-matches [::route nil]
  [route _]
  (route-matches route "/"))

(defmethod route-matches [String ::request]
  [route request]
  (route-matches (route-compile route) request))

(defmethod route-matches [::compiled-route Map]
  [route request]
  (route-matches route (if (:absolute? route)
                         (request-url request)
                         (:uri request))))

(defmethod route-matches [::compiled-route String]
  [route uri]
  (let [matcher (re-matcher (route :regex) (or uri "/"))]
    (if (.matches matcher)
      (assoc-keys-with-groups
        (map urldecode (re-groups* matcher))
        (route :keys)))))

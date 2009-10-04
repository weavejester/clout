;; Copyright (c) James Reeves. All rights reserved.
;; The use and distribution terms for this software are covered by the Eclipse
;; Public License 1.0 (http://opensource.org/licenses/eclipse-1.0.php) which
;; can be found in the file epl-v10.html at the root of this distribution. By
;; using this software in any fashion, you are agreeing to be bound by the
;; terms of this license. You must not remove this notice, or any other, from
;; this software.

(ns http-routes
  "Library for parsing the Rails routes syntax."
  (:import java.net.URLDecoder))

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

(defstruct route
  :regex
  :keywords)

(defn- make-route
  "Construct a route structure."
  [re keywords]
  (with-meta 
    (struct route re keywords)
    {:type ::route}))

(defn route-compile
  "Compile a path string using the routes syntax into a uri-matcher struct."
  [path]
  (let [splat   #"\*"
        word    #":([A-Za-z][\w-]*)"
        literal #"(:[^A-Za-z*]|[^:*])+"]
    (make-route
      (re-pattern
        (apply str
          (lex path
            splat   "(.*?)"
            word    "([^/.,;?]+)"
            literal #(re-escape (.group %)))))
      (vec
        (remove nil?
          (lex path
            splat   :*
            word    #(keyword (.group % 1))
            literal nil))))))

(defn- assoc-vec
  "Associate a key with a value. If the key already exists in the map, create a
  vector of values."
  [map key val]
  (assoc map key
    (if-let [cur (map key)]
      (if (vector? cur)
        (conj cur val)
        [cur val])
      val)))

(defn- assoc-keywords-with-groups
  "Create a hash-map from a series of regex match groups and a collection of
  keywords."
  [groups keywords]
  (reduce
    (fn [m [k v]] (assoc-vec m k v))
    {}
    (map vector keywords groups)))

(defn- urldecode
  "Encode a urlencoded string using the default encoding."
  [string]
  (URLDecoder/decode string))

(defmulti match
  "Match a compiled route or string against a URI string. Returns the matched
  keywords of the route.
  e.g. (match \"/product/:id\" \"/product/10\")
       -> {:id 10}"
  (fn [route uri] (type route)))

(defmethod match String
  [route uri]
  (match (route-compile route) uri))

(defmethod match ::route
  [route uri]
  (let [matcher (re-matcher (route :regex) (or uri "/"))]
    (if (.matches matcher)
      (assoc-keywords-with-groups
        (map urldecode (re-groups* matcher))
        (route :keywords)))))

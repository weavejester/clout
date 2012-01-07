(ns clout.core
  "Library for parsing the Rails routes syntax."
  (:require [clojure.string :as string])
  (:import java.util.Map
           [java.net URLDecoder URLEncoder]))

;; Regular expression utilties

(def ^{:private true} re-chars
  (set "\\.*+|?()[]{}$^"))

(defn- re-escape
  "Escape all special regex chars in a string."
  [s]
  (string/escape
    s
    #(if (re-chars %) (str \\ %))))

(defn re-groups*
  "More consistant re-groups that always returns a vector of groups, even if
  there is only one group."
  [matcher]
  (for [i (range (.groupCount matcher))]
    (.group matcher (inc i))))

;; Route matching

(defn path-decode
  "Decode a path segment in a URI. Defaults to using UTF-8 encoding."
  ([path]
     (path-decode path "UTF-8"))
  ([path encoding]
     (-> (string/replace path "+" (URLEncoder/encode "+" encoding))
         (URLDecoder/decode encoding))))

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

(defn- request-url
  "Return the complete URL for the request."
  [request]
  (str (name (:scheme request))
       "://"
       (get-in request [:headers "host"])
       (:uri request)))

(defn- path-info
  "Return the path info for the request."
  [request]
  (or (:path-info request)
      (:uri request)))

(defprotocol Route
  (route-matches [route request]
    "If the route matches the supplied request, the matched keywords are
    returned as a map. Otherwise, nil is returned."))

(defrecord CompiledRoute [re keys absolute?]
  Route
  (route-matches [route request]
    (let [path-info (if absolute?
                      (request-url request)
                      (path-info request))
          matcher   (re-matcher re path-info)]
      (if (.matches matcher)
        (assoc-keys-with-groups
          (map path-decode (re-groups* matcher))
          keys)))))

;; Compile routes

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
          word    #":([\p{L}_][\p{L}_0-9-]*)"
          literal #"(:[^\p{L}_*]|[^:*])+"
          word-group #(keyword (.group % 1))
          word-regex #(regexs (word-group %) "[^/,;?]+")]
      (CompiledRoute.
        (re-pattern
          (apply str
            (lex path
              splat   "(.*?)"
              word    #(str "(" (word-regex %) ")")
              literal #(re-escape (.group %)))))
        (remove nil?
          (lex path
            splat   :*
            word    word-group
            literal nil))
        (absolute-url? path)))))

(extend-type String
  Route
  (route-matches [route request]
    (route-matches (route-compile route) request)))


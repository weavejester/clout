(ns clout.core
  "Library for parsing the Rails routes syntax."
  (:require [clojure.string :as string]
            [clojure.set :as set])
  (:import java.util.Map
           java.util.regex.Matcher
           [java.net URLDecoder URLEncoder]))

;; Regular expression utilties

(def ^:private re-chars (set "\\.*+|?()[]{}$^"))

(defn- re-escape
  "Escape all special regex chars in a string."
  [s]
  (string/escape
    s
    #(if (re-chars %) (str \\ %))))

(defn re-groups*
  "More consistant re-groups that always returns a vector of groups, even if
  there is only one group."
  [^Matcher matcher]
  (for [i (range (.groupCount matcher))]
    (.group matcher (int (inc i)))))

;; Route matching

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

(defrecord CompiledRoute [source re keys absolute?]
  Route
  (route-matches [_ request]
    (let [path-info (if absolute?
                      (request-url request)
                      (path-info request))
          matcher   (re-matcher re path-info)]
      (if (.matches matcher)
        (assoc-keys-with-groups (re-groups* matcher) keys))))
  Object
  (toString [_] source))

;; Compile routes

(defn- lex-1
  "Lex one symbol from a string, and return the symbol and trailing source."
  [src clauses]
  (some
    (fn [[re action]]
      (let [matcher (re-matcher re src)]
        (if (.lookingAt matcher)
          [(if (fn? action) (action matcher) action)
           (subs src (.end matcher))])))
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
  "True if the path contains an absolute or scheme-relative URL."
  [path]
  (boolean (re-matches #"(https?:)?//.*" path)))

(def ^:private re-word    #":([\p{L}_][\p{L}_0-9-]*)")
(def ^:private re-literal #"(:[^\p{L}_*]|[^:*])+")

(defn- word-group [^Matcher m]
  (keyword (.group m 1)))

(defn- build-route-regex [path regexs]
  (re-pattern
    (apply str
      (lex path
        #"\*"      "(.*?)"
        #"^//"     "https?://"
        re-word    #(str "(" (regexs (word-group %) "[^/,;?]+") ")")
        re-literal #(re-escape (.group ^Matcher %))))))

(defn- find-path-keys [path]
  (remove nil?
    (lex path
      #"\*"      :*
      re-word    word-group
      re-literal nil)))

(defn route-compile
  "Compile a path string using the routes syntax into a uri-matcher struct."
  ([path]
     (route-compile path {}))
  ([path regexs]
     (let [path-keys (find-path-keys path)]
       (assert (set/subset? (set (keys regexs)) (set path-keys))
               "unused keys in regular expression map")
       (CompiledRoute.
        path
        (build-route-regex path regexs)
        (vec path-keys)
        (absolute-url? path)))))

(extend-type String
  Route
  (route-matches [route request]
    (route-matches (route-compile route) request)))


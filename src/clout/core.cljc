(ns clout.core
  "A small language for routing."
  (:require [clojure.string :as string]
            [clojure.set :as set]
            [instaparse.core :as insta]
            #?(:cljs [clout.internal.regex :refer [re-unicode-letter]])))

(def ^:private re-chars (set "\\.*+|?()[]{}$^"))
#?(:cljs (def ^:private re-char-escapes
           (into {} (for [c re-chars] [c (str "\\" c)]))))

(defn- re-escape [s]
  (string/escape s #?(:clj #(if (re-chars %) (str \\ %)) :cljs re-char-escapes)))

(defn- re-match-groups [re s]
  #?(:clj
     (let [matcher (re-matcher re s)]
       (when (.matches matcher)
         (for [i (range (.groupCount matcher))]
           (.group matcher (int (inc i))))))
     :cljs
     (if (string? s)
       (let [matches (.exec re s)]
         (when (= (first matches) s)
           (rest matches)))
       (throw (js/TypeError. "re-match-groups must match against string.")))))

(defn- assoc-conj [m k v]
  (assoc m k
           (if-let [cur (get m k)]
             (if (vector? cur)
               (conj cur v)
               [cur v])
             v)))

(defn- assoc-keys-with-groups [groups keys]
  (reduce (fn [m [k v]] (assoc-conj m k v))
          {}
          (map vector keys groups)))

(defn- request-url [request]
  (str (name (:scheme request))
       "://"
       (get-in request [:headers "host"])
       (:uri request)))

(defn- path-info [request]
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
                      (path-info request))]
      (let [groups (re-match-groups re path-info)]
        (when groups
          (assoc-keys-with-groups groups keys)))))
  Object
  (toString [_] source))

(def ^:private route-parser
  (insta/parser
   (let [letter #?(:clj "\\p{L}" :cljs re-unicode-letter)]
     (str
      "route    = (scheme / part) part*
       scheme   = #'(https?:)?//'
       <part>   = literal | escaped | wildcard | param
       literal  = #'(:[^" letter "_*{}\\\\]|[^:*{}\\\\])+'
       escaped  = #'\\\\.'
       wildcard = '*'
       param    = key pattern?
       key      = <':'> #'([" letter "_][" letter "_0-9-]*)'
       pattern  = '{' (#'(?:[^{}\\\\]|\\\\.)+' | pattern)* '}'"))
   :no-slurp true))

(defn- parse [parser text]
  (let [result (insta/parse parser text)]
    (if (insta/failure? result)
      (throw (ex-info "Parse error in route string" {:failure result}))
      result)))

(defn- find-route-key [form]
  (case (first form)
    :wildcard :*
    :param    (-> form second second keyword)))

(defn- route-keys [parse-tree]
  (->> (rest parse-tree)
       (filter (comp #{:param :wildcard} first))
       (map find-route-key)))

(defn- trim-pattern [pattern]
  (some-> pattern (subs 1 (dec (count pattern)))))

(defn- param-regex [regexs key & [pattern]]
  (let [pattern (or (trim-pattern pattern) (regexs key) "[^/,;?]+")]
    (str "(" #?(:clj  pattern
                :cljs (if (instance? js/RegExp pattern)
                        (.-source pattern)
                        pattern)) ")")))

(defn- route-regex [parse-tree regexs]
  (insta/transform
   {:route    (comp re-pattern #?(:cljs #(str "^" % "$")) str)
    :scheme   #(if (= % "//") "https?://" %)
    :literal  re-escape
    :escaped  #(re-escape (subs % 1))
    :wildcard (constantly "(.*?)")
    :param    (partial param-regex regexs)
    :key      keyword
    :pattern  str}
   parse-tree))

(defn- absolute-url? [path]
  (boolean (re-matches #"(https?:)?//.*" path)))

(defn route-compile
  "Compile a route string for more efficient route matching."
  ([path]
   (route-compile path {}))
  ([path regexs]
   (let [ast (parse route-parser path)
         ks  (route-keys ast)]
     (assert (set/subset? (set (keys regexs)) (set ks))
             "unused keys in regular expression map")
     (CompiledRoute.
      path
      (route-regex ast regexs)
      (vec ks)
      (absolute-url? path)))))

(extend-type #?(:clj String :cljs string)
  Route
  (route-matches [route request]
    (route-matches (route-compile route) request)))

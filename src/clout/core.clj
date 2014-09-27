(ns clout.core
  "A small language for routing."
  (:require [clojure.string :as string]
            [clojure.set :as set]
            [instaparse.core :as insta]))

(def ^:private re-chars (set "\\.*+|?()[]{}$^"))

(defn- re-escape [s]
  (string/escape s #(if (re-chars %) (str \\ %))))

(defn- re-groups* [^java.util.regex.Matcher matcher]
  (for [i (range (.groupCount matcher))]
    (.group matcher (int (inc i)))))

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
                      (path-info request))
          matcher   (re-matcher re path-info)]
      (if (.matches matcher)
        (assoc-keys-with-groups (re-groups* matcher) keys))))
  Object
  (toString [_] source))

(def ^:private route-parse
  (insta/parser
   "route    = (scheme / part) part*
    scheme   = #'(https?:)?//'
    <part>   = literal | param | wildcard
    literal  = #'(:[^\\p{L}_*]|[^:*])+'
    param    = <':'> #'([\\p{L}_][\\p{L}_0-9-]*)'
    wildcard = '*'"))

(defn- route-keys [parse-tree]
  (->> (rest parse-tree)
       (filter (comp #{:param :wildcard} first))
       (map (comp keyword second))))

(defn- route-regex [parse-tree regexs]
  (insta/transform
   {:route    (comp re-pattern str)
    :scheme   #(if (= % "//") "https?://" %)
    :literal  re-escape
    :param    #(str "(" (regexs (keyword %) "[^/,;?]+") ")")
    :wildcard (constantly "(.*?)")}
   parse-tree))

(defn- absolute-url? [path]
  (boolean (re-matches #"(https?:)?//.*" path)))

(defn route-compile
  "Compile a route string for more efficient route matching."
  ([path]
     (route-compile path {}))
  ([path regexs]
     (let [ast (route-parse path)
           ks  (route-keys ast)]
       (assert (set/subset? (set (keys regexs)) (set ks))
               "unused keys in regular expression map")
       (CompiledRoute.
        path
        (route-regex ast regexs)
        (vec ks)
        (absolute-url? path)))))

(extend-type String
  Route
  (route-matches [route request]
    (route-matches (route-compile route) request)))

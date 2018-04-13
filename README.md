# Clout

[![Build Status](https://travis-ci.org/weavejester/clout.svg?branch=master)](https://travis-ci.org/weavejester/clout)

Clout is a library for matching [Ring][1] HTTP requests. It uses the same
routing syntax as used by popular Ruby web frameworks like Ruby on Rails and
Sinatra.

[1]: https://github.com/ring-clojure/ring

## Installation

Add the following to your project.clj dependencies:

```clj
[clout "2.2.1"]
```

## Usage

Require Clout in the normal way:

```clj
(require '[clout.core :as clout])
```

These following examples also make use of the [Ring-Mock][2] library
to generate Ring request maps:

[2]: https://github.com/ring-clojure/ring-mock

```clj
(require '[ring.mock.request :as mock])
```

Routes can match by keyword:

```clj
(clout/route-matches
 "/article/:title"
 (mock/request :get "/article/clojure"))

=> {:title "clojure"}
```

Or with wildcards:

```clj
(clout/route-matches
 "/public/*"
 (mock/request :get "/public/style/screen.css"))
 
=> {:* "style/screen.css"}
```

Clout can also match absolute routes:

```clj
(clout/route-matches
 "http://subdomain.example.com/"
 (mock/request :get "http://subdomain.example.com/"))

=> {}
```
And scheme-relative routes:

```clj
(clout/route-matches
 "//subdomain.example.com/"
 (mock/request :get "http://subdomain.example.com/"))

=> {}

(clout/route-matches
 "//subdomain.example.com/"
 (mock/request :get "https://subdomain.example.com/"))
 
=> {}
```

Clout supports both keywords and wildcards. Keywords (like ":title") will
match any character but the following: `/ . , ; ?`. Wildcards (*) will match
anything.

If a route does not match, nil is returned:

```clj
(clout/route-matches "/products" (mock/request :get "/articles"))

=> nil
```

For additional performance, you can choose to pre-compile a route:

```clj
(def user-route
  (clout/route-compile "/user/:id"))

(clout/route-matches user-route (mock/request :get "/user/10"))

=> {:id "10"}
```

When compiling a route, you can specify a map of regular expressions to use
for different keywords. This allows more specific routing:

```clj
(def user-route
  (clout/route-compile "/user/:id" {:id #"\d+"}))

(clout/route-matches user-route (mock/request :get "/user/10"))

=> {:user "10"}

(clout/route-matches user-route (mock/request :get "/user/jsmith"))

=> nil
```

You can also specify regular expressions inline in braces after the
keyword:

```clj
(def user-route
  (clout/route-compile "/user/:id{\\d+}"))
```

Note that regular expression escape sequences (like `\d`) need to be
double-escaped when placed inline in a string.

## License

Copyright © 2018 James Reeves

Distributed under the Eclipse Public License either version 1.0 or (at
your option) any later version.

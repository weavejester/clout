Clout
=====

Clout is a library for matching HTTP routes in Clojure. It uses the same
routing syntax as used by popular Ruby web frameworks like Ruby on Rails and
Sinatra.

Here is an example of use:

    user=> (use 'clout)
    nil
    user=> (route-matches "/article/:title" "/article/clojure")
    {"title" "clojure"}
    user=> (route-matches "/public/*" "/public/style/screen.css")
    {"*" "style/screen.css"}

Clout can also match Ring requests:

    user=> (route-matches "/book/:id"
                          {:request-method :get
                           :headers {"Host" "example.com"}
                           :uri "/book/123"}
                           :body nil})
    {"id" "123"}

Clout supports both keywords and wildcards. Keywords (like ":title") will
match any character but the following: `/ . , ; ?`. Wildcards will match
anything.

If a route does not match, nil is returned:

    user=> (route-matches "/products" "/articles")
    nil

For additional performance, you can choose to pre-compile a route:

    user=> (def user-route (route-compile "/user/:id"))
    #'user/user-route
    user=> (route-matches user-route "/user/10")
    {"user" "10"}

When compiling a route, you can specify a map of regular expressions to use
for different keywords. This allows more specific routing:

    user=> (def user-route (route-compile "/user/:id" {"id" #"\d+"}))
    #'user/user-route
    user=> (route-matches user-route "/user/10")
    {"user" "10"}
    user=> (route-matches user-route "/user/jsmith")
    nil

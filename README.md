xhttpc - eXtensible HTTP(S) Client for Erlang
==========================================

Easily extensible - just add your own middlewares.
Simple core - all the additional functionality splitted to middleware modules.
Not a http client, but http client wrapper: built on top of lhttpc HTTP client,
support for other HTTP clients (httpc, hackney etc) can be added easily.

Inspired mostly by python's urllib2.
Battle tested in production system.

API
---

```erlang
%% Initialize session
Session = xhttpc:init(
    [{compression_middleware, []},
     {defaults_middleware, [{append_hdrs, {"User-Agent", "xhttpc 0.0.1"}},
                            {default_options, {timeout, 10}}]}]),

%% Perform HTTP request, using positional arguments
{S2, {ok, {200, StatusLine}, Headers, Body}} =
    xhttpc:request(Session, "http://example.com/",
                   get, [], undefined, []),

%% Perform HTTP request, using record `#request{}` as argument
{S3, {ok, {200, StatusLine}, Headers, Body}} =
    xhttpc:request(S2, #request{url="http://example.com/"}),

%% Terminate session
ok = xhttpc:terminate(S3).
```
```
 API call                      | compression_mw |     | defaults_mw   |     | lhttpc/httpc/etc |
-------------------------------+----------------+-----+---------------+-----+------------------+
xhttpc:request/2           --> |  request/3     | --> |  request/3    | --> | lhttpc:request/5 |
                               |                |     |               |     |       VVV        |
response()                 <-- |  response/4    | <-- |  response/4   | <-- |     response()   |

xhttpc:call(defaults_mw, Arg)<-:----------------:---> |  call/2       |     |                  |

xhttpc:(init|terminate)    --> | init|terminate |     |               |     |                  |
                           \---:----------------:---> | init|terminate|     |                  |
```

Batteries included
------------------

* Traffic compression (compression_middleware)
* Redirects (redirect_middleware)
* Cookies (cookie_middleware + RFC6265 cookie parser / serializer)
* Automatic http referer (referer_middleware)
* Constant default / additional headers and options for each request (defaults_middleware)

TODO
----

* Support for partial upload
  actually supported, but just need to create some wrappers
* Support for partial download
  eg, for compression middleware

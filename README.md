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
     {headers_middleware, [{"User-Agent", "xhttpc 0.0.1"}]}]),

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

Batteries included
------------------

* Compression (compression_middleware)
* Redirects (redirect_middleware)
* Cookies (cookie_middleware + RFC6265 cookie parser / serializer)
* Http referrer (referrer_middleware)
* TODO: Constant additional headers (headers_middleware)

TODO
----

* Support for partial upload
  actually supported, but just need to create some wrappers
* Support for partial download
  eg, for compression middleware

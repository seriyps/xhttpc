xhttpc - eXtensible HTTP(S) Client for Erlang
=============================================

Easily extensible - just add your own middlewares.
Simple tiny core - all the additional functionality splitted to middleware modules.
No extra processes and message passing.
Not a http client, but http client wrapper - built on top of lhttpc HTTP client,
support for other HTTP clients (httpc, hackney, ibrowse etc) can be added easily.
Batteries included - see below.

Inspired mostly by python's [urllib2](http://docs.python.org/2/library/urllib2.html).
Battle-tested in production system.

API
---

```erlang
%% Start underlying HTTP client library
% if you use OTP's httpc
inets:start().
% if you use lhttpc (recommended)
[application:start(App) || App <- [crypto, public_key, ssl, lhttpc]].

%% Initialize session
Session = xhttpc:init(
    [{compression_middleware, []},
     {cookie_middleware, []},
     {defaults_middleware, [{append_hdrs, [{"User-Agent", "xhttpc 0.0.1"}]},
                            {default_options, [{timeout, 10}]}]}]),

%% Perform HTTP request, using positional arguments
{S2, {ok, {{200, StatusLine}, Headers, Body}}} =
    xhttpc:request(Session, "http://example.com/",
                   post, [{"Content-Type", "application/x-www-form-urlencoded"}],
                   <<"a=b">>, [{timeout, 10000}]),

%% Perform HTTP request, using `#request{}` record API
-include_lib("xhttpc/include/xhttpc.hrl").
{S3, {ok, {{200, StatusLine}, Headers, Body}}} =
    xhttpc:request(S2, #request{url="http://example.com/"}),

%% Interact with middleware's state
{S4, Cookies} = xhttpc:call(S3, cookie_middleware, {get_cookies, "example.com", "/"}).

%% Terminate session
ok = xhttpc:terminate(S4).
```

Api sequence diagram:
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

Try `rebar doc` to generate HTML API documentation from sources.

FAQ
---

Q: I think API is too verbose / I don't like records in API.

A: Just make your own wrappers / shortcuts.


Q: I don't like this session-chains `S1, S2, S3...`.

A: You can store sessions in a process / gen_server / public ETS or any storage
you like, eg:

```erlang
% this will spawn new gen_server and store session in it.
Pid = my_wrapper:init([...]).
% following calls will lookup / update session from storage using `Pid` as identifier.
{ok, {{200, StatusLine}, Headers, Body}} = my_wrapper:get(Pid, "http://example.com/").
Result = my_wrapper:call(Pid, ...).
ok = my_wrapper:terminate(Pid).
```
If you'll create shared storage for sessions (like public ETS table), you can also use
your worker proces's `self()` pid as session ID and don't pass `Pid` parameter at all.

Batteries included
------------------

* Traffic compression (compression_middleware)
* Redirects, with loop detection (redirect_middleware)
* Cookies (cookie_middleware + RFC6265 cookie parser / serializer)
* Automatic http referer (referer_middleware)
* Constant default / additional headers and options for each request (defaults_middleware)
* TODO: basic HTTP auth middleware

TODO
----

* Support for partial upload
  actually supported, but just need to create some wrappers
* Support for partial download
  eg, for compression middleware
* More examples:
  ETS cookie storage
  API with external session storage (to avoid S1, S2, S3, SN problem)
* More http client  adapters:
  * DONE httpc
  * hackney
  * ibrowse

How to make your own middleware
-------------------------------

As an example, let's implement middleware, named 'logging_middleware'. It will log
some user-defined uniq session ID, request method, url, response code and run-time to error_logger.
Middleware must implement `xhttpc_middleware` behaviour (see xhttpc_middleware.erl).

This include following methods:

* `init/1` - will be called when user initialize session by call `xhttpc:init/1`.
* `request/3` - before request will be sended to server.
* `response/4` - before reply will be returned to user.
* `terminate/2` - when user terminates session by call `xhttpc:terminate/2`, and
* `call/2` - if user wish to call concrete middleware like `xhttpc:call(logging_middleware, Args)`.

So, first we add standard module header:

```erlang
-module(logging_middleware).
-behaviour(xhttpc_middleware).  % declare behaviour

-export([init/1, request/3, response/4, terminate/2, call/2]).
-include_lib("xhttpc/include/xhttpc.hrl").  % #request{} definition there.

-record(state, {session_id, timer_start, n_requests=0}).  % middleware's internal state
```

Now, let's implement some callback functions:

```erlang
init([SessionID]) ->
  {ok, #state{session_id=SessionID}}.
```
So, when user create new session `S = xhttpc:init([{logging_middleware, ["MySessionID"]}]).`, this
function will be called and it return `{ok, #state{session_id="MySessionID"}}`.
We return #state{} just like gen_server's state - it will be passed to each
API function call as last argument later.

```erlang
request(Session, Request, #state{n_requests=NReqs} = State) ->
    Begin = os:timestamp(),
    {update, Session, Request, State#state{timer_start=Begin,
                                           n_requests=NReqs + 1}}.
```
Now, when user call `xhttpc:request/2,6`, request first goes to each middlewares `request/3`
callback. We only capture current timestamp before request been executed
(sended to the server) and increment requests counter.

```erlang
response(Session, #xhttpc_request{url=Url, method=Method}, Response,
         #state{timer_start=Start, n_requests=NReqs, session_id=SID} = State) ->
    Runtime = timer:now_diff(os:timestamp(), Start) / 1000000,
    Status = case Response of
        {ok, {{Code, _}, _Hdrs, _Body}} ->
            Code;
        {error, Reason} ->
            Reason
    end,
    % lager:info(...)
    error_logger:info_msg("Request #~p of session ~p; Method: ~p Url: ~s runtime: ~p status: ~p",
                          [NReqs, SID, Method, Url, Runtime, Status]),
    noaction.
```
After request has been executed and response been returned by underlying HTTP client,
middleware's `response/4` will be called (in reverse order). We calculate request's runtime as diff
of saved in `State` request-start-time and current timestamp. Next, if response
was successful - extract HTTP response code, if not - error reason. And finally,
log all the data to `error_logger`. Note, that we return atom `noaction` as a flag
that Session, State and Response hasn't been modified by this callback.

```erlang
terminate(Reason, #state{session_id=SID}) ->
    error_logger:info_msg("Session ~p has been terminated with reason ~p", [SID, Reason]),
    ok.
```

No comments. Just callback for `xhttpc:terminate/1,2`.

Now imagine, that we need to get number of currently executed requests with that
session. That's why we have `call/2` method here. First, we may implement
user-friendly API method for it:

```erlang
-export([get_requests_count/1]).

get_requests_count(Session) ->
    xhttpc:call(Session, ?MODULE, get_requests_count).
```
This is just a wrapper around `xhttpc:call/3`. Next, we implement a callback

```erlang
call(get_requests_count, #state{n_requests=NReqs} = State) ->
    {NReqs, State}.
```

So simple! Isn't it? Note that we return 2-tuple from `call/2` - that way we can
not only retrieve some data from middleware's state, but can also mutate it's state
(for example, we can change it's `session_id` or reset requests counter on the fly).

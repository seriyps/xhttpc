%%% @author Sergey Prokhorov <me@seriyps.ru>
%%% @copyright (C) 2013, Sergey Prokhorov
%%% @doc
%%% xhttpc main API module.
%%% Example session:
%%% <pre><code>
%%% % initialize new session with 3 middlewares
%%% S = xhttpc:init([{cookie_middleware, []},
%%%                  {referer_middleware, []},
%%%                  {compression_middleware, []}]),
%%%
%%% % perform request
%%% {S2, {ok, {{200, "OK"}, ResponseHeaders, ResponseBody}}} =
%%%     xhttpc:request(S, #xhttpc_request{url="http://example.com/"}),
%%% % the internal states of middlewares (cookies, current referrer etc) + some
%%% % meta-information are stored in S2, so, if you need to perform one more
%%% % request, you should pass new session as first parameter.
%%%
%%% % perform 2'nd request
%%% {S3, {ok, {{200, "OK"}, ResponseHeaders, ResponseBody}}} =
%%%     xhttpc:request(S2, #xhttpc_request{url="http://example.com/some/page"}),
%%%
%%% % terminate session
%%% ok = xhttpc:terminate(S3).
%%% </code></pre>
%%% @end
%%% Created : 14 Jun 2013 by Sergey Prokhorov <me@seriyps.ru>

-module(xhttpc).

-export([init/1, init/2, terminate/2, call/3]).
-export([request/6, request/2]).
-export([header_value/2, normalize_headers/1, normalize_header_name/1, session_param/2]).

-export_type([session/0, request/0, response/0]).
-export_type([http_header/0, http_options/0]).

-include("xhttpc.hrl").

-ifdef(TEST).
-include_lib("eunit/include/eunit.hrl").
-endif.

-record(session,
        {mw_args :: [middleware()],    %initial middleware arguments
         mw_order :: [atom()],         %in which order middlewares should be called
         mw_states :: orddict:orddict(), % {atom(), any()} % orddict with middleware states
         depth = 0 :: non_neg_integer(), % for nested requests (like request from redirect_middleware)
         http_backend = lhttpc
        }).
%% HTTP client's session. Shouldn't be used directly by users!


-type session() :: #session{}.
-type request() :: #xhttpc_request{}.
-type response() :: http_response().

-type http_header() :: {string(), string()}.

-type http_options() :: [{disable_middlewares, [module()]}
                         | {timeout, timeout()}
                         | {client_options, any()}
                         | {atom(), any()}].
%% `disable_middlewares' - list of middlewares, that should be skipped for
%% this particular request.
%% `timeout' - request timeout. If request  isn't finished during this time,
%% `{error, timeout}' will be returned. Note: you may use `infinity' as timeout
%% value.
%% `client_options' - this options will be passed directly to underlying
%% HTTP client. Use with caution, only if you know which client you use and
%% have no plans to change it in the future.

-type middleware() :: {Module :: module(), Args :: [any()]}.

-type http_method() :: get | post | head | string(). % string() is "GET" | "POST" etc

-type http_post_body() :: iolist().

-type http_resp_body() :: binary() | undefined.
-type http_response() :: {ok, {{StatusCode :: pos_integer(), StatusString :: string()},
                               Headers :: [http_header()],
                               Body :: http_resp_body()}}
                       | {error, any()}.


%% Main API

%% @doc Initialize middlewares (call each middleware's `init/1' and collect states)
-spec init([middleware() | atom()]) -> session().
init(Middlewares) ->
    init(Middlewares, lhttpc).

%% @doc Like {@link init/1}, but allow to specify http backend (hardcoded for now)
init(Middlewares, HttpBackend) ->
    Middlewares1 = lists:map(fun({_, _}=NameArg) -> NameArg;
                                (Name) when is_atom(Name) -> {Name, []}
                             end, Middlewares),
    InitFun = fun({Mod, Args}) ->
                      {ok, State} = Mod:init(Args),
                      {Mod, State}
              end,
    States = orddict:from_list(lists:map(InitFun, Middlewares1)),
    #session{mw_args=Middlewares1,
             mw_order=[Mod || {Mod, _} <- Middlewares1],
             mw_states=States,
             http_backend=HttpBackend}.

%% @doc Terminate session (call each middleware's `terminate/2')
-spec terminate(session(), any()) -> ok.
terminate(Session, Reason) ->
    %% XXX: middlewares `terminate' called not in mw_order, but in
    %% regular `sort' order!
    [ok = Module:terminate(Reason, State)
     || {Module, State} <- orddict:to_list(Session#session.mw_states)],
    ok.

%% @doc Call middleware (call `call/2' of middleware `Mod')
-spec call(session(), module(), any()) -> {session(), any()}.
call(#session{mw_states=States} = Session, Mod, Args) ->
    State = orddict:fetch(Mod, States),
    {Response, NewState} = Mod:call(Args,  State),
    NewSession = update_middleware_state(Mod, Session, NewState),
    {NewSession, Response}.

%% @doc Perform HTTP request (positional argument API)
-spec request(session(), string(), http_method(), [http_header()],
              http_post_body(), http_options()) ->
                     {session(), http_response()}.
request(S, Url, Method, Headers, Body, Options) ->
    request(S, #xhttpc_request{url = Url,
                               method = Method,
                               headers = Headers,
                               body = Body,
                               options = Options}).

%% @doc Perform HTTP request (record API)
-spec request(session(), request()) -> {session(), http_response()}.
request(#session{mw_order=MWOrder, depth=Depth} = S,
        #xhttpc_request{options=Options, headers=Headers} = Request) ->
    MWOrder1 = enabled_middlewares(
                 MWOrder,
                 proplists:get_value(disable_middlewares, Options)),
    S0 = S#session{depth = Depth + 1},      % see referer_middleware for reason
    Request1 = Request#xhttpc_request{headers=normalize_headers(Headers)},
    %% Apply pre-request middlewares
    {S1, Request2} = apply_request_middlewares(S0, Request1, MWOrder1),

    Resp = run_request(Request2, S#session.http_backend),
    Resp1 = normalize_response(Resp),

    %% Apply post-request middlewares
    {S2, Resp2} = apply_response_middlewares(S1, Request2, Resp1, MWOrder1),
    S3 = S2#session{depth = S2#session.depth - 1},
    {S3, Resp2}.


%% helpers

%% @doc Returns value of first occurrence of the header, named `Name'.
-spec header_value(string(), [http_header()]) -> string() | undefined.
header_value(Name, Headers) ->
    CanonicalName = normalize_header_name(Name),
    proplists:get_value(CanonicalName, Headers).

%% @doc Prepare headers for comparison, so "user-agent" and "User-Agent" can be
%% compared as equal after normalization
-spec normalize_headers([http_header()]) -> [http_header()].
normalize_headers(Headers) ->
    [{normalize_header_name(Name), Value} || {Name, Value} <- Headers].

%% @doc Prepare header name for comparison (see {@link normalize_headers/1})
%% If your middleware adds new headers, you SHOULD call this function with
%% header name like:
%% <code>NewHeaders = [{normalize_header_name("user-agent"), "xhttpc"}] ++ Headers.</code>
%% If it's too long to type, just define a macro:
%% <code>-define(NH(Name), xhttpc:normalize_header_name(Name)).</code>
-spec normalize_header_name(string()) -> string().
normalize_header_name(Name) ->
    %% string:to_lower(Name).
    %% string:to_upper(Name).
    capitalize_token(Name).

%% @doc Return some session's params. Currently only `depth' supported.
-spec session_param(session(), atom()) -> any().
session_param(#session{depth=Depth}, depth) ->
    Depth.

%%
%% Internal
%%

normalize_response({ok, {Status, Headers, Body}}) ->
    {ok, {Status, normalize_headers(Headers), Body}};
normalize_response(Other) ->
    Other.


capitalize_token(Str) ->
    %% Idea borrowed from cowboy_bstr:capitalize_token/1
    capitalize_token(Str, true).

capitalize_token([$- | Rest], _) ->
    [$- | capitalize_token(Rest, true)];
capitalize_token([Chr | Rest], true) ->
    [string:to_upper(Chr) | capitalize_token(Rest, false)];
capitalize_token([Chr | Rest], false) ->
    [string:to_lower(Chr) | capitalize_token(Rest, false)];
capitalize_token([], _) ->
    [].


%% In case we need possibility to add custom HTTP backends in a flexible way,
%% we can rewrite xhttpc such that just define `run_request' functions in
%% separate adapter modules, like `xhttpc_lhttpc', `xhttpc_test_client',
%% `xhttpc_httpc' etc and call them from
%% request/2 like HttpBackend:run_request(...).
%% For now we just hardcode backends for simplicity.
run_request(#xhttpc_request{url=Url, method=Method, headers=Headers,
                            body=Body, options=Options}, lhttpc) ->
    Timeout = proplists:get_value(timeout, Options, infinity),
    ClientOptions = proplists:get_value(client_options, Options, []),
    RealBody = case Body of undefined -> []; _ -> Body end,
    lhttpc:request(Url, Method, Headers, RealBody, Timeout, ClientOptions);
run_request(#xhttpc_request{url=Url, method=Method, headers=Headers,
                           body=Body, options=Options}, httpc) ->
    Timeout = proplists:get_value(timeout, Options, infinity),
    ClientOptions = proplists:get_value(client_options, Options, []),
    % Don't pass Options for now (4-nd param).
    % Maybe add foldl + ordsets to split options to HTTPOptions and Options next
    % time.
    HttpOptions = [{autoredirect, false},
                   {timeout, Timeout} | ClientOptions],
    Options1 = [{body_format, binary}],
    Request = case Body of
                  No when (No == []) or (No == undefined) ->
                      {Url, Headers};
                  _ ->
                      {Url, Headers, "", Body}
              end,
    case httpc:request(Method, Request, HttpOptions, Options1) of
        {ok, {{_, RCode, RStatus}, RHeaders, RBody}} ->
            {ok, {{RCode, RStatus}, RHeaders, RBody}};
        Error -> Error
    end;
run_request(#xhttpc_request{options=Options} = Request, test_client) ->
    %% test (fake) HTTP client. Returns value of 'respoonse' client option
    %% as response (if defined), else return dummy 200 response.
    ClientOptions = proplists:get_value(client_options, Options, []),
    case proplists:get_value(response, ClientOptions) of
        Resp when is_tuple(Resp) ->
            Resp;
        Fun when is_function(Fun) ->
            Fun(Request);
        undefined ->
            {ok,
             {{200, "OK"},
             [{"Test-Header-Name", "Test-Header-Value"}],
             <<"test-response">>}}
    end.


enabled_middlewares(Middlewares, undefined) ->
    Middlewares;
enabled_middlewares(Middlewares, Disabled) ->
    %% lists:substract/2 also works
    [Mod || Mod <- Middlewares, not lists:member(Mod, Disabled)].


apply_request_middlewares(S, Request, []) ->
    {S, Request};
apply_request_middlewares(#session{mw_states=States} = S, Request, [Mod | Mods]) ->
    State = orddict:fetch(Mod, States),
    case Mod:request(S, Request, State) of
        noaction ->
            apply_request_middlewares(S, Request, Mods);
        {update, S2, Request2, State2} ->
            S3 = update_middleware_state(Mod, S2, State2),
            apply_request_middlewares(S3, Request2, Mods)
    end.

apply_response_middlewares(S, Request, Response, MWOrder) ->
    ReverseMWOrder = lists:reverse(MWOrder),
    apply_response_middlewares1(S, Request, Response, ReverseMWOrder).

apply_response_middlewares1(S, _Request, Response, []) ->
    {S, Response};
apply_response_middlewares1(#session{mw_states=States} = S, Request, Response, [Mod | Mods]) ->
    State = orddict:fetch(Mod, States),
    case Mod:response(S, Request, Response, State) of
        noaction ->
            apply_response_middlewares1(S, Request, Response, Mods);
        {update, S2, Response2, State2} ->
            S3 = update_middleware_state(Mod, S2, State2),
            apply_response_middlewares1(S3, Request, Response2, Mods)
    end.


update_middleware_state(Mod, Session, State) ->
    NewStates = orddict:store(Mod, State, Session#session.mw_states),
    Session#session{mw_states = NewStates}.

-ifdef(TEST).
capitalize_test_() ->
    [?_assertEqual("Test-Header", capitalize_token("test-header")),
     ?_assertEqual("Test-Header", capitalize_token("TEST-HEADER")),
     ?_assertEqual("Test--Header", capitalize_token("test--header"))
    ].
-endif.

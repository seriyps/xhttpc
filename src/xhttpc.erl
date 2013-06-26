%%% @author Sergey Prokhorov <me@seriyps.ru>
%%% @copyright (C) 2013, Sergey Prokhorov
%%% @doc
%%%
%%% @end
%%% Created : 14 Jun 2013 by Sergey Prokhorov <me@seriyps.ru>

-module(xhttpc).

-export([init/1, terminate/2, call/3]).
-export([request/6, request/2]).
-export([header_value/2, normalize_headers/1, normalize_header_name/1]).

-export_type([session/0, request/0, response/0]).

-include("xhttpc.hrl").
%% HTTP client's session. Shouldn't be used directly bu users!
-record(session,
        {mw_args :: [middleware()],    %initial middleware arguments
         mw_order :: [atom()],         %in which order middlewares should be called
         mw_states :: orddict:orddict(), % {atom(), any()} % orddict with middleware states
         http_backend = lhttpc
        }).


-type session() :: #session{}.
-type request() :: #xhttpc_request{}.
-type response() :: http_response().

%% http_header() and http_options() defined in xhttpc.hrl

-type middleware() :: {Module :: module(), Args :: [any()]}.

-type http_method() :: get | post | head | string(). % string() is "GET" | "POST" etc

-type http_post_body() :: iolist().

-type http_resp_body() :: binary() | undefined.
-type http_response() :: {ok, {{pos_integer(), string()}, [http_header()], http_resp_body()}}
                       | {error, atom()}.


%% Main API

%% Initialize middlewares (call each middleware's init/1 and collect states)
-spec init(middleware()) -> session().
init(Middlewares) ->
    InitFun = fun({Mod, Args}) ->
                      {ok, State} = Mod:init(Args),
                      {Mod, State}
              end,
    States = orddict:from_list(lists:map(InitFun, Middlewares)),
    #session{mw_args=Middlewares,
             mw_order=[Mod || {Mod, _} <- Middlewares],
             mw_states=States}.

%% Terminate session (call each middleware's terminate/2)
-spec terminate(session(), any()) -> ok.
terminate(Session, Reason) ->
    %% XXX: middlewares `terminate' called not in mw_order, but in
    %% regular `sort' order!
    [ok = Module:terminate(Reason, State)
     || {Module, State} <- orddict:to_list(Session#session.mw_states)],
    ok.

%% Call middleware (call `call/2' of middleware `Mod')
-spec call(session(), module(), [any()]) -> {session(), any()}.
call(#session{mw_states=States} = Session, Mod, Args) ->
    State = orddict:fetch(Mod, States),
    {Response, NewState} = Mod:call(Args,  State),
    NewSession = update_middleware_state(Mod, Session, NewState),
    {NewSession, Response}.

%% Perform HTTP request (positional argument API)
-spec request(session(), string(), http_method(), [http_header()],
              http_post_body(), http_options()) ->
                     {session(), http_response()}.
request(S, Url, Method, Headers, Body, Options) ->
    request(S, #xhttpc_request{url = Url,
                               method = Method,
                               headers = Headers,
                               body = Body,
                               options = Options}).

%% Perform HTTP request (record API)
-spec request(session(), request()) -> {session(), http_response()}.
request(S, #xhttpc_request{options=Options, headers=Headers} = Request) ->
    MWOrder = enabled_middlewares(
                S#session.mw_order,
                proplists:get_value(disable_middlewares, Options)),
    Request1 = Request#xhttpc_request{headers=normalize_headers(Headers)},
    %% Apply pre-request middlewares
    {S1, Request2} = apply_request_middlewares(S, Request1, MWOrder),
    %% io:format("ROpts ~p, Middlewares ~p~n", [SOpts, MWOrder]),

    Resp = run_request(Request2, S#session.http_backend),
    Resp1 = normalize_response(Resp),

    %% Apply post-request middlewares
    {S2, Resp2} = apply_response_middlewares(S1, Request2, Resp1, MWOrder),
    {S2, Resp2}.


%% helpers

-spec header_value(string(), [http_header()]) -> string() | undefined.
header_value(Name, Headers) ->
    lhttpc_lib:header_value(Name, Headers).

%% Prepare headers for comparision, so "user-agent" and "User-Agent" can be
%% compared as equal after normalization
-spec normalize_headers([http_header()]) -> [http_header()].
normalize_headers(Headers) ->
    [{normalize_header_name(Name), Value} || {Name, Value} <- Headers].

%% Prepare header name for comparision (see `normalize_headers')
%% If your middleware adds new headers, you SHOULD call this function with
%% header name like:
%% <code>NewHeaders = [{normalize_header_name("user-agent"), "xhttpc"}] ++ Headers.</code>
%% If it's too long to type, just define a macro:
%% <code>-define(NH(Name), xhttpc:normalize_header_name(Name)).</code>
-spec normalize_header_name(string()) -> string().
normalize_header_name(Name) ->
    %% TODO: maybe capitalize instead of to_lower?
    string:to_lower(Name).


%%
%% Internal
%%

normalize_response({ok, {Status, Headers, Body}}) ->
    {ok, {Status, normalize_headers(Headers), Body}};
normalize_response(Other) ->
    Other.


run_request(#xhttpc_request{url=Url, method=Method, headers=Headers,
                            body=Body, options=Options}, lhttpc) ->
    Timeout = proplists:get_value(timeout, Options, infinity),
    ClientOptions = proplists:get_value(client_options, Options, []),
    lhttpc:request(Url, Method, Headers, Body, Timeout, ClientOptions).

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

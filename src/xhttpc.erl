%%% @author Sergey Prokhorov <me@seriyps.ru>
%%% @copyright (C) 2013, Sergey Prokhorov
%%% @doc
%%%
%%% @end
%%% Created : 14 Jun 2013 by Sergey Prokhorov <me@seriyps.ru>

-module(xhttpc).

-export([init/1, terminate/2, call/3]).
-export([request/6, request/2]).

-export_type([session/0, request/0, response/0]).


%% session shouldn't be used directly bu users
-record(session,
        {mw_args :: [middleware()],    %initial middleware arguments
         mw_order :: [atom()],         %in which order middlewares should be called
         mw_states :: orddict:orddict() % {atom(), any()} % orddict with middleware states
        }).

%% TODO: move to include/xhttpc.hrl
-record(request,
        {url :: string(),
         method = get :: get | post | head,
         headers = [] :: [http_header()],
         body :: iolist(),
         options = [] :: http_options()
        }).

-type session() :: #session{}.
-type request() :: #request{}.
-type response() :: http_response().

-type middleware() :: {Module :: module(), Args :: [any()]}.

-type http_method() :: get | post | head | string(). % string() is "GET" | "POST" etc
-type http_options() :: [{disable_middlewares, [module()]}
                         | {client_options, any()}
                         | {atom(), any()}].
-type http_post_body() :: iolist().

-type http_header() :: {string(), string()}.
-type http_resp_body() :: binary() | undefined.
-type http_response() :: {ok, {{pos_integer(), string()}, [http_header()], http_resp_body()}}
                       | {error, atom()}.


-spec init(middleware()) -> session().
init(Middlewares) ->
    #session{}.

-spec terminate(session(), any()) -> ok.
terminate(Session, Reason) ->
    ok.

-spec call(session(), module(), [any()]) -> any().
call(Session, Middleware, Args) ->
    ok.


-spec request(session(), string(), http_method(), [http_header()],
              http_post_body(), http_options()) ->
                     {session(), http_response()}.
request(S, Url, Method, Headers, Body, Options) ->
    request(S, #request{url = Url,
                        method = Method,
                        headers = Headers,
                        body = Body,
                        options = Options}).

-spec request(session(), #request{}) -> {session(), http_response()}.
request(S, #request{options=Options} = Request) ->
    MWOrder = enabled_middlewares(
                S#session.mw_order,
                proplists:get_value(disable_middlewares, Options)),

    %% Apply pre-request middlewares
    {S1, NewRequest} = apply_request_middlewares(S, Request, MWOrder),
    %% io:format("ROpts ~p, Middlewares ~p~n", [SOpts, MWOrder]),

    Resp = request(NewRequest),

    %% Apply post-request middlewares
    {S2, Resp2} = apply_response_middlewares(S1, Request, Resp, MWOrder),
    {S2, Resp2}.


request(#request{} = Request) ->
    lhttpc:request().


enabled_middlewares(Middlewares, undefined) ->
    Middlewares;
enabled_middlewares(Middlewares, Disabled) ->
    %% listis:substract/2 also works
    [Mod || Mod <- Middlewares, not lists:member(Mod, Disabled)].


apply_request_middlewares(S, Request, MWOrder) ->
    {S, Request}.

apply_response_middlewares(S, Request, Response, MWOrder) ->
    {S, Response}.

%%% @author Sergey Prokhorov <me@seriyps.ru>
%%% @copyright (C) 2012, Sergey Prokhorov
%%% @doc
%%% HTTP referer middleware
%%% Install 'Referer' HTTP header
%%% TODO: install only text/html responses as referer (or configure allowed
%%% content-types)
%%% @end
%%% Created :  8 Oct 2012 by Sergey Prokhorov <me@seriyps.ru>

-module(referer_middleware).
-behaviour(xhttpc_middleware).

-export([init/1, request/3, response/4, call/2, terminate/2]).
-export([set_referer/2]).

-include("xhttpc.hrl").

-record(state, {ref :: string()}).


set_referer(Session, Url) ->
    xhttpc:call(Session, ?MODULE, {set_referer, Url}).


init([]) ->
    {ok, #state{}};
init([InitialReferer]) ->
    {ok, #state{ref=InitialReferer}}.


request(Session, #xhttpc_request{url=Url} = Req, #state{ref=undefined} = State) ->
    {update, Session, Req, State#state{ref = Url}};
request(Session, #xhttpc_request{url=Url, headers=Headers} = Req, State) ->
    Ref = {<<"Referer">>, State#state.ref},
    NewHdrs = lists:keystore(<<"Referer">>, 1, Headers, Ref),
    {update, Session, Req#xhttpc_request{headers = NewHdrs}, State#state{ref = Url}}.

response(_Session, _Req, _Resp, _State) ->
    noaction.

call({set_referer, Url}, State) ->
    {ok, State#state{ref=Url}}.

terminate(_Reason, _State) ->
    ok.

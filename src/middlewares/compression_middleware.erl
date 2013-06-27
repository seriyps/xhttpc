%%% @author Sergey Prokhorov <me@seriyps.ru>
%%% @copyright (C) 2012, Sergey Prokhorov
%%% @doc
%%% xhttpc compression middleware - set-up `Accept-Encoding: gzip` header and
%%% handles `Content-Encoding: gzip` responses
%%% TODO: deflate
%%% @end
%%% Created :  8 Oct 2012 by Sergey Prokhorov <me@seriyps.ru>

-module(compression_middleware).
-behaviour(xhttpc_middleware).
-include("xhttpc.hrl").

-export([init/1, request/3, response/4, call/2, terminate/2]).


init([]) -> {ok, []}.

request(Session, #xhttpc_request{headers=Headers} = Request, State) ->
    HName = xhttpc:normalize_header_name("Accept-Encoding"),
    Hdr = {HName, "gzip"},
    NewHdrs = lists:keystore(HName, 1, Headers, Hdr),
    {update, Session, Request#xhttpc_request{headers=NewHdrs}, State}.

response(Session, _Req, {ok, {Status, Hdrs, Body}}, State) ->
    case xhttpc:header_value("content-encoding", Hdrs) of
        "gzip" ->
            NewBody = zlib:gunzip(Body),
            {update, Session, {ok, {Status, Hdrs, NewBody}}, State};
        _ ->
            noaction
    end;
response(_Session, _Req, _Resp, _State) ->
    noaction.

call(_, State) -> {ok, State}.

terminate(_Reason, _State) -> ok.

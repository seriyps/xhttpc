%%% @author Sergey Prokhorov <me@seriyps.ru>
%%% @copyright (C) 2012, Sergey Prokhorov
%%% @doc
%%% HTTP redirect middleware.
%%% Follow HTTP redirects (301/302/303).
%%% Can detect infinite redirect loops.
%%% XXX: This middleware MUST be the outermost one, because it can perform
%%% additional HTTP requests with the same session. So, if you, for example,
%%% place cookie middleware after redirect middleware, you can loose your
%%% cookies, which was returned with redirect response.
%%% @end
%%% Created :  8 Oct 2012 by Sergey Prokhorov <me@seriyps.ru>

-module(redirect_middleware).
-behaviour(xhttpc_middleware).

-export([init/1, request/3, response/4, call/2, terminate/2]).

-include("xhttpc.hrl").

init(Opts) ->
    MaxDepth = proplists:get_value(max_depth, Opts, 10),
    {ok, [MaxDepth]}.

request(_Session, _Req, _State) ->
    noaction.

response(Session, Req, {ok, {{Code, _}, Hdrs, _}}, [MaxDepth] = State)
  when Code == 301; Code == 302; Code == 303 ->
    Url = xhttpc:header_value("location", Hdrs),
    Depth = xhttpc:session_param(Session, depth),
    redirect(Code, Url, {Depth, MaxDepth}, Req, Session, State);
response(_Session, _Req, _Resp, _State) ->
    noaction.


redirect(_, undefined, _, _, _, _) ->
    error_logger:info_msg("Invalid redirect location"),
    noaction;
redirect(_, _, {Depth, MaxDepth}, _, _, _) when Depth > MaxDepth ->
    erlang:error({max_redirect_depth_exceeded, Depth, MaxDepth});
redirect(303, Url, _, #xhttpc_request{options=Options, headers=Hdrs}, Session, State) ->
    NewRequest = #xhttpc_request{url=Url, method=get, headers=Hdrs, options=Options},
    {NewSession, NewResp} = xhttpc:request(Session, NewRequest),
    {update, NewSession, NewResp, State};
redirect(Code, Url, _, Req, Session, State)
  when Code == 301; Code == 302 ->
    NewRequest = Req#xhttpc_request{url=Url},
    {NewSession, NewResp} = xhttpc:request(Session, NewRequest),
    {update, NewSession, NewResp, State}.

call(_, S) ->
    {ok, S}.

terminate(_Reason, _State) ->
    ok.

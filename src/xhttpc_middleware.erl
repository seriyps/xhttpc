%%% @author Sergey Prokhorov <me@seriyps.ru>
%%% @copyright (C) 2013, Sergey Prokhorov
%%% @doc
%%% Behaviour module for xhttpc middlewares
%%% @end
%%% Created : 14 Jun 2013 by Sergey Prokhorov <me@seriyps.ru>

-module(xhttpc_middleware).

-callback init([Args]) ->
    {ok, State}
        when Args::any(),
             State::any().

-callback request(Session, Request, State) ->
    {update, Session, Request, State} | noaction
        when Session :: xhttpc:session(),
             Request :: xhttpc:request(),
             State :: any().

-callback response(Session, Request, Response, State) ->
    {update, Session, Response, State} | noaction
        when Session :: xhttpc:session(),
             Request :: xhttpc:request(),
             Response :: xhttpc:response(),
             State :: any().

-callback call([Params], State) ->
    {Response, State}       % XXX: revise response API - need noaction for state
        when Params :: any(),
             State :: any(),
             Response :: any().

-callback terminate(Reason, State) ->
    ok
        when Reason :: normal | any(),
             State :: any().

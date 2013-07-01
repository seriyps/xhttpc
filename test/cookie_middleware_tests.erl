%%% @author Sergey Prokhorov <me@seriyps.ru>
%%% @copyright (C) 2013, Sergey Prokhorov
%%% @doc
%%%
%%% @end
%%% Created : 29 Jun 2013 by Sergey Prokhorov <me@seriyps.ru>

-module(cookie_middleware_tests).

-include_lib("eunit/include/eunit.hrl").
-include("xhttpc.hrl").
-include("xhttpc_test.hrl").
-include("xhttpc_cookie.hrl").

setcookie_response(Cookies) ->
    fun(Request) ->
            {ok, {Status, Hdrs, Body}} = term2bin_response(Request),
            CookieHdrs = [{?NHN("set-cookie"), Cookie} || Cookie <- Cookies],
            {ok, {Status, CookieHdrs ++ Hdrs, Body}}
    end.

cookie_req(S, Url, CookiesToSet) ->
    {S1, {ok, {_, _, ReqBin}}} =
        xhttpc:request(S, #xhttpc_request{url=Url,
                                          options=[{client_options,
                                                    [{response, setcookie_response(CookiesToSet)}]}]}),
    {S1, binary_to_term(ReqBin)}.
    

samedomain_test() ->
    S = xhttpc:init([{cookie_middleware, []}], test_client),
    {S1, _} = cookie_req(S, "http://example.com/", ["key=val"]),
    {S2, #xhttpc_request{headers=ReqHdrs1}} = cookie_req(
                                                S1, "http://example.com/",
                                                ["key=val11", "key1=val1", "key2=val2"]),
    ?assertEqual("key=val", xhttpc:header_value("cookie", ReqHdrs1)),
    {_S3, #xhttpc_request{headers=ReqHdrs2}} = cookie_req(S2, "http://example.com/", []),
    ?assertEqual("key=val11; key1=val1; key2=val2", xhttpc:header_value("cookie", ReqHdrs2)).

delete_cookie_test() ->
    S = xhttpc:init([{cookie_middleware, []}], test_client),
    {S1, _} = cookie_req(S, "http://example.com/", ["key=val"]),
    {S2, #xhttpc_request{headers=ReqHdrs1}} = cookie_req(
                                                S1, "http://example.com/",
                                                ["key=val11; Max-Age=0",
                                                 "key2=val2",
                                                 "key3=val3; Expires=???, 10-Oct-1970 17:02:56 GMT"]),
    ?assertEqual("key=val", xhttpc:header_value("cookie", ReqHdrs1)),
    {_S3, #xhttpc_request{headers=ReqHdrs2}} = cookie_req(S2, "http://example.com/", []),
    ?assertEqual("key2=val2", xhttpc:header_value("cookie", ReqHdrs2)).

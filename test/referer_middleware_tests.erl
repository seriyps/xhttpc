%%% @author Sergey Prokhorov <me@seriyps.ru>
%%% @copyright (C) 2013, Sergey Prokhorov
%%% @doc
%%%
%%% @end
%%% Created : 27 Jun 2013 by Sergey Prokhorov <me@seriyps.ru>

-module(referer_middleware_tests).

-include_lib("eunit/include/eunit.hrl").
-include("xhttpc.hrl").
-include("xhttpc_test.hrl").

auto_referer_test() ->
    S1 = xhttpc:init([{referer_middleware, []}], test_client),
    {S2, {ok, {_, _, BinRequest}}} = xhttpc:request(
                                       S1, #xhttpc_request{url="http://example.com/page1",
                                                           options=[{client_options, [{response, fun term2bin_response/1}]}]}),
    #xhttpc_request{headers=ReqHdrs} = binary_to_term(BinRequest),
    ?assertEqual([], ReqHdrs),
    {_S3, {ok, {_, _, BinRequest2}}} = xhttpc:request(
                                         S2, #xhttpc_request{url="http://example.com/page2",
                                                             options=[{client_options, [{response, fun term2bin_response/1}]}]}),
    #xhttpc_request{headers=ReqHdrs2} = binary_to_term(BinRequest2),
    ?assertEqual(?NH([{"referer", "http://example.com/page1"}]), ReqHdrs2).

initial_referer_test() ->
    S1 = xhttpc:init([{referer_middleware, ["http://example.com/page1"]}], test_client),
    {_S2, {ok, {_, _, BinRequest}}} = xhttpc:request(
                                       S1, #xhttpc_request{url="http://example.com/page2",
                                                           options=[{client_options, [{response, fun term2bin_response/1}]}]}),
    #xhttpc_request{headers=ReqHdrs} = binary_to_term(BinRequest),
    ?assertEqual(?NH([{"referer", "http://example.com/page1"}]), ReqHdrs).

set_referer_test() ->
    S1 = xhttpc:init([{referer_middleware, []}], test_client),
    {S2, {ok, {_, _, BinRequest}}} = xhttpc:request(
                                       S1, #xhttpc_request{url="http://example.com/page1",
                                                           options=[{client_options, [{response, fun term2bin_response/1}]}]}),
    #xhttpc_request{headers=ReqHdrs} = binary_to_term(BinRequest),
    ?assertEqual([], ReqHdrs),
    {S3, ok} = referer_middleware:set_referer(S2, "http://example.com/page111"),
    {_S4, {ok, {_, _, BinRequest2}}} = xhttpc:request(
                                         S3, #xhttpc_request{url="http://example.com/page2",
                                                             options=[{client_options, [{response, fun term2bin_response/1}]}]}),
    #xhttpc_request{headers=ReqHdrs2} = binary_to_term(BinRequest2),
    ?assertEqual(?NH([{"referer", "http://example.com/page111"}]), ReqHdrs2).

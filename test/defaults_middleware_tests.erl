%%% @author Sergey Prokhorov <me@seriyps.ru>
%%% @copyright (C) 2013, Sergey Prokhorov
%%% @doc
%%%
%%% @end
%%% Created : 28 Jun 2013 by Sergey Prokhorov <me@seriyps.ru>

-module(defaults_middleware_tests).

-include_lib("eunit/include/eunit.hrl").
-include("xhttpc.hrl").
-include("xhttpc_test.hrl").

append_headers_test() ->
    AppendHeaders = ?NH([{"hdr1", "v1"},
                         {"hdr2", "v2"}]),
    S1 = xhttpc:init([{defaults_middleware, [{append_hdrs, AppendHeaders}]}], test_client),
    {_S2, {ok, {_, _, BinRequest}}} =
        xhttpc:request(
          S1, #xhttpc_request{url="http://example.com/page1",
                              headers=?NH([{"hdr2", "v22"},
                                           {"hdr3", "v3"}]),
                              options=[{client_options, [{response, fun term2bin_response/1}]}]}),
    #xhttpc_request{headers=ReqHdrs} = binary_to_term(BinRequest),
    ?assertEqual(lists:sort(?NH([{"hdr1", "v1"},
                                 {"hdr2", "v2"},
                                 {"hdr2", "v22"},
                                 {"hdr3", "v3"}])),
                 lists:sort(ReqHdrs)).

default_headers_test() ->
    DefaultHeaders = ?NH([{"hdr1", "v1"},
                          {"hdr2", "v2"}]),
    S1 = xhttpc:init([{defaults_middleware, [{default_hdrs, DefaultHeaders}]}], test_client),
    {_S2, {ok, {_, _, BinRequest}}} =
        xhttpc:request(
          S1, #xhttpc_request{url="http://example.com/page1",
                              headers=?NH([{"hdr2", "v22"},
                                           {"hdr3", "v3"}]),
                              options=[{client_options, [{response, fun term2bin_response/1}]}]}),
    #xhttpc_request{headers=ReqHdrs} = binary_to_term(BinRequest),
    ?assertEqual(lists:sort(?NH([{"hdr1", "v1"},
                                 {"hdr2", "v22"},
                                 {"hdr3", "v3"}])),
                 lists:sort(ReqHdrs)).

default_options_test() ->
    T2BFun = fun term2bin_response/1,
    DefaultOptions = [{timeout, 10},
                      {option1, "v1"},
                      {client_options, [{response, T2BFun},
                                        {coption1, "cv1"},
                                        {coption2, "cv2"}]}],

    S1 = xhttpc:init([{defaults_middleware, [{default_options, DefaultOptions}]}], test_client),
    {_S2, {ok, {_, _, BinRequest}}} =
        xhttpc:request(
          S1, #xhttpc_request{url="http://example.com/page1",
                              options=[{client_options, [{coption1, "cv11"},
                                                         {coption3, "cv3"}]},
                                       {timeout, 20},
                                       {option2, "v2"}]}),
    #xhttpc_request{options=ReqOpts} = binary_to_term(BinRequest),
    {value, {client_options, ReqCliOpts}, RestOpts} = lists:keytake(client_options, 1, ReqOpts),
    ?assertEqual(lists:sort([{timeout, 20}, {option2, "v2"}, {option1, "v1"}]),
                 lists:sort(RestOpts)),
    ?assertEqual(lists:sort([{coption1, "cv11"}, {coption3, "cv3"},
                             {coption2, "cv2"}, {response, T2BFun}]),
                 lists:sort(ReqCliOpts)).

%%% @author Sergey Prokhorov <me@seriyps.ru>
%%% @copyright (C) 2013, Sergey Prokhorov
%%% @doc
%%%
%%% @end
%%% Created : 27 Jun 2013 by Sergey Prokhorov <me@seriyps.ru>

-module(xhttpc_tests).

-include_lib("eunit/include/eunit.hrl").
-include("xhttpc.hrl").

-define(NHN(H), xhttpc:normalize_header_name(H)).
-define(NH(H), xhttpc:normalize_headers(H)).

normalize_test_() ->
    [?_assertEqual(?NHN("SoMe-hEaDeR"), ?NHN("somE-HEADEr")),
     ?_assert(?NHN("some-header") =/= ?NHN("some-other-header")),
     ?_assertEqual(?NH([{"some-header", "v1"},
                        {"some-other-header", "v2"}]),
                   ?NH([{"SOME-HEADER", "v1"},
                        {"SOME-OTHER-HEADER", "v2"}]))
    ].

flow_test() ->
    S = xhttpc:init([], test_client),
    WaitResponse = {ok,
                    {{200, "OK"},
                     ?NH([{"test-resp-hdr-name", "test-resp-hdr-val"}]),
                     <<"the-response">>}},
    {S2, Response} = xhttpc:request(S, "", get, [], <<>>, [{client_options,
                                                            [{response, WaitResponse}]}]),
    ?assertEqual(WaitResponse, Response),
    {S3, Response2} = xhttpc:request(S2, "", get, [], <<>>, [{client_options,
                                                               [{response, WaitResponse}]}]),
    ?assertEqual(WaitResponse, Response2),
    xhttpc:terminate(S3, normal).

disable_middlewares_test() ->
    S = xhttpc:init([{compression_middleware, []}], test_client),
    RespBody = <<"the-body">>,
    GZBody = zlib:gzip(RespBody),
    WaitResponse = {ok, {{200, "OK"}, ?NH([{"content-encoding", "gzip"}]), RespBody}},
    WaitGZResponse = {ok, {{200, "OK"}, ?NH([{"content-encoding", "gzip"}]), GZBody}},
    %% when middleware is enabled, it return unzipped body, but when
    %% disabled - return as is (unzipped)
    {S2, Response} = xhttpc:request(S, "", get, [], <<>>, [{client_options,
                                                            [{response, WaitGZResponse}]}]),
    ?assertEqual(WaitResponse, Response),

    {S3, Response2} = xhttpc:request(S2, "", get, [], <<>>, [{disable_middlewares, [compression_middleware]},
                                                              {client_options,
                                                               [{response, WaitGZResponse}]}]),
    ?assertEqual(WaitGZResponse, Response2),
    xhttpc:terminate(S3, normal).

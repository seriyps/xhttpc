%%% @author Sergey Prokhorov <me@seriyps.ru>
%%% @copyright (C) 2013, Sergey Prokhorov
%%% @doc
%%%
%%% @end
%%% Created : 27 Jun 2013 by Sergey Prokhorov <me@seriyps.ru>

-module(compression_middleware_tests).

-include_lib("eunit/include/eunit.hrl").
-include("xhttpc.hrl").

comp_middleware_test_() ->
    {foreach,
     fun() ->
             Se = xhttpc:init([]),
             St = compression_middleware:init([]),
             {Se, St, #xhttpc_request{}}
     end,
     [fun add_header_tst/1,
      fun skip_notcompressed_tst/1,
      fun unpack_compressed_tst/1]}.

add_header_tst({Se, St, Request}) ->
    {update, Se, NewRequest, St} = compression_middleware:request(Se, Request, St),
    NewHdrs = NewRequest#xhttpc_request.headers,
    [?_assertEqual(1, length(NewHdrs)),
     ?_assertEqual(xhttpc:normalize_headers([{"accept-encoding", "gzip"}]),
                   NewHdrs)].

skip_notcompressed_tst({Se, St, Request}) ->
    Response = {ok, {200, "OK"}, [], <<"the-body">>},
    Resp = compression_middleware:response(Se, Request, Response, St),
    ?_assertEqual(noaction, Resp).

unpack_compressed_tst({Se, St, Request}) ->
    Body = <<"the-body">>,
    Response = {ok, {{200, "OK"},
                xhttpc:normalize_headers([{"content-encoding", "gzip"}]),
                zlib:gzip(<<"the-body">>)}},
    {update, Se, NewResponse, St} = compression_middleware:response(Se, Request, Response, St),
    {ok, {_, _, NewBody}} = NewResponse,
    ?_assertEqual(Body, NewBody).

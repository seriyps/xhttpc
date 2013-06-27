-define(NHN(H), xhttpc:normalize_header_name(H)).
-define(NH(H), xhttpc:normalize_headers(H)).

term2bin_response(Req) ->
    {ok, {{200, "OK"}, [], term_to_binary(Req)}}.

%%% @author Sergey Prokhorov <me@seriyps.ru>
%%% @copyright (C) 2013, Sergey Prokhorov
%%% @doc
%%% Request defaults middleware.
%%% * Append or merge some constant HTTP headers to request
%%% * Merge some default options.
%%% `append_hdrs' - just append some constant headers to request. Don't merged
%%% with request's headers and can't be overwritten (so duplicates are possible)
%%% `default_hdrs' - add some constant headers to request. May be overwritten by
%%% request's headers
%%% `default_options' - like `default_hdrs', but for options + `client_options'
%%% are merged by key too.
%%% TODO: allow duplicate header names
%%% @end
%%% Created : 20 Jun 2013 by Sergey Prokhorov <me@seriyps.ru>

-module(defaults_middleware).

-behaviour(xhttpc_middleware).
-include("xhttpc.hrl").

-export([init/1, request/3, response/4, call/2, terminate/2]).

-record(state,
       {append_hdrs :: [http_header()],
        default_hdrs :: [http_header()],
        default_options :: http_options()}).

init(Options) ->
    State = #state{
      append_hdrs = proplists:get_value(append_hdrs, Options),
      default_hdrs = maybe_normalize_headers(proplists:get_value(default_hdrs, Options)),
      default_options = proplists:get_value(default_options, Options)
     },
    {ok, State}.

request(Session, Request, #state{append_hdrs=AHdrs, default_hdrs=DHdrs, default_options=DOpts} = State) ->
    NewRequest = apply_defaults(Request, DHdrs, DOpts, AHdrs),
    {update, Session, NewRequest, State}.

response(_Session, _Req, _Resp, _State) ->
    noaction.

call(_, State) -> {ok, State}.

terminate(_Reason, _State) -> ok.

apply_defaults(#xhttpc_request{headers=Headers} = Request, DH, DO, AH) when is_list(DH) ->
    %% apply default_hdrs
    NewHeaders = merge_headers(Headers, DH),
    apply_defaults(Request#xhttpc_request{headers=NewHeaders}, undefined, DO, AH);
apply_defaults(#xhttpc_request{headers=Headers} = Request, DH, DO, AH) when is_list(AH) ->
    %% apply append_hdrs. Should be applied AFTER default_hdrs
    NewHeaders = AH ++ Headers,
    apply_defaults(Request#xhttpc_request{headers=NewHeaders}, DH, DO, undefined);
apply_defaults(#xhttpc_request{options=Opts} = Request, DH, DO, AH) when is_list(DO) ->
    %% apply default_options
    NewOpts = merge_options(Opts, DO),
    apply_defaults(Request#xhttpc_request{options=NewOpts}, DH, undefined, AH);
apply_defaults(Request, undefined, undefined, undefined) ->
    Request.

%% Merge headers with default headers.
%% TODO: don't use lists:ukeymerge/3, because duplicated headers are
%% possible and should be kipped; When conflicts - default header will be
%% overwriten
merge_headers([], DH) ->
    DH;
merge_headers(H, []) ->
    H;
merge_headers(Headers, SDHeaders) ->
    SHeaders = lists:ukeysort(1, xhttpc:normalize_headers(Headers)),
    lists:ukeymerge(1, SHeaders, SDHeaders).

maybe_normalize_headers(undefined) ->
    undefined;
maybe_normalize_headers(Hdrs) ->
    lists:sort(xhttpc:normalize_headers(Hdrs)).

%% Merge all options & merge client_options
merge_options([], DO) ->
    DO;
merge_options(O, []) ->
    O;
merge_options(Opts, DOpts) ->
    OptsDict = orddict:from_list(Opts),
    DOptsDict = orddict:from_list(DOpts),
    COptsOK = orddict:find(client_options, OptsDict),
    DCOptsOK = orddict:find(client_options, DOptsDict),
    Opts1 = lists:ukeymerge(1, OptsDict, DOptsDict),
    case {COptsOK, DCOptsOK} of
        {{ok, COpts}, {ok, DCOpts}} ->
            %% should merge client_options
            NewCOpts = merge_client_options(COpts, DCOpts),
            orddict:store(client_options, NewCOpts, Opts1);
        _ -> Opts1
    end.

merge_client_options(COpts, DCOpts) ->
    SCOpts = lists:ukeysort(1, COpts),
    SDCOpts = lists:ukeysort(1, DCOpts),
    lists:ukeymerge(1, SCOpts, SDCOpts).


-ifdef(TEST).

-include_lib("eunit/include/eunit.hrl").
-define(NH(Hdrs), xhttpc:normalize_headers(Hdrs)).

merge_headers_test() ->
    Hdrs = ?NH([{"A", "1"}, {"B", "2"}]),
    DHdrs = ?NH([{"b", "3"}, {"c", "4"}]),
    ?assertEqual(?NH([{"a", "1"}, {"b", "2"}, {"c", "4"}]), merge_headers(Hdrs, DHdrs)).

merge_headers_nodef_test() ->
    ?assertEqual(?NH([{"a", "1"}]), merge_headers(?NH([{"a", "1"}]), [])).

merge_headers_nohdrs_test() ->
    ?assertEqual(?NH([{"a", "1"}]), merge_headers([], ?NH([{"a", "1"}]))).


merge_options_zero_test() ->
    Opts = [],
    DOpts = [],
    ?assertEqual([], merge_options(Opts, DOpts)).

merge_options_test() ->
    Opts = [{timeout, 10},
            {client_options, [{connect_timeout, 5}]}],
    DOpts = [{timeout, 20},
             {tst, false},
             {client_options, [{connect_timeout, 10},
                               {send_retry, 5}]}],
    ?assertEqual([{client_options, [{connect_timeout, 5},
                                    {send_retry, 5}]},
                  {timeout, 10},
                  {tst, false}], merge_options(Opts, DOpts)).

-endif.

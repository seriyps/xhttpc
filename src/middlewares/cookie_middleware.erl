%%% @author Sergey Prokhorov <me@seriyps.ru>
%%% @copyright (C) 2012, Sergey Prokhorov
%%% @doc
%%% HTTP cookie middleware
%%% @end
%%% Created :  8 Oct 2012 by Sergey Prokhorov <me@seriyps.ru>

-module(cookie_middleware).
-behaviour(xhttpc_middleware).

-export([init/1, request/3, response/4, call/2, terminate/2]).

-include("xhttpc.hrl").
-include("xhttpc_cookie.hrl").

-record(cookie_jar,
        {values = [] :: orddict:orddict(), %[#xhttpc_cookie{}],
         last_updated :: erlang:timestamp()}).


init([]) ->
    Cookies = init_cookies(),
    {ok, Cookies}.

request(Session, #xhttpc_request{url=Url, headers=Headers} = Req, State) ->
    {ok, ParsedUrl} = http_uri:parse(Url),
    Cookies = lookup_cookies(State, ParsedUrl),
    NewHdrs = xhttpc_cookielib:cookie_header(ParsedUrl, Cookies) ++ Headers,
    {update, Session, Req#xhttpc_request{headers=NewHdrs}, State}.

response(Session, #xhttpc_request{url=Url}, {ok, {_, Hdrs, _}} = Resp, State) ->
    {ok, ParsedUrl} = http_uri:parse(Url),
    Cookies = xhttpc_cookielib:extract_cookies(ParsedUrl, Hdrs), %, os:timestamp()),
    case Cookies of
        [] ->
            noaction;
        _ ->
            State1 = update_cookies(State, Cookies),
            {update, Session, Resp, State1}
    end;
response(_Session, _Req, _Resp, _State) ->
    noaction.

call(_, S) ->
    {ok, S}.

terminate(_Reason, _State) ->
    %% my_cookie_store:release(State),
    ok.

%% cookie-jar implementation
init_cookies() ->
    #cookie_jar{}.

lookup_cookies(#cookie_jar{values=Cookies}, Url) ->
    UrlKey = xhttpc_cookielib:lookup_key(Url),
    orddict:fold(fun({CookieKey, _Name}, C, Acc) when CookieKey == UrlKey ->
                         [C | Acc];
                      (_, _, Acc) ->
                         Acc
                 end, [], Cookies).

update_cookies(Jar, []) ->
    Jar;
update_cookies(#cookie_jar{values=CurCookies} = CookieJar, Cookies) ->
    Now = os:timestamp(),
    NewCookies = update_cookies1(CurCookies, Cookies, Now),
    CookieJar#cookie_jar{values=NewCookies,
                         last_updated=Now}.

update_cookies1(CurCookies, Cookies, Now) ->
    {ToDelete, ToUpsert} = lists:foldl(
                             fun(#xhttpc_cookie{expires=Exp, value=Val} = C, {D, U})
                                   when (Exp < Now) or (Val == "") ->
                                     {[xhttpc_cookielib:cookie_key(C) | D], U};
                                (C, {D, U}) ->
                                     {D, [{xhttpc_cookielib:cookie_key(C), C} | U]}
                             end,
                             {[], []},
                             Cookies),
    %% remove cookies that expired or with empty values
    CurCookies1 =
        case ToDelete of
            [] ->
                CurCookies;
            _ ->
                DelSet = ordsets:from_list(ToDelete),
                orddict:filter(fun({Key, _Val}) ->
                                       not ordsets:is_element(Key, DelSet)
                               end,
                               CurCookies)
        end,
    %% update / insert new cookies
    case ToUpsert of
        [] ->
            CurCookies1;
        _ ->
            ToUpsertDict = orddict:from_list(ToUpsert),
            %% replace old cookies with new ones on conflict
            orddict:merge(fun(_K, _V1, V2) -> V2 end,
                          CurCookies1, ToUpsertDict)
    end.

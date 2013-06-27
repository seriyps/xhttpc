%%% @author Sergey Prokhorov <me@seriyps.ru>
%%% @copyright (C) 2013, Sergey Prokhorov
%%% @doc
%%% Cookie management library.
%%% Implement cookie header parsing/serialization and cookie policy rules.
%%% RFC6265
%%% [http://tools.ietf.org/html/rfc6265]
%%% See example usage in middleware/cookie_middleware.erl
%%% @end
%%% Created : 21 Jun 2013 by Sergey Prokhorov <me@seriyps.ru>

-module(xhttpc_cookielib).

-export([lookup_key/1, cookie_key/1, cookie_header/2, extract_cookies/2, merge_cookies/3]).
-export([cookie_allowed/2]).

-include("xhttpc_cookie.hrl").

-ifdef(TEST).
-include_lib("eunit/include/eunit.hrl").
-endif.

-type url() :: {Scheme :: atom(),
                UserInfo :: string(),
                Host :: string(),
                Port :: pos_integer(),
                Path :: string(),
                Query :: string()}.

-type lookup_key() :: {string(), string()}.
-type cookie_key() :: {lookup_key(), string()}.

%% You may lookup all cookies, that may be allowed for this URL, using this
%% key, eg:
%% <code>
%% UrlKey = lookup_key(Url),
%% AllowedCookies = orddict:fold(
%%     fun({CookieKey, _Name}, C, Acc)
%%        when CookieKey == UrlKey ->
%%            [C | Acc];
%%        (_, _, Acc) ->
%%            Acc
%%     end, [], Cookies).
%% </code>
%% or
%% <code>
%% UrlKey = lookup_key(Url),
%% AllowedCookies = ets:match(Tid, {{UrlKey, '_'}, '$1'}).
%% </code>
-spec lookup_key(url()) -> lookup_key().
lookup_key({_, _, Host, _Port, Path, _}) ->
    %% FIXME: this doesn't work in major amount of cases
    {Host, Path}.

%% Cookies should be stored in storage by this key, and it should be unique, eg
%% <code>dict:store(cookie_key(Cookie), Cookie, Storage)</code>
%% or
%% <code>Tid = ets:new(my_cookie_jar, [set]),
%% ets:insert(Tid, {cookie_key(Cookie), Cookie})</code>
-spec cookie_key(#xhttpc_cookie{}) -> cookie_key().
cookie_key(Cookie) ->
    #xhttpc_cookie{
            path = Path,
            name = Name,
            domain = Domain} = Cookie,
    {{Domain, Path}, Name}.

%% Generate "Cookie" header
-spec cookie_header(url(), [#xhttpc_cookie{}]) -> [xhttpc:http_header()].
cookie_header(Url, Cookies) ->
    AllowedCookies = [C || C <- Cookies, cookie_allowed(Url, C)],
    case serialize_cookies(AllowedCookies, []) of
        "" ->
            [];
        CookieStr ->
            [{"Cookie", CookieStr}]
    end.

-spec extract_cookies(url(), [xhttpc:http_header()]) -> [#xhttpc_cookie{}].
extract_cookies(Url, Headers) ->
    parse_set_cookie(Url, Headers, []).

merge_cookies(_OldCookies, _NewCookies, _Now) ->
    ok.


%% internal

serialize_cookies([], []) ->
    %% when there is no Cookies
    "";
serialize_cookies([], Acc) ->
    string:join(Acc, "; ");
serialize_cookies([Cookie | Cookies], Acc) ->
    #xhttpc_cookie{name=N, value=V} = Cookie,
    CookieStr = string:join([N, V], "="),
    serialize_cookies(Cookies, [CookieStr | Acc]).


cookie_allowed({_, _, UrlDomain, _Port, UrlPath, _},
               #xhttpc_cookie{path = CookiePath, domain = CookieDomain}) ->
    %% TODO: ensure cookie policy for http/https, host/port, UserInfo
    (domain_policy_allow(UrlDomain, CookieDomain)
     and path_policy_allow(UrlPath, CookiePath)).

domain_policy_allow(UrlDomain, [$. | CookieDomain]) ->
    domain_policy_allow(UrlDomain, CookieDomain);
domain_policy_allow(Domain, Domain) ->
    true;
domain_policy_allow(UrlDomain, CookieDomain) ->
    RUrl = lists:reverse(UrlDomain),
    RCookie = lists:reverse(CookieDomain),
    case string:str(RUrl, RCookie) of
        1 -> true;
        _ -> false
    end.

path_policy_allow(Path, Path) ->
    true;
path_policy_allow(_, undefined) ->
    true;
path_policy_allow(UrlPath, CookiePath) ->
    case string:str(UrlPath, CookiePath) of
        1 -> true;
        _ -> false
    end.

%% set-cookie header parser
parse_set_cookie(_, [], Cookies) ->
    Cookies;
parse_set_cookie({_, _, Host, _Port, _Path, _} = Url, [{"set-cookie", Val} | Headers], Cookies) ->
    %% TODO: capitalize header name
    SetCookieString = string:tokens(Val, ";"),
    [CookiePair | CookieAv] = SetCookieString,
    Pos = string:chr(CookiePair, $=),
    N = string:substr(CookiePair, 1, Pos - 1),
    V = string:substr(CookiePair, Pos + 1),
    Name = string:strip(N),
    Value = string:strip(V),
    Cookie1 = parse_cookie_av(CookieAv, #xhttpc_cookie{name=Name, value=Value}),
    %% if cookie host is not specified in av, use URL hostname
    Cookie2 = case Cookie1#xhttpc_cookie.domain of
                  undefined ->
                      Cookie1#xhttpc_cookie{domain = Host};
                  _ ->
                      Cookie1
              end,
    %% TODO: check domain policy there!
    parse_set_cookie(Url, Headers, [Cookie2 | Cookies]);
parse_set_cookie(Url, [_H | Headers], Cookies) ->
    parse_set_cookie(Url, Headers, Cookies).

parse_cookie_av([], Cookie) ->
    Cookie;
parse_cookie_av([Av | CookieAv], Cookie) ->
    {AName, AValue} =
        case string:tokens(Av, "=") of
            %% Attributes have the form Name=Value except "Secure" and "HttpOnly"!
            [Name] ->
                {Name, ""};
            [Name, Value] ->
                {Name, Value};
            _ ->
                {undefined, undefined}
        end,
    StrippedName  = string:to_lower(string:strip(AName)),
    StrippedValue = string:strip(AValue),
    Cookie2 = set_cookie_attr(StrippedName, StrippedValue, Cookie),
    parse_cookie_av(CookieAv, Cookie2).

set_cookie_attr("expires", Value, Cookie) ->
    try calendar:datetime_to_gregorian_seconds(parse_cookie_date(Value)) of
        Expires -> Cookie#xhttpc_cookie{expires=Expires}
    catch
        _:_ -> Cookie
    end;
set_cookie_attr("domain", Value, Cookie) ->
    Cookie#xhttpc_cookie{domain=Value};
set_cookie_attr("path", Value, Cookie) ->
    Cookie#xhttpc_cookie{path=Value};
set_cookie_attr("secure", _, Cookie) ->
    Cookie#xhttpc_cookie{secure=true};
set_cookie_attr("httponly", _, Cookie) ->
    Cookie#xhttpc_cookie{httponly=true};
set_cookie_attr("max-age", Value, Cookie) ->
    %% TODO: calculate Expires; max-age has more priority (RFC6265, 5.3 3)
    NowSec = calendar:datetime_to_gregorian_seconds({date(), time()}),
    Expires = NowSec + list_to_integer(Value),
    Cookie#xhttpc_cookie{expires=Expires};
set_cookie_attr(_Attr, _, Cookie) ->
    %% io:format("!!!!!!!!!!!!Dropped cookie attr ~s~n", [Attr]),
    Cookie.


%%
%% RFC http://tools.ietf.org/html/rfc2616#section-3.3.1
%% implemented only RFC1123 date, eg "Thu, 10-Oct-2013 17:02:56 GMT"
%% XXX: maybe regexp?
%%
parse_cookie_date([_D, _A, _Y, $,, $ ,
                   D1, D2, $-, M1, M2, M3, $-, Y1, Y2, Y3, Y4, $ ,
                   H1, H2, $:, Mi1, Mi2, $:, S1, S2 | _Gmt]) ->
    Year  = list_to_integer([Y1,Y2,Y3,Y4]),
    Day   = list_to_integer([D1,D2]),
    Month = convert_month([M1,M2,M3]),
    Hour  = list_to_integer([H1,H2]),
    Min   = list_to_integer([Mi1,Mi2]),
    Sec   = list_to_integer([S1,S2]),
    {{Year,Month,Day},{Hour,Min,Sec}}.


convert_month("Jan") -> 1;
convert_month("Feb") -> 2;
convert_month("Mar") -> 3;
convert_month("Apr") -> 4;
convert_month("May") -> 5;
convert_month("Jun") -> 6;
convert_month("Jul") -> 7;
convert_month("Aug") -> 8;
convert_month("Sep") -> 9;
convert_month("Oct") -> 10;
convert_month("Nov") -> 11;
convert_month("Dec") -> 12.


%%
%% TESTS
%%

-ifdef(TEST).
test_parse_url(Url) ->
    {ok, Res} = http_uri:parse(Url),
    Res.

-define(URL(Url), test_parse_url(Url)).

domain_policy_allow_test() ->
    ?assert(domain_policy_allow("example.com", "example.com")),
    ?assert(domain_policy_allow("www.example.com", "example.com")),
    ?assert(domain_policy_allow("www.example.com", "www.example.com")),
    ?assert(domain_policy_allow("example.com", ".example.com")),
    ?assert(not domain_policy_allow("example.com", "www.example.com")).

cookie_allowed_test() ->
    Url = ?URL("http://example.com/"),
    ?assert(cookie_allowed(Url, #xhttpc_cookie{domain = "example.com", path = "/"})),
    ?assert(cookie_allowed(Url, #xhttpc_cookie{domain = ".example.com", path = "/"})),
    ?assert(not cookie_allowed(Url, #xhttpc_cookie{domain = "www.example.com", path = "/"})),
    ?assert(not cookie_allowed(Url, #xhttpc_cookie{domain = "www.example.com", path = "/mypath"})),

    Url2 = ?URL("http://www.example.com/"),
    ?assert(cookie_allowed(Url2, #xhttpc_cookie{domain = "example.com", path = "/"})),
    ?assert(not cookie_allowed(Url2, #xhttpc_cookie{domain = "www2.example.com", path = "/"})),
    ?assert(cookie_allowed(Url2, #xhttpc_cookie{domain = "www.example.com", path = "/"})),

    Url3 = ?URL("http://example.com/exampleurl"),
    ?assert(cookie_allowed(Url3, #xhttpc_cookie{domain = "example.com", path = "/"})),
    ?assert(cookie_allowed(Url3, #xhttpc_cookie{domain = "example.com", path = "/example"})), % is it ok?
    ?assert(not cookie_allowed(Url3, #xhttpc_cookie{domain = "example.com", path = "/notexample"})).

parse_cookie_date_test() ->
    ?assertEqual({{2013, 10, 9}, {17, 2, 56}},
                 parse_cookie_date("Thu, 09-Oct-2013 17:02:56 GMT")).

parse_set_cookie_test() ->
    Url1 = ?URL("http://example.com/"),
    H1 = {"set-cookie", "k=v"},
    C1 = #xhttpc_cookie{name = "k", value = "v", domain = "example.com"},
    ?assertEqual([C1], parse_set_cookie(Url1, [H1], [])),

    Url2 = Url1,
    H2 = {"set-cookie",
          "k=v; Expires=Thu, 09-Oct-2013 17:02:56 GMT; Domain=.example.com; HttpOnly; Secure; Path=/path"},
    C2 = #xhttpc_cookie{name = "k", value = "v",
                 expires = 63548557376,
                 path = "/path",
                 httponly = true,
                 secure = true,
                 domain = ".example.com"},
    ?assertEqual([C2], parse_set_cookie(Url2, [H2], [])).

parse_set_cookie_with_equals_sign_regression_test() ->
    Url1 = ?URL("http://example.com/"),
    H1 = {"set-cookie", "k=SGVsbG8gd29ybGQ=; Secure"},
    C1 = #xhttpc_cookie{name = "k",
                 value = "SGVsbG8gd29ybGQ=",
                 domain = "example.com",
                 secure = true},
    ?assertEqual([C1], parse_set_cookie(Url1, [H1], [])).

serialize_cookies_test() ->
    Url = ?URL("http://www.example.com/path"),
    Cookies = [#xhttpc_cookie{name="k1",
                              value="v1",
                              domain="example.com"},
               #xhttpc_cookie{name="k2",
                              value="v2",
                              domain=".example.com",
                              path="/path",
                              expires=9999999999999999999},
               #xhttpc_cookie{name="k3",
                              value="v3",
                              domain="subdomain.example.com"},
               %% #xhttpc_cookie{name="k4",
               %%                value="v4",
               %%                domain=".example.com",
               %%                expires=0},
               #xhttpc_cookie{name="k5",
                              value="v5",
                              domain=".example.com",
                              path="/other_path"}],
    ?assertEqual([{"Cookie", "k2=v2; k1=v1"}], cookie_header(Url, Cookies)).

-endif.

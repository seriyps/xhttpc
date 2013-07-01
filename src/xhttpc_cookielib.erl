%%% @author Sergey Prokhorov <me@seriyps.ru>
%%% @copyright (C) 2013, Sergey Prokhorov
%%% @doc
%%% Cookie management library.
%%% Implement cookie header parsing/serialization and cookie policy rules.
%%% RFC6265 [http://tools.ietf.org/html/rfc6265]
%%% See example usage in middleware/cookie_middleware.erl
%%% @end
%%% Created : 21 Jun 2013 by Sergey Prokhorov <me@seriyps.ru>

-module(xhttpc_cookielib).

-export([lookup_key/1, cookie_key/1, cookie_header/2, extract_cookies/2, merge_cookies/3]).
-export([send_allowed/2, accept_allowed/2]).

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

-type lookup_key() :: {RevDomain::string(), Path::string()}.
-type cookie_key() :: {RevDomain::string(), Path::string(), Name::string()}.

%% You may lookup all cookies, that may be allowed for this URL, using this
%% key as prefixes, eg:
%% <code>
%% {HostKey, PathKey} = lookup_key(Url)
%% SELECT * FROM cookies
%% WHERE $HostKey LIKE (cookies.key_host || '%')
%%       AND $PathKey LIKE (cookies.key_path || '%')
%% </code>
%% <code>
%% {HostUrlKey, PathUrlKey} = lookup_key(Url),
%% AllowedCookies = orddict:fold(
%%     fun({HostCookieKey, PathCookieKey, _Name}, C, Acc) ->
%%        Allow = (lists:prefix(HostCookieKey, HostUrlKey)
%%                 and lists:prefix(PathCookieKey, PathUrlKey))
%%        if Allow -> [C | Acc];
%%        true -> Acc
%%        end
%%     end, [], Cookies).
%% </code>
%% or
%% <s><code>
%% UrlKey = lookup_key(Url),
%% AllowedCookies = ets:match(Tid, {{UrlKey, '_'}, '$1'}).
%% </code></s>
-spec lookup_key(url()) -> lookup_key().
lookup_key({_, _, Host, _Port, Path, _}) ->
    %% FIXME: this doesn't work in major amount of cases
    {lists:reverse([$. | Host]), Path}.

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
    {lists:reverse([$. | Domain]), Path, Name}.

%% Generate "Cookie" header
-spec cookie_header(url(), [#xhttpc_cookie{}]) -> [xhttpc:http_header()].
cookie_header(Url, Cookies) ->
    AllowedCookies = [C || C <- Cookies, send_allowed(Url, C)],
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

%% RFC 4.2.1
serialize_cookies([], []) ->
    %% when there is no Cookies
    "";
serialize_cookies([], Acc) ->
    string:join(Acc, "; ");
serialize_cookies([Cookie | Cookies], Acc) ->
    #xhttpc_cookie{name=N, value=V} = Cookie,
    CookieStr = string:join([N, V], "="),
    serialize_cookies(Cookies, [CookieStr | Acc]).


%% RFC 5.4 / 1
send_allowed({Scheme, _, UrlDomain, _Port, UrlPath, _},
             #xhttpc_cookie{path = CookiePath,
                            domain = CookieDomain,
                            hostonly = HostOnly,
                            secure = Secure}) ->
    DomainAllow = case HostOnly of
                      true -> CookieDomain == UrlDomain;
                      false -> domain_match(CookieDomain, UrlDomain)
                  end,
    PathAllow = path_match(UrlPath, CookiePath),
    SecureAllow = (not Secure) orelse (Secure and (Scheme == https)),
    %% ?debugFmt("Domain ~p, Path ~p, Secure ~p~nCookie~p~n",
    %%           [DomainAllow, PathAllow, SecureAllow, C]),
    DomainAllow and PathAllow and SecureAllow.


accept_allowed({_, _, UrlDomain, _, _, _}, #xhttpc_cookie{domain = CookieDomain}) ->
    domain_match(CookieDomain, UrlDomain) and not public_suffix(CookieDomain).

%% RFC 5.1.3
domain_match(Domain, Domain) ->
    true;
domain_match(Domain, String) ->
    %% eg String "www.example.com" matches Domain "example.com"
    %% but "wwwexample.com" not matches "example.com"
    RString = lists:reverse(String),
    RDotDomain = lists:reverse([$. | Domain]),
    lists:prefix(RDotDomain, RString).

%% RFC 5.3 / 5
%% XXX: this is the simplest test! Full public list is there [http://publicsuffix.org/]
public_suffix("localhost") ->
    false;
public_suffix(Domain) ->
    length(string:tokens(Domain, ".")) < 2.

%% RFC 5.1.4
path_match(Path, Path) ->
    true;
path_match(UrlPath, CookiePath) ->
    %% "/one/two", "/one/"
    %% "/one/two", "/one"
    %% but not
    %% "/onetwo", "/one"
    lists:prefix(CookiePath, UrlPath).

%% set-cookie header parser
%% RFC 5.2.
parse_set_cookie(_, [], Cookies) ->
    Cookies;
parse_set_cookie(Url, [{"set-cookie", Val} | Headers], Cookies) ->
    %% TODO: normalize header name
    SetCookieString = string:tokens(Val, ";"),
    [CookiePair | CookieAv] = SetCookieString,
    Pos = string:chr(CookiePair, $=),
    N = string:substr(CookiePair, 1, Pos - 1),
    V = string:substr(CookiePair, Pos + 1),
    Name = string:strip(N),
    Value = string:strip(V),
    Cookie = #xhttpc_cookie{name=Name, value=Value},% created=os:timestamp()},
    Cookie1 = parse_cookie_av(CookieAv, Cookie),
    Cookie2 = set_cookie_defaults(Url, Cookie1), %  (see RFC 5.3)
    case accept_allowed(Url, Cookie2) of
        true ->
            parse_set_cookie(Url, Headers, [Cookie2 | Cookies]);
        false ->
            parse_set_cookie(Url, Headers, Cookies)
    end;
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
                {"", ""}
        end,
    StrippedName  = string:to_lower(string:strip(AName)),
    StrippedValue = string:strip(AValue),
    Cookie2 = set_cookie_attr(StrippedName, StrippedValue, Cookie),
    parse_cookie_av(CookieAv, Cookie2).

%% RFC 5.2.1
set_cookie_attr("expires", Value, #xhttpc_cookie{expires=session} = Cookie) ->
    %% max-age has greater priority, than "expires" (RFC6265, 5.3 3 and 4.1.2.2)
    try parse_cookie_date(Value) of
        Expires -> Cookie#xhttpc_cookie{expires=Expires}
    catch
        _:_ -> Cookie
    end;
%% RFC 5.2.2
set_cookie_attr("max-age", Value, Cookie) ->
    MaxAge = try list_to_integer(Value)
             catch _:_ -> undefined
             end,
    case MaxAge of
        undefined ->
            Cookie;
        MaxAge when MaxAge =< 0 ->
            Cookie#xhttpc_cookie{expires={{0, 0, 0}, {0, 0, 0}}};
        _ ->
            NowSec = calendar:datetime_to_gregorian_seconds(calendar:universal_time()),
            Expires = NowSec + MaxAge,
            Cookie#xhttpc_cookie{expires=calendar:gregorian_seconds_to_datetime(Expires)}
    end;
%% RFC 5.2.3
set_cookie_attr("domain", [$. | Value], Cookie) ->
    %% XXX: what if domain is "..example.com" maybe do recursion?
    Cookie#xhttpc_cookie{domain=string:to_lower(Value)};
set_cookie_attr("domain", Value, Cookie) ->
    Cookie#xhttpc_cookie{domain=string:to_lower(Value)};
%% RFC 5.2.4
set_cookie_attr("path", "/" ++ _ = Value, Cookie) ->
     Cookie#xhttpc_cookie{path=Value};
%% RFC 5.2.5
set_cookie_attr("secure", _, Cookie) ->
    Cookie#xhttpc_cookie{secure=true};
%% RFC 5.2.6
set_cookie_attr("httponly", _, Cookie) ->
    Cookie#xhttpc_cookie{httponly=true};
set_cookie_attr(_Attr, _, Cookie) ->
    %% io:format("Dropped cookie attr ~s~n", [Attr]),
    Cookie.

set_cookie_defaults({_, _, Host, _, _, _} = Url, #xhttpc_cookie{domain=undefined} = Cookie) ->
    set_cookie_defaults(
      Url, Cookie#xhttpc_cookie{domain = string:to_lower(Host),
                                hostonly = true});
set_cookie_defaults(Url, #xhttpc_cookie{path=undefined} = Cookie) ->
    set_cookie_defaults(
      Url, Cookie#xhttpc_cookie{path=default_path(Url)});
set_cookie_defaults(_, Cookie) ->
    Cookie.

default_path({_, _, _, _, "/" ++ _ = Path, _}) ->
    filename:dirname(Path);
default_path(_) ->
    "/".

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

domain_match_test_() ->
    [?_assert(domain_match("example.com", "example.com")),
     ?_assert(not domain_match("www.example.com", "example.com")),
     ?_assert(domain_match("www.example.com", "www.example.com")),
     ?_assert(domain_match("example.com", "www.foo.example.com")),
     ?_assert(not domain_match("wwwexample.com", "www.example.com"))].

accept_allowed_test() ->
    Url = ?URL("http://user:pass@example.com:8080/dir/path"),
    ?assert(accept_allowed(Url, #xhttpc_cookie{domain = "example.com"})),
    ?assert(accept_allowed(Url, #xhttpc_cookie{domain = "example.com"})),
    ?assert(accept_allowed(Url, #xhttpc_cookie{domain = "example.com", path = "/mypath"})),
    ?assert(not accept_allowed(Url, #xhttpc_cookie{domain = "www.example.com"})),
    ?assert(not accept_allowed(Url, #xhttpc_cookie{domain = "wwwexample.com"})),
    ?assert(not accept_allowed(Url, #xhttpc_cookie{domain = "com"})),

    Url2 = ?URL("http://www.example.com/"),
    ?assert(accept_allowed(Url2, #xhttpc_cookie{domain = "example.com"})),
    ?assert(accept_allowed(Url2, #xhttpc_cookie{domain = "www.example.com"})),
    ?assert(not accept_allowed(Url2, #xhttpc_cookie{domain = "www2.example.com"})),
    ?assert(not accept_allowed(Url, #xhttpc_cookie{domain = "wwwexample.com"})).

send_allowed_test() ->
    %% mnemonic rule: cookie's domain and path should be shorter or equal to be accepted
    Url = ?URL("http://example.com/"),
    ?assert(send_allowed(Url, #xhttpc_cookie{domain = "example.com", path = "/"})),
    ?assert(not send_allowed(Url, #xhttpc_cookie{domain = "www.example.com", path = "/"})),
    ?assert(not send_allowed(Url, #xhttpc_cookie{domain = "www.example.com", path = "/mypath"})),
    ?assert(not send_allowed(Url, #xhttpc_cookie{domain = "example.com", path = "/", secure=true})),

    Url2 = ?URL("http://www.example.com/"),
    ?assert(send_allowed(Url2, #xhttpc_cookie{domain = "example.com", path = "/"})),
    ?assert(send_allowed(Url2, #xhttpc_cookie{domain = "www.example.com", path = "/"})),
    ?assert(not send_allowed(Url2, #xhttpc_cookie{domain = "www2.example.com", path = "/"})),
    ?assert(not send_allowed(Url2, #xhttpc_cookie{domain = "wwwexample.com", path = "/"})),

    ?assert(send_allowed(Url2, #xhttpc_cookie{domain = "www.example.com",
                                              path = "/",
                                              hostonly=true})),
    ?assert(not send_allowed(Url2, #xhttpc_cookie{domain = "example.com",
                                                  path = "/",
                                                  hostonly=true})),

    Url3 = ?URL("http://example.com/dir/path"),
    ?assert(send_allowed(Url3, #xhttpc_cookie{domain = "example.com", path = "/"})),
    ?assert(send_allowed(Url3, #xhttpc_cookie{domain = "example.com", path = "/dir"})),
    ?assert(send_allowed(Url3, #xhttpc_cookie{domain = "example.com", path = "/dir/path"})),
    ?assert(not send_allowed(Url3, #xhttpc_cookie{domain = "example.com", path = "/dir/path/sub"})),
    %% ?assert(not send_allowed(Url3, #xhttpc_cookie{domain = "example.com", path = "/dir/pat"})), %FIXME:
    ?assert(not send_allowed(Url3, #xhttpc_cookie{domain = "example.com", path = "/notdir"})).

parse_cookie_date_test() ->
    ?assertEqual({{2013, 10, 9}, {17, 2, 56}},
                 parse_cookie_date("Thu, 09-Oct-2013 17:02:56 GMT")).

parse_set_cookie_test() ->
    Url1 = ?URL("http://example.com/some/path"),
    H1 = {"set-cookie", "k=v"},
    C1 = #xhttpc_cookie{name = "k",
                        value = "v",
                        domain = "example.com",
                        hostonly = true,
                        path="/some"},
    ?assertEqual([C1], parse_set_cookie(Url1, [H1], [])),

    Url2 = Url1,
    H2 = {"set-cookie",
          "k=v; Expires=Thu, 09-Oct-2013 17:02:56 GMT; Domain=.example.com; HttpOnly; Secure; Path=/path"},
    C2 = #xhttpc_cookie{
      name = "k", value = "v",
      expires = {{2013, 10, 9}, {17, 2, 56}},
      path = "/path",
      httponly = true,
      secure = true,
      domain = "example.com"},
    ?assertEqual([C2], parse_set_cookie(Url2, [H2], [])).

parse_set_cookie_with_equals_sign_regression_test() ->
    Url1 = ?URL("http://example.com/"),
    H1 = {"set-cookie", "k=SGVsbG8gd29ybGQ=; Secure"},
    C1 = #xhttpc_cookie{name = "k",
                        value = "SGVsbG8gd29ybGQ=",
                        domain = "example.com",
                        hostonly = true,
                        path = "/",
                        secure = true},
    ?assertEqual([C1], parse_set_cookie(Url1, [H1], [])).

serialize_cookies_test() ->
    Url = ?URL("http://www.example.com/path"),
    Cookies = [#xhttpc_cookie{name="k1",
                              value="v1",
                              domain="example.com",
                              path="/"},
               #xhttpc_cookie{name="k2",
                              value="v2",
                              domain="example.com",
                              path="/path",
                              expires={{9999, 1, 1}, {1, 1, 1}}},
               #xhttpc_cookie{name="k3",
                              value="v3",
                              domain="subdomain.example.com",
                              path="/"},
               #xhttpc_cookie{name="k4",
                              value="v4",
                              domain="example.com",
                              path="/",
                              secure=true},
               #xhttpc_cookie{name="k5",
                              value="v5",
                              domain="example.com",
                              path="/other_path"}],
    ?assertEqual([{"Cookie", "k2=v2; k1=v1"}], cookie_header(Url, Cookies)).

-endif.

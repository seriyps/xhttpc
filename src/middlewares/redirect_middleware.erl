%%% @author Sergey Prokhorov <me@seriyps.ru>
%%% @copyright (C) 2012, Sergey Prokhorov
%%% @doc
%%% HTTP redirect middleware.
%%% Follow HTTP redirects (301/302/303).
%%% Can detect infinite redirect loops.
%%% XXX: This middleware MUST be the outermost one, because it can perform
%%% additional HTTP requests with the same session. So, if you, for example,
%%% place cookie middleware after redirect middleware, you can loose your
%%% cookies, which was returned with redirect response.
%%% @end
%%% Created :  8 Oct 2012 by Sergey Prokhorov <me@seriyps.ru>

-module(redirect_middleware).
-behaviour(xhttpc_middleware).

-export([init/1, request/3, response/4, call/2, terminate/2]).

-include("xhttpc.hrl").

init(Opts) ->
    MaxDepth = proplists:get_value(max_depth, Opts, 10),
    {ok, [MaxDepth]}.

request(_Session, _Req, _State) ->
    noaction.

response(Session, Req, {ok, {{Code, _}, Hdrs, _}}, [MaxDepth] = State)
  when Code == 301; Code == 302; Code == 303 ->
    Url = xhttpc:header_value("location", Hdrs),
    Depth = xhttpc:session_param(Session, depth),
    redirect(Code, Url, {Depth, MaxDepth}, Req, Session, State);
response(_Session, _Req, _Resp, _State) ->
    noaction.


redirect(_, undefined, _, _, _, _) ->
    error_logger:info_msg("Invalid redirect location"),
    noaction;
redirect(_, _, {Depth, MaxDepth}, _, _, _) when Depth > MaxDepth ->
    erlang:error({max_redirect_depth_exceeded, Depth, MaxDepth});
redirect(303, Location, _, #xhttpc_request{options=Options, url=CurrentUrl}, Session, State) ->
    Url = absolute_url(Location, CurrentUrl),
    NewRequest = #xhttpc_request{url=Url, method=get, options=Options},
    {NewSession, NewResp} = xhttpc:request(Session, NewRequest),
    {update, NewSession, NewResp, State};
redirect(Code, Location, _, Req, Session, State)
  when Code == 301; Code == 302 ->
    Url = absolute_url(Location, Req#xhttpc_request.url),
    NewRequest = Req#xhttpc_request{url=Url, headers=[]},
    {NewSession, NewResp} = xhttpc:request(Session, NewRequest),
    {update, NewSession, NewResp, State}.

call(_, S) ->
    {ok, S}.

terminate(_Reason, _State) ->
    ok.


%% Internal

%% Convert relative urls to absolute, using current page URL
%% Useful to calculate img@src or form@action or relative redirects
%% like "Location: /form/success"
%% https://github.com/seriyps/python-form-parser/blob/master/form_parser.py#L55
%%
%% TODO: fix how to handle querystring
%% when form action is "" and current page has GET params (query string) and:
%% * form method is GET: current query string is dropped, but when
%% * form method is POST: current query string is keeped!
%% when form action is "?key=val" and:
%% * form method is GET: action QS is ignored
%% * form method is POST: action QS is passed
%% when regular <a href="action.html?key=val"/>, querystring always passed
-spec absolute_url(binary() | string(), string()) -> string().
absolute_url(Url, CurrentUrl) when is_binary(Url) ->
    absolute_url(binary_to_list(Url), CurrentUrl);
absolute_url(("http://" ++ _) = Url, _CurrentUrl) ->
    Url;
absolute_url(("https://" ++ _) = Url, _CurrentUrl) ->
    Url;
absolute_url(("//" ++ _) = Url, CurrentUrl) ->
    {ok, {Scheme, _, _, _, _, _}} = http_uri:parse(CurrentUrl),
    case Scheme of
        https ->
            "https:" ++ Url;
        http ->
            "http:" ++ Url
    end;
absolute_url(("/" ++ _) = AbsPath, CurrentUrl) ->
    {ok, {Scheme, UserInfo, Host, Port, _Path, _Query}} = http_uri:parse(CurrentUrl),
    format_url({Scheme, UserInfo, Host, Port, AbsPath, ""});
absolute_url(RelPath, CurrentUrl) ->
    %% Rel path is "action.html" or "?key=value&..." or
    %% "action.html?key=value&..." or ""
    {ok, {Scheme, UserInfo, Host, Port, CurrentPath, _Query}} = http_uri:parse(CurrentUrl),
    BasePath = filename:dirname(CurrentPath),
    NewPath = case BasePath of
                  "/" -> "/" ++ RelPath;
                  _ -> BasePath ++ "/" ++ RelPath
              end,
    format_url({Scheme, UserInfo, Host, Port, NewPath, ""}).

format_url({Scheme, UserInfo, Host, Port, Path, Query}) ->
    U1 = add_scheme("", Scheme),
    U2 = add_credentials(U1, UserInfo),
    U3 = add_host(U2, Host),
    U4 = add_port(U3, Port, Scheme),
    U5 = add_path(U4, Path),
    U6 = add_query(U5, Query),
    lists:flatten(U6).

add_scheme(_, https) ->
    "https://";
add_scheme(_, http) ->
    "http://".

add_credentials(Scheme, "") ->
    Scheme;
add_credentials(Scheme, UserInfo) ->
    [Scheme, UserInfo, "@"].

add_host(SUP, Host) ->
    [SUP, Host].

add_port(SUPH, 80, http) ->
    SUPH;
add_port(SUPH, 443, https) ->
    SUPH;
add_port(SUPH, Port, _Scheme) ->
    [SUPH, ":", integer_to_list(Port)].

add_path(SUPHP, "") ->
    [SUPHP, "/"];
add_path(SUPHP, Path) ->
    [SUPHP, Path].

add_query(SUPHPP, "") ->
    SUPHPP;
add_query(SUPHPP, Query) ->
    [SUPHPP, "?", Query].




-ifdef(TEST).

-include_lib("eunit/include/eunit.hrl").

absolute_url_test() ->
    Sample = "http://example.com/dir/path?k=v",
    ?assertEqual(Sample, absolute_url("http://example.com/dir/path?k=v", "any")),
    ?assertEqual(Sample, absolute_url("//example.com/dir/path?k=v", "http://www.example.com/other/")),
    ?assertEqual(Sample, absolute_url("/dir/path?k=v", "http://example.com/other/path")),
    ?assertEqual(Sample, absolute_url("path?k=v", "http://example.com/dir/")),
    ?assertEqual(Sample, absolute_url("path?k=v", "http://example.com/dir/form?c=d")),
    ?assertEqual("http://user:passwd@example.com:8080/dir/path?k=v",
                 absolute_url("path?k=v", "http://user:passwd@example.com:8080/dir/form?c=d")),
    %% https
    ?assertEqual("https://example.com/dir/path?k=v",
                 absolute_url("//example.com/dir/path?k=v", "https://www.example.com/other/")),
    %% binary
    ?assertEqual(Sample,
                 absolute_url(<<"//example.com/dir/path?k=v">>, "http://www.example.com/other/")),
    %% kv with slashes
    ?assertEqual(Sample, absolute_url("path?k=v", "http://example.com/dir/form?url=http://www2.example.com/path")),
    %% CurrentUrl is root
    ?assertEqual(Sample, absolute_url("dir/path?k=v", "http://example.com/")).

-endif.

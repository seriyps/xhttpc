
-record(xhttpc_cookie,
        {name :: string(),
         value :: string(),
         expires = session :: non_neg_integer() | session, %FIXME: replace with erlang:timestamp()
         domain :: string(),
         path :: string(),
         secure = false :: boolean(),
         httponly = false :: boolean()}).

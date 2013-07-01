
-record(xhttpc_cookie,
        {name :: string(),
         value :: string(),
         expires = session :: calendar:datetime() | session, % in UTC!
         domain :: string(),
         hostonly = false :: boolean(),
         path :: string(),
         secure = false :: boolean(),
         httponly = false :: boolean(),
         created :: erlang:timestamp()}).

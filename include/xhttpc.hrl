
-type http_header() :: {string(), string()}.
-type http_options() :: [{disable_middlewares, [module()]}
                         | {timeout, timeout()}
                         | {client_options, any()}
                         | {atom(), any()}].

-record(xhttpc_request,
        {url :: string(),
         method = get :: get | post | head,
         headers = [] :: [http_header()],
         body :: iolist(),
         options = [] :: http_options()
        }).

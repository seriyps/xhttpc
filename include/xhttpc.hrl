
-record(xhttpc_request,
        {url :: string(),
         method = get :: get | post | head,
         headers = [] :: [xhttpc:http_header()],
         body :: iodata(),
         options = [] :: xhttpc:http_options()
        }).

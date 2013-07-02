
-record(xhttpc_request,
        {url :: string(),
         method = get :: get | post | head,
         headers = [] :: [xhttpc:http_header()],
         body :: iolist(),
         options = [] :: xhttpc:http_options()
        }).

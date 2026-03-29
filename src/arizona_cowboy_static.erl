-module(arizona_cowboy_static).
-export([init/2]).
-ignore_xref([init/2]).

init(Req, #{dir := Dir} = State) ->
    PathInfo = cowboy_req:path_info(Req),
    FilePath = filename:join([Dir | PathInfo]),
    case file:read_file(FilePath) of
        {ok, Body} ->
            ContentType = content_type(FilePath),
            Req2 = cowboy_req:reply(
                200,
                #{
                    <<"content-type">> => ContentType,
                    <<"cache-control">> => <<"public, max-age=31536000, immutable">>
                },
                Body,
                Req
            ),
            {ok, Req2, State};
        {error, enoent} ->
            Req2 = cowboy_req:reply(404, #{}, <<"Not Found">>, Req),
            {ok, Req2, State}
    end.

content_type(Path) ->
    case filename:extension(Path) of
        Ext when Ext =:= ".js"; Ext =:= <<".js">> -> <<"application/javascript">>;
        Ext when Ext =:= ".css"; Ext =:= <<".css">> -> <<"text/css">>;
        Ext when Ext =:= ".html"; Ext =:= <<".html">> -> <<"text/html">>;
        Ext when Ext =:= ".json"; Ext =:= <<".json">> -> <<"application/json">>;
        Ext when Ext =:= ".svg"; Ext =:= <<".svg">> -> <<"image/svg+xml">>;
        Ext when Ext =:= ".png"; Ext =:= <<".png">> -> <<"image/png">>;
        Ext when Ext =:= ".ico"; Ext =:= <<".ico">> -> <<"image/x-icon">>;
        _ -> <<"application/octet-stream">>
    end.

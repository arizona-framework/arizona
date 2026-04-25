-module(arizona_cowboy_static).

-ifdef(TEST).
-include_lib("eunit/include/eunit.hrl").
-endif.

-moduledoc """
Cowboy handler that serves static files from a directory.

Wired up by `arizona_cowboy_router` for `{asset, Path, ...}` route
specs. Reads the file from disk on every request, sets a long-lived
`cache-control` header (immutable, 1 year), and infers a small set
of content types from the extension. Falls back to
`application/octet-stream` for unknown types and `404` for missing
files.

This is intentionally minimal -- production deployments should put a
proper CDN or `cowboy_static` in front of the asset path.
""".

%% --------------------------------------------------------------------
%% API function exports
%% --------------------------------------------------------------------

-export([init/2]).

%% --------------------------------------------------------------------
%% Ignore xref warnings
%% --------------------------------------------------------------------

-ignore_xref([init/2]).

%% --------------------------------------------------------------------
%% API Functions
%% --------------------------------------------------------------------

-doc """
Cowboy `init/2` callback. Reads `dir/<path_info>` from disk and
replies with the file body and an inferred content type. Replies
with `404` if the file is missing.
""".
-spec init(Req, State) -> {ok, Req1, State} when
    Req :: cowboy_req:req(),
    State :: map(),
    Req1 :: cowboy_req:req().
init(Req, #{dir := Dir} = State) ->
    PathInfo = cowboy_req:path_info(Req),
    FilePath = filename:join([Dir | PathInfo]),
    case file:read_file(FilePath) of
        {ok, Body} ->
            ContentType = content_type(FilePath),
            Req2 = cowboy_req:reply(
                200,
                #{
                    ~"content-type" => ContentType,
                    ~"cache-control" => ~"public, max-age=31536000, immutable"
                },
                Body,
                Req
            ),
            {ok, Req2, State};
        {error, enoent} ->
            Req2 = cowboy_req:reply(404, #{}, ~"Not Found", Req),
            {ok, Req2, State}
    end.

%% --------------------------------------------------------------------
%% Internal functions
%% --------------------------------------------------------------------

content_type(Path) ->
    case filename:extension(Path) of
        Ext when Ext =:= ".js"; Ext =:= ~".js" -> ~"application/javascript";
        Ext when Ext =:= ".css"; Ext =:= ~".css" -> ~"text/css";
        Ext when Ext =:= ".html"; Ext =:= ~".html" -> ~"text/html";
        Ext when Ext =:= ".json"; Ext =:= ~".json" -> ~"application/json";
        Ext when Ext =:= ".svg"; Ext =:= ~".svg" -> ~"image/svg+xml";
        Ext when Ext =:= ".png"; Ext =:= ~".png" -> ~"image/png";
        Ext when Ext =:= ".ico"; Ext =:= ~".ico" -> ~"image/x-icon";
        _ -> ~"application/octet-stream"
    end.

-ifdef(TEST).

content_type_test() ->
    ?assertEqual(~"application/javascript", content_type("app.js")),
    ?assertEqual(~"text/css", content_type("style.css")),
    ?assertEqual(~"text/html", content_type("index.html")),
    ?assertEqual(~"application/json", content_type("data.json")),
    ?assertEqual(~"image/svg+xml", content_type("logo.svg")),
    ?assertEqual(~"image/png", content_type("img.png")),
    ?assertEqual(~"image/x-icon", content_type("favicon.ico")),
    ?assertEqual(~"application/octet-stream", content_type("data.bin")),
    ?assertEqual(~"application/octet-stream", content_type("no-extension")).

-endif.

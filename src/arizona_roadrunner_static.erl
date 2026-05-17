-module(arizona_roadrunner_static).

-ifdef(TEST).
-include_lib("eunit/include/eunit.hrl").
-endif.

-moduledoc """
Roadrunner handler that serves static files from a directory.

Wired up by `arizona_roadrunner_router` for `{asset, Path, ...}`
route specs. Reads the file from disk on every request, sets a
long-lived `cache-control` header (immutable, 1 year), and infers a
small set of content types from the extension. Falls back to
`application/octet-stream` for unknown types and `404` for missing
files.

This is intentionally minimal -- production deployments should put a
proper CDN or `roadrunner_static` (which adds ETag, Range, symlink
guards, etc.) in front of the asset path.
""".

-behaviour(roadrunner_handler).

%% --------------------------------------------------------------------
%% API function exports
%% --------------------------------------------------------------------

-export([handle/1]).

%% --------------------------------------------------------------------
%% API Functions
%% --------------------------------------------------------------------

-doc """
Roadrunner `handle/1` callback. Reads `dir/<wildcard_path>` from
disk and replies with the file body and an inferred content type.
Replies with `404` if the file is missing.
""".
-spec handle(Req) -> {Response, Req} when
    Req :: roadrunner_req:request(),
    Response :: roadrunner_handler:response().
handle(Req) ->
    #{arizona := #{dir := Dir}} = roadrunner_req:state(Req),
    PathSegments = path_segments(Req),
    FilePath = filename:join([Dir | PathSegments]),
    case file:read_file(FilePath) of
        {ok, Body} ->
            ContentType = content_type(FilePath),
            {
                {200,
                    [
                        {~"content-type", ContentType},
                        {~"cache-control", ~"public, max-age=31536000, immutable"},
                        {~"content-length", integer_to_binary(byte_size(Body))}
                    ],
                    Body},
                Req
            };
        {error, enoent} ->
            {{404, [{~"content-length", ~"9"}], ~"Not Found"}, Req}
    end.

%% --------------------------------------------------------------------
%% Internal functions
%% --------------------------------------------------------------------

%% Wildcard segments are bound under the `path` binary key by
%% arizona_roadrunner_router's `*path` pattern. Returns the list of
%% segment binaries; empty list for a bare directory hit.
path_segments(Req) ->
    case roadrunner_req:bindings(Req) of
        #{~"path" := Segs} when is_list(Segs) -> Segs;
        _ -> []
    end.

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

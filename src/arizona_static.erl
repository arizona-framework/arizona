-module(arizona_static).

%% --------------------------------------------------------------------
%% API function exports
%% --------------------------------------------------------------------

-export([generate/0]).
-export([generate/2]).

%

-ignore_xref([generate/0]).
-ignore_xref([generate/2]).

%% --------------------------------------------------------------------
%% API function definitions
%% --------------------------------------------------------------------

-spec generate() -> ok.
generate() ->
    Routes = maps:get(routes, arizona_config:endpoint()),
    OutDir = arizona_config:static_dir(),
    generate(Routes, OutDir).

-spec generate(Routes, OutDir) -> ok when
    Routes :: list(),
    OutDir :: file:filename_all().
generate(Routes, OutDir) when is_list(Routes), is_list(OutDir) ->
    ok = filelib:ensure_path(OutDir),
    lists:foreach(fun(Route) -> parse_route(Route, OutDir) end, Routes).

%% --------------------------------------------------------------------
%% Private functions
%% --------------------------------------------------------------------

parse_route({Path, cowboy_static, {priv_file, App, Filename}}, OutDir) ->
    ok = check_path_segments(Path),
    AppDir = code:lib_dir(App),
    Source = filename:join([AppDir, "priv", Filename]),
    Destination = filename:join([OutDir, tl(Path)]),
    {ok, Bin} = file:read_file(Source),
    ok = filelib:ensure_path(filename:dirname(Destination)),
    ok = file:write_file(Destination, Bin),
    ok;
parse_route({Path0, cowboy_static, {priv_dir, App, Dir}}, OutDir) ->
    Path = lists:flatten(string:replace(Path0, "/[...]", "")),
    ok = check_path_segments(Path),
    AppDir = code:lib_dir(App),
    Wildcard = filename:join([AppDir, "priv", Dir, "**", "*"]),
    Files = [File || File <- filelib:wildcard(Wildcard), filelib:is_regular(File)],
    ok = lists:foreach(
        fun(Filename) ->
            Source = filename:join([AppDir, "priv", Filename]),
            Destination = filename:join([OutDir, tl(Path), filename:basename(Filename)]),
            {ok, Bin} = file:read_file(Source),
            ok = filelib:ensure_path(filename:dirname(Destination)),
            ok = file:write_file(Destination, Bin),
            ok
        end,
        Files
    ),
    ok;
parse_route({Path, arizona_view_handler, {Mod, Assigns}}, OutDir) ->
    ok = check_path_segments(Path),
    Socket0 = arizona_socket:new(render),
    {ok, View0} = arizona_view:mount(Mod, Assigns, Socket0),
    Token = arizona_view:render(Mod, View0),
    {View, _Socket} = arizona_render:render(Token, View0, View0, Socket0),
    Html = arizona_view:rendered_to_iolist(View),
    Destination = filename:join([OutDir, tl(Path), "index.html"]),
    ok = file:write_file(Destination, Html),
    ok;
parse_route(Route, OutDir) ->
    error(invalid_route, [Route, OutDir], [
        {error_info, #{cause => ~"only static files are allowed"}}
    ]).

check_path_segments(Path) ->
    [[] | Segments] = string:split(Path, "/", all),
    case check_path_segments_1(Segments) of
        ok ->
            ok;
        error ->
            error(badpath, [Path], [
                {error_info, #{cause => ~"match syntax is not allowed"}}
            ])
    end.

check_path_segments_1([]) ->
    ok;
check_path_segments_1([Segment | T]) ->
    case Segment =/= [] andalso hd(Segment) of
        $[ ->
            error;
        _ ->
            check_path_segments_1(T)
    end.

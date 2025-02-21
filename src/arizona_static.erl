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
    StaticDir = arizona_config:static_dir(),
    generate(Routes, StaticDir).

-spec generate(Routes, StaticDir) -> ok when
    Routes :: list(),
    StaticDir :: file:filename_all().
generate(Routes, StaticDir) when
    is_list(Routes), (is_list(StaticDir) orelse is_binary(StaticDir))
->
    ok = filelib:ensure_path(StaticDir),
    lists:foreach(fun(Route) -> parse_route(Route, StaticDir) end, Routes).

%% --------------------------------------------------------------------
%% Private functions
%% --------------------------------------------------------------------

parse_route({Path, cowboy_static, {priv_file, App, Filename}}, StaticDir) ->
    parse_priv_file(Path, App, Filename, StaticDir);
parse_route({Path, cowboy_static, {priv_dir, App, Dir}}, StaticDir) ->
    parse_priv_dir(Path, App, Dir, StaticDir);
parse_route({Path, arizona_view_handler, {Mod, Assigns}}, StaticDir) ->
    parse_view(Path, Mod, Assigns, StaticDir);
parse_route(Route, StaticDir) ->
    error(invalid_route, [Route, StaticDir], [
        {error_info, #{cause => ~"only static route is allowed"}}
    ]).

parse_priv_file(Path, App, Filename, StaticDir) ->
    ok = check_path_segments(Path),
    AppDir = code:lib_dir(App),
    Source = filename:join([AppDir, "priv", Filename]),
    Destination = filename:join([StaticDir, tl(Path)]),
    {ok, Bin} = file:read_file(Source),
    ok = filelib:ensure_path(filename:dirname(Destination)),
    ok = file:write_file(Destination, Bin).

parse_priv_dir(Path0, App, Dir, StaticDir) ->
    Path = lists:flatten(string:replace(Path0, "/[...]", "")),
    ok = check_path_segments(Path),
    AppDir = code:lib_dir(App),
    Wildcard = filename:join([AppDir, "priv", Dir, "**", "*"]),
    Files = [File || File <- filelib:wildcard(Wildcard), filelib:is_regular(File)],
    ok = lists:foreach(
        fun(Filename) ->
            Source = filename:join([AppDir, "priv", Filename]),
            Destination = filename:join([StaticDir, tl(Path), filename:basename(Filename)]),
            {ok, Bin} = file:read_file(Source),
            ok = filelib:ensure_path(filename:dirname(Destination)),
            ok = file:write_file(Destination, Bin)
        end,
        Files
    ).

parse_view(Path, Mod, Assigns, StaticDir) ->
    ok = check_path_segments(Path),
    Socket0 = arizona_socket:new(render),
    {ok, View0} = arizona_view:mount(Mod, Assigns, Socket0),
    Token = arizona_view:render(Mod, View0),
    {View, _Socket} = arizona_render:render(Token, View0, View0, Socket0),
    Html = arizona_view:rendered_to_iolist(View),
    Destination = filename:join([StaticDir, tl(Path), "index.html"]),
    ok = file:write_file(Destination, Html).

check_path_segments(Path) ->
    [[] | Segments] = string:split(Path, "/", all),
    case contains_dynamic_segment(Segments) of
        true ->
            error(badpath, [Path], [
                {error_info, #{cause => ~"match syntax is not allowed"}}
            ]);
        false ->
            ok
    end.

% Searches for dynamic segments, e.g "/[:user_id]/profile".
contains_dynamic_segment([]) ->
    ok;
contains_dynamic_segment([Segment | T]) ->
    case Segment =/= [] andalso hd(Segment) of
        $[ ->
            error;
        _ ->
            contains_dynamic_segment(T)
    end.

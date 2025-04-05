-module(arizona_static_SUITE).
-behaviour(ct_suite).
-include_lib("stdlib/include/assert.hrl").
-compile([export_all, nowarn_export_all]).

%% --------------------------------------------------------------------
%% Behaviour (ct_suite) callbacks
%% --------------------------------------------------------------------

all() ->
    [
        {group, write},
        {group, error}
    ].

groups() ->
    [
        {write, [parallel], [
            write_priv_file,
            write_deep_priv_file,
            write_priv_dir,
            write_deep_priv_dir,
            write_view_as_html,
            write_deep_view_as_html
        ]},
        {error, [parallel], [
            badpath_error,
            invalid_route_error
        ]}
    ].

%% --------------------------------------------------------------------
%% Tests
%% --------------------------------------------------------------------

write_priv_file(Config) when is_list(Config) ->
    Route = {"/arizona.js", cowboy_static, {priv_file, arizona, "static/assets/js/arizona.min.js"}},
    ?assertEqual(ok, generate(Route)),
    ?assert(exists("arizona.js")).

write_deep_priv_file(Config) when is_list(Config) ->
    Route =
        {"/a/b/c/arizona.js", cowboy_static, {priv_file, arizona, "static/assets/js/arizona.min.js"}},
    ?assertEqual(ok, generate(Route)),
    ?assert(exists("a/b/c/arizona.js")).

write_priv_dir(Config) when is_list(Config) ->
    Route = {"/[...]", cowboy_static, {priv_dir, arizona, "static/assets/js"}},
    ?assertEqual(ok, generate(Route)),
    ?assert(exists("arizona.min.js")),
    ?assert(exists("arizona-worker.min.js")).

write_deep_priv_dir(Config) when is_list(Config) ->
    Route = {"/a/b/c/[...]", cowboy_static, {priv_dir, arizona, "static/assets/js"}},
    ?assertEqual(ok, generate(Route)),
    ?assert(exists("a/b/c/arizona.min.js")),
    ?assert(exists("a/b/c/arizona-worker.min.js")).

write_view_as_html(Config) when is_list(Config) ->
    Route = {"/", arizona_view_handler, {arizona_example_counter, #{count => 0}}},
    ?assertEqual(ok, generate(Route)),
    ?assert(exists("index.html")).

write_deep_view_as_html(Config) when is_list(Config) ->
    Route = {"/a/b/c/", arizona_view_handler, {arizona_example_counter, #{count => 0}}},
    ?assertEqual(ok, generate(Route)),
    ?assert(exists("a/b/c/index.html")).

badpath_error(Config) when is_list(Config) ->
    Route = {"/[:user_id]/profile", cowboy_static, {priv_file, foo, "foo.html"}},
    ?assertError(badpath, generate(Route)).

invalid_route_error(Config) when is_list(Config) ->
    Route = {"/websocket", arizona_websocket, []},
    ?assertError(invalid_route, generate(Route)).

%% --------------------------------------------------------------------
%% Support
%% --------------------------------------------------------------------

generate(Route) ->
    arizona_static:generate([Route], static_dir()).

exists(Filename) ->
    filelib:is_regular(filename:join(static_dir(), Filename)).

static_dir() -> ~"static".

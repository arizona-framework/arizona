-module(arizona_static_SUITE).
-include_lib("stdlib/include/assert.hrl").

-export([all/0]).
-export([groups/0]).

-export([writes_file_and_returns_path/1]).
-export([applies_bindings/1]).
-export([applies_layout/1]).
-export([creates_nested_dirs/1]).
-export([applies_on_mount/1]).
-export([multiple_specs_return_all_paths/1]).
-export([default_opts_apply_to_every_spec/1]).
-export([spec_opts_override_defaults/1]).
-export([collects_failures/1]).
-export([collects_write_errors/1]).

all() ->
    [{group, generate}].

groups() ->
    [
        {generate, [parallel], [
            writes_file_and_returns_path,
            applies_bindings,
            applies_layout,
            creates_nested_dirs,
            applies_on_mount,
            multiple_specs_return_all_paths,
            default_opts_apply_to_every_spec,
            spec_opts_override_defaults,
            collects_failures,
            collects_write_errors
        ]}
    ].

writes_file_and_returns_path(Config) when is_list(Config) ->
    Dir = subdir(Config, "writes"),
    {[Out], []} = arizona_static:generate(Dir, [{arizona_static_page, ~"index.html"}]),
    ?assert(filelib:is_regular(Out)),
    Content = read(Out),
    ?assertMatch({_, _}, binary:match(Content, ~"static_page")),
    ?assertMatch({_, _}, binary:match(Content, ~"Static")).

applies_bindings(Config) when is_list(Config) ->
    Dir = subdir(Config, "bindings"),
    {[Out], []} = arizona_static:generate(
        Dir, [{arizona_static_page, ~"index.html", #{bindings => #{title => ~"Hello"}}}]
    ),
    ?assertMatch({_, _}, binary:match(read(Out), ~"Hello")).

applies_layout(Config) when is_list(Config) ->
    Dir = subdir(Config, "layout"),
    {[Out], []} = arizona_static:generate(
        Dir, [{arizona_static_page, ~"index.html", #{layouts => [{arizona_layout, render}]}}]
    ),
    Content = read(Out),
    ?assertMatch({_, _}, binary:match(Content, ~"<!DOCTYPE html>")),
    ?assertMatch({_, _}, binary:match(Content, ~"static_page")).

creates_nested_dirs(Config) when is_list(Config) ->
    Dir = subdir(Config, "nested"),
    {[Out], []} = arizona_static:generate(Dir, [{arizona_static_page, ~"deep/page.html"}]),
    ?assert(filelib:is_regular(Out)).

applies_on_mount(Config) when is_list(Config) ->
    Dir = subdir(Config, "on_mount"),
    Hook = fun(B) -> B#{title => ~"FromHook"} end,
    {[Out], []} = arizona_static:generate(
        Dir, [{arizona_static_page, ~"index.html", #{on_mount => [Hook]}}]
    ),
    ?assertMatch({_, _}, binary:match(read(Out), ~"FromHook")).

multiple_specs_return_all_paths(Config) when is_list(Config) ->
    Dir = subdir(Config, "batch"),
    {[A, B], []} = arizona_static:generate(Dir, [
        {arizona_static_page, ~"a.html"},
        {arizona_static_page, ~"sub/b.html", #{bindings => #{title => ~"Bee"}}}
    ]),
    ?assert(filelib:is_regular(A)),
    ?assertMatch({_, _}, binary:match(read(B), ~"Bee")).

default_opts_apply_to_every_spec(Config) when is_list(Config) ->
    Dir = subdir(Config, "defaults"),
    {[P1, P2], []} = arizona_static:generate(
        Dir,
        [{arizona_static_page, ~"a.html"}, {arizona_static_page, ~"b.html"}],
        #{layouts => [{arizona_layout, render}]}
    ),
    %% the shared layout wraps both pages
    ?assertMatch({_, _}, binary:match(read(P1), ~"<!DOCTYPE html>")),
    ?assertMatch({_, _}, binary:match(read(P2), ~"<!DOCTYPE html>")).

spec_opts_override_defaults(Config) when is_list(Config) ->
    Dir = subdir(Config, "override"),
    {[Out], []} = arizona_static:generate(
        Dir,
        [{arizona_static_page, ~"index.html", #{bindings => #{title => ~"Override"}}}],
        #{bindings => #{title => ~"Default"}}
    ),
    Content = read(Out),
    ?assertMatch({_, _}, binary:match(Content, ~"Override")),
    ?assertEqual(nomatch, binary:match(Content, ~"Default")).

collects_failures(Config) when is_list(Config) ->
    Dir = subdir(Config, "failures"),
    %% A good spec is written; a spec whose handler does not exist is collected
    %% in Failed (its `mount/1` raises `undef`) without stopping the batch.
    {[Good], [{BadSpec, Reason}]} = arizona_static:generate(Dir, [
        {arizona_static_page, ~"ok.html"},
        {nonexistent_handler, ~"bad.html"}
    ]),
    ?assert(filelib:is_regular(Good)),
    ?assertEqual({nonexistent_handler, ~"bad.html"}, BadSpec),
    ?assertEqual(undef, Reason),
    ?assertNot(filelib:is_regular(filename:join(Dir, "bad.html"))).

collects_write_errors(Config) when is_list(Config) ->
    Dir = subdir(Config, "write_errors"),
    %% Put a regular file where a spec needs a parent directory: filelib:ensure_dir
    %% *returns* {error, _} (it does not raise), so this exercises generate's
    %% error-return path -- distinct from a handler crash, which is caught. The
    %% batch still continues and the other spec is written.
    Collide = filename:join(Dir, "collide"),
    ok = filelib:ensure_dir(Collide),
    ok = file:write_file(Collide, <<>>),
    {[Good], [{BadSpec, _Reason}]} = arizona_static:generate(Dir, [
        {arizona_static_page, ~"ok.html"},
        {arizona_static_page, ~"collide/page.html"}
    ]),
    ?assert(filelib:is_regular(Good)),
    ?assertEqual({arizona_static_page, ~"collide/page.html"}, BadSpec),
    ?assertNot(filelib:is_regular(filename:join(Dir, "collide/page.html"))).

%% --------------------------------------------------------------------
%% Helpers
%% --------------------------------------------------------------------

subdir(Config, Name) ->
    filename:join(proplists:get_value(priv_dir, Config), Name).

read(Path) ->
    {ok, Bin} = file:read_file(Path),
    Bin.

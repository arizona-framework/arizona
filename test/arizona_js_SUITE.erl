-module(arizona_js_SUITE).
-include_lib("stdlib/include/assert.hrl").
-include_lib("arizona/include/arizona_effect.hrl").

-export([all/0]).
-export([groups/0]).

-export([fetch_unwraps_single_on_error_cmd/1]).
-export([fetch_unwraps_on_error_cmd_list/1]).
-export([fetch_without_on_error_unchanged/1]).
-export([fetch_empty_on_error_list/1]).
-export([transition_empty_cmd_list/1]).
-export([on_key_empty_cmd_list/1]).

all() ->
    [{group, fetch_on_error}, {group, empty_cmd_lists}].

groups() ->
    [
        {fetch_on_error, [parallel], [
            fetch_unwraps_single_on_error_cmd,
            fetch_unwraps_on_error_cmd_list,
            fetch_without_on_error_unchanged
        ]},
        {empty_cmd_lists, [parallel], [
            fetch_empty_on_error_list,
            transition_empty_cmd_list,
            on_key_empty_cmd_list
        ]}
    ].

%% --------------------------------------------------------------------
%% fetch/2 on_error unwrapping
%% --------------------------------------------------------------------

%% A single on_error cmd is unwrapped to its bare op-array so the Opts map is
%% JSON-encodable. Before the fix the wrapped {arizona_effect, ...} tuple reached
%% json:encode/1 (arizona_effect:encode/1 does not recurse into map values) and
%% crashed with {unsupported_type, ...}.
fetch_unwraps_single_on_error_cmd(Config) when is_list(Config) ->
    Cmd = arizona_js:fetch(~"/x", #{
        method => post,
        on_error => arizona_js:remove_attr(~"#f", ~"disabled")
    }),
    ?assertMatch(
        {arizona_effect, [
            ?EFFECT_FETCH,
            ~"/x",
            #{on_error := [?EFFECT_REMOVE_ATTR, ~"#f", ~"disabled"]}
        ]},
        Cmd
    ),
    Bin = arizona_effect:encode(Cmd),
    ?assert(is_binary(Bin)),
    ?assertNotEqual(nomatch, binary:match(Bin, ~"on_error")).

%% A list of on_error cmds is unwrapped to a list of bare op-arrays (the
%% comprehension clause), mirroring on_key/2 and transition/2.
fetch_unwraps_on_error_cmd_list(Config) when is_list(Config) ->
    Cmd = arizona_js:fetch(~"/x", #{
        method => post,
        on_error => [
            arizona_js:add_class(~"#f", ~"err"),
            arizona_js:remove_attr(~"#f", ~"disabled")
        ]
    }),
    ?assertMatch(
        {arizona_effect, [
            ?EFFECT_FETCH,
            ~"/x",
            #{
                on_error := [
                    [?EFFECT_ADD_CLASS, ~"#f", ~"err"],
                    [?EFFECT_REMOVE_ATTR, ~"#f", ~"disabled"]
                ]
            }
        ]},
        Cmd
    ),
    ?assert(is_binary(arizona_effect:encode(Cmd))).

%% Opts without on_error pass through untouched (the catch-all clause).
fetch_without_on_error_unchanged(Config) when is_list(Config) ->
    ?assertMatch(
        {arizona_effect, [?EFFECT_FETCH, ~"/x", #{method := post}]},
        arizona_js:fetch(~"/x", #{method => post})
    ).

%% --------------------------------------------------------------------
%% Empty command lists
%% --------------------------------------------------------------------

%% The public specs admit [] (dynamic construction via a comprehension can
%% legitimately yield an empty command list), and the client treats an empty
%% effect list as a no-op -- so [] must build a harmless wire value instead of
%% crashing function_clause inside unwrap_cmds/1.

fetch_empty_on_error_list(Config) when is_list(Config) ->
    Cmd = arizona_js:fetch(~"/x", #{method => post, on_error => []}),
    ?assertMatch(
        {arizona_effect, [?EFFECT_FETCH, ~"/x", #{on_error := []}]},
        Cmd
    ),
    ?assert(is_binary(arizona_effect:encode(Cmd))).

transition_empty_cmd_list(Config) when is_list(Config) ->
    Cmd = arizona_js:transition([]),
    ?assertMatch({arizona_effect, [?EFFECT_TRANSITION, #{}, []]}, Cmd),
    ?assert(is_binary(arizona_effect:encode(Cmd))).

on_key_empty_cmd_list(Config) when is_list(Config) ->
    Cmd = arizona_js:on_key(enter, []),
    ?assertMatch({arizona_effect, [?EFFECT_ON_KEY, _Key, []]}, Cmd),
    ?assert(is_binary(arizona_effect:encode(Cmd))).

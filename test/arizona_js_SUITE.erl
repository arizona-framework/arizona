-module(arizona_js_SUITE).
-include_lib("stdlib/include/assert.hrl").
-include_lib("arizona/include/arizona_effect.hrl").

-export([all/0]).
-export([groups/0]).

-export([fetch_unwraps_single_on_error_cmd/1]).
-export([fetch_unwraps_on_error_cmd_list/1]).
-export([fetch_without_on_error_unchanged/1]).

all() ->
    [{group, fetch_on_error}].

groups() ->
    [
        {fetch_on_error, [parallel], [
            fetch_unwraps_single_on_error_cmd,
            fetch_unwraps_on_error_cmd_list,
            fetch_without_on_error_unchanged
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

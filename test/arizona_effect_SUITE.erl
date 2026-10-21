-module(arizona_effect_SUITE).
-include_lib("stdlib/include/assert.hrl").

-export([all/0]).
-export([groups/0]).

-export([encode_flattens_nested_lists/1]).
-export([encode_drops_empty_lists/1]).
-export([encode_rejects_non_command_item/1]).

all() ->
    [{group, encode}].

groups() ->
    [
        {encode, [parallel], [
            encode_flattens_nested_lists,
            encode_drops_empty_lists,
            encode_rejects_non_command_item
        ]}
    ].

%% --------------------------------------------------------------------
%% Test cases
%% --------------------------------------------------------------------

%% A command list may nest: a conditional item that yields a list of commands
%% splices into the list it sits in, at any depth, keeping the order.
encode_flattens_nested_lists(Config) when is_list(Config) ->
    A = arizona_js:push_event(~"a"),
    B = arizona_js:toggle(~"#b"),
    C = arizona_js:hide(~"#c"),
    ?assertEqual([[0, ~"a"], [1, ~"#b"], [3, ~"#c"]], arizona_effect:json_value([A, [B, [C]]])),
    ?assertEqual([[1, ~"#b"], [3, ~"#c"], [0, ~"a"]], arizona_effect:json_value([[B, C], A])),
    ?assertEqual(
        ~"[[0,&quot;a&quot;],[1,&quot;#b&quot;]]", arizona_effect:encode([A, [B]])
    ),
    %% to_bin/1 matches the same shapes classify_trusted/1 names, nested included.
    ?assertEqual({effect, [[], [B], A]}, arizona_template:classify_trusted([[], [B], A])),
    ?assertEqual(
        ~"[[1,&quot;#b&quot;],[0,&quot;a&quot;]]", arizona_template:to_bin([[], [B], A])
    ),
    ?assertEqual(value, arizona_template:classify_trusted([[~"text"], A])),
    %% A command list wrapped by another command flattens the same way.
    ?assertMatch(
        {arizona_effect, [_OnKey, _Key, [[0, ~"a"], [1, ~"#b"]]]},
        arizona_js:on_key(enter, [A, [], [B]])
    ).

%% `[]` is no command: dropped wherever it sits, so a conditional item with a
%% `false -> []` branch needs no filtering. A list of nothing but empty items is
%% the empty command list.
encode_drops_empty_lists(Config) when is_list(Config) ->
    A = arizona_js:push_event(~"a"),
    ?assertEqual([[0, ~"a"]], arizona_effect:json_value([[], A, [[]], []])),
    ?assertEqual([], arizona_effect:json_value([[], [[]]])),
    ?assertEqual(~"[]", arizona_effect:encode_json([])).

%% Flattening is for commands only: any other item is a caller bug and fails
%% loudly instead of reaching the client as a malformed command.
encode_rejects_non_command_item(Config) when is_list(Config) ->
    A = arizona_js:push_event(~"a"),
    ?assertError(function_clause, arizona_effect:json_value([A, ~"text"])),
    ?assertError(function_clause, arizona_effect:json_value([A, [ok]])).

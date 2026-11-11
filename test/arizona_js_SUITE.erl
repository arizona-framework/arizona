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
-export([set_attr_writes_a_command_as_an_array/1]).
-export([set_attr_writes_a_command_list_as_an_array/1]).
-export([set_attr_keeps_a_binary_value_a_string/1]).
-export([set_attr_refuses_a_command_under_another_name/1]).
-export([toggle_attr_toggles_between_commands/1]).
-export([toggle_attr_refuses_a_command_and_a_string/1]).
-export([toggle_attr_refuses_commands_under_another_name/1]).
-export([set_attr_refuses_a_value_neither_binary_nor_command/1]).
-export([toggle_attr_refuses_a_value_neither_binary_nor_command/1]).
-export([attr_errors_name_the_public_function/1]).
-export([attr_refuses_an_improper_list_value/1]).
-export([set_attr_refuses_a_value_with_no_command/1]).
-export([toggle_attr_refuses_a_value_with_no_command/1]).
-export([set_attr_writes_no_command_as_an_empty_list/1]).
-export([toggle_attr_toggles_between_a_command_and_none/1]).
-export([attr_refuses_a_name_that_is_not_a_binary/1]).
-export([attr_refuses_a_selector_that_is_not_a_binary/1]).

all() ->
    [{group, fetch_on_error}, {group, empty_cmd_lists}, {group, az_attr_commands}].

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
        ]},
        {az_attr_commands, [parallel], [
            set_attr_writes_a_command_as_an_array,
            set_attr_writes_a_command_list_as_an_array,
            set_attr_keeps_a_binary_value_a_string,
            set_attr_refuses_a_command_under_another_name,
            toggle_attr_toggles_between_commands,
            toggle_attr_refuses_a_command_and_a_string,
            toggle_attr_refuses_commands_under_another_name,
            set_attr_refuses_a_value_neither_binary_nor_command,
            toggle_attr_refuses_a_value_neither_binary_nor_command,
            attr_errors_name_the_public_function,
            attr_refuses_an_improper_list_value,
            set_attr_refuses_a_value_with_no_command,
            toggle_attr_refuses_a_value_with_no_command,
            set_attr_writes_no_command_as_an_empty_list,
            toggle_attr_toggles_between_a_command_and_none,
            attr_refuses_a_name_that_is_not_a_binary,
            attr_refuses_a_selector_that_is_not_a_binary
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
        [
            ?EFFECT_FETCH,
            ~"/x",
            #{on_error := [?EFFECT_REMOVE_ATTR, ~"#f", ~"disabled"]}
        ],
        arizona_effect:json_value(Cmd)
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
        [
            ?EFFECT_FETCH,
            ~"/x",
            #{
                on_error := [
                    [?EFFECT_ADD_CLASS, ~"#f", ~"err"],
                    [?EFFECT_REMOVE_ATTR, ~"#f", ~"disabled"]
                ]
            }
        ],
        arizona_effect:json_value(Cmd)
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
        [?EFFECT_FETCH, ~"/x", #{on_error := []}],
        arizona_effect:json_value(Cmd)
    ),
    ?assert(is_binary(arizona_effect:encode(Cmd))).

transition_empty_cmd_list(Config) when is_list(Config) ->
    Cmd = arizona_js:transition([]),
    ?assertMatch([?EFFECT_TRANSITION, #{}, []], arizona_effect:json_value(Cmd)),
    ?assert(is_binary(arizona_effect:encode(Cmd))).

on_key_empty_cmd_list(Config) when is_list(Config) ->
    Cmd = arizona_js:on_key(enter, []),
    ?assertMatch([?EFFECT_ON_KEY, _Key, []], arizona_effect:json_value(Cmd)),
    ?assert(is_binary(arizona_effect:encode(Cmd))).

%% --------------------------------------------------------------------
%% Commands written under an az-* name
%% --------------------------------------------------------------------

%% The client proves an `az-*` name a `set_attr` writes only when the value is a
%% command, which it tells from data by type: an array on the wire, never a
%% string. So a command value is encoded as the array an event attribute holds.
set_attr_writes_a_command_as_an_array(Config) when is_list(Config) ->
    Cmd = arizona_js:set_attr(~"#x", ~"az-select", arizona_js:push_event(~"picked")),
    ?assertEqual(
        [?EFFECT_SET_ATTR, ~"#x", ~"az-select", [?EFFECT_PUSH_EVENT, ~"picked"]],
        arizona_effect:json_value(Cmd)
    ),
    ?assertEqual(
        ~"[7,\"#x\",\"az-select\",[0,\"picked\"]]",
        arizona_effect:encode_json(Cmd)
    ).

set_attr_writes_a_command_list_as_an_array(Config) when is_list(Config) ->
    Cmd = arizona_js:set_attr(~"#x", ~"az-click", [
        arizona_js:push_event(~"a"), [], [arizona_js:hide(~"#m")]
    ]),
    ?assertEqual(
        [?EFFECT_SET_ATTR, ~"#x", ~"az-click", [[?EFFECT_PUSH_EVENT, ~"a"], [?EFFECT_HIDE, ~"#m"]]],
        arizona_effect:json_value(Cmd)
    ),
    ?assert(is_binary(arizona_effect:encode(Cmd))).

%% A binary under an az-* name stays a string: data, which proves nothing.
set_attr_keeps_a_binary_value_a_string(Config) when is_list(Config) ->
    ?assertEqual(
        [?EFFECT_SET_ATTR, ~"#x", ~"az-select", ~"[1,2,3]"],
        arizona_effect:json_value(arizona_js:set_attr(~"#x", ~"az-select", ~"[1,2,3]"))
    ).

%% Only an `az-*` attribute runs a command; anywhere else its JSON is just text.
set_attr_refuses_a_command_under_another_name(Config) when is_list(Config) ->
    Message = formatted_error(fun() ->
        arizona_js:set_attr(~"#x", ~"title", arizona_js:push_event(~"a"))
    end),
    ?assertNotEqual(nomatch, string:find(Message, ~"a command value needs an az-* attribute")),
    ?assertNotEqual(nomatch, string:find(Message, ~"title")).

toggle_attr_toggles_between_commands(Config) when is_list(Config) ->
    Cmd = arizona_js:toggle_attr(
        ~"#x", ~"az-click", arizona_js:push_event(~"open"), arizona_js:push_event(~"close")
    ),
    ?assertEqual(
        [
            ?EFFECT_TOGGLE_ATTR,
            ~"#x",
            ~"az-click",
            [?EFFECT_PUSH_EVENT, ~"open"],
            [?EFFECT_PUSH_EVENT, ~"close"]
        ],
        arizona_effect:json_value(Cmd)
    ).

%% A command and a string would leave the client unable to say whether the
%% name it writes carries commands.
toggle_attr_refuses_a_command_and_a_string(Config) when is_list(Config) ->
    Expected = ~"toggle_attr values must both be binaries or both commands",
    ?assertNotEqual(
        nomatch,
        string:find(
            formatted_error(fun() ->
                arizona_js:toggle_attr(
                    ~"#x", ~"az-click", arizona_js:push_event(~"a"), ~"[0,\"b\"]"
                )
            end),
            Expected
        )
    ),
    ?assertNotEqual(
        nomatch,
        string:find(
            formatted_error(fun() ->
                arizona_js:toggle_attr(~"#x", ~"title", ~"a", arizona_js:push_event(~"b"))
            end),
            Expected
        )
    ).

toggle_attr_refuses_commands_under_another_name(Config) when is_list(Config) ->
    Message = formatted_error(fun() ->
        arizona_js:toggle_attr(
            ~"#x", ~"title", arizona_js:push_event(~"a"), arizona_js:push_event(~"b")
        )
    end),
    ?assertNotEqual(nomatch, string:find(Message, ~"a command value needs an az-* attribute")),
    ?assertNotEqual(nomatch, string:find(Message, ~"title")).

%% A charlist is not a command: suggesting an az-* name would turn the text
%% into a command nothing can run, so the value itself is named.
set_attr_refuses_a_value_neither_binary_nor_command(Config) when is_list(Config) ->
    [
        assert_value_error(fun() -> arizona_js:set_attr(~"#x", Attr, "hello") end)
     || Attr <- [~"title", ~"az-select"]
    ],
    assert_value_error(fun() ->
        arizona_js:set_attr(~"#x", ~"az-select", [arizona_js:push_event(~"a"), "b"])
    end).

toggle_attr_refuses_a_value_neither_binary_nor_command(Config) when is_list(Config) ->
    assert_value_error(fun() -> arizona_js:toggle_attr(~"#x", ~"type", "password", "text") end),
    assert_value_error(fun() -> arizona_js:toggle_attr(~"#x", ~"type", ~"password", "text") end),
    assert_value_error(fun() ->
        arizona_js:toggle_attr(~"#x", ~"az-click", arizona_js:push_event(~"a"), "b")
    end).

attr_errors_name_the_public_function(Config) when is_list(Config) ->
    ?assertMatch(
        {arizona_js, set_attr, [~"#x", ~"title", "hello"], _},
        top_frame(fun() -> arizona_js:set_attr(~"#x", ~"title", "hello") end)
    ),
    ?assertMatch(
        {arizona_js, toggle_attr, [~"#x", ~"title", ~"a", _], _},
        top_frame(fun() ->
            arizona_js:toggle_attr(~"#x", ~"title", ~"a", arizona_js:push_event(~"b"))
        end)
    ).

%% An improper list is no command list: it is refused as a value by the public
%% function, not by whatever walked it. That holds when its tail is a command
%% too, which is a command as an element but not as a list's tail.
attr_refuses_an_improper_list_value(Config) when is_list(Config) ->
    P = arizona_js:push_event(~"a"),
    Improper = [P | foo],
    [
        begin
            assert_value_error(Fun),
            ?assertMatch({arizona_js, Name, _, _}, top_frame(Fun))
        end
     || {Name, Fun} <- [
            {set_attr, fun() -> arizona_js:set_attr(~"#x", ~"az-click", Improper) end},
            {set_attr, fun() -> arizona_js:set_attr(~"#x", ~"title", Improper) end},
            {set_attr, fun() -> arizona_js:set_attr(~"#x", ~"az-click", [P | P]) end},
            {set_attr, fun() -> arizona_js:set_attr(~"#x", ~"az-click", [[] | P]) end},
            {set_attr, fun() -> arizona_js:set_attr(~"#x", ~"az-click", [[P | P]]) end},
            {toggle_attr, fun() ->
                arizona_js:toggle_attr(~"#x", ~"az-click", Improper, arizona_js:push_event(~"b"))
            end},
            {toggle_attr, fun() ->
                arizona_js:toggle_attr(~"#x", ~"az-click", arizona_js:push_event(~"b"), Improper)
            end},
            {toggle_attr, fun() -> arizona_js:toggle_attr(~"#x", ~"az-click", [P | P], []) end}
        ]
    ].

%% An empty charlist is `[]`, which flattens to no command. Under a plain name
%% that is no command misplaced but no value at all.
set_attr_refuses_a_value_with_no_command(Config) when is_list(Config) ->
    [
        assert_no_command_error(fun() -> arizona_js:set_attr(~"#x", ~"class", Value) end)
     || Value <- ["", [[]], [[], [[]]]]
    ].

toggle_attr_refuses_a_value_with_no_command(Config) when is_list(Config) ->
    Cmd = arizona_js:push_event(~"a"),
    [
        assert_no_command_error(fun() -> arizona_js:toggle_attr(~"#x", ~"class", A, B) end)
     || {A, B} <- [{"", ""}, {[], Cmd}, {Cmd, [[]]}]
    ].

%% Under an az-* name no command clears the handler: it is written as the empty
%% list, which runs nothing (and so the client proves no name from it).
set_attr_writes_no_command_as_an_empty_list(Config) when is_list(Config) ->
    [
        ?assertEqual(
            [?EFFECT_SET_ATTR, ~"#x", ~"az-click", []],
            arizona_effect:json_value(arizona_js:set_attr(~"#x", ~"az-click", Value))
        )
     || Value <- ["", [[]], [[], [[]]]]
    ],
    ?assertEqual(
        ~"[7,\"#x\",\"az-click\",[]]",
        arizona_effect:encode_json(arizona_js:set_attr(~"#x", ~"az-click", []))
    ).

%% Toggling between a command and none turns a handler on and off.
toggle_attr_toggles_between_a_command_and_none(Config) when is_list(Config) ->
    Cmd = arizona_js:push_event(~"a"),
    ?assertEqual(
        [?EFFECT_TOGGLE_ATTR, ~"#x", ~"az-click", [?EFFECT_PUSH_EVENT, ~"a"], []],
        arizona_effect:json_value(arizona_js:toggle_attr(~"#x", ~"az-click", Cmd, [[]]))
    ),
    ?assertEqual(
        [?EFFECT_TOGGLE_ATTR, ~"#x", ~"az-click", [], [?EFFECT_PUSH_EVENT, ~"a"]],
        arizona_effect:json_value(arizona_js:toggle_attr(~"#x", ~"az-click", "", Cmd))
    ),
    ?assertEqual(
        [?EFFECT_TOGGLE_ATTR, ~"#x", ~"az-click", [], []],
        arizona_effect:json_value(arizona_js:toggle_attr(~"#x", ~"az-click", [], []))
    ).

%% The name is what the client writes and, under az-*, proves, so an atom or a
%% charlist is refused whatever the value, binary values included.
attr_refuses_a_name_that_is_not_a_binary(Config) when is_list(Config) ->
    Cmd = arizona_js:push_event(~"a"),
    [
        assert_target_error(Name, ~"an attribute name must be a binary", Fun)
     || Attr <- [title, "title", 'az-click'],
        {Name, Fun} <- [
            {set_attr, fun() -> arizona_js:set_attr(~"#x", Attr, ~"v") end},
            {set_attr, fun() -> arizona_js:set_attr(~"#x", Attr, Cmd) end},
            {toggle_attr, fun() -> arizona_js:toggle_attr(~"#x", Attr, ~"a", ~"b") end},
            {toggle_attr, fun() -> arizona_js:toggle_attr(~"#x", Attr, Cmd, Cmd) end},
            {toggle_attr, fun() -> arizona_js:toggle_attr(~"#x", Attr) end},
            {remove_attr, fun() -> arizona_js:remove_attr(~"#x", Attr) end}
        ]
    ].

attr_refuses_a_selector_that_is_not_a_binary(Config) when is_list(Config) ->
    Cmd = arizona_js:push_event(~"a"),
    [
        assert_target_error(Name, ~"a selector must be a binary", Fun)
     || Sel <- [x, "#x", 1],
        {Name, Fun} <- [
            {set_attr, fun() -> arizona_js:set_attr(Sel, ~"title", ~"v") end},
            {set_attr, fun() -> arizona_js:set_attr(Sel, ~"az-click", Cmd) end},
            {toggle_attr, fun() -> arizona_js:toggle_attr(Sel, ~"title", ~"a", ~"b") end},
            {toggle_attr, fun() -> arizona_js:toggle_attr(Sel, ~"az-click", Cmd, Cmd) end},
            {toggle_attr, fun() -> arizona_js:toggle_attr(Sel, ~"hidden") end},
            {remove_attr, fun() -> arizona_js:remove_attr(Sel, ~"hidden") end}
        ]
    ].

%% --------------------------------------------------------------------
%% Helpers
%% --------------------------------------------------------------------

%% The message `erl_error` renders for the error `Fun` raises, through the
%% raise site's `error_info` annotation.
formatted_error(Fun) ->
    try Fun() of
        Result -> error({no_error, Result})
    catch
        error:Reason:Stacktrace ->
            unicode:characters_to_binary(erl_error:format_exception(error, Reason, Stacktrace))
    end.

assert_value_error(Fun) ->
    Message = formatted_error(Fun),
    ?assertNotEqual(
        nomatch, string:find(Message, ~"a value must be a binary, or under an az-* name a command")
    ),
    ?assertEqual(nomatch, string:find(Message, ~"needs an az-* attribute")),
    ?assertEqual(nomatch, string:find(Message, ~"must both be binaries or both commands")).

top_frame(Fun) ->
    try Fun() of
        Result -> error({no_error, Result})
    catch
        error:_:Stacktrace ->
            [Frame | _] = Stacktrace,
            Frame
    end.

assert_no_command_error(Fun) ->
    assert_value_error(Fun),
    Message = formatted_error(Fun),
    ?assertNotEqual(nomatch, string:find(Message, ~"an empty string is <<>>")),
    ?assertEqual(nomatch, string:find(Message, ~"proves no name")).

assert_target_error(Name, Expected, Fun) ->
    ?assertNotEqual(nomatch, string:find(formatted_error(Fun), Expected)),
    ?assertMatch({arizona_js, Name, _, _}, top_frame(Fun)).

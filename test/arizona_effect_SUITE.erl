-module(arizona_effect_SUITE).
-include_lib("stdlib/include/assert.hrl").
-include_lib("arizona/include/arizona_effect.hrl").

-export([all/0]).
-export([groups/0]).

-export([encode_flattens_nested_lists/1]).
-export([encode_drops_empty_lists/1]).
-export([encode_rejects_non_command_item/1]).
-export([encode_matches_escaped_json/1]).
-export([encode_refuses_non_utf8_like_json/1]).
-export([protocol_builders_build_protocol_ops/1]).
-export([platform_builders_share_protocol_builders/1]).
-export([nested_reaches_wire_as_plain_list/1]).
-export([mapfold_nested_walks_every_nested_list/1]).
-export([nested_flash_refused_whatever_the_op/1]).

all() ->
    [{group, encode}, {group, protocol}, {group, nested}].

groups() ->
    [
        {encode, [parallel], [
            encode_flattens_nested_lists,
            encode_drops_empty_lists,
            encode_rejects_non_command_item,
            encode_matches_escaped_json,
            encode_refuses_non_utf8_like_json
        ]},
        {protocol, [parallel], [
            protocol_builders_build_protocol_ops,
            platform_builders_share_protocol_builders
        ]},
        {nested, [parallel], [
            nested_reaches_wire_as_plain_list,
            mapfold_nested_walks_every_nested_list,
            nested_flash_refused_whatever_the_op
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
    %% A command list another command nests flattens the same way, and reaches the
    %% wire as that plain list.
    OnKey = arizona_js:on_key(enter, [A, [], [B]]),
    ?assertMatch(
        {arizona_effect, [_OnKey, _Key, {arizona_effect, deferred, [[0, ~"a"], [1, ~"#b"]]}]},
        OnKey
    ),
    ?assertMatch([_OnKey, _Key, [[0, ~"a"], [1, ~"#b"]]], arizona_effect:json_value(OnKey)).

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

%% `encode/1` escapes each string as it encodes it; the bytes must be exactly
%% those of escaping `encode_json/1`'s finished JSON, for every shape a command
%% argument takes: strings with and without JSON or attribute escapes, UTF-8, map
%% keys of every type json accepts, scalars, atoms and nesting.
encode_matches_escaped_json(Config) when is_list(Config) ->
    _ = rand:seed(exsss, {7, 11, 13}),
    Builders = [
        arizona_js:push_event(~"inc"),
        arizona_js:push_event(~"select", #{id => 42, name => ~"row-42"}),
        [arizona_js:push_event(~"del", #{id => 7}), arizona_js:toggle(~"#modal")],
        arizona_js:fetch(~"/account", #{method => post}),
        arizona_js:on_key(enter, arizona_js:push_event(~"x")),
        arizona_js:set_title(~"Tom & \"Jerry\" <3")
    ],
    %% Op codes of commands that wrap none (a wrapper validates what it holds).
    Ops = [
        ?EFFECT_PUSH_EVENT, ?EFFECT_SET_ATTR, ?EFFECT_DISPATCH_EVENT, ?EFFECT_SET_LOCAL, ?EFFECT_OS
    ],
    Random = [
        {arizona_effect, [lists:nth(rand:uniform(length(Ops)), Ops) | gen_list(3)]}
     || _ <- lists:seq(1, 2000)
    ],
    [
        ?assertEqual(escape_attr_ref(arizona_effect:encode_json(C)), arizona_effect:encode(C))
     || C <- Builders ++ Random
    ].

%% A string that is not UTF-8 fails the same way through both encoders.
encode_refuses_non_utf8_like_json(Config) when is_list(Config) ->
    Cmd = {arizona_effect, [0, ~"ok", #{k => <<"a", 16#FF, "b">>}]},
    {error, Reason} =
        try
            arizona_effect:encode_json(Cmd)
        catch
            error:R1 -> {error, R1}
        end,
    ?assertMatch({non_utf8_render, 16#FF, _}, Reason),
    ?assertError(Reason, arizona_effect:encode(Cmd)).

%% --------------------------------------------------------------------
%% Helpers
%% --------------------------------------------------------------------

escape_attr_ref(Json) ->
    <<<<(escape_attr_byte(C))/binary>> || <<C>> <= Json>>.

escape_attr_byte($&) -> ~"&amp;";
escape_attr_byte($") -> ~"&quot;";
escape_attr_byte($<) -> ~"&lt;";
escape_attr_byte(C) -> <<C>>.

gen_list(Depth) ->
    [gen_value(Depth) || _ <- lists:seq(1, rand:uniform(4) - 1)].

gen_value(0) ->
    gen_scalar();
gen_value(Depth) ->
    case rand:uniform(6) of
        1 -> gen_list(Depth - 1);
        2 -> maps:from_list([{gen_key(), gen_value(Depth - 1)} || _ <- gen_list(0)]);
        _ -> gen_scalar()
    end.

gen_scalar() ->
    case rand:uniform(8) of
        1 -> rand:uniform(100000) - 500;
        2 -> rand:uniform() * 1000 - 10;
        3 -> lists:nth(rand:uniform(5), [true, false, null, some_atom, 'a&b<"c']);
        _ -> gen_string()
    end.

gen_key() ->
    case rand:uniform(4) of
        1 -> list_to_atom("k" ++ integer_to_list(rand:uniform(20)));
        2 -> rand:uniform(50);
        3 -> rand:uniform() * 10;
        4 -> gen_string()
    end.

gen_string() ->
    Pieces = [
        ~"a",
        ~"row",
        ~"&",
        ~"\"",
        ~"<",
        ~">",
        ~"\\",
        ~"\n",
        ~"\t",
        ~"'",
        ~" ",
        ~"é",
        ~"€",
        ~"#modal",
        <<1>>
    ],
    iolist_to_binary([
        lists:nth(rand:uniform(length(Pieces)), Pieces)
     || _ <- lists:seq(1, rand:uniform(12) - 1)
    ]).

%% The commands the socket protocol defines are built in core, so the server builds
%% a redirect without naming a platform's builders.
protocol_builders_build_protocol_ops(Config) when is_list(Config) ->
    ?assertEqual(
        {arizona_effect, [?EFFECT_PUSH_EVENT, ~"e"]}, arizona_effect:push_event(~"e")
    ),
    ?assertEqual(
        {arizona_effect, [?EFFECT_PUSH_EVENT, ~"e", #{a => 1}]},
        arizona_effect:push_event(~"e", #{a => 1})
    ),
    ?assertEqual({arizona_effect, [?EFFECT_NAVIGATE, ~"/p"]}, arizona_effect:navigate(~"/p")),
    ?assertEqual(
        {arizona_effect, [?EFFECT_NAVIGATE, ~"/p", #{full => true}]},
        arizona_effect:navigate(~"/p", #{full => true})
    ),
    ?assertEqual({arizona_effect, [?EFFECT_PATCH, ~"/p"]}, arizona_effect:patch(~"/p")),
    ?assertEqual(
        {arizona_effect, [?EFFECT_PATCH, ~"/p", #{replace => true}]},
        arizona_effect:patch(~"/p", #{replace => true})
    ).

%% Every platform's builders return the very protocol command, so a client of any
%% platform receives the same op.
platform_builders_share_protocol_builders(Config) when is_list(Config) ->
    %% function_exported/3 is false for a module not yet loaded.
    {module, arizona_js} = code:ensure_loaded(arizona_js),
    {module, arizona_android} = code:ensure_loaded(arizona_android),
    Calls = [
        {push_event, [~"e"]},
        {push_event, [~"e", #{a => 1}]},
        {navigate, [~"/p"]},
        {navigate, [~"/p", #{replace => true}]},
        {patch, [~"/p"]},
        {patch, [~"/p", #{replace => true}]}
    ],
    [
        ?assertEqual({F, A, apply(arizona_effect, F, A)}, {F, A, apply(Mod, F, A)})
     || {F, A} <- Calls,
        Mod <- [arizona_js, arizona_android],
        erlang:function_exported(Mod, F, length(A))
    ].

%% A nested command reaches the client as the bare command or list it holds, in the
%% shape the builder gave it, wherever it sits: an argument, a map value, or inside
%% a command that is itself nested.
nested_reaches_wire_as_plain_list(Config) when is_list(Config) ->
    Toggle = arizona_js:toggle(~"#m"),
    ?assertEqual(
        [?EFFECT_TRANSITION, #{}, [?EFFECT_TOGGLE, ~"#m"]],
        arizona_effect:json_value(arizona_js:transition(Toggle))
    ),
    ?assertEqual(
        [?EFFECT_FETCH, ~"/x", #{on_error => [[?EFFECT_TOGGLE, ~"#m"], [?EFFECT_HIDE, ~"#n"]]}],
        arizona_effect:json_value(
            arizona_js:fetch(~"/x", #{on_error => [Toggle, arizona_js:hide(~"#n")]})
        )
    ),
    ?assertEqual(
        [?EFFECT_ON_KEY, [escape], [?EFFECT_TRANSITION, #{}, [?EFFECT_TOGGLE, ~"#m"]]],
        arizona_effect:json_value(arizona_js:on_key(escape, arizona_js:transition(Toggle)))
    ).

%% The walk hands each nested list to the fun with when it runs, and rebuilds the
%% command around what the fun returns; a command nesting nothing is unchanged.
mapfold_nested_walks_every_nested_list(Config) when is_list(Config) ->
    {arizona_effect, Fetch} = arizona_js:fetch(~"/x", #{
        on_error => arizona_js:transition(arizona_js:toggle(~"#m"))
    }),
    Hide = arizona_js:hide(~"#h"),
    {Rebuilt, Seen} = arizona_effect:mapfold_nested(
        fun(When, [_Inner], Acc) ->
            {arizona_effect, Bare} = Hide,
            {[Bare], [When | Acc]}
        end,
        [],
        Fetch
    ),
    ?assertEqual([deferred], Seen),
    ?assertEqual(
        [?EFFECT_FETCH, ~"/x", #{on_error => [?EFFECT_HIDE, ~"#h"]}],
        arizona_effect:to_wire(Rebuilt)
    ),
    {arizona_effect, Plain} = arizona_js:toggle(~"#p"),
    ?assertEqual(
        {Plain, none},
        arizona_effect:mapfold_nested(fun(_When, Inner, _Acc) -> {Inner, called} end, none, Plain)
    ).

%% The encoder knows no op that nests: a command of any op code holding a
%% navigate with a flash, as data, is refused like a bare one.
nested_flash_refused_whatever_the_op(Config) when is_list(Config) ->
    Nav = arizona_effect:navigate(~"/p", #{flash => #{~"k" => ~"v"}}),
    Cmd = {arizona_effect, [999, arizona_effect:nest(now, Nav)]},
    ?assertError({flash_outside_live_effect, navigate}, arizona_effect:json_value(Cmd)).

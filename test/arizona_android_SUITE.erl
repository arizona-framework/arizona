-module(arizona_android_SUITE).
-moduledoc """
Builder unit tests for `arizona_android`, the `?native` client command module.

Asserts each builder produces the shared `{arizona_effect, [OpCode | Args]}`
wire tuple with the right op code and arguments, and -- crucially for the native
path, which has no form/input auto-collection -- that an explicit `push_event/2`
payload rides all the way through `arizona_effect:encode_json/1` into the wire
frame rather than being silently dropped.
""".
-include_lib("stdlib/include/assert.hrl").
-include_lib("arizona/include/arizona_effect.hrl").

-export([all/0]).
-export([groups/0]).

-export([push_event_1_builds_command/1]).
-export([push_event_2_carries_payload/1]).
-export([navigate_1_builds_command/1]).
-export([navigate_2_carries_opts/1]).
-export([encode_json_preserves_payload/1]).

all() ->
    [{group, builders}].

groups() ->
    [
        {builders, [parallel], [
            push_event_1_builds_command,
            push_event_2_carries_payload,
            navigate_1_builds_command,
            navigate_2_carries_opts,
            encode_json_preserves_payload
        ]}
    ].

%% --------------------------------------------------------------------
%% Test cases
%% --------------------------------------------------------------------

push_event_1_builds_command(Config) when is_list(Config) ->
    ?assertMatch(
        {arizona_effect, [?EFFECT_PUSH_EVENT, ~"inc"]},
        arizona_android:push_event(~"inc")
    ).

%% The explicit payload is retained in the command tuple: on native it is the
%% only way to attach data to an event (no auto-collection), so dropping it makes
%% a handler matching a required key crash the live process.
push_event_2_carries_payload(Config) when is_list(Config) ->
    ?assertMatch(
        {arizona_effect, [?EFFECT_PUSH_EVENT, ~"save", #{id := ~"42"}]},
        arizona_android:push_event(~"save", #{id => ~"42"})
    ).

navigate_1_builds_command(Config) when is_list(Config) ->
    ?assertMatch(
        {arizona_effect, [?EFFECT_NAVIGATE, ~"/p"]},
        arizona_android:navigate(~"/p")
    ).

navigate_2_carries_opts(Config) when is_list(Config) ->
    ?assertMatch(
        {arizona_effect, [?EFFECT_NAVIGATE, ~"/p", #{replace := true}]},
        arizona_android:navigate(~"/p", #{replace => true})
    ).

%% The neutral encoder (the native wire path) serializes the whole command,
%% payload included -- the server half of the "carry the explicit payload"
%% guarantee the client dispatch tests assert on the frame side.
encode_json_preserves_payload(Config) when is_list(Config) ->
    Cmd = arizona_android:push_event(~"save", #{id => ~"42"}),
    ?assertEqual(~"[0,\"save\",{\"id\":\"42\"}]", arizona_effect:encode_json(Cmd)).

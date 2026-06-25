-module(arizona_os_SUITE).
-include_lib("stdlib/include/assert.hrl").
-include_lib("arizona/include/arizona_effect.hrl").

-export([all/0]).
-export([groups/0]).

-export([command_bare/1]).
-export([command_with_args/1]).
-export([set_title/1]).
-export([focus/1]).
-export([minimize/1]).
-export([maximize/1]).
-export([fullscreen/1]).
-export([notify/1]).
-export([notify_with_opts/1]).
-export([capture_protection/1]).
-export([command_encodes_to_json/1]).

-export([capability_true/1]).
-export([capability_false_value/1]).
-export([capability_absent_key/1]).
-export([capability_not_connected/1]).
-export([capabilities_map/1]).
-export([capabilities_default_empty/1]).

all() ->
    [{group, builders}, {group, read_api}].

groups() ->
    [
        {builders, [parallel], [
            command_bare,
            command_with_args,
            set_title,
            focus,
            minimize,
            maximize,
            fullscreen,
            notify,
            notify_with_opts,
            capture_protection,
            command_encodes_to_json
        ]},
        {read_api, [parallel], [
            capability_true,
            capability_false_value,
            capability_absent_key,
            capability_not_connected,
            capabilities_map,
            capabilities_default_empty
        ]}
    ].

%% --------------------------------------------------------------------
%% Builders -- every command funnels through the single generic ?EFFECT_OS op
%% carrying a capability name + args (the shell owns the vocabulary).
%% --------------------------------------------------------------------

command_bare(Config) when is_list(Config) ->
    ?assertEqual(
        {arizona_effect, [?EFFECT_OS, ~"window_focus"]}, arizona_os:command(~"window_focus")
    ).

command_with_args(Config) when is_list(Config) ->
    %% Args is a list spliced after the name.
    ?assertEqual(
        {arizona_effect, [?EFFECT_OS, ~"window_title", ~"Hi", 1]},
        arizona_os:command(~"window_title", [~"Hi", 1])
    ).

set_title(Config) when is_list(Config) ->
    ?assertEqual(
        {arizona_effect, [?EFFECT_OS, ~"window_title", ~"Hello"]},
        arizona_os:set_title(~"Hello")
    ).

focus(Config) when is_list(Config) ->
    ?assertEqual({arizona_effect, [?EFFECT_OS, ~"window_focus"]}, arizona_os:focus()).

minimize(Config) when is_list(Config) ->
    ?assertEqual({arizona_effect, [?EFFECT_OS, ~"window_minimize"]}, arizona_os:minimize()).

maximize(Config) when is_list(Config) ->
    ?assertEqual({arizona_effect, [?EFFECT_OS, ~"window_maximize"]}, arizona_os:maximize()).

fullscreen(Config) when is_list(Config) ->
    ?assertEqual(
        {arizona_effect, [?EFFECT_OS, ~"window_fullscreen", true]},
        arizona_os:fullscreen(true)
    ).

notify(Config) when is_list(Config) ->
    ?assertEqual({arizona_effect, [?EFFECT_OS, ~"notify", ~"Done"]}, arizona_os:notify(~"Done")).

notify_with_opts(Config) when is_list(Config) ->
    ?assertEqual(
        {arizona_effect, [?EFFECT_OS, ~"notify", ~"Done", #{body => ~"All set"}]},
        arizona_os:notify(~"Done", #{body => ~"All set"})
    ).

capture_protection(Config) when is_list(Config) ->
    ?assertEqual(
        {arizona_effect, [?EFFECT_OS, ~"screen_capture_protection", true]},
        arizona_os:capture_protection(true)
    ).

command_encodes_to_json(Config) when is_list(Config) ->
    %% The neutral tuple serializes on the JSON wire like every other effect.
    Bin = arizona_effect:encode_json(arizona_os:set_title(~"T")),
    ?assert(is_binary(Bin)),
    ?assertEqual([?EFFECT_OS, ~"window_title", ~"T"], json:decode(Bin)).

%% --------------------------------------------------------------------
%% Read API -- capability/1 and capabilities/0 read the live process's
%% `$arizona_capabilities` dict (set at connect from `_az_caps`). Each CT case
%% runs in its own process, so the dict is isolated per test.
%% --------------------------------------------------------------------

capability_true(Config) when is_list(Config) ->
    erlang:put('$arizona_capabilities', #{~"window_focus" => true}),
    ?assert(arizona_live:capability(~"window_focus")).

capability_false_value(Config) when is_list(Config) ->
    %% Advertised but not enabled -> false.
    erlang:put('$arizona_capabilities', #{~"window_focus" => false}),
    ?assertNot(arizona_live:capability(~"window_focus")).

capability_absent_key(Config) when is_list(Config) ->
    erlang:put('$arizona_capabilities', #{~"window_focus" => true}),
    ?assertNot(arizona_live:capability(~"notify")).

capability_not_connected(Config) when is_list(Config) ->
    %% No dict (a plain browser / SSR) -> every capability is false.
    erlang:erase('$arizona_capabilities'),
    ?assertNot(arizona_live:capability(~"window_focus")).

capabilities_map(Config) when is_list(Config) ->
    Caps = #{~"window_focus" => true, ~"notify" => true},
    erlang:put('$arizona_capabilities', Caps),
    ?assertEqual(Caps, arizona_live:capabilities()).

capabilities_default_empty(Config) when is_list(Config) ->
    erlang:erase('$arizona_capabilities'),
    ?assertEqual(#{}, arizona_live:capabilities()).

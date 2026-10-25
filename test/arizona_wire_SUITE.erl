-module(arizona_wire_SUITE).
-moduledoc """
Cross-language wire op-code sync check.

The diff op codes in `include/arizona_wire.hrl` and the effect op codes in
`include/arizona_effect.hrl` are hand-copied into every client runtime
(`assets/js/arizona-ops.js` and `arizona-commands.js` and the worker, the reference `e2e/utils/native_client.js`,
and the Android/iOS `Wire.kt`/`Wire.swift`). Nothing else compares them, so a
renumber or addition in one artifact drifts silently -- a native client skips an
unknown op non-strictly, hiding the mismatch until a patch or event misfires in
production. This suite reads the Erlang headers as the source of truth and asserts
every client's declared codes still agree with them.
""".
-include_lib("stdlib/include/assert.hrl").

-export([all/0]).
-export([groups/0]).

-export([canonical_ops_extraction_sane/1]).
-export([web_js_ops_match_canonical/1]).
-export([web_worker_ops_match_canonical/1]).
-export([android_ops_match_native_backend/1]).
-export([ios_ops_match_native_backend/1]).
-export([native_client_ops_match_native_backend/1]).
-export([canonical_effects_extraction_sane/1]).
-export([web_js_effects_match_canonical/1]).
-export([android_effects_match_canonical/1]).
-export([ios_effects_match_canonical/1]).
-export([native_client_effects_match_canonical/1]).
-export([native_clients_effects_agree/1]).

all() ->
    [{group, ops}, {group, effects}].

groups() ->
    [
        {ops, [parallel], [
            canonical_ops_extraction_sane,
            web_js_ops_match_canonical,
            web_worker_ops_match_canonical,
            android_ops_match_native_backend,
            ios_ops_match_native_backend,
            native_client_ops_match_native_backend
        ]},
        {effects, [parallel], [
            canonical_effects_extraction_sane,
            web_js_effects_match_canonical,
            android_effects_match_canonical,
            ios_effects_match_canonical,
            native_client_effects_match_canonical,
            native_clients_effects_agree
        ]}
    ].

%% --------------------------------------------------------------------
%% Diff op code test cases
%% --------------------------------------------------------------------

%% Guard the extraction itself: a broken regex that captures nothing would make
%% every comparison pass trivially (empty == empty), so anchor on both ends of the
%% range and on the unassigned code 3.
canonical_ops_extraction_sane(Config) when is_list(Config) ->
    Canonical = canonical(ops),
    ?assertEqual(10, map_size(Canonical)),
    ?assertMatch(#{~"TEXT" := 0, ~"LIST_PATCH" := 10}, Canonical),
    ?assertNot(lists:member(3, maps:values(Canonical))).

%% The web client applies every op the html backend emits, and the html backend
%% emits all of them, so its table must equal the header in both directions.
web_js_ops_match_canonical(Config) when is_list(Config) ->
    ?assertEqual(canonical(ops), web_js(ops)).

%% The worker only names the ops it pre-resolves, so it need only agree on those.
web_worker_ops_match_canonical(Config) when is_list(Config) ->
    assert_agrees_with_canonical(ops, web_worker, web_worker()).

%% A native client must handle every op the native backend can emit -- exactly
%% the header minus what the backend opts out of -- so a new op the backend starts
%% emitting fails here until each native client implements it.
android_ops_match_native_backend(Config) when is_list(Config) ->
    ?assertEqual(native_backend_ops(), android_wire(ops)).

ios_ops_match_native_backend(Config) when is_list(Config) ->
    ?assertEqual(native_backend_ops(), ios_wire(ops)).

native_client_ops_match_native_backend(Config) when is_list(Config) ->
    ?assertEqual(native_backend_ops(), native_client(ops)).

%% --------------------------------------------------------------------
%% Effect op code test cases
%% --------------------------------------------------------------------

canonical_effects_extraction_sane(Config) when is_list(Config) ->
    Canonical = canonical(effects),
    ?assert(map_size(Canonical) >= 20),
    ?assertMatch(#{~"PUSH_EVENT" := 0, ~"NAVIGATE" := 10}, Canonical).

%% The web client is the full-fidelity runtime: it must declare exactly the same
%% name -> code map as the header (both directions -- an addition on either side
%% is caught).
web_js_effects_match_canonical(Config) when is_list(Config) ->
    ?assertEqual(canonical(effects), web_js(effects)).

%% Native clients implement only the portable subset (push_event/navigate), so
%% they need only agree on the codes they do declare -- but that set must be
%% non-empty and every entry must match the header.
android_effects_match_canonical(Config) when is_list(Config) ->
    assert_agrees_with_canonical(effects, android, android_wire(effects)).

ios_effects_match_canonical(Config) when is_list(Config) ->
    assert_agrees_with_canonical(effects, ios, ios_wire(effects)).

native_client_effects_match_canonical(Config) when is_list(Config) ->
    assert_agrees_with_canonical(effects, native_client, native_client(effects)).

%% The three native clients must declare the identical effect set: a portable
%% effect added to one but not the others is a drift the per-client checks above
%% (subset-only) would miss.
native_clients_effects_agree(Config) when is_list(Config) ->
    Android = android_wire(effects),
    ?assertEqual(Android, ios_wire(effects)),
    ?assertEqual(Android, native_client(effects)).

%% --------------------------------------------------------------------
%% Source extraction
%% --------------------------------------------------------------------

%% Canonical name -> code map from the Erlang header (the source of truth).
canonical(ops) ->
    extract(read(["include", "arizona_wire.hrl"]), ~"OP_([A-Z0-9_]+),\\s*([0-9]+)");
canonical(effects) ->
    extract(read(["include", "arizona_effect.hrl"]), ~"EFFECT_([A-Z0-9_]+),\\s*([0-9]+)").

%% The ops the native backend can emit: the header minus the capability-gated ones.
native_backend_ops() ->
    maps:without([~"LIST_PATCH" || not arizona_native:supports_list_patch()], canonical(ops)).

%% The web client's `const OP = { NAME: N, ... }` table, and its 30 `JS_NAME = N`
%% effect constants.
web_js(ops) ->
    Body = block(read(["assets", "js", "arizona-ops.js"]), ~"const OP = \\{(.*?)\\}"),
    extract(Body, ~"([A-Z0-9_]+):\\s*([0-9]+)");
web_js(effects) ->
    extract(read(["assets", "js", "arizona-commands.js"]), ~"JS_([A-Z0-9_]+)\\s*=\\s*([0-9]+)").

%% The `const OP_NAME = N` constants in the web client's frame worker.
web_worker() ->
    extract(read(["assets", "js", "arizona-worker.js"]), ~"OP_([A-Z0-9_]+)\\s*=\\s*([0-9]+)").

%% The `OP_NAME = N` / `EFFECT_NAME = N` constants in the reference native
%% (JSON-wire) client.
native_client(ops) ->
    extract(read(native_client_path()), ~"OP_([A-Z0-9_]+)\\s*=\\s*([0-9]+)");
native_client(effects) ->
    extract(read(native_client_path()), ~"EFFECT_([A-Z0-9_]+)\\s*=\\s*([0-9]+)").

native_client_path() ->
    ["e2e", "utils", "native_client.js"].

%% The `const val NAME = N` entries inside the Kotlin `object Op { ... }` /
%% `object Effect { ... }`.
android_wire(Kind) ->
    Body = block(read(android_wire_path()), [~"object ", block_name(Kind), ~" \\{(.*?)\\}"]),
    extract(Body, ~"([A-Z0-9_]+)\\s*=\\s*([0-9]+)").

android_wire_path() ->
    [
        "clients",
        "android",
        "arizona",
        "src",
        "main",
        "kotlin",
        "dev",
        "arizona",
        "client",
        "Wire.kt"
    ].

%% The `let name = N` entries inside the Swift `enum Op { ... }` /
%% `enum Effect { ... }`, with the camelCase names normalized to the header's
%% UPPER_SNAKE spelling.
ios_wire(Kind) ->
    Body = block(
        read(["clients", "ios", "Sources", "AzWire", "Wire.swift"]),
        [~"enum ", block_name(Kind), ~" \\{(.*?)\\}"]
    ),
    CamelCased = extract(Body, ~"let ([a-zA-Z0-9]+)\\s*=\\s*([0-9]+)"),
    #{to_upper_snake(Name) => Code || Name := Code <- CamelCased}.

block_name(ops) -> ~"Op";
block_name(effects) -> ~"Effect".

%% --------------------------------------------------------------------
%% Helpers
%% --------------------------------------------------------------------

assert_agrees_with_canonical(Kind, Client, Declared) ->
    Canonical = canonical(Kind),
    ?assertNotEqual(#{}, Declared),
    maps:foreach(
        fun(Name, Code) ->
            case Canonical of
                #{Name := CanonCode} ->
                    ?assertEqual({Client, Name, CanonCode}, {Client, Name, Code});
                _ ->
                    ct:fail({unknown_code_in_client, Kind, Client, Name, Code})
            end
        end,
        Declared
    ).

%% Build a `#{Name => Code}` map from a 2-group regex over the given text.
extract(Text, Pattern) ->
    case re:run(Text, Pattern, [global, {capture, all_but_first, binary}]) of
        {match, Rows} ->
            maps:from_list([{Name, binary_to_integer(Code)} || [Name, Code] <- Rows]);
        nomatch ->
            #{}
    end.

%% Capture the first `{ ... }` body following a block header (e.g. an `object
%% Effect`/`enum Op` declaration), so a sibling block is never picked up by the
%% constant extractor.
block(Text, Pattern) ->
    case re:run(Text, Pattern, [dotall, {capture, all_but_first, binary}]) of
        {match, [Body]} -> Body;
        nomatch -> ct:fail({block_not_found, iolist_to_binary(Pattern)})
    end.

%% camelCase -> UPPER_SNAKE: `pushEvent` -> `PUSH_EVENT`, `navigate` -> `NAVIGATE`.
to_upper_snake(Name) when is_binary(Name) ->
    list_to_binary(to_upper_snake(binary_to_list(Name), [])).

to_upper_snake([C | Rest], Acc) when C >= $A, C =< $Z, Acc =/= [] ->
    to_upper_snake(Rest, [C, $_ | Acc]);
to_upper_snake([C | Rest], Acc) when C >= $a, C =< $z ->
    to_upper_snake(Rest, [C - 32 | Acc]);
to_upper_snake([C | Rest], Acc) ->
    to_upper_snake(Rest, [C | Acc]);
to_upper_snake([], Acc) ->
    lists:reverse(Acc).

read(Parts) ->
    {ok, Bin} = file:read_file(filename:join([repo_root() | Parts])),
    Bin.

%% The client artifacts (assets/, clients/, e2e/) live at the repo root, which
%% rebar3 does not symlink into `_build`. Walk up from the app lib dir (four
%% levels: arizona -> lib -> <profile> -> _build -> root), falling back to the
%% test cwd, and pick whichever actually holds the sources.
repo_root() ->
    find_root([
        filename:join(code:lib_dir(arizona), "../../../.."),
        cwd()
    ]).

cwd() ->
    {ok, Cwd} = file:get_cwd(),
    Cwd.

find_root([Dir | Rest]) ->
    Web = filename:join([Dir, "assets", "js", "arizona.js"]),
    Header = filename:join([Dir, "include", "arizona_wire.hrl"]),
    case filelib:is_regular(Web) andalso filelib:is_regular(Header) of
        true -> Dir;
        false -> find_root(Rest)
    end;
find_root([]) ->
    ct:fail(repo_root_not_found).

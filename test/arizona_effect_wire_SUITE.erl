-module(arizona_effect_wire_SUITE).
-moduledoc """
Cross-language effect op-code sync check.

The effect op codes in `include/arizona_effect.hrl` are hand-copied into every
client runtime (`assets/js/arizona.js`, the reference `e2e/utils/native_client.js`,
and the Android/iOS `Wire.kt`/`Wire.swift`). Nothing else compares them, so a
renumber or addition in one artifact drifts silently -- a native client skips an
unknown op non-strictly, hiding the mismatch until an event misfires in
production. This suite reads the Erlang header as the source of truth and asserts
every client's declared codes still agree with it.
""".
-include_lib("stdlib/include/assert.hrl").

-export([all/0]).
-export([groups/0]).

-export([canonical_extraction_sane/1]).
-export([web_js_matches_canonical/1]).
-export([android_wire_matches_canonical/1]).
-export([ios_wire_matches_canonical/1]).
-export([native_client_matches_canonical/1]).
-export([native_clients_agree/1]).

all() ->
    [{group, wire_sync}].

groups() ->
    [
        {wire_sync, [parallel], [
            canonical_extraction_sane,
            web_js_matches_canonical,
            android_wire_matches_canonical,
            ios_wire_matches_canonical,
            native_client_matches_canonical,
            native_clients_agree
        ]}
    ].

%% --------------------------------------------------------------------
%% Test cases
%% --------------------------------------------------------------------

%% Guard the extraction itself: a broken regex that captures nothing would make
%% every "matches canonical" assertion pass trivially (empty == empty), so anchor
%% on the two portable effects and a plausible count.
canonical_extraction_sane(Config) when is_list(Config) ->
    Canonical = canonical(),
    ?assert(map_size(Canonical) >= 20),
    ?assertMatch(#{~"PUSH_EVENT" := 0}, Canonical),
    ?assertMatch(#{~"NAVIGATE" := 10}, Canonical).

%% The web client is the full-fidelity runtime: it must declare exactly the same
%% name -> code map as the header (both directions -- an addition on either side
%% is caught).
web_js_matches_canonical(Config) when is_list(Config) ->
    ?assertEqual(canonical(), web_js()).

%% Native clients implement only the portable subset (push_event/navigate), so
%% they need only agree on the codes they do declare -- but that set must be
%% non-empty and every entry must match the header.
android_wire_matches_canonical(Config) when is_list(Config) ->
    assert_agrees_with_canonical(android, android_wire()).

ios_wire_matches_canonical(Config) when is_list(Config) ->
    assert_agrees_with_canonical(ios, ios_wire()).

native_client_matches_canonical(Config) when is_list(Config) ->
    assert_agrees_with_canonical(native_client, native_client()).

%% The three native clients must declare the identical effect set: a portable
%% effect added to one but not the others is a drift the per-client checks above
%% (subset-only) would miss.
native_clients_agree(Config) when is_list(Config) ->
    Android = android_wire(),
    ?assertEqual(Android, ios_wire()),
    ?assertEqual(Android, native_client()).

%% --------------------------------------------------------------------
%% Source extraction
%% --------------------------------------------------------------------

%% Canonical name -> code map from the Erlang header (the source of truth).
canonical() ->
    extract(read(["include", "arizona_effect.hrl"]), ~"EFFECT_([A-Z0-9_]+),\\s*([0-9]+)").

%% All 30 `JS_NAME = N` effect constants in the web client.
web_js() ->
    extract(read(["assets", "js", "arizona.js"]), ~"JS_([A-Z0-9_]+)\\s*=\\s*([0-9]+)").

%% The `EFFECT_NAME = N` constants in the reference native (JSON-wire) client.
native_client() ->
    extract(read(["e2e", "utils", "native_client.js"]), ~"EFFECT_([A-Z0-9_]+)\\s*=\\s*([0-9]+)").

%% The `const val NAME = N` entries inside the Kotlin `object Effect { ... }`.
android_wire() ->
    Body = block(
        read([
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
        ]),
        ~"object Effect \\{(.*?)\\}"
    ),
    extract(Body, ~"([A-Z0-9_]+)\\s*=\\s*([0-9]+)").

%% The `let name = N` entries inside the Swift `enum Effect { ... }`, with the
%% camelCase names normalized to the header's UPPER_SNAKE spelling.
ios_wire() ->
    Body = block(
        read(["clients", "ios", "Sources", "AzWire", "Wire.swift"]),
        ~"enum Effect \\{(.*?)\\}"
    ),
    CamelCased = extract(Body, ~"let ([a-zA-Z0-9]+)\\s*=\\s*([0-9]+)"),
    maps:fold(
        fun(Name, Code, Acc) -> Acc#{to_upper_snake(Name) => Code} end, #{}, CamelCased
    ).

%% --------------------------------------------------------------------
%% Helpers
%% --------------------------------------------------------------------

assert_agrees_with_canonical(Client, Declared) ->
    Canonical = canonical(),
    ?assertNotEqual(#{}, Declared),
    maps:foreach(
        fun(Name, Code) ->
            case Canonical of
                #{Name := CanonCode} ->
                    ?assertEqual({Client, Name, CanonCode}, {Client, Name, Code});
                _ ->
                    ct:fail({unknown_effect_in_client, Client, Name, Code})
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
%% Effect`/`enum Effect` declaration), so a sibling diff-op block is never picked
%% up by the constant extractor.
block(Text, Pattern) ->
    case re:run(Text, Pattern, [dotall, {capture, all_but_first, binary}]) of
        {match, [Body]} -> Body;
        nomatch -> ct:fail({block_not_found, Pattern})
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
    Header = filename:join([Dir, "include", "arizona_effect.hrl"]),
    case filelib:is_regular(Web) andalso filelib:is_regular(Header) of
        true -> Dir;
        false -> find_root(Rest)
    end;
find_root([]) ->
    ct:fail(repo_root_not_found).

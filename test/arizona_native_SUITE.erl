-module(arizona_native_SUITE).
-include_lib("stdlib/include/assert.hrl").

-export([all/0]).
-export([static_single_element/1]).
-export([static_nested_elements/1]).
-export([static_empty_children/1]).
-export([void_shorthand_has_empty_children/1]).
-export([dynamic_text_uses_text_node/1]).

all() ->
    [
        static_single_element,
        static_nested_elements,
        static_empty_children,
        void_shorthand_has_empty_children,
        dynamic_text_uses_text_node
    ].

%% --------------------------------------------------------------------
%% Test cases
%% --------------------------------------------------------------------

static_single_element(Config) when is_list(Config) ->
    Mod = compile_module(
        "-module(nt_single). "
        "-export([render/1]). "
        "render(Bindings) -> "
        "    az:native({'Text', [{style, <<\"title\">>}], [<<\"Hello\">>]}). "
    ),
    T = Mod:render(#{}),
    %% Fully static: one static fragment, no dynamics.
    ?assertEqual([], maps:get(d, T)),
    ?assertEqual(
        #{
            ~"type" => ~"Text",
            ~"style" => ~"title",
            ~"children" => [~"Hello"]
        },
        decode_static(T)
    ).

static_nested_elements(Config) when is_list(Config) ->
    Mod = compile_module(
        "-module(nt_nested). "
        "-export([render/1]). "
        "render(Bindings) -> "
        "    az:native("
        "        {'Column', [{padding, <<\"16\">>}], ["
        "            {'Text', [], [<<\"a\">>]},"
        "            {'Text', [], [<<\"b\">>]}"
        "        ]}"
        "    ). "
    ),
    T = Mod:render(#{}),
    ?assertEqual(
        #{
            ~"type" => ~"Column",
            ~"padding" => ~"16",
            ~"children" => [
                #{~"type" => ~"Text", ~"children" => [~"a"]},
                #{~"type" => ~"Text", ~"children" => [~"b"]}
            ]
        },
        decode_static(T)
    ).

static_empty_children(Config) when is_list(Config) ->
    Mod = compile_module(
        "-module(nt_empty). "
        "-export([render/1]). "
        "render(Bindings) -> "
        "    az:native({'Spacer', [], []}). "
    ),
    T = Mod:render(#{}),
    ?assertEqual(#{~"type" => ~"Spacer", ~"children" => []}, decode_static(T)).

void_shorthand_has_empty_children(Config) when is_list(Config) ->
    %% Native has no void elements: the {Tag, Attrs} shorthand yields an empty
    %% children array, not a self-closed node.
    Mod = compile_module(
        "-module(nt_void). "
        "-export([render/1]). "
        "render(Bindings) -> "
        "    az:native({'Image', [{src, <<\"x.png\">>}]}). "
    ),
    T = Mod:render(#{}),
    ?assertEqual(
        #{~"type" => ~"Image", ~"src" => ~"x.png", ~"children" => []},
        decode_static(T)
    ).

dynamic_text_uses_text_node(Config) when is_list(Config) ->
    %% A dynamic text child compiles to an addressable `#text` node so OP_TEXT
    %% can target it by `az`. The dynamic value, JSON-encoded at render time,
    %% slots into "value".
    Mod = compile_module(
        "-module(nt_dyn). "
        "-export([render/1]). "
        "render(Bindings) -> "
        "    az:native({'Text', [], [az:get(name, Bindings, <<\"world\">>)]}). "
    ),
    T = Mod:render(#{name => ~"Alice"}),
    {_Html, Snap} = arizona_render:render(T),
    %% The wire payload is the structured, cacheable fingerprint form -- the
    %% same `#{f, s, d}` shape arizona_live's (format-agnostic) dedup strips
    %% statics from once the client has the fingerprint. Statics are JSON
    %% fragments; `d` carries the raw value for the client to encode.
    Payload = arizona_render:fingerprint_payload(Snap),
    ?assert(is_binary(maps:get(~"f", Payload))),
    ?assert(is_list(maps:get(~"s", Payload))),
    ?assertEqual([~"Alice"], maps:get(~"d", Payload)),
    %% A simulated client interleave (JSON-encode each value, mirroring the
    %% browser worker's zipTemplate) must yield valid JSON.
    #{~"type" := ~"Text", ~"az" := ElemAz, ~"children" := [Child]} =
        simulate_interleave(Payload),
    ?assertMatch(#{~"type" := ~"#text", ~"value" := ~"Alice"}, Child),
    %% The #text node must carry an `az` distinct from its parent element's.
    ?assertNotEqual(ElemAz, maps:get(~"az", Child)).

%% --------------------------------------------------------------------
%% Helpers
%% --------------------------------------------------------------------

%% Decode a fully-static native template (single static fragment, no dynamics).
decode_static(#{s := [Static], d := []}) ->
    json:decode(Static).

%% Mirror the browser worker's interleave for a fingerprint payload: stitch the
%% JSON-fragment statics back together with the JSON-encoded dynamic values,
%% then decode. This documents the native client contract and validates that
%% the server output forms valid JSON.
simulate_interleave(#{~"s" := Statics, ~"d" := Dynamics}) ->
    json:decode(iolist_to_binary(interleave(Statics, Dynamics))).

interleave([S], []) ->
    [S];
interleave([S | Statics], [V | Dynamics]) ->
    [S, encode_value(V) | interleave(Statics, Dynamics)].

encode_value(V) when is_binary(V); is_number(V); is_boolean(V) ->
    json:encode(V);
encode_value(#{~"s" := _} = Nested) ->
    iolist_to_binary(interleave(maps:get(~"s", Nested), maps:get(~"d", Nested))).

compile_module(Source) ->
    {ok, Tokens, _} = erl_scan:string(Source),
    Forms = split_forms(Tokens),
    ParsedForms = [
        begin
            {ok, F} = erl_parse:parse_form(Toks),
            F
        end
     || Toks <- Forms
    ],
    TransformedForms = arizona_parse_transform:parse_transform(ParsedForms, []),
    {ok, Mod, Bin} = compile:forms(TransformedForms, [return_errors]),
    {module, Mod} = code:load_binary(Mod, "", Bin),
    Mod.

split_forms(Tokens) ->
    split_forms(Tokens, [], []).

split_forms([], [], Acc) ->
    lists:reverse(Acc);
split_forms([], Current, Acc) ->
    lists:reverse([lists:reverse(Current) | Acc]);
split_forms([{dot, _} = Dot | Rest], Current, Acc) ->
    split_forms(Rest, [], [lists:reverse([Dot | Current]) | Acc]);
split_forms([Token | Rest], Current, Acc) ->
    split_forms(Rest, [Token | Current], Acc).

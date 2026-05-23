-module(arizona_native_SUITE).
-include_lib("stdlib/include/assert.hrl").

-export([all/0]).
-export([static_single_element/1]).
-export([static_nested_elements/1]).
-export([static_empty_children/1]).
-export([void_shorthand_has_empty_children/1]).
-export([dynamic_text_uses_text_node/1]).
-export([dynamic_attr_inlines_as_prop/1]).
-export([live_view_caches_statics/1]).
-export([native_each_renders_item_array/1]).
-export([native_each_empty_is_valid_json/1]).

all() ->
    [
        static_single_element,
        static_nested_elements,
        static_empty_children,
        void_shorthand_has_empty_children,
        dynamic_text_uses_text_node,
        dynamic_attr_inlines_as_prop,
        live_view_caches_statics,
        native_each_renders_item_array,
        native_each_empty_is_valid_json
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
    %% A dynamic child compiles to an addressable, transparent `#slot` whose
    %% children hold the rendered value, so OP_TEXT can target it by `az`.
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
    #{~"type" := ~"Text", ~"az" := ElemAz, ~"children" := [Slot]} =
        simulate_interleave(Payload),
    ?assertMatch(#{~"type" := ~"#slot", ~"children" := [~"Alice"]}, Slot),
    %% The slot's `az` must be distinct from its parent element's.
    ?assertNotEqual(ElemAz, maps:get(~"az", Slot)).

dynamic_attr_inlines_as_prop(Config) when is_list(Config) ->
    %% A dynamic prop: the name is baked into the static, the value rides in
    %% `d` as a bare value the client JSON-encodes (uniform with text values).
    %% Diff updates still use OP_SET_ATTR (name + value separate), unchanged.
    Mod = compile_module(
        "-module(nt_dynattr). "
        "-export([render/1]). "
        "render(Bindings) -> "
        "    az:native({'Button', [{color, az:get(color, Bindings, <<\"gray\">>)}], [<<\"OK\">>]}). "
    ),
    T = Mod:render(#{color => ~"red"}),
    {_Html, Snap} = arizona_render:render(T),
    Payload = arizona_render:fingerprint_payload(Snap),
    ?assertEqual([~"red"], maps:get(~"d", Payload)),
    ?assertMatch(
        #{~"type" := ~"Button", ~"color" := ~"red", ~"children" := [~"OK"]},
        simulate_interleave(Payload)
    ).

live_view_caches_statics(Config) when is_list(Config) ->
    %% Wire-efficiency proof: a native live view's first mount ships the full
    %% widget tree (statics present); a fresh instance whose client already
    %% cached the fingerprint gets the statics stripped (`#{f,d}` only). Native
    %% reuses the exact same fingerprint cache as the browser -- statics are
    %% sent once, never re-sent.
    Req = arizona_req_test_adapter:new(),
    {ok, Pid1} = arizona_live:start_link(arizona_native_counter, #{}, undefined, [], Req),
    {ok, ViewId, Frame1} = arizona_live:mount_and_render(Pid1),
    ?assertEqual(~"native_counter", ViewId),
    ?assert(maps:is_key(~"s", Frame1)),
    ?assertMatch(#{~"type" := ~"Column", ~"id" := ~"native_counter"}, simulate_interleave(Frame1)),
    Fp = maps:get(~"f", Frame1),
    %% Fresh instance, fingerprint pre-seeded (as the client would report).
    {ok, Pid2} = arizona_live:start_link(arizona_native_counter, #{}, undefined, [], Req),
    arizona_live:seed_fps(Pid2, [Fp]),
    sys:get_state(Pid2),
    {ok, _ViewId, Frame2} = arizona_live:mount_and_render(Pid2),
    ?assertEqual(Fp, maps:get(~"f", Frame2)),
    ?assertNot(maps:is_key(~"s", Frame2)).

native_each_renders_item_array(Config) when is_list(Config) ->
    %% A plain ?each inside ?native is rewritten to compile the per-item fragment
    %% with the native backend; the each payload reuses the #{t,f,s,d} fingerprint
    %% form, and the client splices the items into the parent's children.
    Mod = compile_module(
        "-module(nt_each). "
        "-export([render/1]). "
        "render(Bindings) -> "
        "    az:native({'Column', [], ["
        "        az:each(fun(I) -> {'Text', [], [I]} end, az:get(items, Bindings, []))"
        "    ]}). "
    ),
    T = Mod:render(#{items => [~"a", ~"b"]}),
    {_Html, Snap} = arizona_render:render(T),
    #{~"type" := ~"Column", ~"children" := Items} =
        flatten(simulate_interleave(arizona_render:fingerprint_payload(Snap))),
    ?assertMatch(
        [
            #{~"type" := ~"Text", ~"children" := [~"a"]},
            #{~"type" := ~"Text", ~"children" := [~"b"]}
        ],
        Items
    ).

native_each_empty_is_valid_json(Config) when is_list(Config) ->
    %% An empty each renders to `[]` -- valid JSON, no dangling comma even with a
    %% preceding sibling.
    Mod = compile_module(
        "-module(nt_each_empty). "
        "-export([render/1]). "
        "render(Bindings) -> "
        "    az:native({'Column', [], ["
        "        {'Text', [], [<<\"Header\">>]},"
        "        az:each(fun(I) -> {'Text', [], [I]} end, az:get(items, Bindings, []))"
        "    ]}). "
    ),
    T = Mod:render(#{items => []}),
    {_Html, Snap} = arizona_render:render(T),
    #{~"type" := ~"Column", ~"children" := Children} =
        flatten(simulate_interleave(arizona_render:fingerprint_payload(Snap))),
    ?assertMatch([#{~"type" := ~"Text", ~"children" := [~"Header"]}], Children).

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

encode_value(#{~"t" := _, ~"s" := S, ~"d" := ItemsList}) ->
    %% Native each -> a JSON array of item objects the client splices (flattens)
    %% into the parent's children. Valid JSON even when empty (`[]`), so the
    %% parent's compile-time child commas never dangle.
    Items = [iolist_to_binary(interleave(S, ItemD)) || ItemD <- ItemsList],
    iolist_to_binary([$[, lists:join($,, Items), $]]);
encode_value(V) when is_binary(V); is_number(V); is_boolean(V) ->
    json:encode(V);
encode_value(#{~"s" := _} = Nested) ->
    iolist_to_binary(interleave(maps:get(~"s", Nested), maps:get(~"d", Nested))).

%% Mirror the client's fragment handling: a `#slot` is transparent (its children
%% splice into the parent) and a nested array (an each expansion) flattens one
%% level. Produces the rendered widget tree for assertions.
flatten(#{~"children" := Children} = Node) ->
    Node#{~"children" => lists:flatmap(fun flatten_child/1, Children)};
flatten(Other) ->
    Other.

flatten_child(#{~"type" := ~"#slot", ~"children" := SlotChildren}) ->
    lists:flatmap(fun flatten_child/1, SlotChildren);
flatten_child(List) when is_list(List) ->
    lists:flatmap(fun flatten_child/1, List);
flatten_child(#{~"children" := _} = Node) ->
    [flatten(Node)];
flatten_child(Other) ->
    [Other].

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

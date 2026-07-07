-module(arizona_native_SUITE).
-include_lib("stdlib/include/assert.hrl").
-include("arizona.hrl").

-export([all/0]).
-export([static_single_element/1]).
-export([static_nested_elements/1]).
-export([static_empty_children/1]).
-export([void_shorthand_has_empty_children/1]).
-export([dynamic_text_uses_text_node/1]).
-export([dynamic_attr_inlines_as_prop/1]).
-export([event_prop_is_raw_json_command/1]).
-export([event_prop_navigate_command/1]).
-export([live_view_caches_statics/1]).
-export([native_each_renders_item_array/1]).
-export([native_each_empty_is_valid_json/1]).
-export([nested_native_stateless_component/1]).
-export([repeated_native_stateless_distinct/1]).
-export([nested_native_stateful_component/1]).
-export([nested_native_stateful_child_event_routes/1]).
-export([diff_dynamic_text_op/1]).
-export([diff_dynamic_attr_op/1]).
-export([diff_conditional_subtree_op/1]).
-export([diff_independent_regions_op/1]).
-export([diff_stream_insert_op/1]).
-export([diff_stream_remove_op/1]).
-export([diff_stream_move_op/1]).
-export([diff_remove_node_op/1]).

all() ->
    [
        static_single_element,
        static_nested_elements,
        static_empty_children,
        void_shorthand_has_empty_children,
        dynamic_text_uses_text_node,
        dynamic_attr_inlines_as_prop,
        event_prop_is_raw_json_command,
        event_prop_navigate_command,
        live_view_caches_statics,
        native_each_renders_item_array,
        native_each_empty_is_valid_json,
        nested_native_stateless_component,
        repeated_native_stateless_distinct,
        nested_native_stateful_component,
        nested_native_stateful_child_event_routes,
        diff_dynamic_text_op,
        diff_dynamic_attr_op,
        diff_conditional_subtree_op,
        diff_independent_regions_op,
        diff_stream_insert_op,
        diff_stream_remove_op,
        diff_stream_move_op,
        diff_remove_node_op
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
        "    az:native({'Button', [{color, az:get(color, Bindings, "
        "<<\"gray\">>)}], [<<\"OK\">>]}). "
    ),
    T = Mod:render(#{color => ~"red"}),
    {_Html, Snap} = arizona_render:render(T),
    Payload = arizona_render:fingerprint_payload(Snap),
    ?assertEqual([~"red"], maps:get(~"d", Payload)),
    ?assertMatch(
        #{~"type" := ~"Button", ~"color" := ~"red", ~"children" := [~"OK"]},
        simulate_interleave(Payload)
    ).

event_prop_is_raw_json_command(Config) when is_list(Config) ->
    %% A folded effect command prop (on_tap), built with the native platform
    %% module, is emitted as a raw JSON array the client interprets directly --
    %% NOT an HTML-escaped string. This is what makes a native widget tappable.
    Mod = compile_module(
        "-module(nt_event). "
        "-export([render/1]). "
        "render(Bindings) -> "
        "    az:native({'Button', [{on_tap, arizona_android:push_event(<<\"inc\">>)}], "
        "[<<\"OK\">>]}). "
    ),
    Decoded = decode_static(Mod:render(#{})),
    %% [0, "inc"] = [EFFECT_PUSH_EVENT, EventName], a JSON array (not a quoted string).
    ?assertEqual([0, ~"inc"], maps:get(~"on_tap", Decoded)).

event_prop_navigate_command(Config) when is_list(Config) ->
    %% A folded navigate command prop encodes as the raw array [10, Path]
    %% (EFFECT_NAVIGATE), so a native client can transition to another view on tap.
    Mod = compile_module(
        "-module(nt_nav). "
        "-export([render/1]). "
        "render(Bindings) -> "
        "    az:native({'Button', [{on_tap, arizona_android:navigate(<<\"/native/counter\">>)}], "
        "[<<\"Go\">>]}). "
    ),
    Decoded = decode_static(Mod:render(#{})),
    ?assertEqual([10, ~"/native/counter"], maps:get(~"on_tap", Decoded)).

live_view_caches_statics(Config) when is_list(Config) ->
    %% Wire-efficiency proof: a native live view's first mount ships the full
    %% widget tree (statics present); a fresh instance whose client already
    %% cached the fingerprint gets the statics stripped (`#{f,d}` only). Native
    %% reuses the exact same fingerprint cache as the browser -- statics are
    %% sent once, never re-sent.
    {ok, Pid1} = arizona_live:start_link(arizona_native_counter, #{}, undefined, []),
    {ok, ViewId, Frame1} = arizona_live:mount_and_render(Pid1),
    ?assertEqual(~"native_counter", ViewId),
    ?assert(maps:is_key(~"s", Frame1)),
    ?assertMatch(#{~"type" := ~"Column", ~"id" := ~"native_counter"}, simulate_interleave(Frame1)),
    Fp = maps:get(~"f", Frame1),
    %% Fresh instance, fingerprint pre-seeded (as the client would report).
    {ok, Pid2} = arizona_live:start_link(arizona_native_counter, #{}, undefined, []),
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

nested_native_stateless_component(Config) when is_list(Config) ->
    %% A native view embeds a native stateless child. The child renders to its
    %% own JSON tree (carrying target=native), which the parent splices into the
    %% #slot -- no special handling beyond the existing nested-template path.
    Mod = compile_module(
        "-module(nt_nested_comp). "
        "-export([render/1, child/1]). "
        "render(Bindings) -> "
        "    az:native({'Column', [], [az:stateless(fun child/1, #{label => <<\"Hi\">>})]}). "
        "child(Bindings) -> "
        "    az:native({'Text', [], [az:get(label, Bindings, <<\"\">>)]}). "
    ),
    T = Mod:render(#{}),
    {_Html, Snap, _Views} = arizona_render:render(T, #{}),
    #{~"type" := ~"Column", ~"children" := Children} =
        flatten(simulate_interleave(arizona_render:fingerprint_payload(Snap))),
    ?assertMatch([#{~"type" := ~"Text", ~"children" := [~"Hi"]}], Children).

repeated_native_stateless_distinct(Config) when is_list(Config) ->
    %% The same native stateless rendered twice: each instance is namespaced by
    %% its slot, so the two children carry DISTINCT scoped fingerprints (and az)
    %% -- pre-fix they were byte-identical and diff ops would collide, same as the
    %% HTML target. Confirms the scoping is target-aware (arizona_native backend).
    Mod = compile_module(
        "-module(nt_repeated_comp). "
        "-export([render/1, chip/1]). "
        "render(Bindings) -> "
        "    az:native({'Column', [], ["
        "        az:stateless(fun chip/1, #{label => <<\"A\">>}), "
        "        az:stateless(fun chip/1, #{label => <<\"B\">>})]}). "
        "chip(Props) -> "
        "    az:native({'Text', [], [maps:get(label, Props)]}). "
    ),
    T = Mod:render(#{}),
    {_Html, Snap, _Views} = arizona_render:render(T, #{}),
    #{~"d" := [ChildA, ChildB]} = arizona_render:fingerprint_payload(Snap),
    ?assertNotEqual(maps:get(~"f", ChildA), maps:get(~"f", ChildB)).

nested_native_stateful_component(Config) when is_list(Config) ->
    %% A native view embeds a native *stateful* child. Through the live frame
    %% path (mount_and_render), the child's own #{f,s,d} payload (carrying its
    %% target=native statics and az_view marker) is inlined into the parent
    %% frame and interleaves to a valid JSON widget tree. (arizona_render:render/3
    %% instead decomposes the child into a separate diff-time view, so the live
    %% path is the one that proves the first-frame inlining.)
    {ok, Pid} = arizona_live:start_link(arizona_native_parent, #{}, undefined, []),
    {ok, ~"native_parent", Frame} = arizona_live:mount_and_render(Pid),
    #{~"type" := ~"Column", ~"children" := Children} = flatten(simulate_interleave(Frame)),
    ?assertMatch([#{~"type" := ~"Badge", ~"children" := [~"5"]}], Children).

nested_native_stateful_child_event_routes(Config) when is_list(Config) ->
    %% A native view embeds two stateful child counters. Each is registered in the
    %% live process's views map by its own id (target-agnostic, like HTML), so an
    %% event addressed to a child's id routes to THAT child's handle_event -- the
    %% parent view has none, so a misroute to the root would crash. Only the
    %% addressed child's count diffs (one OP_TEXT). The `az_view`+`id` marker the
    %% client targets is covered end-to-end by e2e/native/nested.spec.js.
    {ok, Pid} = arizona_live:start_link(arizona_native_nested, #{}, undefined, []),
    {ok, ~"native_nested", _Frame} = arizona_live:mount_and_render(Pid),
    {ok, OpsA, _} = arizona_live:handle_event(Pid, ~"child_a", ~"inc", #{}),
    ?assertMatch([[?OP_TEXT, _Az, ~"1"]], OpsA),
    {ok, OpsB, _} = arizona_live:handle_event(Pid, ~"child_b", ~"inc", #{}),
    ?assertMatch([[?OP_TEXT, _Az, ~"1"]], OpsB).

diff_dynamic_text_op(Config) when is_list(Config) ->
    %% A native dynamic text node diffs to OP_TEXT carrying the raw new value
    %% (target-neutral; the client JSON-encodes it). The op targets the
    %% text-node `az`, distinct from the element `az`.
    Mod = compile_module(
        "-module(nt_diff_text). "
        "-export([render/1]). "
        "render(Bindings) -> "
        "    az:native({'Text', [], [az:get(label, Bindings, <<\"\">>)]}). "
    ),
    {_, Snap0} = arizona_render:render(Mod:render(#{label => ~"a"})),
    {Ops, _} = arizona_diff:diff(Mod:render(#{label => ~"b"}), Snap0),
    ?assertMatch([[?OP_TEXT, _Az, ~"b"]], Ops).

diff_dynamic_attr_op(Config) when is_list(Config) ->
    %% A native dynamic prop diffs to OP_SET_ATTR with name and value separate
    %% (format-neutral) -- the client sets the prop on the node.
    Mod = compile_module(
        "-module(nt_diff_attr). "
        "-export([render/1]). "
        "render(Bindings) -> "
        "    az:native({'Button', [{color, az:get(color, Bindings, "
        "<<\"gray\">>)}], [<<\"OK\">>]}). "
    ),
    {_, Snap0} = arizona_render:render(Mod:render(#{color => ~"red"})),
    {Ops, _} = arizona_diff:diff(Mod:render(#{color => ~"blue"}), Snap0),
    ?assertMatch([[?OP_SET_ATTR, _Az, ~"color", ~"blue"]], Ops).

diff_conditional_subtree_op(Config) when is_list(Config) ->
    %% A native dynamic that is a nested template (a conditional/tab subtree)
    %% diffs to OP_TEXT whose value is the re-rendered subtree payload (a
    %% {s, d} map), not a scalar -- the client decodes it like the first render.
    Mod = compile_module(
        "-module(nt_diff_cond). "
        "-export([render/1, content/1]). "
        "render(Bindings) -> "
        "    az:native({'Column', [], [az:stateless(fun content/1, "
        "#{sel => az:get(sel, Bindings)})]}). "
        "content(#{sel := <<\"about\">>}) -> "
        "    az:native({'Column', [], [{'Text', [], [<<\"About\">>]}]}); "
        "content(#{sel := _}) -> az:native({'Text', [], [<<\"Home\">>]}). "
    ),
    {_, Snap0} = arizona_render:render(Mod:render(#{sel => ~"home"})),
    {Ops, _} = arizona_diff:diff(Mod:render(#{sel => ~"about"}), Snap0),
    ?assertMatch([[?OP_TEXT, _Az, #{<<"s">> := _, <<"d">> := _}]], Ops).

diff_independent_regions_op(Config) when is_list(Config) ->
    %% Three independent native text regions: changing only the middle one diffs
    %% to a single OP_TEXT targeting its own az -- the others are untouched
    %% (distinct az slots, the multi-counter guarantee).
    Mod = compile_module(
        "-module(nt_diff_multi). "
        "-export([render/1]). "
        "render(Bindings) -> "
        "    az:native({'Column', [], ["
        "        {'Text', [], [az:get(a, Bindings)]}, "
        "        {'Text', [], [az:get(b, Bindings)]}, "
        "        {'Text', [], [az:get(c, Bindings)]}]}). "
    ),
    Old = #{a => ~"0", b => ~"0", c => ~"0"},
    {_, Snap0} = arizona_render:render(Mod:render(Old)),
    {Ops, _} = arizona_diff:diff(Mod:render(Old#{b => ~"1"}), Snap0),
    ?assertMatch([[?OP_TEXT, _Az, ~"1"]], Ops).

diff_stream_insert_op(Config) when is_list(Config) ->
    %% A native stream insert diffs to OP_INSERT whose item payload carries
    %% native JSON statics that interleave to a valid JSON widget.
    {B0, _} = arizona_native_list:mount(#{}),
    {_, Snap0, V0} = arizona_render:render(arizona_native_list:render(B0), #{}),
    B1 = arizona_stream:clear_stream_pending(B0, arizona_stream:stream_keys(B0)),
    {B2, _, _} = arizona_native_list:handle_event(~"add", #{~"id" => 1, ~"text" => ~"First"}, B1),
    Changed = compute_changed(B1, B2),
    {Ops, _, _} = arizona_diff:diff(arizona_native_list:render(B2), Snap0, V0, Changed),
    ?assertMatch([[?OP_INSERT, _Az, ~"1", -1, _Payload]], Ops),
    [[_, _, _, _, Payload]] = Ops,
    ?assertMatch(
        #{~"type" := ~"Text", ~"children" := [~"First"]},
        flatten(simulate_interleave(Payload))
    ).

diff_stream_remove_op(Config) when is_list(Config) ->
    Items = [#{id => 1, text => ~"A"}, #{id => 2, text => ~"B"}],
    {B0, _} = arizona_native_list:mount(#{items => Items}),
    {_, Snap0, V0} = arizona_render:render(arizona_native_list:render(B0), #{}),
    B1 = arizona_stream:clear_stream_pending(B0, arizona_stream:stream_keys(B0)),
    {B2, _, _} = arizona_native_list:handle_event(~"remove", #{~"id" => 1}, B1),
    Changed = compute_changed(B1, B2),
    {Ops, _, _} = arizona_diff:diff(arizona_native_list:render(B2), Snap0, V0, Changed),
    ?assertMatch([[?OP_REMOVE, _Az, ~"1"]], Ops).

diff_stream_move_op(Config) when is_list(Config) ->
    Items = [#{id => 1, text => ~"A"}, #{id => 2, text => ~"B"}],
    {B0, _} = arizona_native_list:mount(#{items => Items}),
    {_, Snap0, V0} = arizona_render:render(arizona_native_list:render(B0), #{}),
    B1 = arizona_stream:clear_stream_pending(B0, arizona_stream:stream_keys(B0)),
    {B2, _, _} = arizona_native_list:handle_event(~"move", #{~"id" => 2, ~"pos" => 0}, B1),
    Changed = compute_changed(B1, B2),
    {Ops, _, _} = arizona_diff:diff(arizona_native_list:render(B2), Snap0, V0, Changed),
    ?assertMatch([[?OP_MOVE, _Az, ~"2", _Ref]], Ops).

diff_remove_node_op(Config) when is_list(Config) ->
    %% A native dynamic returning the `remove` sentinel (element present, then
    %% gone) diffs to OP_REMOVE_NODE targeting that element's az -- the same
    %% target-agnostic path HTML uses (arizona_diff:make_op/3). Proves native emits
    %% op 4, which the native clients must handle (remove the node) rather than crash.
    Mod = compile_module(
        "-module(nt_remove). "
        "-export([render/1]). "
        "render(Bindings) -> "
        "    az:native({'Text', [], [az:get(label, Bindings, <<\"x\">>)]}). "
    ),
    {_, Snap0} = arizona_render:render(Mod:render(#{label => ~"Banner"})),
    {Ops, _} = arizona_diff:diff(Mod:render(#{label => remove}), Snap0),
    ?assertMatch([[?OP_REMOVE_NODE, _Az]], Ops).

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

%% Mirrors arizona_live:compute_changed/2: the bindings whose value changed.
compute_changed(OldBindings, NewBindings) ->
    maps:filter(
        fun(K, V) ->
            case OldBindings of
                #{K := V} -> false;
                #{} -> true
            end
        end,
        NewBindings
    ).

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

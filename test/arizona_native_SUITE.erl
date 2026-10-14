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
-export([dynamic_event_prop_is_json_array/1]).
-export([diff_dynamic_event_prop_op/1]).
-export([dedup_keeps_command_prop_payload/1]).
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
-export([scope_keeps_command_payload_az/1]).
-export([zipped_document_matches_client/1]).
-export([ssr_view_matches_mount_frame/1]).

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
        dynamic_event_prop_is_json_array,
        diff_dynamic_event_prop_op,
        dedup_keeps_command_prop_payload,
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
        diff_remove_node_op,
        scope_keeps_command_payload_az,
        zipped_document_matches_client,
        ssr_view_matches_mount_frame
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

%% A DYNAMIC command prop reaches the client as the same array a folded one is:
%% the client's tap reads an array, and a string (HTML-escaped at that) made the
%% widget dead. Holds in the hydration payload and in the zipped document.
dynamic_event_prop_is_json_array(Config) when is_list(Config) ->
    Mod = compile_module(
        "-module(nt_dyn_event). "
        "-export([render/1]). "
        "render(Bindings) -> "
        "    az:native({'Column', [], ["
        "        {'Button', [{on_tap, arizona_android:push_event(az:get(ev, Bindings))}], "
        "[<<\"A\">>]}, "
        "        {'Button', [{on_tap, az:get(cmd, Bindings)}], [<<\"B\">>]}"
        "    ]}). "
    ),
    %% One command per prop: the native clients run a single command, not a list.
    Cmd = arizona_android:navigate(~"/n"),
    T = Mod:render(#{ev => ~"a<b\"c", cmd => Cmd}),
    {Live, Snap} = arizona_render:render(T),
    #{~"children" := [A, B]} = simulate_interleave(arizona_render:fingerprint_payload(Snap)),
    ?assertEqual([0, ~"a<b\"c"], maps:get(~"on_tap", A)),
    ?assertEqual([10, ~"/n"], maps:get(~"on_tap", B)),
    ?assertMatch(
        #{~"children" := [#{~"on_tap" := [0, ~"a<b\"c"]}, #{~"on_tap" := [10, ~"/n"]}]},
        json:decode(iolist_to_binary(Live))
    ).

%% A dynamic command prop that changes patches with its array too.
diff_dynamic_event_prop_op(Config) when is_list(Config) ->
    Mod = compile_module(
        "-module(nt_diff_event). "
        "-export([render/1]). "
        "render(Bindings) -> "
        "    az:native({'Button', [{on_tap, arizona_android:push_event(az:get(ev, Bindings))}], "
        "[<<\"OK\">>]}). "
    ),
    {_, Snap0} = arizona_render:render(Mod:render(#{ev => ~"a"})),
    {Ops, _} = arizona_diff:diff(Mod:render(#{ev => ~"b&"}), Snap0),
    ?assertMatch([[?OP_SET_ATTR, _Az, ~"on_tap", [0, ~"b&"]]], Ops).

%% A command prop's payload map is app data, even when it carries `f`/`s`/`d`
%% keys: the fingerprint dedup leaves it whole, in the hydration payload and in a
%% diff frame that also ships a real fingerprint payload.
dedup_keeps_command_prop_payload(Config) when is_list(Config) ->
    Mod = compile_module(
        "-module(nt_dedup_cmd). "
        "-export([render/1]). "
        "render(Bindings) -> "
        "    az:native({'Column', [], ["
        "        {'Button', [{on_tap, az:get(cmd, Bindings)}], [<<\"B\">>]}, "
        "        case az:get(show, Bindings) of "
        "            true -> {'Text', [], [<<\"on\">>]}; "
        "            false -> <<>> "
        "        end"
        "    ]}). "
    ),
    Payload = #{~"f" => ~"seen", ~"s" => ~"keep", ~"d" => ~"x"},
    Cmd0 = arizona_android:push_event(~"a", Payload),
    Cmd1 = arizona_android:push_event(~"b", Payload),
    {_, Snap0} = arizona_render:render(Mod:render(#{cmd => Cmd0, show => false})),
    Seen = #{~"seen" => true},
    {[[?OP_TEXT, ~"0", Mount]], _} =
        arizona_live:dedup_fps([[?OP_TEXT, ~"0", arizona_render:fingerprint_payload(Snap0)]], Seen),
    #{~"children" := [Button | _]} = simulate_interleave(Mount),
    ?assertEqual([0, ~"a", Payload], maps:get(~"on_tap", Button)),
    {Ops, _} = arizona_diff:diff(Mod:render(#{cmd => Cmd1, show => true}), Snap0),
    {Ops1, Fps} = arizona_live:dedup_fps(Ops, Seen),
    ?assertMatch([[?OP_SET_ATTR, _, ~"on_tap", [0, ~"b", Payload]], [?OP_TEXT, _, #{}]], Ops1),
    ?assertEqual(2, map_size(Fps)).

live_view_caches_statics(Config) when is_list(Config) ->
    %% Wire-efficiency proof: a native live view's first mount ships the full
    %% widget tree (statics present); a fresh instance whose client already
    %% cached the fingerprint gets the statics stripped (`#{f,d}` only). Native
    %% reuses the exact same fingerprint cache as the browser -- statics are
    %% sent once, never re-sent.
    {ok, Pid1} = arizona_live:start_link(arizona_native_counter, #{}, undefined, []),
    {ok, ViewId, Frame1, _} = arizona_live:mount_and_render(Pid1),
    ?assertEqual(~"native_counter", ViewId),
    ?assert(maps:is_key(~"s", Frame1)),
    ?assertMatch(#{~"type" := ~"Column", ~"id" := ~"native_counter"}, simulate_interleave(Frame1)),
    Fp = maps:get(~"f", Frame1),
    %% Fresh instance, fingerprint pre-seeded (as the client would report).
    {ok, Pid2} = arizona_live:start_link(arizona_native_counter, #{}, undefined, []),
    arizona_live:seed_fps(Pid2, [Fp]),
    sys:get_state(Pid2),
    {ok, _ViewId, Frame2, _} = arizona_live:mount_and_render(Pid2),
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
    {ok, ~"native_parent", Frame, _} = arizona_live:mount_and_render(Pid),
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
    {ok, ~"native_nested", _Frame, _} = arizona_live:mount_and_render(Pid),
    {ok, OpsA, _, _} = arizona_live:handle_event(Pid, ~"child_a", ~"inc", #{}),
    ?assertMatch([[?OP_TEXT, _Az, ~"1"]], OpsA),
    {ok, OpsB, _, _} = arizona_live:handle_event(Pid, ~"child_b", ~"inc", #{}),
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

%% A folded effect command is spliced into the static as raw JSON, so a payload
%% key named `az` puts a literal `"az":"` in the statics. Fingerprint scoping
%% must rebuild only the node ids the transform allocated and leave the
%% command's own JSON alone.
scope_keeps_command_payload_az(Config) when is_list(Config) ->
    Mod = compile_module(
        "-module(nt_scope_cmd). "
        "-export([render/1]). "
        "render(Bindings) -> "
        "    az:native({'Button', [{on_tap, arizona_android:push_event(<<\"inc\">>, "
        "                                     #{<<\"az\">> => <<\"7\">>})}], "
        "              [arizona_template:get(v, Bindings)]}). "
    ),
    T = Mod:render(#{v => ~"V"}),
    Fp = maps:get(f, T),
    [S1, _S2] = maps:get(s, T),
    %% The command's payload is untouched...
    ?assertNotEqual(nomatch, binary:match(S1, ~"[0,\"inc\",{\"az\":\"7\"}]")),
    %% ...while the widget's own `az` and its slot node id are scoped.
    ?assertNotEqual(nomatch, binary:match(S1, <<"\"az\":\"", Fp/binary, "-0\"">>)),
    ?assertNotEqual(nomatch, binary:match(S1, <<"\"az\":\"", Fp/binary, "-0t0\"">>)).

%% --------------------------------------------------------------------
%% Helpers
%% --------------------------------------------------------------------

%% Decode a fully-static native template (single static fragment, no dynamics).
decode_static(#{s := [Static], d := []}) ->
    json:decode(Static).

%% The zipped document of a `?native` template -- what render_to_iolist/1 returns
%% and the text render/1 returns -- is byte for byte the JSON a client builds by
%% interleaving the fingerprint payload, for every value shape and position.
zipped_document_matches_client(Config) when is_list(Config) ->
    lists:foreach(
        fun({Name, Tmpl}) ->
            {Live, Snap} = arizona_render:render(Tmpl),
            Client = interleave_bin(arizona_render:fingerprint_payload(Snap)),
            ?assertMatch(#{}, json:decode(Client)),
            ?assertEqual(
                {Name, Client}, {Name, iolist_to_binary(arizona_render:render_to_iolist(Tmpl))}
            ),
            ?assertEqual({Name, Client}, {Name, iolist_to_binary(Live)})
        end,
        arizona_native_ssr_shapes:templates()
    ).

%% A `?native` view rendered request-free (render_view_to_iolist/2, the path
%% static generation and the dev tools take) is the document a client builds from
%% the live process's first frame, stateful children included.
ssr_view_matches_mount_frame(Config) when is_list(Config) ->
    Views = [
        {arizona_native_counter, #{}},
        {arizona_native_nested, #{}},
        {arizona_native_conditional, #{}},
        {arizona_native_stream_child, #{}},
        {arizona_native_tabs, #{}},
        {arizona_native_list, #{
            items => [#{id => 1, text => ~"a \"b\""}, #{id => 2, text => ~"c"}]
        }}
    ],
    lists:foreach(
        fun({Handler, Bindings}) ->
            {ok, Pid} = arizona_live:start_link(Handler, Bindings, undefined, []),
            {ok, _ViewId, Frame, _} = arizona_live:mount_and_render(Pid),
            {ok, Current, _} = arizona_live:render_current(Pid),
            ok = gen_server:stop(Pid),
            SSR = arizona_render:render_view_to_iolist(Handler, #{bindings => Bindings}),
            Client = iolist_to_binary(cached_interleave(Frame, frame_statics(Frame, #{}))),
            ?assertEqual({Handler, Client}, {Handler, iolist_to_binary(SSR)}),
            ?assertEqual({Handler, Client}, {Handler, Current})
        end,
        Views
    ).

interleave_bin(#{~"s" := Statics, ~"d" := Dynamics}) ->
    iolist_to_binary(interleave(Statics, Dynamics)).

%% A frame ships a fingerprint's statics once and omits them on every repeat, so a
%% client resolves `#{f, d}` from what it already holds. Collect them first.
frame_statics(#{~"f" := F, ~"s" := S} = Payload, Acc) ->
    frame_statics(maps:get(~"d", Payload), Acc#{F => S});
frame_statics(#{~"d" := D}, Acc) ->
    frame_statics(D, Acc);
frame_statics(List, Acc) when is_list(List) ->
    lists:foldl(fun frame_statics/2, Acc, List);
frame_statics(_Scalar, Acc) ->
    Acc.

cached_interleave(#{~"t" := _, ~"f" := F, ~"d" := ItemsList}, Cache) ->
    #{F := S} = Cache,
    Items = [cached_zip(S, ItemD, Cache) || ItemD <- ItemsList],
    [$[, lists:join($,, Items), $]];
cached_interleave(#{~"f" := F, ~"d" := D}, Cache) ->
    #{F := S} = Cache,
    cached_zip(S, D, Cache);
cached_interleave(V, _Cache) when is_binary(V) ->
    json:encode(V).

cached_zip([S], [], _Cache) ->
    [S];
cached_zip([S | Statics], [V | Dynamics], Cache) ->
    [S, cached_interleave(V, Cache) | cached_zip(Statics, Dynamics, Cache)].

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
encode_value(V) when is_binary(V); is_number(V); is_boolean(V); is_list(V) ->
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

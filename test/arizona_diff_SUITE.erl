-module(arizona_diff_SUITE).
-include_lib("stdlib/include/assert.hrl").
-include("arizona.hrl").

-export([all/0, groups/0]).
-export([
    diff_attr_op/1,
    diff_bool_attr_add/1,
    diff_bool_attr_remove/1,
    diff_bool_attr_no_change/1,
    diff_mixed_op/1,
    diff_nested_text_op/1,
    diff_no_change_op/1,
    diff_no_diff_stateful_child_diff4/1,
    diff_only_changed_emits_ops/1,
    diff_remove_node_op/1,
    diff_replace_with_template_op/1,
    diff_text_op/1,
    no_diff_diff3/1,
    no_diff_diff4_top_level/1,
    no_diff_nested/1,
    no_diff_ops/1,
    no_diff_skips_eval/1,
    no_diff_stateful_child/1
]).

all() ->
    [{group, basic_ops}, {group, no_diff}].

groups() ->
    [
        {basic_ops, [parallel], [
            diff_text_op,
            diff_attr_op,
            diff_no_change_op,
            diff_nested_text_op,
            diff_mixed_op,
            diff_remove_node_op,
            diff_replace_with_template_op,
            diff_only_changed_emits_ops,
            diff_bool_attr_add,
            diff_bool_attr_remove,
            diff_bool_attr_no_change
        ]},
        {no_diff, [parallel], [
            no_diff_ops,
            no_diff_nested,
            no_diff_skips_eval,
            no_diff_stateful_child,
            no_diff_diff3,
            no_diff_diff4_top_level,
            diff_no_diff_stateful_child_diff4
        ]}
    ].

%% --- diff/2 basic ops ---

diff_text_op(Config) when is_list(Config) ->
    OldSnap = #{
        s => [<<"<p az=\"0\">Hello, ">>, <<"!</p>">>],
        d => [{<<"0">>, <<"World">>}]
    },
    NewTmpl = #{
        s => [<<"<p az=\"0\">Hello, ">>, <<"!</p>">>],
        d => [{<<"0">>, fun() -> <<"Alice">> end}],
        f => <<"test">>
    },
    {Ops, NewSnap} = arizona_diff:diff(NewTmpl, OldSnap),
    ?assertEqual([[?OP_TEXT, <<"0">>, <<"Alice">>]], Ops),
    ?assertEqual([{<<"0">>, <<"Alice">>}], maps:get(d, NewSnap)).

diff_attr_op(Config) when is_list(Config) ->
    OldSnap = #{
        s => [<<"<div az=\"0\" class=\"">>, <<"\">ok</div>">>],
        d => [{<<"0">>, {attr, <<"class">>, <<"active">>}}]
    },
    NewTmpl = #{
        s => [<<"<div az=\"0\" class=\"">>, <<"\">ok</div>">>],
        d => [{<<"0">>, {attr, <<"class">>, fun() -> <<"inactive">> end}}],
        f => <<"test">>
    },
    {Ops, _} = arizona_diff:diff(NewTmpl, OldSnap),
    ?assertEqual([[?OP_SET_ATTR, <<"0">>, <<"class">>, <<"inactive">>]], Ops).

%% Bool attr: false -> true produces OP_SET_ATTR with empty value.
diff_bool_attr_add(Config) when is_list(Config) ->
    OldSnap = #{
        s => [<<"<input az=\"0\"">>, <<" />">>],
        d => [{<<"0">>, {attr, <<"checked">>, false}}]
    },
    NewTmpl = #{
        s => [<<"<input az=\"0\"">>, <<" />">>],
        d => [{<<"0">>, {attr, <<"checked">>, fun() -> true end}}],
        f => <<"test">>
    },
    {Ops, _} = arizona_diff:diff(NewTmpl, OldSnap),
    ?assertEqual([[?OP_SET_ATTR, <<"0">>, <<"checked">>, <<>>]], Ops).

%% Bool attr: true -> false produces OP_REM_ATTR.
diff_bool_attr_remove(Config) when is_list(Config) ->
    OldSnap = #{
        s => [<<"<input az=\"0\"">>, <<" />">>],
        d => [{<<"0">>, {attr, <<"checked">>, true}}]
    },
    NewTmpl = #{
        s => [<<"<input az=\"0\"">>, <<" />">>],
        d => [{<<"0">>, {attr, <<"checked">>, fun() -> false end}}],
        f => <<"test">>
    },
    {Ops, _} = arizona_diff:diff(NewTmpl, OldSnap),
    ?assertEqual([[?OP_REM_ATTR, <<"0">>, <<"checked">>]], Ops).

%% Bool attr: true -> true produces no ops.
diff_bool_attr_no_change(Config) when is_list(Config) ->
    OldSnap = #{
        s => [<<"<input az=\"0\"">>, <<" />">>],
        d => [{<<"0">>, {attr, <<"checked">>, true}}]
    },
    NewTmpl = #{
        s => [<<"<input az=\"0\"">>, <<" />">>],
        d => [{<<"0">>, {attr, <<"checked">>, fun() -> true end}}],
        f => <<"test">>
    },
    {Ops, _} = arizona_diff:diff(NewTmpl, OldSnap),
    ?assertEqual([], Ops).

diff_no_change_op(Config) when is_list(Config) ->
    OldSnap = #{
        s => [<<"<p az=\"0\">">>, <<"</p>">>],
        d => [{<<"0">>, <<"same">>}]
    },
    NewTmpl = #{
        s => [<<"<p az=\"0\">">>, <<"</p>">>],
        d => [{<<"0">>, fun() -> <<"same">> end}],
        f => <<"test">>
    },
    {Ops, _} = arizona_diff:diff(NewTmpl, OldSnap),
    ?assertEqual([], Ops).

diff_nested_text_op(Config) when is_list(Config) ->
    OldSnap = #{
        s => [<<"<p az=\"0\">">>, <<"</p>">>],
        d => [{<<"0">>, #{s => [<<"Hello, ">>, <<"!">>], d => [{<<"i">>, <<"World">>}]}}]
    },
    NewTmpl = #{
        s => [<<"<p az=\"0\">">>, <<"</p>">>],
        d => [
            {<<"0">>, #{
                s => [<<"Hello, ">>, <<"!">>],
                d => [{<<"i">>, fun() -> <<"Alice">> end}],
                f => <<"test">>
            }}
        ],
        f => <<"test">>
    },
    {Ops, _} = arizona_diff:diff(NewTmpl, OldSnap),
    ?assertEqual(
        [
            [
                ?OP_TEXT,
                <<"0">>,
                #{
                    <<"f">> => <<"test">>,
                    <<"s">> => [<<"Hello, ">>, <<"!">>],
                    <<"d">> => [<<"Alice">>]
                }
            ]
        ],
        Ops
    ).

diff_mixed_op(Config) when is_list(Config) ->
    OldSnap = #{
        s => [<<"<div az=\"0\" class=\"">>, <<"\"><p az=\"0.0\">">>, <<"</p></div>">>],
        d => [{<<"0">>, {attr, <<"class">>, <<"active">>}}, {<<"0.0">>, <<"hello">>}]
    },
    NewTmpl = #{
        s => [<<"<div az=\"0\" class=\"">>, <<"\"><p az=\"0.0\">">>, <<"</p></div>">>],
        d => [
            {<<"0">>, {attr, <<"class">>, fun() -> <<"inactive">> end}},
            {<<"0.0">>, fun() -> <<"goodbye">> end}
        ],
        f => <<"test">>
    },
    {Ops, _} = arizona_diff:diff(NewTmpl, OldSnap),
    ?assertEqual(
        [
            [?OP_SET_ATTR, <<"0">>, <<"class">>, <<"inactive">>],
            [?OP_TEXT, <<"0.0">>, <<"goodbye">>]
        ],
        Ops
    ).

diff_remove_node_op(Config) when is_list(Config) ->
    OldSnap = #{
        s => [<<"<div az=\"0\">">>, <<"</div>">>],
        d => [{<<"0">>, <<"visible">>}]
    },
    NewTmpl = #{
        s => [<<"<div az=\"0\">">>, <<"</div>">>],
        d => [{<<"0">>, fun() -> remove end}],
        f => <<"test">>
    },
    {Ops, _} = arizona_diff:diff(NewTmpl, OldSnap),
    ?assertEqual([[?OP_REMOVE_NODE, <<"0">>]], Ops).

diff_replace_with_template_op(Config) when is_list(Config) ->
    OldSnap = #{
        s => [<<"<div az=\"0\">">>, <<"</div>">>],
        d => [{<<"0">>, <<"plain">>}]
    },
    NewTmpl = #{
        s => [<<"<div az=\"0\">">>, <<"</div>">>],
        d => [
            {<<"0">>, fun() ->
                #{s => [<<"<b>">>, <<"</b>">>], d => [{<<"i">>, <<"bold">>}], f => <<"test">>}
            end}
        ],
        f => <<"test">>
    },
    {Ops, _} = arizona_diff:diff(NewTmpl, OldSnap),
    ?assertEqual(
        [
            [
                ?OP_UPDATE,
                <<"0">>,
                #{
                    <<"f">> => <<"test">>,
                    <<"s">> => [<<"<b>">>, <<"</b>">>],
                    <<"d">> => [<<"bold">>]
                }
            ]
        ],
        Ops
    ).

diff_only_changed_emits_ops(Config) when is_list(Config) ->
    OldSnap = #{
        s => [<<"<div az=\"0\">">>, <<" ">>, <<"</div>">>],
        d => [{<<"0">>, <<"stay">>}, {<<"0">>, <<"change">>}]
    },
    NewTmpl = #{
        s => [<<"<div az=\"0\">">>, <<" ">>, <<"</div>">>],
        d => [{<<"0">>, fun() -> <<"stay">> end}, {<<"0">>, fun() -> <<"changed">> end}],
        f => <<"test">>
    },
    {Ops, _} = arizona_diff:diff(NewTmpl, OldSnap),
    ?assertEqual([[?OP_TEXT, <<"0">>, <<"changed">>]], Ops).

%% --- diff => false ---

no_diff_ops(Config) when is_list(Config) ->
    %% diff/2 produces no ops when old snapshot has diff => false
    OldSnap = #{
        s => [<<"<p az=\"0\">">>, <<"</p>">>],
        d => [{<<"0">>, <<"old">>}],
        diff => false
    },
    NewTmpl = #{
        s => [<<"<p az=\"0\">">>, <<"</p>">>],
        d => [{<<"0">>, fun() -> <<"new">> end}],
        diff => false,
        f => <<"test">>
    },
    {Ops, _} = arizona_diff:diff(NewTmpl, OldSnap),
    ?assertEqual([], Ops).

no_diff_nested(Config) when is_list(Config) ->
    %% Only the non-ignored dynamic produces ops
    OldSnap = #{
        s => [<<"<div az=\"0\">">>, <<" ">>, <<"</div>">>],
        d => [
            {<<"0">>, <<"title">>},
            {<<"1">>, #{
                s => [<<"<p>">>, <<"</p>">>],
                d => [{<<"i">>, <<"old">>}],
                diff => false
            }}
        ]
    },
    NewTmpl = #{
        s => [<<"<div az=\"0\">">>, <<" ">>, <<"</div>">>],
        d => [
            {<<"0">>, fun() -> <<"new title">> end},
            {<<"1">>, #{
                s => [<<"<p>">>, <<"</p>">>],
                d => [{<<"i">>, fun() -> <<"new">> end}],
                diff => false,
                f => <<"test">>
            }}
        ],
        f => <<"test">>
    },
    {Ops, _} = arizona_diff:diff(NewTmpl, OldSnap),
    ?assertEqual([[?OP_TEXT, <<"0">>, <<"new title">>]], Ops).

no_diff_skips_eval(Config) when is_list(Config) ->
    %% diff/4 reuses old value (no eval) for diff => false dynamic
    Bindings0 = #{title => <<"A">>, content => <<"old">>},
    T0 = #{
        s => [<<"<div az=\"0\">">>, <<" ">>, <<"</div>">>],
        d => [
            {<<"0">>, fun() -> arizona_template:get(title, Bindings0) end},
            {<<"1">>, #{
                s => [<<"<span>">>, <<"</span>">>],
                d => [{<<"i">>, fun() -> arizona_template:get(content, Bindings0) end}],
                diff => false,
                f => <<"test">>
            }}
        ],
        f => <<"test">>
    },
    {_, Snap0, V0} = arizona_render:render(T0, #{}),
    %% Change both title and content
    Bindings1 = #{title => <<"B">>, content => <<"new">>},
    T1 = #{
        s => [<<"<div az=\"0\">">>, <<" ">>, <<"</div>">>],
        d => [
            {<<"0">>, fun() -> arizona_template:get(title, Bindings1) end},
            {<<"1">>, #{
                s => [<<"<span>">>, <<"</span>">>],
                d => [{<<"i">>, fun() -> arizona_template:get(content, Bindings1) end}],
                diff => false,
                f => <<"test">>
            }}
        ],
        f => <<"test">>
    },
    Changed = compute_changed(Bindings0, Bindings1),
    {Ops, Snap1, _} = arizona_diff:diff(T1, Snap0, V0, Changed),
    %% Only title produces an op
    ?assertEqual([[?OP_TEXT, <<"0">>, <<"B">>]], Ops),
    %% The ignored dynamic retains old value in snapshot
    [{<<"0">>, _}, {<<"1">>, IgnoredSnap}] = maps:get(d, Snap1),
    [{<<"i">>, ContentVal}] = maps:get(d, IgnoredSnap),
    ?assertEqual(<<"old">>, ContentVal).

no_diff_stateful_child(Config) when is_list(Config) ->
    %% Stateful child whose render/1 returns diff => false.
    %% After initial render, diff/3 with changed parent props emits no child ops
    %% and the child snapshot preserves diff => false.
    Bindings0 = #{title => <<"T">>, count => 1},
    T0 = #{
        s => [<<"<main az=\"0\">">>, <<" ">>, <<"</main>">>],
        d => [
            {<<"0">>, fun() -> arizona_template:get(title, Bindings0) end},
            {<<"1">>,
                arizona_template:stateful(
                    arizona_no_diff_counter,
                    #{id => <<"nd_counter">>, count => arizona_template:get(count, Bindings0)}
                )}
        ],
        f => <<"test">>
    },
    {_HTML0, Snap0, Views0} = arizona_render:render(T0, #{}),
    %% Verify child is in views and snapshot has diff => false
    ?assertMatch(#{<<"nd_counter">> := #{handler := arizona_no_diff_counter}}, Views0),
    [{<<"0">>, _}, {<<"1">>, ChildSnap0}] = maps:get(d, Snap0),
    ?assertEqual(false, maps:get(diff, ChildSnap0)),
    %% Now diff/3 with changed count -- child should produce no ops
    Bindings1 = #{title => <<"T">>, count => 99},
    T1 = #{
        s => [<<"<main az=\"0\">">>, <<" ">>, <<"</main>">>],
        d => [
            {<<"0">>, fun() -> arizona_template:get(title, Bindings1) end},
            {<<"1">>,
                arizona_template:stateful(
                    arizona_no_diff_counter,
                    #{id => <<"nd_counter">>, count => arizona_template:get(count, Bindings1)}
                )}
        ],
        f => <<"test">>
    },
    {Ops1, Snap1, _Views1} = arizona_diff:diff(T1, Snap0, Views0),
    %% No child ops -- only empty child op list or none at all
    lists:foreach(
        fun(Op) ->
            case Op of
                [<<"nd_counter">>, ChildOps] -> ?assertEqual([], ChildOps);
                _ -> ok
            end
        end,
        Ops1
    ),
    %% Child snapshot still has diff => false
    [{<<"0">>, _}, {<<"1">>, ChildSnap1}] = maps:get(d, Snap1),
    ?assertEqual(false, maps:get(diff, ChildSnap1)).

no_diff_diff3(Config) when is_list(Config) ->
    %% diff/3 path: nested diff => false dynamic produces no ops.
    %% Note: diff/3 evaluates new dynamics (funs are called) but diff_dynamics
    %% skips op generation when the OLD snapshot has diff => false. The new
    %% snapshot contains the freshly evaluated values (unlike diff/4 which
    %% reuses old values entirely).
    Bindings0 = #{title => <<"A">>, content => <<"old">>},
    T0 = #{
        s => [<<"<div az=\"0\">">>, <<" ">>, <<"</div>">>],
        d => [
            {<<"0">>, fun() -> arizona_template:get(title, Bindings0) end},
            {<<"1">>, #{
                s => [<<"<span>">>, <<"</span>">>],
                d => [{<<"i">>, fun() -> arizona_template:get(content, Bindings0) end}],
                diff => false,
                f => <<"test">>
            }}
        ],
        f => <<"test">>
    },
    {_HTML0, Snap0, Views0} = arizona_render:render(T0, #{}),
    %% Change both title and content, diff/3 (no Changed map)
    Bindings1 = #{title => <<"B">>, content => <<"new">>},
    T1 = #{
        s => [<<"<div az=\"0\">">>, <<" ">>, <<"</div>">>],
        d => [
            {<<"0">>, fun() -> arizona_template:get(title, Bindings1) end},
            {<<"1">>, #{
                s => [<<"<span>">>, <<"</span>">>],
                d => [{<<"i">>, fun() -> arizona_template:get(content, Bindings1) end}],
                diff => false,
                f => <<"test">>
            }}
        ],
        f => <<"test">>
    },
    {Ops, Snap1, _Views1} = arizona_diff:diff(T1, Snap0, Views0),
    %% Only title produces an op -- the diff => false nested dynamic is skipped
    ?assertEqual([[?OP_TEXT, <<"0">>, <<"B">>]], Ops),
    %% Nested snapshot has diff => false and freshly evaluated content
    [{<<"0">>, _}, {<<"1">>, NestedSnap}] = maps:get(d, Snap1),
    ?assertEqual(false, maps:get(diff, NestedSnap)).

no_diff_diff4_top_level(Config) when is_list(Config) ->
    %% diff/4 with top-level diff => false short-circuits entirely
    OldSnap = #{
        s => [<<"<div az=\"0\">">>, <<"</div>">>],
        d => [{<<"0">>, <<"old">>}],
        deps => [#{title => true}],
        diff => false
    },
    NewTmpl = #{
        s => [<<"<div az=\"0\">">>, <<"</div>">>],
        d => [{<<"0">>, fun() -> <<"new">> end}],
        diff => false,
        f => <<"test">>
    },
    Views = #{},
    Changed = #{title => <<"new">>},
    {Ops, ResultSnap, ResultViews} = arizona_diff:diff(NewTmpl, OldSnap, Views, Changed),
    %% Complete short-circuit: no ops, old snapshot unchanged, views unchanged
    ?assertEqual([], Ops),
    ?assertEqual(OldSnap, ResultSnap),
    ?assertEqual(Views, ResultViews).

diff_no_diff_stateful_child_diff4(Config) when is_list(Config) ->
    %% diff/4 skips eval entirely for stateful child with diff => false
    %% when the child's deps haven't changed
    Bindings0 = #{title => <<"T">>, count => 1},
    T0 = #{
        s => [<<"<main az=\"0\">">>, <<" ">>, <<"</main>">>],
        d => [
            {<<"0">>, fun() -> arizona_template:get(title, Bindings0) end},
            {<<"1">>,
                arizona_template:stateful(
                    arizona_no_diff_counter,
                    #{id => <<"nd_counter">>, count => arizona_template:get(count, Bindings0)}
                )}
        ],
        f => <<"test">>
    },
    {_HTML0, Snap0, Views0} = arizona_render:render(T0, #{}),
    %% Change only title (not count) -- child deps haven't changed
    Bindings1 = #{title => <<"New Title">>, count => 1},
    T1 = #{
        s => [<<"<main az=\"0\">">>, <<" ">>, <<"</main>">>],
        d => [
            {<<"0">>, fun() -> arizona_template:get(title, Bindings1) end},
            {<<"1">>,
                arizona_template:stateful(
                    arizona_no_diff_counter,
                    #{id => <<"nd_counter">>, count => arizona_template:get(count, Bindings1)}
                )}
        ],
        f => <<"test">>
    },
    Changed = compute_changed(Bindings0, Bindings1),
    {Ops, Snap1, _Views1} = arizona_diff:diff(T1, Snap0, Views0, Changed),
    %% Only title op, no child ops
    ?assertEqual([[?OP_TEXT, <<"0">>, <<"New Title">>]], Ops),
    %% Child snapshot preserved with diff => false
    [{<<"0">>, _}, {<<"1">>, ChildSnap}] = maps:get(d, Snap1),
    ?assertEqual(false, maps:get(diff, ChildSnap)).

%% =============================================================================
%% Helpers
%% =============================================================================

%% Mirrors arizona_live:compute_changed/2 for unit tests
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

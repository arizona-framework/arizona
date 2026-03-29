-module(arizona_render_SUITE).
-include_lib("stdlib/include/assert.hrl").
-include("arizona.hrl").
-dialyzer({nowarn_function, fingerprint_absent_when_no_f/1}).

-export([all/0, groups/0]).
-export([
    diff4_nodiff_shortcircuit/1,
    diff_nodiff_shortcircuit/1,
    error_page_arizona_loc/1,
    error_page_empty_stacktrace/1,
    error_page_error_summaries/1,
    error_page_escapes_unsafe_chars/1,
    error_page_format_reason_unwraps_loc/1,
    error_page_typical_crash/1,
    error_page_undef/1,
    fingerprint_absent_when_no_f/1,
    fingerprint_payload_with_f/1,
    fingerprint_payload_without_f/1,
    fingerprint_propagated_in_diff/1,
    fingerprint_propagated_in_render2/1,
    fingerprint_propagated_in_render/1,
    no_diff_render/1,
    no_diff_render_with_views/1,
    no_diff_ssr_nested/1,
    render_attr/1,
    render_nested_sd/1,
    render_nodiff_layout_location_success/1,
    render_nodiff_layout_location_wrapping/1,
    render_nodiff_snapshot/1,
    render_ssr_nested_3tuple_dyn_az/1,
    render_text/1,
    render_to_iolist_nodiff_multi/1,
    render_to_iolist_nodiff_static/1,
    render_to_iolist_nodiff/1,
    render_v_nodiff_snapshot/1,
    render_with_views_no_children/1,
    resolve_id_binary/1,
    resolve_id_template/1,
    ssr_about_page/1,
    ssr_counter_custom_id/1,
    ssr_counter/1,
    ssr_counter_with_bindings/1,
    ssr_page_with_child/1,
    zip_nested_element/1,
    zip_nested_sd/1,
    zip_static_only/1,
    zip_text_slot/1
]).

all() ->
    [
        {group, zip},
        {group, render},
        {group, ssr},
        {group, fingerprint},
        {group, error_page},
        {group, nodiff}
    ].

groups() ->
    [
        {zip, [parallel], [
            zip_static_only,
            zip_text_slot,
            zip_nested_sd,
            zip_nested_element
        ]},
        {render, [parallel], [
            render_text,
            render_attr,
            render_nested_sd,
            render_with_views_no_children
        ]},
        {ssr, [parallel], [
            ssr_counter,
            ssr_counter_with_bindings,
            ssr_counter_custom_id,
            ssr_page_with_child,
            ssr_about_page,
            resolve_id_binary,
            resolve_id_template
        ]},
        {fingerprint, [parallel], [
            fingerprint_payload_with_f,
            fingerprint_payload_without_f,
            fingerprint_propagated_in_render,
            fingerprint_propagated_in_render2,
            fingerprint_propagated_in_diff,
            fingerprint_absent_when_no_f
        ]},
        {error_page, [parallel], [
            error_page_typical_crash,
            error_page_empty_stacktrace,
            error_page_undef,
            error_page_escapes_unsafe_chars,
            error_page_arizona_loc,
            render_nodiff_layout_location_wrapping,
            render_nodiff_layout_location_success,
            render_ssr_nested_3tuple_dyn_az,
            error_page_format_reason_unwraps_loc,
            error_page_error_summaries
        ]},
        {nodiff, [parallel], [
            no_diff_render,
            no_diff_render_with_views,
            no_diff_ssr_nested,
            render_to_iolist_nodiff,
            render_to_iolist_nodiff_multi,
            render_to_iolist_nodiff_static,
            render_nodiff_snapshot,
            render_v_nodiff_snapshot,
            diff_nodiff_shortcircuit,
            diff4_nodiff_shortcircuit
        ]}
    ].

%% =============================================================================
%% 1. zip tests
%% =============================================================================

zip_static_only(Config) when is_list(Config) ->
    ?assertEqual([<<"Hello!">>], arizona_render:zip([<<"Hello!">>], [])).

zip_text_slot(Config) when is_list(Config) ->
    ?assertEqual(
        <<"<p>Hello, World!</p>">>,
        iolist_to_binary(
            arizona_render:zip(
                [<<"<p>Hello, ">>, <<"!</p>">>],
                [<<"World">>]
            )
        )
    ).

zip_nested_sd(Config) when is_list(Config) ->
    ?assertEqual(
        <<"<div class=\"foo bar\">ok</div>">>,
        iolist_to_binary(
            arizona_render:zip(
                [<<"<div class=\"">>, <<"\">ok</div>">>],
                [#{s => [<<"foo ">>, <<>>], d => [{<<"i">>, <<"bar">>}]}]
            )
        )
    ).

zip_nested_element(Config) when is_list(Config) ->
    ?assertEqual(
        <<"<div><b>bold</b></div>">>,
        iolist_to_binary(
            arizona_render:zip(
                [<<"<div>">>, <<"</div>">>],
                [#{s => [<<"<b>">>, <<"</b>">>], d => [{<<"i">>, <<"bold">>}]}]
            )
        )
    ).

%% =============================================================================
%% 2. render tests (render/1 -- no views, no dep tracking)
%% =============================================================================

render_text(Config) when is_list(Config) ->
    T = #{
        s => [<<"<p az=\"0\">">>, <<"</p>">>],
        d => [{<<"0">>, fun() -> <<"hello">> end}],
        f => <<"test">>
    },
    {HTML, Snap} = arizona_render:render(T),
    ?assertEqual(<<"<p az=\"0\">hello</p>">>, iolist_to_binary(HTML)),
    ?assertEqual(
        #{s => [<<"<p az=\"0\">">>, <<"</p>">>], d => [{<<"0">>, <<"hello">>}], f => <<"test">>},
        Snap
    ).

render_attr(Config) when is_list(Config) ->
    T = #{
        s => [<<"<div az=\"0\"">>, <<">ok</div>">>],
        d => [{<<"0">>, {attr, <<"class">>, fun() -> <<"active">> end}}],
        f => <<"test">>
    },
    {HTML, Snap} = arizona_render:render(T),
    ?assertEqual(<<"<div az=\"0\" class=\"active\">ok</div>">>, iolist_to_binary(HTML)),
    ?assertEqual(
        #{
            s => [<<"<div az=\"0\"">>, <<">ok</div>">>],
            d => [{<<"0">>, {attr, <<"class">>, <<"active">>}}],
            f => <<"test">>
        },
        Snap
    ).

render_nested_sd(Config) when is_list(Config) ->
    T = #{
        s => [<<"<p az=\"0\">">>, <<"</p>">>],
        d => [
            {<<"0">>, #{
                s => [<<"a ">>, <<>>], d => [{<<"i">>, fun() -> <<"b">> end}], f => <<"test">>
            }}
        ],
        f => <<"test">>
    },
    {HTML, Snap} = arizona_render:render(T),
    ?assertEqual(<<"<p az=\"0\">a b</p>">>, iolist_to_binary(HTML)),
    ?assertEqual(
        #{
            s => [<<"<p az=\"0\">">>, <<"</p>">>],
            d => [{<<"0">>, #{s => [<<"a ">>, <<>>], d => [{<<"i">>, <<"b">>}], f => <<"test">>}}],
            f => <<"test">>
        },
        Snap
    ).

%% =============================================================================
%% 3. SSR tests -- render_to_iolist
%% =============================================================================

ssr_counter(Config) when is_list(Config) ->
    HTML = iolist_to_binary(arizona_render:render_to_iolist(arizona_counter, #{})),
    ?assertNotEqual(nomatch, binary:match(HTML, <<"0<!--/az-->">>)),
    ?assertNotEqual(nomatch, binary:match(HTML, <<"az-click=\"[0,&quot;inc&quot;]\"">>)).

ssr_counter_with_bindings(Config) when is_list(Config) ->
    HTML = iolist_to_binary(
        arizona_render:render_to_iolist(arizona_counter, #{bindings => #{count => 5}})
    ),
    ?assertNotEqual(nomatch, binary:match(HTML, <<"5<!--/az-->">>)).

ssr_counter_custom_id(Config) when is_list(Config) ->
    HTML = iolist_to_binary(
        arizona_render:render_to_iolist(
            arizona_counter,
            #{bindings => #{id => <<"c2">>, count => 99}}
        )
    ),
    ?assertNotEqual(nomatch, binary:match(HTML, <<"99<!--/az-->">>)).

ssr_page_with_child(Config) when is_list(Config) ->
    HTML = iolist_to_binary(
        arizona_render:render_to_iolist(
            arizona_page,
            #{bindings => #{title => <<"Welcome">>}, layout => {arizona_layout, render}}
        )
    ),
    ?assertNotEqual(nomatch, binary:match(HTML, <<"<!DOCTYPE html>">>)),
    ?assertNotEqual(nomatch, binary:match(HTML, <<"<title>Welcome</title>">>)),
    ?assertNotEqual(nomatch, binary:match(HTML, <<"az-view id=\"page\"">>)),
    ?assertNotEqual(nomatch, binary:match(HTML, <<"Welcome<!--/az-->">>)),
    ?assertNotEqual(nomatch, binary:match(HTML, <<"az-view id=\"counter\"">>)),
    ?assertNotEqual(nomatch, binary:match(HTML, <<"az-view id=\"counter2\"">>)),
    ?assertNotEqual(nomatch, binary:match(HTML, <<"az-view id=\"counter3\"">>)),
    ?assertNotEqual(nomatch, binary:match(HTML, <<"42<!--/az-->">>)),
    ?assertNotEqual(nomatch, binary:match(HTML, <<"Connecting...<!--/az-->">>)),
    ?assertNotEqual(nomatch, binary:match(HTML, <<"az-navigate">>)),
    ?assertNotEqual(nomatch, binary:match(HTML, <<"connect('/ws')">>)),
    ?assertNotEqual(nomatch, binary:match(HTML, <<"</html>">>)).

%% =============================================================================
%% 4. resolve_id tests
%% =============================================================================

resolve_id_binary(Config) when is_list(Config) ->
    ?assertEqual(<<"foo">>, arizona_render:resolve_id(<<"foo">>)).

resolve_id_template(Config) when is_list(Config) ->
    Tmpl = #{s => [<<"hello">>], d => [], f => <<"test">>},
    ?assertEqual(<<"hello">>, arizona_render:resolve_id(Tmpl)).

%% =============================================================================
%% 5. render/2 no children
%% =============================================================================

render_with_views_no_children(Config) when is_list(Config) ->
    Tmpl = #{
        s => [<<"<p az=\"0\">">>, <<"</p>">>],
        d => [{<<"0">>, fun() -> <<"hello">> end}],
        f => <<"test">>
    },
    {HTML, Snap, Views} = arizona_render:render(Tmpl, #{}),
    ?assertEqual(<<"<p az=\"0\">hello</p>">>, iolist_to_binary(HTML)),
    ?assertEqual(#{}, Views),
    ?assert(maps:is_key(deps, Snap)).

%% =============================================================================
%% 6. SSR about page
%% =============================================================================

ssr_about_page(Config) when is_list(Config) ->
    HTML = iolist_to_binary(
        arizona_render:render_to_iolist(
            arizona_about, #{
                bindings => #{title => <<"About">>}, layout => {arizona_layout, render}
            }
        )
    ),
    ?assertMatch({_, _}, binary:match(HTML, <<"<title>About</title>">>)),
    ?assertMatch({_, _}, binary:match(HTML, <<"az-view id=\"page\"">>)),
    ?assertMatch({_, _}, binary:match(HTML, <<"About">>)),
    ?assertMatch({_, _}, binary:match(HTML, <<"az-hook=\"Tick\"">>)),
    ?assertNotEqual(nomatch, binary:match(HTML, <<"0<!--/az-->">>)).

%% =============================================================================
%% 7. Fingerprint payload/render tests
%% =============================================================================

fingerprint_payload_with_f(Config) when is_list(Config) ->
    Snap = #{s => [<<"a">>, <<"b">>], d => [{<<"0">>, <<"val">>}], f => <<"fp1">>},
    Result = arizona_render:fingerprint_payload(Snap),
    ?assertEqual(
        #{
            <<"f">> => <<"fp1">>,
            <<"s">> => [<<"a">>, <<"b">>],
            <<"d">> => [<<"val">>]
        },
        Result
    ).

fingerprint_payload_without_f(Config) when is_list(Config) ->
    Snap = #{s => [<<"a">>, <<"b">>], d => [{<<"0">>, <<"val">>}]},
    Result = arizona_render:fingerprint_payload(Snap),
    ?assertEqual(<<"avalb">>, Result).

fingerprint_propagated_in_render(Config) when is_list(Config) ->
    Tmpl = #{
        s => [<<"<p>">>, <<"</p>">>],
        d => [{<<"0">>, fun() -> <<"hi">> end}],
        f => <<"fp_test">>
    },
    {_HTML, Snap} = arizona_render:render(Tmpl),
    ?assertEqual(<<"fp_test">>, maps:get(f, Snap)).

fingerprint_propagated_in_render2(Config) when is_list(Config) ->
    Tmpl = #{
        s => [<<"<p>">>, <<"</p>">>],
        d => [{<<"0">>, fun() -> <<"hi">> end}],
        f => <<"fp2">>
    },
    {_HTML, Snap, _Views} = arizona_render:render(Tmpl, #{}),
    ?assertEqual(<<"fp2">>, maps:get(f, Snap)).

fingerprint_propagated_in_diff(Config) when is_list(Config) ->
    Tmpl = #{
        s => [<<"<p>">>, <<"</p>">>],
        d => [{<<"0">>, fun() -> <<"hi">> end}],
        f => <<"fp3">>
    },
    {_HTML, Snap0, V0} = arizona_render:render(Tmpl, #{}),
    ?assertEqual(<<"fp3">>, maps:get(f, Snap0)),
    {_Ops, Snap1, _V1} = arizona_diff:diff(Tmpl, Snap0, V0, #{}),
    ?assertEqual(<<"fp3">>, maps:get(f, Snap1)).

fingerprint_absent_when_no_f(Config) when is_list(Config) ->
    Tmpl = #{
        s => [<<"<p>">>, <<"</p>">>],
        d => [{<<"0">>, fun() -> <<"hi">> end}]
    },
    {_HTML, Snap} = arizona_render:render(Tmpl),
    ?assertNot(maps:is_key(f, Snap)).

%% =============================================================================
%% 8. Error page + render_ssr_one tests
%% =============================================================================

error_page_typical_crash(Config) when is_list(Config) ->
    Bindings = #{
        class => error,
        reason => badarg,
        stacktrace => [
            {my_mod, my_fun, 2, [{file, "src/my_mod.erl"}, {line, 42}]},
            {other_mod, call, 1, [{file, "src/other_mod.erl"}, {line, 10}]}
        ]
    },
    Result = iolist_to_binary(
        arizona_render:render_to_iolist(arizona_error_page:render(#{error_info => Bindings}))
    ),
    ?assertMatch({match, _}, re:run(Result, <<"<h1">>)),
    ?assertMatch({match, _}, re:run(Result, <<"bad argument">>)),
    ?assertMatch({match, _}, re:run(Result, <<"my_mod">>)),
    ?assertMatch({match, _}, re:run(Result, <<"my_fun/2">>)),
    ?assertMatch({match, _}, re:run(Result, <<"src/my_mod\\.erl:42">>)).

error_page_empty_stacktrace(Config) when is_list(Config) ->
    Bindings = #{
        class => error,
        reason => some_reason,
        stacktrace => []
    },
    Result = iolist_to_binary(
        arizona_render:render_to_iolist(arizona_error_page:render(#{error_info => Bindings}))
    ),
    ?assertMatch({match, _}, re:run(Result, <<"<h1">>)),
    ?assertMatch({match, _}, re:run(Result, <<"id=\"stacktrace\"">>)).

error_page_undef(Config) when is_list(Config) ->
    Bindings = #{
        class => error,
        reason => {undef, [{missing_mod, missing_fun, [arg1], []}]},
        stacktrace => [{missing_mod, missing_fun, [arg1], []}]
    },
    Result = iolist_to_binary(
        arizona_render:render_to_iolist(arizona_error_page:render(#{error_info => Bindings}))
    ),
    ?assertMatch({match, _}, re:run(Result, <<"missing_mod">>)).

error_page_escapes_unsafe_chars(Config) when is_list(Config) ->
    Bindings = #{
        class => error,
        reason => <<"<script>alert('xss')</script>">>,
        stacktrace => [{mod, fn, 0, []}]
    },
    Result = iolist_to_binary(
        arizona_render:render_to_iolist(arizona_error_page:render(#{error_info => Bindings}))
    ),
    ?assertEqual(nomatch, re:run(Result, <<"<script>">>)),
    ?assertMatch({match, _}, re:run(Result, <<"&lt;script&gt;">>)).

error_page_arizona_loc(Config) when is_list(Config) ->
    Bindings = #{
        class => error,
        reason => {arizona_loc, {my_handler, 42}, {bad_template_value, {}}},
        stacktrace => [
            {arizona_template, to_bin, 1, [{file, "src/arizona_template.erl"}, {line, 74}]}
        ]
    },
    Result = iolist_to_binary(
        arizona_render:render_to_iolist(arizona_error_page:render(#{error_info => Bindings}))
    ),
    ?assertMatch({match, _}, re:run(Result, <<"my_handler:42:">>)),
    ?assertMatch({match, _}, re:run(Result, <<"bad_template_value">>)).

render_nodiff_layout_location_wrapping(Config) when is_list(Config) ->
    Layout = #{
        s => [<<"<html>">>, <<"</html>">>],
        d => [{undefined, fun() -> error(boom) end, {my_layout, 5}}],
        f => <<"test">>,
        diff => false
    },
    ?assertError(
        {arizona_loc, {my_layout, 5}, boom},
        arizona_render:render_to_iolist(Layout)
    ).

render_nodiff_layout_location_success(Config) when is_list(Config) ->
    Layout = #{
        s => [<<"<title>">>, <<"</title>">>],
        d => [{undefined, fun() -> <<"Hello">> end, {my_layout, 10}}],
        f => <<"test">>,
        diff => false
    },
    ?assertEqual(
        <<"<title>Hello</title>">>,
        iolist_to_binary(arizona_render:render_to_iolist(Layout))
    ).

render_ssr_nested_3tuple_dyn_az(Config) when is_list(Config) ->
    Inner = #{
        s => [<<"<b>">>, <<"</b>">>],
        d => [{<<"i">>, fun() -> <<"bold">> end, {inner_mod, 3}}],
        f => <<"test">>
    },
    Outer = #{
        s => [<<"<div>">>, <<"</div>">>],
        d => [{<<"0">>, Inner, {outer_mod, 1}}],
        f => <<"test">>
    },
    Result = iolist_to_binary(arizona_render:render_to_iolist(Outer)),
    ?assertEqual(<<"<div><b>bold</b></div>">>, Result).

%% =============================================================================
%% 9. diff => false (nodiff) render tests
%% =============================================================================

no_diff_render(Config) when is_list(Config) ->
    %% render/1 fully evaluates template with diff => false; snapshot preserves flag
    T = #{
        s => [<<"<div az=\"0\">">>, <<"</div>">>],
        d => [{<<"0">>, fun() -> <<"hello">> end}],
        diff => false,
        f => <<"test">>
    },
    {HTML, Snap} = arizona_render:render(T),
    ?assertEqual(<<"<div az=\"0\">hello</div>">>, iolist_to_binary(HTML)),
    ?assertEqual(false, maps:get(diff, Snap)).

no_diff_render_with_views(Config) when is_list(Config) ->
    %% render/2 preserves diff => false in nested snapshot
    T = #{
        s => [<<"<div az=\"0\">">>, <<"</div>">>],
        d => [
            {<<"0">>, #{
                s => [<<"<p>">>, <<"</p>">>],
                d => [{<<"i">>, fun() -> <<"content">> end}],
                diff => false,
                f => <<"test">>
            }}
        ],
        f => <<"test">>
    },
    {HTML, Snap, _Views} = arizona_render:render(T, #{}),
    ?assertEqual(<<"<div az=\"0\"><p>content</p></div>">>, iolist_to_binary(HTML)),
    [{<<"0">>, NestedSnap}] = maps:get(d, Snap),
    ?assertEqual(false, maps:get(diff, NestedSnap)).

no_diff_ssr_nested(Config) when is_list(Config) ->
    %% render/1 on a template with a nested diff => false sub-template
    %% verifies HTML is correct and snapshot propagates the flag
    T = #{
        s => [<<"<div az=\"0\">">>, <<" ">>, <<"</div>">>],
        d => [
            {<<"0">>, fun() -> <<"title">> end},
            {<<"1">>, #{
                s => [<<"<p>">>, <<"</p>">>],
                d => [{<<"i">>, fun() -> <<"frozen">> end}],
                diff => false,
                f => <<"test">>
            }}
        ],
        f => <<"test">>
    },
    {HTML, Snap} = arizona_render:render(T),
    ?assertEqual(<<"<div az=\"0\">title <p>frozen</p></div>">>, iolist_to_binary(HTML)),
    %% Top-level snapshot has no diff flag (only inner nested does)
    ?assertEqual(error, maps:find(diff, Snap)),
    %% Nested sub-template snapshot preserves diff => false
    [{<<"0">>, <<"title">>}, {<<"1">>, NestedSnap}] = maps:get(d, Snap),
    ?assertEqual(false, maps:get(diff, NestedSnap)),
    %% Nested dynamics are evaluated
    [{<<"i">>, <<"frozen">>}] = maps:get(d, NestedSnap).

error_page_format_reason_unwraps_loc(Config) when is_list(Config) ->
    Bindings = #{
        class => error,
        reason => {arizona_loc, {handler_mod, 10}, badarg},
        stacktrace => [{m, f, 0, []}]
    },
    Result = iolist_to_binary(
        arizona_render:render_to_iolist(arizona_error_page:render(#{error_info => Bindings}))
    ),
    ?assertMatch({match, _}, re:run(Result, <<"badarg">>)),
    ?assertMatch({match, _}, re:run(Result, <<"handler_mod:10:">>)).

error_page_error_summaries(Config) when is_list(Config) ->
    Base = #{stacktrace => [{m, f, 0, []}]},
    RenderError = fun(EI) ->
        iolist_to_binary(
            arizona_render:render_to_iolist(
                arizona_error_page:render(#{error_info => EI})
            )
        )
    end,
    FunctionClause = RenderError(Base#{class => error, reason => function_clause}),
    ?assertMatch({match, _}, re:run(FunctionClause, <<"function_clause">>)),
    Badmatch = RenderError(Base#{class => error, reason => {badmatch, foo}}),
    ?assertMatch({match, _}, re:run(Badmatch, <<"no match of right hand side">>)),
    CaseClause = RenderError(Base#{class => error, reason => {case_clause, bar}}),
    ?assertMatch({match, _}, re:run(CaseClause, <<"no case clause matching">>)),
    Badarg = RenderError(Base#{class => error, reason => badarg}),
    ?assertMatch({match, _}, re:run(Badarg, <<"bad argument">>)).

%% =============================================================================
%% 11. Unified layout (nodiff) render tests
%% =============================================================================

%% render_to_iolist/1 with undefined Az dynamics -- evaluates correctly.
render_to_iolist_nodiff(Config) when is_list(Config) ->
    Tmpl = #{
        s => [<<"<html><title>">>, <<"</title></html>">>],
        d => [{undefined, fun() -> <<"Hello">> end, {my_layout, 1}}],
        f => <<"abc">>,
        diff => false
    },
    ?assertEqual(
        <<"<html><title>Hello</title></html>">>,
        iolist_to_binary(arizona_render:render_to_iolist(Tmpl))
    ).

%% render_to_iolist/1 with multiple undefined Az dynamics.
render_to_iolist_nodiff_multi(Config) when is_list(Config) ->
    Tmpl = #{
        s => [<<"<head>">>, <<"</head><body>">>, <<"</body>">>],
        d => [
            {undefined, fun() -> <<"Title">> end, {my_layout, 1}},
            {undefined, fun() -> <<"Content">> end, {my_layout, 2}}
        ],
        f => <<"abc">>,
        diff => false
    },
    ?assertEqual(
        <<"<head>Title</head><body>Content</body>">>,
        iolist_to_binary(arizona_render:render_to_iolist(Tmpl))
    ).

%% render_to_iolist/1 with static-only nodiff template.
render_to_iolist_nodiff_static(Config) when is_list(Config) ->
    Tmpl = #{
        s => [<<"<p>Hello</p>">>],
        d => [],
        f => <<"abc">>,
        diff => false
    },
    ?assertEqual(
        <<"<p>Hello</p>">>,
        iolist_to_binary(arizona_render:render_to_iolist(Tmpl))
    ).

%% render/1 on nodiff template -- snapshot preserves diff => false and undefined Az.
render_nodiff_snapshot(Config) when is_list(Config) ->
    Tmpl = #{
        s => [<<"<p>">>, <<"</p>">>],
        d => [{undefined, fun() -> <<"hi">> end, {m, 1}}],
        f => <<"xyz">>,
        diff => false
    },
    {HTML, Snap} = arizona_render:render(Tmpl),
    ?assertEqual(<<"<p>hi</p>">>, iolist_to_binary(HTML)),
    ?assertEqual([{undefined, <<"hi">>}], maps:get(d, Snap)),
    ?assertEqual(false, maps:get(diff, Snap)),
    ?assertEqual(<<"xyz">>, maps:get(f, Snap)).

%% render/2 on nodiff template -- snapshot has deps and preserves diff => false.
render_v_nodiff_snapshot(Config) when is_list(Config) ->
    Tmpl = #{
        s => [<<"<p>">>, <<"</p>">>],
        d => [{undefined, fun() -> <<"hi">> end, {m, 1}}],
        f => <<"xyz">>,
        diff => false
    },
    {HTML, Snap, Views} = arizona_render:render(Tmpl, #{}),
    ?assertEqual(<<"<p>hi</p>">>, iolist_to_binary(HTML)),
    ?assertEqual([{undefined, <<"hi">>}], maps:get(d, Snap)),
    ?assertEqual(false, maps:get(diff, Snap)),
    ?assertEqual(#{}, Views).

%% diff/2 short-circuits on nodiff template -- no ops, snapshot unchanged.
diff_nodiff_shortcircuit(Config) when is_list(Config) ->
    OldSnap = #{
        s => [<<"<p>">>, <<"</p>">>],
        d => [{undefined, <<"old">>}],
        f => <<"xyz">>,
        diff => false
    },
    NewTmpl = #{
        s => [<<"<p>">>, <<"</p>">>],
        d => [{undefined, fun() -> <<"new">> end, {m, 1}}],
        f => <<"xyz">>,
        diff => false
    },
    {Ops, Snap} = arizona_diff:diff(NewTmpl, OldSnap),
    ?assertEqual([], Ops),
    ?assertEqual(OldSnap, Snap).

%% diff/4 short-circuits on nodiff -- dynamics never evaluated.
diff4_nodiff_shortcircuit(Config) when is_list(Config) ->
    OldSnap = #{
        s => [<<"<p>">>, <<"</p>">>],
        d => [{undefined, <<"old">>}],
        deps => [#{x => true}],
        f => <<"xyz">>,
        diff => false
    },
    NewTmpl = #{
        s => [<<"<p>">>, <<"</p>">>],
        d => [{undefined, fun() -> error(should_not_be_called) end, {m, 1}}],
        f => <<"xyz">>,
        diff => false
    },
    {Ops, Snap, Views} = arizona_diff:diff(NewTmpl, OldSnap, #{}, #{x => true}),
    ?assertEqual([], Ops),
    ?assertEqual(OldSnap, Snap),
    ?assertEqual(#{}, Views).

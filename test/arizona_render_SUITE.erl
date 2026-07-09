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
    error_page_title_is_user_frame_location/1,
    error_page_format_error_sentence_in_body/1,
    error_page_title_falls_back_to_arizona_loc/1,
    error_page_typical_crash/1,
    error_page_undef/1,
    fingerprint_absent_when_no_f/1,
    fingerprint_payload_with_f/1,
    fingerprint_payload_without_f/1,
    fingerprint_payload_local_slots/1,
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
    render_ssr_nested_failure_preserves_deepest_loc/1,
    render_text/1,
    repeated_stateless_distinct_az/1,
    repeated_stateless_diff_targets_each/1,
    nested_stateless_scoped/1,
    each_in_stateless_distinct_containers/1,
    stateful_inside_stateless_ssr_matches_live/1,
    stateless_descriptor_from_conditional_live/1,
    stateful_descriptor_from_conditional_live/1,
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
    ssr_each_map/1,
    nested_each_render/1,
    ssr_layouts_empty_list/1,
    ssr_layouts_nest_outer_first/1,
    ssr_page_with_child/1,
    ssr_nested_local/1,
    ssr_local_app/1,
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
            render_with_views_no_children,
            repeated_stateless_distinct_az,
            repeated_stateless_diff_targets_each,
            nested_stateless_scoped,
            each_in_stateless_distinct_containers,
            stateful_inside_stateless_ssr_matches_live,
            stateless_descriptor_from_conditional_live,
            stateful_descriptor_from_conditional_live
        ]},
        {ssr, [parallel], [
            ssr_counter,
            ssr_counter_with_bindings,
            ssr_counter_custom_id,
            ssr_page_with_child,
            ssr_nested_local,
            ssr_local_app,
            ssr_about_page,
            ssr_each_map,
            nested_each_render,
            ssr_layouts_nest_outer_first,
            ssr_layouts_empty_list,
            resolve_id_binary,
            resolve_id_template
        ]},
        {fingerprint, [parallel], [
            fingerprint_payload_with_f,
            fingerprint_payload_without_f,
            fingerprint_payload_local_slots,
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
            render_ssr_nested_failure_preserves_deepest_loc,
            error_page_format_reason_unwraps_loc,
            error_page_title_is_user_frame_location,
            error_page_format_error_sentence_in_body,
            error_page_title_falls_back_to_arizona_loc,
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
    %% The nested sub-template is namespaced by content slot `0` (scope_slot), so
    %% its inner az becomes `0-i` and its fingerprint `0-test`.
    ?assertEqual(
        #{
            s => [<<"<p az=\"0\">">>, <<"</p>">>],
            d => [
                {<<"0">>, #{s => [<<"a ">>, <<>>], d => [{<<"0-i">>, <<"b">>}], f => <<"0-test">>}}
            ],
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
        arizona_render:render_view_to_iolist(
            arizona_page,
            #{bindings => #{title => <<"Welcome">>}, layouts => [{arizona_layout, render}]}
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

%% A ?local inside nested stateful children renders end to end: each child gets
%% its own az-view + id (so its ?local is scoped to the child), and both the
%% children's and the parent's ?local slots render their SSR initial + descriptor.
ssr_nested_local(Config) when is_list(Config) ->
    HTML = iolist_to_binary(
        arizona_render:render_view_to_iolist(
            arizona_local_nested,
            #{bindings => #{title => <<"Nested">>}, layouts => [{arizona_layout, render}]}
        )
    ),
    %% Both stateful children render with their own az-view + id.
    ?assertNotEqual(nomatch, binary:match(HTML, <<"az-view id=\"child_a\"">>)),
    ?assertNotEqual(nomatch, binary:match(HTML, <<"az-view id=\"child_b\"">>)),
    %% Each child's ?local("note") renders its SSR initial inside an az-local element.
    ?assertNotEqual(nomatch, binary:match(HTML, <<"untouched<!--/az-->">>)),
    ?assertNotEqual(nomatch, binary:match(HTML, <<"az-local=">>)),
    %% The parent's ?local("pnote") renders too.
    ?assertNotEqual(nomatch, binary:match(HTML, <<"parent<!--/az-->">>)),
    %% Children show the parent-propagated initial label.
    ?assertNotEqual(nomatch, binary:match(HTML, <<"v1<!--/az-->">>)).

%% The fully client-only app renders: nested stateful widgets (own az-view + id),
%% a stateless tab bar, and interpolated/content ?local slots, all with their SSR
%% initials -- the page is interactive purely via client-owned slots.
ssr_local_app(Config) when is_list(Config) ->
    HTML = iolist_to_binary(
        arizona_render:render_view_to_iolist(
            arizona_local_app,
            #{bindings => #{title => <<"App">>}, layouts => [{arizona_layout, render}]}
        )
    ),
    %% Nested stateful widgets each render with their own az-view + id.
    ?assertNotEqual(nomatch, binary:match(HTML, <<"az-view id=\"widget_a\"">>)),
    ?assertNotEqual(nomatch, binary:match(HTML, <<"az-view id=\"widget_b\"">>)),
    %% Stateless tab bar + the theme wrapper render their initial ?local values.
    ?assertNotEqual(nomatch, binary:match(HTML, <<"data-active=\"home\"">>)),
    ?assertNotEqual(nomatch, binary:match(HTML, <<"class=\"app theme-light\"">>)),
    %% Each widget's status ?local renders its initial (interpolated class).
    ?assertNotEqual(nomatch, binary:match(HTML, <<"status status-idle">>)),
    ?assertNotEqual(nomatch, binary:match(HTML, <<"az-local=">>)).

ssr_layouts_nest_outer_first(Config) when is_list(Config) ->
    HTML = iolist_to_binary(
        arizona_render:render_view_to_iolist(
            arizona_about,
            #{
                bindings => #{title => <<"Nested">>},
                layouts => [{arizona_outer_layout, render}, {arizona_inner_layout, render}]
            }
        )
    ),
    {OuterOpenAt, _} = binary:match(HTML, <<"<outer>">>),
    {InnerOpenAt, _} = binary:match(HTML, <<"<inner>">>),
    {InnerCloseAt, _} = binary:match(HTML, <<"</inner>">>),
    {OuterCloseAt, _} = binary:match(HTML, <<"</outer>">>),
    ?assert(OuterOpenAt < InnerOpenAt),
    ?assert(InnerCloseAt < OuterCloseAt),
    ?assert(InnerOpenAt < InnerCloseAt).

ssr_layouts_empty_list(Config) when is_list(Config) ->
    HTML = iolist_to_binary(
        arizona_render:render_view_to_iolist(
            arizona_about,
            #{bindings => #{title => <<"Bare">>}, layouts => []}
        )
    ),
    ?assertEqual(nomatch, binary:match(HTML, <<"<!DOCTYPE html>">>)),
    ?assertNotEqual(nomatch, binary:match(HTML, <<"az-view id=\"about-page\"">>)).

ssr_each_map(Config) when is_list(Config) ->
    HTML = iolist_to_binary(arizona_render:render_to_iolist(arizona_each_map, #{})),
    %% Both map entries (order not guaranteed) should render as <li>key: value</li>.
    ?assertNotEqual(nomatch, binary:match(HTML, <<"<li">>)),
    ?assertNotEqual(nomatch, binary:match(HTML, <<"a<!--/az-->">>)),
    ?assertNotEqual(nomatch, binary:match(HTML, <<"b<!--/az-->">>)),
    ?assertNotEqual(nomatch, binary:match(HTML, <<"1<!--/az-->">>)),
    ?assertNotEqual(nomatch, binary:match(HTML, <<"2<!--/az-->">>)).

%% each-in-each (arizona_bench_nested_each): an outer ?each over sections, each section
%% wrapping an inner ?each over its items. Grounds the synthetic nested-fingerprint tests
%% in a real two-level nested render.
nested_each_render(Config) when is_list(Config) ->
    %% Uses the fixture's default 10x10 sections (mount-provided).
    HTML = iolist_to_binary(
        arizona_render:render_to_iolist(arizona_bench_nested_each, #{})
    ),
    %% Outer each renders section titles; inner each renders each section's items.
    ?assertNotEqual(nomatch, binary:match(HTML, ~"Section 1")),
    ?assertNotEqual(nomatch, binary:match(HTML, ~"Section 10")),
    ?assertNotEqual(nomatch, binary:match(HTML, ~"Item 1-1")),
    ?assertNotEqual(nomatch, binary:match(HTML, ~"Item 10-10")),
    ?assertNotEqual(nomatch, binary:match(HTML, ~"class=\"section\"")),
    ?assertNotEqual(nomatch, binary:match(HTML, ~"<ul")).

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

%% Regression: a view that renders the SAME stateless render function twice must
%% give each instance a DISTINCT az id -- otherwise every diff op resolves (via
%% querySelector first-match) onto the first DOM instance. Each card's inner
%% `<span class="card">` az must be namespaced by its enclosing slot az.
repeated_stateless_distinct_az(Config) when is_list(Config) ->
    HTML = iolist_to_binary(
        arizona_render:render_view_to_iolist(arizona_repeated_stateless, #{
            bindings => #{a => <<"AAA">>, b => <<"BBB">>}
        })
    ),
    %% Both cards render their labels.
    ?assertNotEqual(nomatch, binary:match(HTML, <<"AAA">>)),
    ?assertNotEqual(nomatch, binary:match(HTML, <<"BBB">>)),
    %% Every card az id appears exactly once (no shared/duplicated diff targets).
    CardAzIds = card_az_ids(HTML),
    ?assertEqual(lists:sort(CardAzIds), lists:usort(CardAzIds)),
    %% There are two card spans, so two distinct card az ids.
    ?assertEqual(2, length(CardAzIds)),
    %% The ids carry no bare-colon-then-content that would trip the client's
    %% first-colon slot-suffix fallback: the slot prefix is colon-free.
    lists:foreach(
        fun(Az) -> ?assertEqual(nomatch, binary:match(Az, <<":">>)) end,
        CardAzIds
    ).

%% Regression: diffing a change to only one instance must emit an op whose target
%% is that instance's az -- and the two instances' targets must differ.
repeated_stateless_diff_targets_each(Config) when is_list(Config) ->
    B0 = #{id => <<"panel">>, a => <<"AAA">>, b => <<"BBB">>},
    Tmpl0 = arizona_repeated_stateless:render(B0),
    {_HTML, Snap, _V} = arizona_render:render(Tmpl0, #{}),
    %% Change only card A's label.
    {OpsA, _} = arizona_diff:diff(arizona_repeated_stateless:render(B0#{a => <<"AAA2">>}), Snap),
    %% Change only card B's label.
    {OpsB, _} = arizona_diff:diff(arizona_repeated_stateless:render(B0#{b => <<"BBB2">>}), Snap),
    [[_, TargetA, <<"AAA2">>]] = OpsA,
    [[_, TargetB, <<"BBB2">>]] = OpsB,
    %% Each op targets its own instance, and the two targets are distinct.
    ?assertNotEqual(TargetA, TargetB).

%% Regression: a `?stateful` child nested inside a `?stateless` component keeps
%% its own view-id boundary -- its inner az ids must stay unscoped so the SSR
%% HTML (which the client reuses on connect) matches the live snapshot the diff
%% targets. If the SSR path scoped them, the child's ops would miss the DOM and
%% its updates would be silently dropped.
stateful_inside_stateless_ssr_matches_live(Config) when is_list(Config) ->
    Tmpl = arizona_stateless_stateful_nest:page(#{id => <<"host">>}),
    SSR = iolist_to_binary(arizona_render:render_to_iolist(Tmpl)),
    {HTML, _Snap, _Views} = arizona_render:render(Tmpl, #{}),
    Live = iolist_to_binary(HTML),
    ?assertEqual(Live, SSR),
    %% The stateful child's own element renders (its view-id boundary is intact).
    ?assertNotEqual(nomatch, binary:match(SSR, <<"az-view id=\"c1\"">>)).

%% Regression (#602 regression): a `?stateless` descriptor returned from a
%% conditional-tail content slot must render structurally on the LIVE path,
%% exactly as on SSR -- not be escaped as a scalar and crash in `to_bin/1` with
%% `bad_template_value`. The SSR-only path always passed, so this test must
%% exercise `render/2` (the live render) and the diff, not just SSR.
stateless_descriptor_from_conditional_live(Config) when is_list(Config) ->
    B = #{user => <<"ada">>},
    Tmpl = arizona_conditional_descriptor:stateless_nav(B),
    SSR = iolist_to_binary(arizona_render:render_to_iolist(Tmpl)),
    %% Live render must not crash and must match the SSR bytes.
    {HTML, Snap, _Views} = arizona_render:render(Tmpl, #{}),
    ?assertEqual(SSR, iolist_to_binary(HTML)),
    %% The menu child rendered structurally, namespaced by the enclosing slot az.
    ?assertNotEqual(nomatch, binary:match(SSR, <<"class=\"menu\"">>)),
    ?assertNotEqual(nomatch, binary:match(SSR, <<"ada">>)),
    %% A diff changing the menu's inner value patches its scoped inner az.
    {Ops, _} = arizona_diff:diff(
        arizona_conditional_descriptor:stateless_nav(#{user => <<"bob">>}), Snap
    ),
    ?assertMatch([[_Code, _Target, <<"bob">>]], Ops),
    %% Switching to the anonymous branch re-renders the slot in place.
    {Ops2, _} = arizona_diff:diff(
        arizona_conditional_descriptor:stateless_nav(#{user => undefined}), Snap
    ),
    ?assertMatch([[_Code, _Target, #{<<"s">> := [_ | _]}]], Ops2).

%% Regression: a `?stateful` descriptor returned from a conditional-tail content
%% slot renders structurally on both paths. It never regressed (a stateful child
%% snapshot is a plain map, which `mark_esc` passes through), but the idiom is
%% documented, so pin it -- and confirm the fix for the stateless case did not
%% disturb it.
stateful_descriptor_from_conditional_live(Config) when is_list(Config) ->
    B = #{user => <<"ada">>},
    Tmpl = arizona_conditional_descriptor:stateful_nav(B),
    SSR = iolist_to_binary(arizona_render:render_to_iolist(Tmpl)),
    {HTML, _Snap, Views} = arizona_render:render(Tmpl, #{}),
    ?assertEqual(SSR, iolist_to_binary(HTML)),
    %% The stateful child keeps its own view-id boundary and spawns a child view.
    ?assertNotEqual(nomatch, binary:match(SSR, <<"az-view id=\"c1\"">>)),
    ?assert(maps:is_key(<<"c1">>, Views)).

%% Regression: a repeated stateless whose body has an inline conditional, a
%% client-owned `?local`, and a nested stateless grandchild. With no `?each`,
%% every scoped az across the whole subtree of both instances is distinct, the
%% SSR matches the live snapshot, and a change to one instance hits targets
%% disjoint from the other.
nested_stateless_scoped(Config) when is_list(Config) ->
    B0 = #{id => <<"host">>, x => <<"X">>, y => <<"Y">>, flag => true},
    Tmpl = arizona_nested_stateless:render(B0),
    SSR = iolist_to_binary(arizona_render:render_to_iolist(Tmpl)),
    {HTML, Snap, _Views} = arizona_render:render(Tmpl, #{}),
    ?assertEqual(iolist_to_binary(HTML), SSR),
    Ids = all_az_ids(SSR),
    ?assertEqual(lists:sort(Ids), lists:usort(Ids)),
    TA = diff_targets(arizona_nested_stateless:render(B0#{x => <<"X2">>}), Snap),
    TB = diff_targets(arizona_nested_stateless:render(B0#{y => <<"Y2">>}), Snap),
    ?assertNotEqual([], TA),
    ?assertNotEqual([], TB),
    ?assertEqual([], [T || T <- TA, lists:member(T, TB)]).

%% Regression: a repeated stateless whose body holds an `?each`. The each
%% CONTAINER is scoped per instance (distinct `<ul>` az), while item ids stay
%% container-relative; a diff routes container ops to the right instance.
each_in_stateless_distinct_containers(Config) when is_list(Config) ->
    B0 = #{id => <<"host">>, xs => [<<"x1">>, <<"x2">>], ys => [<<"y1">>]},
    Tmpl = arizona_each_in_stateless:render(B0),
    SSR = iolist_to_binary(arizona_render:render_to_iolist(Tmpl)),
    {HTML, Snap, _Views} = arizona_render:render(Tmpl, #{}),
    ?assertEqual(iolist_to_binary(HTML), SSR),
    Containers = ul_az_ids(SSR),
    ?assertEqual(2, length(Containers)),
    ?assertEqual(lists:sort(Containers), lists:usort(Containers)),
    %% Growing card A's list re-renders both eaches wholesale (plain diff, no
    %% dep-skip), but to DISTINCT container targets -- pre-fix both collided.
    Targets = diff_targets(
        arizona_each_in_stateless:render(B0#{xs => [<<"x1">>, <<"x2">>, <<"x3">>]}), Snap
    ),
    ?assertEqual(lists:sort(Containers), lists:usort(Targets)).

%% Pull every `az="..."` id off a `<span class="card">` element in the SSR HTML.
card_az_ids(HTML) ->
    Parts = binary:split(HTML, <<"<span az=\"">>, [global]),
    [
        Az
     || Part <- tl(Parts),
        {P, _} <- [binary:match(Part, <<"\"">>)],
        <<Az:P/binary, Rest/binary>> <- [Part],
        binary:match(Rest, <<"class=\"card\"">>) =/= nomatch
    ].

%% Every `az="..."` id in the HTML, in document order.
all_az_ids(HTML) ->
    [
        Az
     || Part <- tl(binary:split(HTML, <<"az=\"">>, [global])),
        {P, _} <- [binary:match(Part, <<"\"">>)],
        <<Az:P/binary, _/binary>> <- [Part]
    ].

%% Every `<ul az="...">` container id in the HTML.
ul_az_ids(HTML) ->
    [
        Az
     || Part <- tl(binary:split(HTML, <<"<ul az=\"">>, [global])),
        {P, _} <- [binary:match(Part, <<"\"">>)],
        <<Az:P/binary, _/binary>> <- [Part]
    ].

%% Diff `Tmpl` against `Snap` and collect the op targets (op = [Code, Target|_]).
diff_targets(Tmpl, Snap) ->
    {Ops, _} = arizona_diff:diff(Tmpl, Snap),
    [Target || [_Code, Target | _] <- Ops].

%% =============================================================================
%% 6. SSR about page
%% =============================================================================

ssr_about_page(Config) when is_list(Config) ->
    HTML = iolist_to_binary(
        arizona_render:render_view_to_iolist(
            arizona_about,
            #{bindings => #{title => <<"About">>}, layouts => [{arizona_layout, render}]}
        )
    ),
    ?assertMatch({_, _}, binary:match(HTML, <<"<title>About</title>">>)),
    ?assertMatch({_, _}, binary:match(HTML, <<"az-view id=\"about-page\"">>)),
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

%% A ?local slot inside a fingerprinted snapshot (e.g. a nested stateless/stateful
%% child) is unwrapped by render_fp_val's az_local clauses: an attribute target
%% becomes the rendered attribute, a boolean false vanishes, content renders its
%% value. The slot is wire-rendered once and never diffed thereafter.
fingerprint_payload_local_slots(Config) when is_list(Config) ->
    Snap = #{
        s => [<<"<div">>, <<"><dialog">>, <<"><span>">>, <<"</span></dialog></div>">>],
        d => [
            {<<"0">>, #{
                diff => false, az_local => ~"tab", target => {attr, ~"data-active"}, v => ~"home"
            }},
            {<<"1">>, #{diff => false, az_local => ~"open", target => {attr, ~"open"}, v => false}},
            {<<"2">>, #{diff => false, az_local => ~"title", v => ~"hi"}}
        ],
        f => <<"fp_local">>
    },
    ?assertEqual(
        #{
            <<"f">> => <<"fp_local">>,
            <<"s">> => [<<"<div">>, <<"><dialog">>, <<"><span>">>, <<"</span></dialog></div>">>],
            <<"d">> => [<<" data-active=\"home\"">>, <<>>, <<"hi">>]
        },
        arizona_render:fingerprint_payload(Snap)
    ).

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
    %% Empty stack -> title falls back to arizona_loc's captured location.
    %% (When the stack has a user frame, the frame wins; that's covered
    %% in error_page_typical_crash and the format_error tests below.)
    Bindings = #{
        class => error,
        reason => {arizona_loc, {my_handler, 42}, {bad_template_value, {}}},
        stacktrace => []
    },
    Result = iolist_to_binary(
        arizona_render:render_to_iolist(arizona_error_page:render(#{error_info => Bindings}))
    ),
    ?assertMatch({match, _}, re:run(Result, <<"my_handler:42">>)),
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

%% Nested failures must surface the DEEPEST arizona_loc (the one closest to the
%% actual bug) rather than wrapping it again at each enclosing render frame.
render_ssr_nested_failure_preserves_deepest_loc(Config) when is_list(Config) ->
    Inner = #{
        s => [<<"<b>">>, <<"</b>">>],
        d => [{<<"i">>, fun() -> erlang:error({badkey, oops}) end, {inner_mod, 42}}],
        f => <<"test">>
    },
    Outer = #{
        s => [<<"<div>">>, <<"</div>">>],
        d => [{<<"0">>, Inner, {outer_mod, 7}}],
        f => <<"test">>
    },
    ?assertError(
        {arizona_loc, {inner_mod, 42}, {badkey, oops}},
        iolist_to_binary(arizona_render:render_to_iolist(Outer))
    ).

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
    %% Nested dynamics are evaluated; the inner az is namespaced by content slot
    %% `1` (scope_slot), so it reads back as `1-i`.
    [{<<"1-i">>, <<"frozen">>}] = maps:get(d, NestedSnap).

error_page_format_reason_unwraps_loc(Config) when is_list(Config) ->
    %% Top frame has no line -> title falls back to arizona_loc; body
    %% still renders the unwrapped reason via erl_error ("bad argument").
    Bindings = #{
        class => error,
        reason => {arizona_loc, {handler_mod, 10}, badarg},
        stacktrace => [{m, f, 0, []}]
    },
    Result = iolist_to_binary(
        arizona_render:render_to_iolist(arizona_error_page:render(#{error_info => Bindings}))
    ),
    ?assertMatch({match, _}, re:run(Result, <<"bad argument">>)),
    ?assertMatch({match, _}, re:run(Result, <<"handler_mod:10">>)).

%% --- format_error/2 sentence surfaces in the dev page ---

error_page_title_is_user_frame_location(Config) when is_list(Config) ->
    %% Title surfaces the user's nearest call site -- the first stack
    %% frame outside arizona_* and OTP. Here a user view's render fun
    %% calls arizona_template:get/2 which raises; the user wants to see
    %% their own line, not the framework frame.
    Bindings = #{
        class => error,
        reason => missing_binding,
        stacktrace => [
            {arizona_template, get, [missing_key, #{a => 1, b => 2}], [
                {file, "src/arizona_template.erl"},
                {line, 153},
                {error_info, #{module => arizona_template}}
            ]},
            {asobi_user_view, render, 1, [
                {file, "src/asobi_user_view.erl"},
                {line, 42}
            ]}
        ]
    },
    Result = iolist_to_binary(
        arizona_render:render_to_iolist(arizona_error_page:render(#{error_info => Bindings}))
    ),
    Title = slice_between(Result, <<"<h1">>, <<"</h1>">>),
    %% User frame wins: title shows asobi_user_view:42, not arizona_template:153.
    ?assertNotEqual(nomatch, binary:match(Title, <<"asobi_user_view:42">>)),
    ?assertEqual(nomatch, binary:match(Title, <<"arizona_template">>)),
    %% format_error sentence stays out of the title (it's in the body).
    ?assertEqual(nomatch, binary:match(Title, <<"binding missing_key not found">>)).

error_page_format_error_sentence_in_body(Config) when is_list(Config) ->
    %% Same setup; the reason `<pre>` carries the FULL format_error
    %% sentence including the detail clauses the title trims out.
    Bindings = error_info_for_missing_binding(),
    Result = iolist_to_binary(
        arizona_render:render_to_iolist(arizona_error_page:render(#{error_info => Bindings}))
    ),
    Body = slice_between(Result, <<"<pre">>, <<"</pre>">>),
    ?assertNotEqual(nomatch, binary:match(Body, <<"binding missing_key not found">>)),
    ?assertNotEqual(nomatch, binary:match(Body, <<"Available bindings:">>)),
    ?assertNotEqual(nomatch, binary:match(Body, <<"Use arizona_template">>)).

error_page_title_falls_back_to_arizona_loc(Config) when is_list(Config) ->
    %% When the whole stack is framework/OTP and frames carry no line
    %% the user can navigate to, the title falls back to arizona_loc's
    %% captured location. The format_error sentence stays in the body.
    Bindings = #{
        class => error,
        reason => {arizona_loc, {my_view, 17}, missing_binding},
        stacktrace => [
            {arizona_template, get, [missing_key, #{a => 1}], [
                {error_info, #{module => arizona_template}}
            ]}
        ]
    },
    Result = iolist_to_binary(
        arizona_render:render_to_iolist(arizona_error_page:render(#{error_info => Bindings}))
    ),
    Title = slice_between(Result, <<"<h1">>, <<"</h1>">>),
    ?assertNotEqual(nomatch, binary:match(Title, <<"my_view:17">>)),
    ?assertEqual(nomatch, binary:match(Title, <<"binding missing_key not found">>)),
    Body = slice_between(Result, <<"<pre">>, <<"</pre>">>),
    ?assertNotEqual(nomatch, binary:match(Body, <<"binding missing_key not found">>)).

%% Builds an `error_info` map mirroring what arizona_template:get/2 raises
%% when a binding is absent: reason = atom, top stack frame carries the
%% args + an `error_info` annotation pointing at arizona_template (which
%% has the format_error/2 clause).
error_info_for_missing_binding() ->
    Bindings = #{a => 1, b => 2},
    Stack = [
        {arizona_template, get, [missing_key, Bindings], [
            {file, "src/arizona_template.erl"},
            {line, 153},
            {error_info, #{module => arizona_template}}
        ]}
    ],
    #{class => error, reason => missing_binding, stacktrace => Stack}.

%% Returns the substring between `Open` and the next `Close` after it.
%% The opening tag itself plus any attributes are included up to the
%% matching close. Used to scope assertions to a particular HTML element
%% in test rendered output.
slice_between(Bin, Open, Close) ->
    [_, Tail] = binary:split(Bin, Open),
    [Inner | _] = binary:split(Tail, Close),
    Inner.

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

-module(arizona_select_value_SUITE).
-moduledoc """
A `value` on an HTML `<select>` selects the option whose value equals it.

HTML has no such attribute: the parser ignores it and the page loads on the first
option, while a diff writes the `.value` property. So the transform marks the matching
option `selected` instead, at first render and on every diff, wherever the option is
written (see `arizona_select_value`). An option it cannot see -- from a component,
a runtime value, raw markup -- is marked by the SSR render in that child's own output,
and the select keeps its value for the client to apply after.
""".
-include_lib("stdlib/include/assert.hrl").
-include("arizona_wire.hrl").

-export([all/0, groups/0]).
-export([
    ssr_marks_the_chosen_option/1,
    live_render_marks_the_chosen_option/1,
    no_value_attribute_on_the_select/1,
    option_without_value_uses_its_text/1,
    multiple_selects_every_listed_value/1,
    multiple_decided_at_runtime/1,
    multiple_is_any_value_but_false/1,
    nothing_selected_without_a_match/1,
    duplicate_values_mark_every_match/1,
    diff_moves_selected_to_the_new_option/1,
    diff_is_fine_grained/1,
    diff_of_unrelated_binding_is_empty/1,
    stream_item_update_keeps_selection/1,
    static_generation_marks_the_chosen_option/1,
    nodiff_select_marks_the_chosen_option/1,
    local_value_loads_on_its_initial/1,
    select_inside_each_item/1,
    json_value_prop_is_unchanged/1,
    opaque_ssr_marks_the_chosen_option/1,
    opaque_select_keeps_its_value/1,
    opaque_multiple_marks_every_listed_value/1,
    opaque_static_generation_marks_the_chosen_option/1,
    opaque_live_render_is_left_to_the_client/1,
    opaque_diff_ships_the_value/1,
    mark_choices_reads_the_fragment/1,
    authored_selected_follows_the_value/1,
    fragment_reads_as_the_parser_preprocesses/1,
    opaque_child_compiles/1,
    opaque_child_is_marked_in_its_own_slot/1,
    opaque_each_keeps_its_item_diff/1,
    local_value_marks_render_once/1,
    local_value_marks_only_the_page/1,
    option_with_runtime_text_compiles/1,
    runtime_text_option_diffs/1,
    runtime_text_option_in_each_row/1,
    runtime_text_option_valued_by_its_markup/1,
    raw_template_option_is_valued_by_its_text/1,
    markup_text_tracks_what_its_slot_tracks/1,
    authored_selected_is_refused/1,
    choice_text_needs_markup_text/1,
    shadowing_parameter_is_renamed/1,
    shadowing_hoisted_read_is_refused/1,
    static_value_selects_at_compile_time/1,
    static_text_with_references_folds/1,
    literal_value_options_are_not_bound/1,
    opaque_options_bind_the_select_value/1,
    values_compare_as_the_parser_reads_them/1,
    helper_raw_option_value_is_its_text/1,
    helper_raw_value_compares_as_its_text/1,
    raw_option_value_reads_as_the_page_holds_it/1,
    raw_select_value_reads_as_the_page_holds_it/1,
    interpolated_raw_value_reads_as_one_text/1,
    interpolated_local_value_compiles/1,
    local_value_names_what_the_page_selects/1,
    multiple_local_value_is_a_json_array/1,
    local_value_parts_follow_runtime_multiple/1
]).

all() ->
    [{group, render}, {group, diff}, {group, compile}].

groups() ->
    [
        {render, [parallel], [
            ssr_marks_the_chosen_option,
            live_render_marks_the_chosen_option,
            no_value_attribute_on_the_select,
            option_without_value_uses_its_text,
            multiple_selects_every_listed_value,
            multiple_decided_at_runtime,
            multiple_is_any_value_but_false,
            nothing_selected_without_a_match,
            duplicate_values_mark_every_match,
            static_generation_marks_the_chosen_option,
            nodiff_select_marks_the_chosen_option,
            local_value_loads_on_its_initial,
            select_inside_each_item,
            json_value_prop_is_unchanged,
            opaque_ssr_marks_the_chosen_option,
            opaque_select_keeps_its_value,
            opaque_multiple_marks_every_listed_value,
            opaque_static_generation_marks_the_chosen_option,
            opaque_live_render_is_left_to_the_client,
            mark_choices_reads_the_fragment,
            authored_selected_follows_the_value,
            fragment_reads_as_the_parser_preprocesses,
            local_value_names_what_the_page_selects,
            multiple_local_value_is_a_json_array,
            local_value_parts_follow_runtime_multiple
        ]},
        {diff, [parallel], [
            diff_moves_selected_to_the_new_option,
            diff_is_fine_grained,
            diff_of_unrelated_binding_is_empty,
            stream_item_update_keeps_selection,
            runtime_text_option_diffs,
            runtime_text_option_in_each_row,
            runtime_text_option_valued_by_its_markup,
            raw_template_option_is_valued_by_its_text,
            markup_text_tracks_what_its_slot_tracks,
            opaque_diff_ships_the_value,
            opaque_each_keeps_its_item_diff,
            local_value_marks_render_once,
            local_value_marks_only_the_page
        ]},
        {compile, [parallel], [
            opaque_child_compiles,
            opaque_child_is_marked_in_its_own_slot,
            option_with_runtime_text_compiles,
            authored_selected_is_refused,
            choice_text_needs_markup_text,
            shadowing_parameter_is_renamed,
            shadowing_hoisted_read_is_refused,
            static_value_selects_at_compile_time,
            static_text_with_references_folds,
            literal_value_options_are_not_bound,
            opaque_options_bind_the_select_value,
            values_compare_as_the_parser_reads_them,
            helper_raw_option_value_is_its_text,
            helper_raw_value_compares_as_its_text,
            raw_option_value_reads_as_the_page_holds_it,
            raw_select_value_reads_as_the_page_holds_it,
            interpolated_raw_value_reads_as_one_text,
            interpolated_local_value_compiles
        ]}
    ].

%% --------------------------------------------------------------------
%% Render
%% --------------------------------------------------------------------

ssr_marks_the_chosen_option(Config) when is_list(Config) ->
    lists:foreach(
        fun(Shape) ->
            HTML = ssr(Shape, ~"b"),
            ?assertEqual([~"b"], selected_values(HTML), {Shape, HTML})
        end,
        shapes()
    ).

live_render_marks_the_chosen_option(Config) when is_list(Config) ->
    lists:foreach(
        fun(Shape) ->
            {HTML, _Snap, _Views} = arizona_render:render(tmpl(Shape, ~"c"), #{}),
            ?assertEqual([~"c"], selected_values(iolist_to_binary(HTML)), Shape)
        end,
        shapes()
    ).

%% The attribute HTML would ignore is not emitted at all, static or dynamic.
no_value_attribute_on_the_select(Config) when is_list(Config) ->
    lists:foreach(
        fun(Shape) ->
            HTML = ssr(Shape, ~"b"),
            ?assertEqual(
                nomatch, re:run(HTML, ~"<select[^>]* value[ =>]", [caseless]), {Shape, HTML}
            )
        end,
        shapes()
    ).

%% An option without a `value` is valued by its text, whitespace stripped.
option_without_value_uses_its_text(Config) when is_list(Config) ->
    HTML = iolist_to_binary(
        arizona_render:render_to_iolist(arizona_select_value:text_value(#{v => ~"a"}))
    ),
    ?assertMatch({match, _}, re:run(HTML, ~"<option[^>]* selected[^>]*> a </option>"), HTML),
    ?assertEqual(1, count_selected(HTML)),
    %% The server renders the value, so it owns each option's selection.
    ?assertMatch({match, _}, re:run(HTML, ~"<option[^>]* az-bound=\"selected\""), HTML).

%% A literal value, single or multiple, picks the same options on every render: no
%% option is the server's to correct.
literal_value_options_are_not_bound(Config) when is_list(Config) ->
    lists:foreach(
        fun({Mod, Attrs}) ->
            M = compile_module(
                raw_src(
                    Mod,
                    Attrs,
                    "{option, [{value, ~\"a\"}], [~\"A\"]}, {option, [{value, ~\"b\"}], [~\"B\"]}"
                )
            ),
            HTML = ssr_html(M, #{}),
            ?assertEqual([~"a"], selected_values(HTML), {Mod, HTML}),
            ?assertEqual(nomatch, binary:match(HTML, ~"az-bound"), {Mod, HTML})
        end,
        [
            {"sel_lit_single", "{value, ~\"a\"}"},
            {"sel_lit_multi", "multiple, {value, [~\"a\"]}"}
        ]
    ).

%% Options a component renders are out of the compiler's reach: the select keeps its
%% server-rendered value, which owns their selection (`az-bound="value"`). Options it
%% marks itself drop the value, each option owning its own.
opaque_options_bind_the_select_value(Config) when is_list(Config) ->
    Opaque = compile_module(
        raw_src(
            "sel_opaque_bound",
            "{value, arizona_template:get(v, Bindings)}",
            "arizona_template:stateless(fun(_) -> arizona_template:target(arizona_html, ["
            "{option, [{value, ~\"a\"}], [~\"A\"]}, "
            "{option, [{value, ~\"b\"}], [~\"B\"]}"
            "]) end, #{})"
        )
    ),
    OpaqueHTML = ssr_html(Opaque, #{v => ~"b"}),
    ?assertMatch({match, _}, re:run(OpaqueHTML, ~"<select[^>]* az-bound=\"value\""), OpaqueHTML),
    Reached = compile_module(
        raw_src(
            "sel_reached_bound",
            "{value, arizona_template:get(v, Bindings)}",
            "{option, [{value, ~\"a\"}], [~\"A\"]}, {option, [{value, ~\"b\"}], [~\"B\"]}"
        )
    ),
    ReachedHTML = ssr_html(Reached, #{v => ~"b"}),
    ?assertEqual(nomatch, re:run(ReachedHTML, ~"<select[^>]* az-bound"), ReachedHTML),
    ?assertMatch(
        {match, _}, re:run(ReachedHTML, ~"<option[^>]* az-bound=\"selected\""), ReachedHTML
    ).

multiple_selects_every_listed_value(Config) when is_list(Config) ->
    HTML = iolist_to_binary(
        arizona_render:render_to_iolist(arizona_select_value:multiple(#{vs => [~"a", ~"c"]}))
    ),
    ?assertEqual([~"a", ~"c"], selected_values(HTML)),
    %% A single value is a one-element selection.
    One = iolist_to_binary(
        arizona_render:render_to_iolist(arizona_select_value:multiple(#{vs => ~"b"}))
    ),
    ?assertEqual([~"b"], selected_values(One)).

multiple_decided_at_runtime(Config) when is_list(Config) ->
    Many = iolist_to_binary(
        arizona_render:render_to_iolist(
            arizona_select_value:multiple_dynamic(#{many => true, vs => [~"a", ~"b"]})
        )
    ),
    ?assertEqual([~"a", ~"b"], selected_values(Many)),
    %% Not multiple: the value is one text, here the chardata "ab", matching neither.
    One = iolist_to_binary(
        arizona_render:render_to_iolist(
            arizona_select_value:multiple_dynamic(#{many => false, vs => [~"a", ~"b"]})
        )
    ),
    ?assertEqual([], selected_values(One)).

%% `multiple` is present whatever it holds but `false`, as the attribute renders: a
%% `~"multiple"` makes the value a list of picks, on the options the transform marks
%% and on the value a select with a component keeps for the client.
multiple_is_any_value_but_false(Config) when is_list(Config) ->
    Many = iolist_to_binary(
        arizona_render:render_to_iolist(
            arizona_select_value:multiple_dynamic(#{many => ~"multiple", vs => [~"a", ~"b"]})
        )
    ),
    ?assertEqual([~"a", ~"b"], selected_values(Many), Many),
    Kept = iolist_to_binary(
        arizona_render:render_to_iolist(
            arizona_select_value:multiple_component_dynamic(
                (opaque_bindings(~"b"))#{many => ~"multiple"}
            )
        )
    ),
    ?assertEqual([~"a", ~"c"], selected_values(Kept), Kept),
    ?assertNotEqual(nomatch, binary:match(Kept, ~"value=\"[&quot;a&quot;,&quot;c&quot;]\""), Kept).

%% A value no option has selects none, as the `.value` property would; integers
%% compare by the text they render as.
nothing_selected_without_a_match(Config) when is_list(Config) ->
    ?assertEqual([], selected_values(ssr(literal, ~"z"))),
    ?assertEqual([], selected_values(ssr(literal, false))),
    Int = iolist_to_binary(
        arizona_render:render_to_iolist(
            arizona_select_value:each_list(#{v => 2, opts => [1, 2, 3]})
        )
    ),
    ?assertEqual([~"2"], selected_values(Int)).

%% Options sharing the value are all marked, the ones the transform reaches and the
%% ones a component renders alike: the browser then selects the last, as the client
%% does when it applies the value.
duplicate_values_mark_every_match(Config) when is_list(Config) ->
    Mod = compile_module(
        select_src_mod(
            "sel_dups",
            "{option, [{value, ~\"a\"}], [~\"A1\"]}, "
            "arizona_template:stateless(fun opts/1, #{o => [~\"a\", ~\"b\"]})"
        )
    ),
    HTML = iolist_to_binary(arizona_render:render_to_iolist(Mod:render(#{v => ~"a"}))),
    ?assertEqual([~"a", ~"a"], selected_values(HTML), HTML).

static_generation_marks_the_chosen_option(Config) when is_list(Config) ->
    Dir = filename:join(proplists:get_value(priv_dir, Config), "select_static"),
    {[Path], []} = arizona_static:generate(
        Dir, [{arizona_select_value_page, ~"index.html", #{bindings => #{v => ~"b"}}}]
    ),
    {ok, HTML} = file:read_file(Path),
    %% Literal, list ?each and stream ?each on `b`; the multiple select on its two;
    %% the component and the child view on `b`; the `?local` selects on their
    %% initials, the one whose `multiple` flips (off) on its one text.
    ?assertEqual(
        [
            ~"b",
            ~"b",
            ~"b",
            ~"a",
            ~"c",
            ~"b",
            ~"b",
            ~"a",
            ~"c",
            ~"a",
            ~"c",
            ~"b",
            ~"a",
            ~"c",
            ~"b",
            ~"ac"
        ],
        selected_values(HTML)
    ).

nodiff_select_marks_the_chosen_option(Config) when is_list(Config) ->
    HTML = ssr(nodiff, ~"b"),
    ?assertEqual([~"b"], selected_values(HTML), HTML),
    %% Rendered once: no diff markers inside, and no value on the select.
    ?assertEqual(nomatch, binary:match(HTML, [~" az=\"", ~"<!--az:"])),
    ?assertEqual(nomatch, re:run(HTML, ~"<select[^>]* value[ =>]")).

%% A `?local` value stays on the select for the client to move (it writes the
%% `.value` property); the page loads on the option its initial names.
local_value_loads_on_its_initial(Config) when is_list(Config) ->
    HTML = iolist_to_binary(
        arizona_render:render_to_iolist(arizona_select_value:local(#{}))
    ),
    ?assertEqual([~"b"], selected_values(HTML), HTML),
    ?assertMatch({match, _}, re:run(HTML, ~"<select[^>]* az-local="), HTML).

%% Each row renders its own select, valued from the row.
select_inside_each_item(Config) when is_list(Config) ->
    Mod = compile_module(
        "-module(sel_rows). -export([render/1]). render(Bindings) -> "
        "arizona_template:target(arizona_html, {table, [], [arizona_template:each(fun(Row) -> "
        "{tr, [], [{td, [], [{select, [{value, maps:get(pick, Row)}], ["
        "arizona_template:each(fun(O) -> {option, [{value, O}], [O]} end, "
        "arizona_template:get(opts, Bindings))]}]}]} end, "
        "arizona_template:get(rows, Bindings))]}). "
    ),
    B = #{opts => [~"a", ~"b"], rows => [#{pick => ~"b"}, #{pick => ~"a"}]},
    HTML = iolist_to_binary(arizona_render:render_to_iolist(Mod:render(B))),
    ?assertEqual([~"b", ~"a"], selected_values(HTML), HTML),
    {_, Snap0, V0} = arizona_render:render(Mod:render(B), #{}),
    B1 = B#{rows => [#{pick => ~"a"}, #{pick => ~"a"}]},
    {_Ops, Snap1, _} = arizona_diff:diff(Mod:render(B1), Snap0, V0, #{rows => true}),
    ?assertEqual(
        [~"a", ~"a"], selected_values(iolist_to_binary(arizona_render:snapshot_html(Snap1)))
    ).

%% `?json` has no selection rule: `value` stays a prop of the node.
json_value_prop_is_unchanged(Config) when is_list(Config) ->
    Mod = compile_module(
        "-module(sel_json). -export([render/1]). render(Bindings) -> "
        "arizona_template:target(arizona_json, {select, [{value, "
        "arizona_template:get(v, Bindings)}], [{option, [{value, ~\"b\"}], []}]}). "
    ),
    Doc = iolist_to_binary(arizona_render:render_to_iolist(Mod:render(#{v => ~"b"}))),
    ?assertMatch(#{~"type" := ~"select", ~"value" := ~"b"}, json:decode(Doc)),
    ?assertEqual(nomatch, binary:match(Doc, ~"selected")).

%% Options the transform cannot see are marked in the document the SSR render
%% serves, so the page loads on the chosen one before any script runs.
opaque_ssr_marks_the_chosen_option(Config) when is_list(Config) ->
    lists:foreach(
        fun(Shape) ->
            HTML = opaque_ssr(Shape, ~"b"),
            ?assertEqual([~"b"], selected_values(HTML), {Shape, HTML}),
            ?assertEqual([], selected_values(opaque_ssr(Shape, ~"z")), Shape)
        end,
        opaque_shapes()
    ),
    %% Valued by its text: an option without a value attribute.
    Raw = opaque_ssr(raw_options, ~"b"),
    ?assertMatch({match, _}, re:run(Raw, ~"<option selected>b</option>"), Raw),
    %% A `?local` value loads on its initial.
    Local = iolist_to_binary(
        arizona_render:render_to_iolist(
            arizona_select_value:local_value_component(opaque_bindings(~"z"))
        )
    ),
    ?assertEqual([~"b"], selected_values(Local), Local).

%% The value stays on the select, as the text the options compare against: the
%% client applies it to options the server never marks.
opaque_select_keeps_its_value(Config) when is_list(Config) ->
    lists:foreach(
        fun(Shape) ->
            HTML = opaque_ssr(Shape, ~"a&b"),
            ?assertMatch(
                {match, _}, re:run(HTML, ~"<select[^>]* value=\"a&amp;b\""), {Shape, HTML}
            )
        end,
        opaque_shapes()
    ),
    %% `false` picks nothing and renders no value.
    None = opaque_ssr(component, false),
    ?assertEqual(nomatch, re:run(None, ~"<select[^>]* value[ =>]"), None),
    ?assertEqual([], selected_values(None)).

opaque_multiple_marks_every_listed_value(Config) when is_list(Config) ->
    HTML = iolist_to_binary(
        arizona_render:render_to_iolist(
            arizona_select_value:multiple_component(opaque_bindings(~"b"))
        )
    ),
    ?assertEqual([~"a", ~"c"], selected_values(HTML), HTML),
    ?assertNotEqual(nomatch, binary:match(HTML, ~"value=\"[&quot;a&quot;,&quot;c&quot;]\"")),
    One = iolist_to_binary(
        arizona_render:render_to_iolist(
            arizona_select_value:multiple_component((opaque_bindings(~"b"))#{vs => ~"c"})
        )
    ),
    ?assertEqual([~"c"], selected_values(One), One).

opaque_static_generation_marks_the_chosen_option(Config) when is_list(Config) ->
    Dir = filename:join(proplists:get_value(priv_dir, Config), "select_opaque_static"),
    {[Path], []} = arizona_static:generate(
        Dir, [{arizona_select_value_page, ~"index.html", #{bindings => #{v => ~"c"}}}]
    ),
    {ok, HTML} = file:read_file(Path),
    %% Literal, list ?each, stream ?each, the multiple select, then the component
    %% and the child view, and the `?local` selects.
    ?assertEqual(
        [
            ~"c",
            ~"c",
            ~"c",
            ~"a",
            ~"c",
            ~"c",
            ~"c",
            ~"a",
            ~"c",
            ~"a",
            ~"c",
            ~"c",
            ~"a",
            ~"c",
            ~"c",
            ~"ac"
        ],
        selected_values(HTML),
        HTML
    ).

%% A live render, outside a page or inside a live process alike, leaves the options a
%% component renders to the client: the select carries its value, and nothing is left
%% behind in the process.
opaque_live_render_is_left_to_the_client(Config) when is_list(Config) ->
    Before = get(),
    {HTML0, _Snap, _Views} = arizona_render:render(
        arizona_select_value:component(opaque_bindings(~"b")), #{}
    ),
    HTML = iolist_to_binary(HTML0),
    ?assertEqual([], selected_values(HTML), HTML),
    ?assertMatch({match, _}, re:run(HTML, ~"<select[^>]* value=\"b\""), HTML),
    _ = iolist_to_binary(
        arizona_render:render_to_iolist(arizona_select_value:component(opaque_bindings(~"b")))
    ),
    %% The renders' own caches aside (keyed by tuples), no flag is left set.
    ?assertEqual([], [K || {K, _} <- get() -- Before, is_atom(K)]).

%% The backend marks the options in one child's output, reading it as the parser
%% does: only real option start tags, values decoded with every named reference,
%% an option's text as its `text` property reads it, and nothing past what closes
%% the select.
mark_choices_reads_the_fragment(Config) when is_list(Config) ->
    Mark = fun(Fragment, Picks) ->
        iolist_to_binary(arizona_html:mark_choices(Fragment, Picks))
    end,
    Same = fun(Fragment, Picks) -> ?assertEqual(Fragment, Mark(Fragment, Picks), Fragment) end,
    ?assertEqual(
        ~"<option value=\"a\">A</option><option value=\"b\" selected>B</option>",
        Mark(~"<option value=\"a\">A</option><option value=\"b\">B</option>", [~"b"])
    ),
    %% An attribute value holding markup is a value, wherever a `>` sits in it.
    ?assertEqual(
        <<"<option value=\"b\" title=\"<option value=a>\">B</option>",
            "<option value=\"a\" selected>A</option>">>,
        Mark(
            <<"<option value=\"b\" title=\"<option value=a>\">B</option>",
                "<option value=\"a\">A</option>">>,
            [~"a"]
        )
    ),
    ?assertEqual(
        ~"<option data-y=\"x>y\" value=\"a\" selected>A</option>",
        Mark(~"<option data-y=\"x>y\" value=\"a\">A</option>", [~"a"])
    ),
    ?assertEqual(
        ~"<OPTION value=a selected>A<option value=b>B",
        Mark(~"<OPTION value=a>A<option value=b>B", [~"a"])
    ),
    %% Raw text (as the backend classifies it), comments, bogus comments, template
    %% content, and CDATA in foreign content hold no option.
    lists:foreach(
        fun(Fragment) -> Same(Fragment, [~"a"]) end,
        [
            ~"<xmp><option value=a>A</option></xmp>",
            ~"<iframe><option value=a></iframe>",
            ~"<noembed><option value=a></noembed>",
            ~"<noframes><option value=a></noframes>",
            ~"<script>var o = '<option value=a>';</script>",
            ~"<STYLE>/* <option value=a> */</STYLE>",
            ~"<title><option value=a></title>",
            ~"<!-- <option value=a> -->",
            ~"<!-- a --!><!---->",
            ~"<![CDATA[<option value=a>]]>",
            ~"<?pi <option value=a> ?>",
            ~"<svg><![CDATA[<option value=a>]]></svg>",
            ~"<svg><option value=a></option></svg>",
            ~"<template><template></template><option value=a></template>"
        ]
    ),
    ?assertEqual(
        ~"<svg><title>t</title></svg><option value=a selected>",
        Mark(~"<svg><title>t</title></svg><option value=a>", [~"a"])
    ),
    ?assertEqual(
        ~"<svg><foreignObject><option value=a selected></foreignObject></svg>",
        Mark(~"<svg><foreignObject><option value=a></foreignObject></svg>", [~"a"])
    ),
    %% What closes the select ends the walk; the same text in a comment, a script or
    %% an attribute value does not.
    Same(~"</select><option value=a>", [~"a"]),
    Same(~"<input><option value=a>", [~"a"]),
    ?assertEqual(
        <<"<!-- </select> --><script>'</select>'</script>",
            "<option title=\"</select>\" value=a selected>">>,
        Mark(
            <<"<!-- </select> --><script>'</select>'</script>",
                "<option title=\"</select>\" value=a>">>,
            [
                ~"a"
            ]
        )
    ),
    %% Character references, as the parser decodes them: the whole named set, legacy
    %% names without `;` (kept as written in an attribute when a name could go on),
    %% numeric ones with the standard's replacements.
    lists:foreach(
        fun({Fragment, Pick}) ->
            ?assertEqual(1, count_selected(Mark(Fragment, [Pick])), {Fragment, Pick})
        end,
        [
            {~"<option>caf&eacute;</option>", ~"café"},
            {~"<option>caf&eacute</option>", ~"café"},
            {~"<option value=\"a&nbsp;b\">x</option>", ~"a\x{a0}b"},
            {~"<option value=\"&notin;&notit;\">x</option>", ~"∉&notit;"},
            {~"<option>&notin;&notit;</option>", ~"∉¬it;"},
            {~"<option value=\"?a=1&copy=2\">x</option>", ~"?a=1&copy=2"},
            {~"<option value=\"&copy 2\">x</option>", ~"© 2"},
            {~"<option value=\"&#233;&#xE9;&#x20ac\">x</option>", ~"éé€"},
            {~"<option value=\"&#128;&#0;&#xD800;\">x</option>", ~"€\x{fffd}\x{fffd}"},
            {~"<option value=\"a&#b\">x</option>", ~"a&#b"},
            {~"<option value=\"a&ampb\">x</option>", ~"a&ampb"},
            {~"<option>a&ampb</option>", ~"a&b"},
            {~"<option value=\"&bogus;\">x</option>", ~"&bogus;"}
        ]
    ),
    %% An option's text: whitespace collapsed, tags and comments dropped, a script's
    %% content left out, up to what ends the option.
    lists:foreach(
        fun({Fragment, Pick}) ->
            ?assertEqual(1, count_selected(Mark(Fragment, [Pick])), {Fragment, Pick})
        end,
        [
            {~"<option>\n a <b>b</b> <!-- c --></option>", ~"a b"},
            {~"<option>a<script>x</script></option>", ~"a"},
            {~"<option>a<hr>b</option>", ~"a"},
            {~"<option>a<optgroup><option>b", ~"a"},
            {~"<option>a < b</option>", ~"a < b"},
            {~"<option>b", ~"b"},
            {~"<option>a<style>b</style></option>", ~"ab"}
        ]
    ),
    %% An option the page already marks and the value picks keeps it; the first of a
    %% repeated attribute counts.
    ?assertEqual(
        ~"<option SELECTED value=a>A</option><option value=b>B</option>",
        Mark(~"<option SELECTED value=a>A</option><option value=b>B</option>", [~"a"])
    ),
    Same(~"<option value=b value=a>A</option>", [~"a"]),
    ?assertEqual(~"<option value=\"a\"/ selected>", Mark(~"<option value=\"a\"/>", [~"a"])),
    %% A fragment that ends inside a tag marks nothing there.
    Same(~"<option value=a", [~"a"]),
    %% Every pick is marked.
    ?assertEqual(
        2,
        count_selected(
            Mark(~"<option>a</option><option>b</option><option>c</option>", [~"a", ~"c"])
        )
    ).

%% A `selected` the markup carries is the value's to decide, as the client decides it
%% once it applies the value: kept on an option the value picks, taken off any other,
%% every copy of it, so the page loads on the same option with or without the client.
authored_selected_follows_the_value(Config) when is_list(Config) ->
    Mark = fun(Fragment, Picks) ->
        iolist_to_binary(arizona_html:mark_choices(Fragment, Picks))
    end,
    ?assertEqual(
        ~"<option value=a selected>A<option value=b >B",
        Mark(~"<option value=a>A<option value=b selected>B", [~"a"])
    ),
    ?assertEqual(
        ~"<option value=\"b\"  >B<option SELECTED>a",
        Mark(~"<option value=\"b\" SELECTED=\"\" selected>B<option SELECTED>a", [~"a"])
    ),
    %% End to end: a component's markup marking another option.
    Mod = compile_module(
        select_src_mod("sel_authored", "arizona_template:raw(~\"<option>a<option selected>b\")")
    ),
    HTML = iolist_to_binary(arizona_render:render_to_iolist(Mod:render(#{v => ~"a"}))),
    ?assertEqual([~"a"], selected_values(HTML), HTML).

%% The parser normalizes newlines (CR LF and CR to LF) and replaces NUL in attribute
%% values with U+FFFD before anything compares; the picks render as an attribute too,
%% so both sides are read alike. A `<template>` in an option holds no text of it.
fragment_reads_as_the_parser_preprocesses(Config) when is_list(Config) ->
    One = fun(Fragment, Picks) ->
        count_selected(iolist_to_binary(arizona_html:mark_choices(Fragment, Picks)))
    end,
    ?assertEqual(1, One(~"<option value=\"a\nb\">x", [~"a\r\nb"])),
    ?assertEqual(1, One(~"<option value=\"a\r\nb\">x", [~"a\rb"])),
    ?assertEqual(1, One(<<"<option value=\"a", 0, "\">x">>, [<<"a", 0>>])),
    ?assertEqual(1, One(<<"<option value=\"a", 0, "\">x">>, [<<"a", 16#FFFD/utf8>>])),
    ?assertEqual(1, One(~"<option>a<template>b</template></option>", [~"a"])),
    ?assertEqual(0, One(~"<option>a<template>b</template></option>", [~"ab"])).

%% --------------------------------------------------------------------
%% Diff
%% --------------------------------------------------------------------

diff_moves_selected_to_the_new_option(Config) when is_list(Config) ->
    lists:foreach(
        fun(Shape) ->
            {_HTML, Snap0, V0} = arizona_render:render(tmpl(Shape, ~"a"), #{}),
            {Ops, Snap1, _V1} = arizona_diff:diff(tmpl(Shape, ~"c"), Snap0, V0, #{v => true}),
            ok = assert_selection_ops(Shape, Ops),
            HTML = iolist_to_binary(arizona_render:snapshot_html(Snap1)),
            ?assertEqual([~"c"], selected_values(HTML), {Shape, HTML})
        end,
        shapes()
    ).

%% Only the two options whose selection changed are touched: no slot re-renders.
%% An `?each` nested in another's item is the exception: a change to a binding it
%% reads re-renders the inner list, selection or not.
diff_is_fine_grained(Config) when is_list(Config) ->
    lists:foreach(
        fun(Shape) ->
            {_HTML, Snap0, V0} = arizona_render:render(tmpl(Shape, ~"a"), #{}),
            {Ops, _Snap1, _V1} = arizona_diff:diff(tmpl(Shape, ~"b"), Snap0, V0, #{v => true}),
            ?assertEqual([], [Op || Op <- flatten_ops(Ops), hd(Op) =:= ?OP_TEXT], {Shape, Ops})
        end,
        shapes() -- [nested_each]
    ).

diff_of_unrelated_binding_is_empty(Config) when is_list(Config) ->
    lists:foreach(
        fun(Shape) ->
            {_HTML, Snap0, V0} = arizona_render:render(tmpl(Shape, ~"a"), #{}),
            {Ops, _Snap1, _V1} = arizona_diff:diff(
                tmpl(Shape, ~"a"), Snap0, V0, #{unrelated => true}
            ),
            ?assertEqual([], Ops, Shape)
        end,
        shapes()
    ).

%% A stream item updated in place keeps its selection, and an inserted item that
%% matches the value is inserted selected.
stream_item_update_keeps_selection(Config) when is_list(Config) ->
    B0 = stream_bindings(~"b"),
    {_HTML, Snap0, V0} = arizona_render:render(arizona_select_value:each_stream(B0), #{}),
    Items1 = arizona_stream:update(
        arizona_stream:insert(maps:get(items, B0), #{id => ~"d", label => ~"D"}),
        ~"b",
        #{id => ~"b", label => ~"Bee"}
    ),
    B1 = B0#{items => Items1, v => ~"d"},
    {Ops, Snap1, _V1} = arizona_diff:diff(
        arizona_select_value:each_stream(B1), Snap0, V0, #{items => true, v => true}
    ),
    HTML = iolist_to_binary(arizona_render:snapshot_html(Snap1)),
    ?assertEqual([~"d"], selected_values(HTML), {Ops, HTML}),
    [Inserted] = [H || [?OP_INSERT, _, ~"d", _, H] <- Ops],
    ?assertEqual([~"d"], selected_values(payload_html(Inserted))).

%% A select the server cannot mark every option of ships its value, for the client
%% to apply; the options it does mark move as before.
opaque_diff_ships_the_value(Config) when is_list(Config) ->
    {_HTML, Snap0, V0} = arizona_render:render(
        arizona_select_value:mixed(opaque_bindings(~"a")), #{}
    ),
    {Ops, _Snap1, _V1} = arizona_diff:diff(
        arizona_select_value:mixed(opaque_bindings(~"c")), Snap0, V0, #{v => true}
    ),
    ?assertEqual([{remove, ~"selected"}, {set, ~"value"}], lists:sort(attr_ops(Ops)), Ops),
    ?assertMatch([~"c"], [V || [?OP_SET_ATTR, _, ~"value", V] <- flatten_ops(Ops)]).

%% A runtime-text option's `selected` tracks both its text and the value: a change
%% to either ships the attribute ops, and only those.
runtime_text_option_diffs(Config) when is_list(Config) ->
    Mod = compile_module(
        select_src_mod(
            "sel_text_diff",
            "{option, [], [arizona_template:get(label, Bindings)]}, {option, [], [~\"y\"]}"
        )
    ),
    B0 = #{v => ~"x", label => ~"x"},
    {_, Snap0, V0} = arizona_render:render(Mod:render(B0), #{}),
    B1 = B0#{label => ~"z"},
    {Ops1, Snap1, V1} = arizona_diff:diff(Mod:render(B1), Snap0, V0, #{label => true}),
    ?assertEqual([{remove, ~"selected"}], attr_ops(Ops1), Ops1),
    ?assertEqual([], selected_values(iolist_to_binary(arizona_render:snapshot_html(Snap1)))),
    {Ops2, Snap2, _} = arizona_diff:diff(Mod:render(B1#{v => ~"y"}), Snap1, V1, #{v => true}),
    ?assertEqual([{set, ~"selected"}], attr_ops(Ops2), Ops2),
    ?assertEqual([~"y"], selected_values(iolist_to_binary(arizona_render:snapshot_html(Snap2)))).

%% A select in each row of an `?each`, its option valued by the row's text: the row
%% that comes to name the value is item-patched selected.
runtime_text_option_in_each_row(Config) when is_list(Config) ->
    Mod = compile_module(
        "-module(sel_text_rows). -export([render/1]). render(Bindings) -> "
        "arizona_template:target(arizona_html, {'div', [], [arizona_template:each(fun(R) -> "
        "{'div', [], [{select, [{value, arizona_template:get(v, Bindings)}], "
        "[{option, [], [R]}, {option, [], [~\"z\"]}]}]} end, "
        "arizona_template:get(rows, Bindings))]}). "
    ),
    B0 = #{rows => [~"x"], v => ~"b"},
    HTML0 = iolist_to_binary(arizona_render:render_to_iolist(Mod:render(B0))),
    ?assertEqual([], selected_values(HTML0), HTML0),
    ?assertEqual(nomatch, re:run(HTML0, ~"<select[^>]* value[ =>]"), HTML0),
    Hit = iolist_to_binary(arizona_render:render_to_iolist(Mod:render(B0#{rows => [~"b"]}))),
    ?assertEqual([~"b"], selected_values(Hit), Hit),
    {_, Snap0, V0} = arizona_render:render(Mod:render(B0), #{}),
    {Ops, Snap1, _} = arizona_diff:diff(Mod:render(B0#{rows => [~"b"]}), Snap0, V0, #{
        rows => true
    }),
    ?assertMatch([{set, ~"selected"}], [A || {set, ~"selected"} = A <- attr_ops(Ops)], Ops),
    ?assertEqual(
        [~"b"], selected_values(iolist_to_binary(arizona_render:snapshot_html(Snap1)))
    ).

%% An `?each` the select wraps whole (its item's option is valued by markup) still
%% diffs item by item: the wrapper is transparent to a live render.
opaque_each_keeps_its_item_diff(Config) when is_list(Config) ->
    Each =
        "arizona_template:each(fun(O) -> {option, [], [arizona_template:raw(O)]} end, "
        "arizona_template:get(opts, Bindings))",
    %% A raw around the each is the each (arizona_template:raw/1), chooser or not.
    lists:foreach(
        fun({Name, Child}) ->
            Mod = compile_module(select_src_mod(Name, Child)),
            B0 = #{v => ~"b", opts => [~"a", ~"<i>b</i>"]},
            {_, Snap0, V0} = arizona_render:render(Mod:render(B0), #{}),
            {Ops, Snap1, _} = arizona_diff:diff(
                Mod:render(B0#{opts => [~"a", ~"<i>c</i>"]}), Snap0, V0, #{opts => true}
            ),
            ?assertMatch([[?OP_LIST_PATCH, _, [[?OP_ITEM_PATCH, 1, _]]]], Ops, Child),
            ?assertEqual([], [Op || Op <- flatten_ops(Ops), hd(Op) =:= ?OP_SET_ATTR]),
            ?assertNotEqual(
                nomatch,
                binary:match(iolist_to_binary(arizona_render:snapshot_html(Snap1)), ~"<i>c</i>")
            )
        end,
        [
            {"sel_opaque_each", Each},
            {"sel_opaque_raw_each", "arizona_template:raw(" ++ Each ++ ")"}
        ]
    ).

%% An inserted item ships as its HTML, or as statics plus values.
payload_html(#{~"s" := [S | Statics], ~"d" := Values}) ->
    iolist_to_binary([S | lists:zipwith(fun(V, St) -> [V, St] end, Values, Statics)]);
payload_html(HTML) ->
    iolist_to_binary(HTML).

%% --------------------------------------------------------------------
%% Compile
%% --------------------------------------------------------------------

%% Whatever may hold an option the transform cannot see compiles: the select keeps
%% its value, and the document marks the option.
opaque_child_compiles(Config) when is_list(Config) ->
    lists:foreach(
        fun({N, Child}) ->
            Mod = compile_module(select_src_mod("sel_opaque_" ++ integer_to_list(N), Child)),
            HTML = iolist_to_binary(
                arizona_render:render_to_iolist(Mod:render(#{v => ~"a", opts => []}))
            ),
            ?assertMatch({match, _}, re:run(HTML, ~"<select[^>]* value=\"a\""), {Child, HTML})
        end,
        lists:enumerate([
            "arizona_template:stateless(fun opts/1, #{})",
            "arizona_template:get(opts, Bindings)",
            "arizona_template:raw(~\"<option>a</option>\")",
            "~\"<option value=\\\"a\\\">A</option>\"",
            "{optgroup, [], [~\"<OPTION>a\"]}",
            "{optgroup, [], [arizona_template:target(arizona_html, {option, [], [~\"a\"]})]}"
        ])
    ),
    %% A remote helper compiles the same way (its module need not exist to compile).
    _ = compile_module(select_src_mod("sel_opaque_remote", "other_mod:options()")),
    %% An `?each` handed to a helper that renders the select compiles before the
    %% select is seen.
    Field = compile_module(
        "-module(sel_field). -export([render/1]). render(Bindings) -> "
        "arizona_template:target(arizona_html, {form, [], [field(arizona_template:get(v, "
        "Bindings), arizona_template:each(fun(O) -> {option, [{value, O}], [O]} end, "
        "arizona_template:get(opts, Bindings)))]}). "
        "field(V, Opts) -> {select, [{value, V}], [Opts]}. "
    ),
    FieldHTML = iolist_to_binary(
        arizona_render:render_to_iolist(Field:render(#{v => ~"b", opts => [~"a", ~"b"]}))
    ),
    ?assertEqual([~"b"], selected_values(FieldHTML), FieldHTML),
    %% A plain-text static and an empty one are no options: the value is dropped.
    Ws = compile_module(select_src_mod("sel_ws", "~\" \", <<>>")),
    WsHTML = iolist_to_binary(arizona_render:render_to_iolist(Ws:render(#{v => ~"a"}))),
    ?assertEqual(nomatch, binary:match(WsHTML, ~"value"), WsHTML).

%% Every place an option can come from the compiler cannot see: the SSR render marks
%% it in that child's own output, a live render leaves it to the client, and the
%% select keeps its value for the client. A value list child holds no option and is
%% left as it is.
opaque_child_is_marked_in_its_own_slot(Config) when is_list(Config) ->
    lists:foreach(
        fun({N, {Child, Picked, LivePicked}}) ->
            Mod = compile_module(select_src_mod("sel_slot_" ++ integer_to_list(N), Child)),
            B = #{v => ~"b", more => true, x => ~"y"},
            HTML = iolist_to_binary(arizona_render:render_to_iolist(Mod:render(B))),
            ?assertEqual(Picked, count_selected(HTML), {Child, HTML}),
            ?assertMatch({match, _}, re:run(HTML, ~"<select[^>]* value=\"b\""), {Child, HTML}),
            {Live, _Snap, _Views} = arizona_render:render(Mod:render(B), #{}),
            ?assertEqual(LivePicked, count_selected(iolist_to_binary(Live)), Child)
        end,
        lists:enumerate([
            {"arizona_template:stateless(fun opts/1, #{o => [~\"a\", ~\"b\"]})", 1, 0},
            {"{option, [], [arizona_template:raw(~\"<b>b</b>\")]}", 1, 0},
            {"{option, [], [~\"<b>b</b>\"]}, {option, [], [~\"c\"]}", 1, 0},
            {"{optgroup, [], [arizona_template:raw(~\"<option>b\")]}", 1, 0},
            {
                "case arizona_template:get(more, Bindings) of "
                "true -> arizona_template:stateless(fun opts/1, #{o => [~\"b\"]}); "
                "false -> {option, [{value, ~\"b\"}], [~\"B\"]} end",
                1,
                0
            },
            {"[~\"x\", arizona_template:stateless(fun opts/1, #{o => [~\"b\"]})]", 1, 0},
            {
                "arizona_template:each(fun(O) -> {option, [], [arizona_template:raw(O)]} end, "
                "[~\"a\", ~\"<i>b</i>\"])",
                1,
                0
            },
            {"arizona_template:raw(~\"<option>b</option><option value=b>B</option>\")", 2, 0}
        ])
    ),
    Values = compile_module(
        select_src_mod("sel_values", "[~\"a\", arizona_template:get(x, Bindings)]")
    ),
    ValuesHTML = iolist_to_binary(
        arizona_render:render_to_iolist(Values:render(#{v => ~"b", x => ~"y"}))
    ),
    ?assertEqual(nomatch, binary:match(ValuesHTML, ~"value"), ValuesHTML),
    %% In an az-nodiff element (a layout's render path), the same.
    Nodiff = compile_module(
        "-module(sel_slot_nodiff). -export([render/1]). render(Bindings) -> "
        "arizona_template:target(arizona_html, {'div', ['az-nodiff'], [{select, "
        "[{value, arizona_template:get(v, Bindings)}], [arizona_template:stateless(fun opts/1, "
        "#{o => [~\"a\", ~\"b\"]})]}]}). "
        "opts(B) -> arizona_template:target(arizona_html, [arizona_template:each(fun(O) -> "
        "{option, [{value, O}], [O]} end, maps:get(o, B))]). "
    ),
    NodiffHTML = iolist_to_binary(arizona_render:render_to_iolist(Nodiff:render(#{v => ~"b"}))),
    ?assertEqual([~"b"], selected_values(NodiffHTML), NodiffHTML).

%% An option without a value whose text is computed is valued by that text, at
%% compile time: an ordinary `selected` attribute, so the select keeps no value.
option_with_runtime_text_compiles(Config) when is_list(Config) ->
    Mod = compile_module(
        select_src_mod(
            "sel_text",
            "{option, [], [arizona_template:get(label, Bindings)]}, "
            "{option, [], [~\"Tag: \", arizona_template:get(n, Bindings), ~\" \"]}, "
            "{option, [], [[~\"a&b\", arizona_template:get(s, Bindings)]]}"
        )
    ),
    B = #{v => ~"x", label => ~" x ", n => 7, s => ~"c"},
    HTML = iolist_to_binary(arizona_render:render_to_iolist(Mod:render(B))),
    ?assertMatch({match, _}, re:run(HTML, ~"<option[^>]* selected[^>]*>"), HTML),
    ?assertEqual(1, count_selected(HTML), HTML),
    ?assertEqual(nomatch, re:run(HTML, ~"<select[^>]* value[ =>]"), HTML),
    %% Every part counts, whitespace collapsed; a literal list is text, `&` included.
    Tag = iolist_to_binary(arizona_render:render_to_iolist(Mod:render(B#{v => ~"Tag: 7"}))),
    ?assertMatch({match, _}, re:run(Tag, ~"<option[^>]* selected[^>]*>Tag: "), Tag),
    Amp = iolist_to_binary(arizona_render:render_to_iolist(Mod:render(B#{v => ~"a&bc"}))),
    ?assertMatch(
        {match, _}, re:run(Amp, ~"<option[^>]* selected[^>]*>(<!--[^>]*-->)?a&amp;b"), Amp
    ),
    {Live, _Snap, _Views} = arizona_render:render(Mod:render(B), #{}),
    ?assertEqual(1, count_selected(iolist_to_binary(Live))).

%% A child of a runtime-text option that renders markup values the option by the
%% text that markup shows, tags left out, character references decoded and
%% whitespace collapsed, as the browser reads it: still an ordinary `selected`, so the
%% SSR, the live render and a diff agree, and the select keeps no value.
runtime_text_option_valued_by_its_markup(Config) when is_list(Config) ->
    Mod = compile_module(
        "-module(sel_text_markup). -export([render/1, label/1, card/1]). "
        "render(Bindings) -> arizona_template:target(arizona_html, {select, "
        "[{value, arizona_template:get(v, Bindings)}], "
        "[{option, [], [~\"Pick \", arizona_template:get(label, Bindings)]}, "
        "{option, [], [~\"y\"]}]}). "
        "label(T) -> arizona_template:target(arizona_html, "
        "{b, [{title, ~\"no\"}], [~\" \", T, ~\"&amp;!\"]}). "
        "card(Bindings) -> arizona_template:target(arizona_html, "
        "{i, [], [arizona_template:get(t, Bindings)]}). "
    ),
    Label = Mod:label(~"A<B"),
    B0 = #{v => ~"Pick A<B&!", label => Label},
    SSR = iolist_to_binary(arizona_render:render_to_iolist(Mod:render(B0))),
    ?assertMatch({match, _}, re:run(SSR, ~"<option[^>]* selected[^>]*>Pick "), SSR),
    ?assertEqual(1, count_selected(SSR), SSR),
    ?assertEqual(nomatch, re:run(SSR, ~"<select[^>]* value[ =>]"), SSR),
    Miss = iolist_to_binary(arizona_render:render_to_iolist(Mod:render(B0#{v => ~"A<B&!"}))),
    ?assertEqual(0, count_selected(Miss), Miss),
    %% A component renders markup too.
    Card = arizona_template:stateless(fun Mod:card/1, #{t => ~"C&D"}),
    CardHTML = iolist_to_binary(
        arizona_render:render_to_iolist(Mod:render(#{v => ~"Pick C&D", label => Card}))
    ),
    ?assertEqual(1, count_selected(CardHTML), CardHTML),
    %% A live view's text would mean mounting it again, outside the view tree.
    Child = arizona_template:stateful(arizona_select_value_child, #{id => ~"c", opts => [~"a"]}),
    ?assertError(
        {arizona_loc, _, {stateful_in_text, arizona_select_value_child}},
        arizona_render:render_to_iolist(Mod:render(#{v => ~"a", label => Child}))
    ),
    #{general := Message} = arizona_render:format_error(
        {stateful_in_text, arizona_select_value_child}, []
    ),
    ?assertNotEqual(nomatch, string:find(Message, "arizona_select_value_child"), Message),
    {Live, Snap0, V0} = arizona_render:render(Mod:render(B0), #{}),
    ?assertMatch(
        {match, _}, re:run(iolist_to_binary(Live), ~"<option[^>]* selected[^>]*>Pick "), Live
    ),
    ?assertEqual(1, count_selected(iolist_to_binary(Live))),
    B1 = B0#{label => Mod:label(~"Z")},
    {Ops1, Snap1, V1} = arizona_diff:diff(Mod:render(B1), Snap0, V0, #{label => true}),
    ?assertEqual([{remove, ~"selected"}], attr_ops(Ops1), Ops1),
    {Ops2, _Snap2, _} = arizona_diff:diff(
        Mod:render(B1#{v => ~"Pick Z&!"}), Snap1, V1, #{v => true}
    ),
    ?assertEqual([{set, ~"selected"}], attr_ops(Ops2), Ops2).

%% A raw around a template in an option renders as that template, so the option is
%% valued by the text the template shows. One a value holds is read where the
%% attribute renders, as the bare template is, on the SSR, the live render and a diff;
%% one written in the option is markup the compiler cannot see into, marked in the SSR
%% output and left to the client live.
raw_template_option_is_valued_by_its_text(Config) when is_list(Config) ->
    Mod = compile_module(
        "-module(sel_raw_tmpl). -export([render/1, label/1]). "
        "render(Bindings) -> arizona_template:target(arizona_html, {select, "
        "[{value, arizona_template:get(v, Bindings)}], "
        "[{option, [], [arizona_template:get(label, Bindings)]}, {option, [], [~\"y\"]}]}). "
        "label(T) -> arizona_template:raw(arizona_template:target(arizona_html, {b, [], [T]})). "
    ),
    B0 = #{v => ~"A&B", label => Mod:label(~"A&B")},
    SSR = iolist_to_binary(arizona_render:render_to_iolist(Mod:render(B0))),
    ?assertMatch({match, _}, re:run(SSR, ~"<option[^>]* selected[^>]*>(<!--[^>]*-->)?<b"), SSR),
    ?assertEqual(1, count_selected(SSR), SSR),
    ?assertEqual(nomatch, re:run(SSR, ~"<select[^>]* value[ =>]"), SSR),
    {Live, Snap0, V0} = arizona_render:render(Mod:render(B0), #{}),
    ?assertEqual(1, count_selected(iolist_to_binary(Live)), Live),
    B1 = B0#{label => Mod:label(~"Z")},
    {Ops1, Snap1, V1} = arizona_diff:diff(Mod:render(B1), Snap0, V0, #{label => true}),
    ?assertEqual([{remove, ~"selected"}], attr_ops(Ops1), Ops1),
    {Ops2, _Snap2, _} = arizona_diff:diff(Mod:render(B1#{v => ~"Z"}), Snap1, V1, #{v => true}),
    ?assertEqual([{set, ~"selected"}], attr_ops(Ops2), Ops2),
    Literal = compile_module(
        select_src_mod(
            "sel_raw_tmpl_literal",
            "{option, [], [~\"a\"]}, {option, [], [arizona_template:raw("
            "arizona_template:target(arizona_html, {b, [], [arizona_template:get(x, Bindings)]}))]}"
        )
    ),
    LB = #{v => ~"b", x => ~"b"},
    LitHTML = iolist_to_binary(arizona_render:render_to_iolist(Literal:render(LB))),
    ?assertEqual([~"b"], [strip_tags(V) || V <- selected_values(LitHTML)], LitHTML),
    ?assertMatch({match, _}, re:run(LitHTML, ~"<select[^>]* value=\"b\""), LitHTML),
    {LitLive, _, _} = arizona_render:render(Literal:render(LB), #{}),
    ?assertEqual(0, count_selected(iolist_to_binary(LitLive)), LitLive).

%% The `selected` of an option valued by a markup child tracks what the child's slot
%% tracks, and no more: a read inside the markup's own closures (a remote helper's) is
%% not the slot's, so a change to it moves neither the text nor the selection.
markup_text_tracks_what_its_slot_tracks(Config) when is_list(Config) ->
    Mod = compile_module(
        select_src_mod(
            "sel_text_remote",
            "{option, [], [arizona_select_value:name_label(Bindings)]}, {option, [], [~\"y\"]}"
        )
    ),
    B0 = #{v => ~"Ann", name => ~"Ann"},
    {Live, Snap0, V0} = arizona_render:render(Mod:render(B0), #{}),
    ?assertEqual(1, count_selected(iolist_to_binary(Live)), Live),
    {Ops, _Snap1, _} = arizona_diff:diff(Mod:render(B0#{name => ~"Bob"}), Snap0, V0, #{
        name => true
    }),
    ?assertEqual([], attr_ops(Ops), Ops).

authored_selected_is_refused(Config) when is_list(Config) ->
    Conflict = fun
        ({select_value, {option_selected, _Tag, _Flag, _Chooser}}) -> true;
        (_) -> false
    end,
    assert_parse_error(select_src("{option, [{value, ~\"a\"}, selected], []}"), Conflict),
    assert_parse_error(
        select_src("{option, [{value, ~\"a\"}, {<<\"SELECTED\">>, false}], []}"), Conflict
    ).

%% A choice whose text holds a value is valued by what it renders, read by the
%% backend's markup_text/2: a backend naming the roles without it is refused where the
%% template compiles, not when a value first renders markup.
choice_text_needs_markup_text(Config) when is_list(Config) ->
    Backend = sel_no_markup_text,
    Exports = [FA || {F, _A} = FA <- arizona_html:module_info(exports), F =/= module_info],
    Delegates = [
        io_lib:format("~s(~s) -> arizona_html:~s(~s). ", [F, args(A), F, args(A)])
     || {F, A} <- Exports, F =/= markup_text
    ],
    {module, Backend} = load_source(
        [
            "-module(",
            atom_to_list(Backend),
            "). -compile([export_all, nowarn_export_all]). ",
            Delegates
        ]
    ),
    Source = lists:flatten([
        "-module(sel_no_mt). -export([render/1]). render(Bindings) -> "
        "arizona_template:target(sel_no_markup_text, {select, "
        "[{value, arizona_template:get(v, Bindings)}], "
        "[{option, [], [arizona_template:get(label, Bindings)]}]}). "
    ]),
    assert_parse_error(Source, fun
        ({select_value, {no_markup_text, sel_no_markup_text, _Tag}}) -> true;
        (_) -> false
    end),
    %% Static text needs no reading at render, so the same backend compiles it.
    {module, _} = load_source(
        "-module(sel_no_mt_static). -export([render/1]). render(Bindings) -> "
        "arizona_template:target(sel_no_markup_text, {select, "
        "[{value, arizona_template:get(v, Bindings)}], [{option, [], [~\"a\"]}]}). "
    ).

args(0) -> "";
args(A) -> lists:join(", ", ["A" ++ integer_to_list(N) || N <- lists:seq(1, A)]).

load_source(Source) ->
    Forms = arizona_parse_transform:parse_transform(parse_forms(lists:flatten(Source)), []),
    {ok, Mod, Bin} = compile:forms(Forms, [return_errors]),
    code:load_binary(Mod, "", Bin).

%% The value is read inside every option the item builds, so an `?each` parameter
%% of a name it reads is renamed: `Bindings` here is the item, and the select's
%% value still the view's.
shadowing_parameter_is_renamed(Config) when is_list(Config) ->
    Mod = compile_module(
        select_src_mod(
            "sel_shadow",
            "arizona_template:each(fun(Bindings) -> {option, [{value, "
            "arizona_template:get(id, Bindings)}], []} end, "
            "arizona_template:get(items, Bindings))"
        )
    ),
    B = #{v => ~"b", items => [#{id => ~"a"}, #{id => ~"b"}]},
    HTML = iolist_to_binary(arizona_render:render_to_iolist(Mod:render(B))),
    ?assertEqual([~"b"], selected_values(HTML), HTML),
    {_, Snap0, V0} = arizona_render:render(Mod:render(B), #{}),
    {_Ops, Snap1, _} = arizona_diff:diff(Mod:render(B#{v => ~"a"}), Snap0, V0, #{v => true}),
    ?assertEqual([~"a"], selected_values(iolist_to_binary(arizona_render:snapshot_html(Snap1)))).

%% A hoisted value whose definition reads a name the callback rebinds is refused
%% like any hoisted read there.
shadowing_hoisted_read_is_refused(Config) when is_list(Config) ->
    assert_parse_error(
        "-module(sel_hoist_shadow). -export([render/1]). render(Bindings) -> "
        "V = arizona_template:get(v, Bindings), "
        "arizona_template:target(arizona_html, {select, [{value, V}], ["
        "arizona_template:each(fun(Bindings) -> {option, [{value, "
        "arizona_template:get(id, Bindings)}], []} end, [])]}). ",
        fun
            ({hoisted_var_captured, fun_param, 'Bindings', 'V'}) -> true;
            (_) -> false
        end
    ).

%% A literal value needs no runtime check: the options it matches are marked in
%% the static markup.
static_value_selects_at_compile_time(Config) when is_list(Config) ->
    Mod = compile_module(
        "-module(sel_static). -export([render/1]). render(_Bindings) -> "
        "arizona_template:target(arizona_html, {select, [{value, ~\"b\"}], "
        "[{option, [{value, ~\"a\"}], []}, "
        "{option, [{value, ~\"b\"}], []}]}). "
    ),
    Tmpl = Mod:render(#{}),
    ?assertEqual([], maps:get(d, Tmpl)),
    ?assertEqual(
        ~"<select><option value=\"a\"></option><option value=\"b\" selected></option></select>",
        iolist_to_binary(arizona_render:render_to_iolist(Tmpl))
    ).

%% Static option text is markup, so a reference in it is decoded as the parser decodes
%% it, at compile time: the option is valued by that text and stays static, no slot.
static_text_with_references_folds(Config) when is_list(Config) ->
    Mod = compile_module(
        "-module(sel_refs). -export([render/1]). render(_Bindings) -> "
        "arizona_template:target(arizona_html, {select, [{value, ~\"R&D\"}], "
        "[{option, [], [~\"R&amp;D\"]}, {option, [], [<<\"Caf&eacute;\">>]}]}). "
    ),
    Tmpl = Mod:render(#{}),
    ?assertEqual([], maps:get(d, Tmpl)),
    ?assertEqual(
        ~"<select><option selected>R&amp;D</option><option>Caf&eacute;</option></select>",
        iolist_to_binary(arizona_render:render_to_iolist(Tmpl))
    ),
    Dyn = compile_module(
        select_src_mod(
            "sel_refs_dyn", "{option, [], [<<\"Caf&eacute;\">>]}, {option, [], [~\"x\"]}"
        )
    ),
    HTML = iolist_to_binary(
        arizona_render:render_to_iolist(Dyn:render(#{v => <<"Caf", 16#E9/utf8>>}))
    ),
    ?assertEqual(nomatch, binary:match(HTML, [~"<!--az", ~" value="]), HTML),
    ?assertMatch({match, _}, re:run(HTML, ~"<option[^>]* selected[^>]*>Caf&eacute;"), HTML).

%% The parser normalizes newlines and NUL before an attribute value exists, so a value
%% and an option compare as it reads them on every path: an option the transform
%% marks, and the value a select keeps for the client (a JSON array under `multiple`).
values_compare_as_the_parser_reads_them(Config) when is_list(Config) ->
    Mod = compile_module(
        select_src_mod("sel_crlf", "{option, [{value, ~\"a\\nb\"}], []}, {option, [], [~\"z\"]}")
    ),
    HTML = iolist_to_binary(arizona_render:render_to_iolist(Mod:render(#{v => ~"a\r\nb"}))),
    ?assertEqual(1, count_selected(HTML), HTML),
    Static = compile_module(
        "-module(sel_crlf_static). -export([render/1]). render(_Bindings) -> "
        "arizona_template:target(arizona_html, {select, [{value, ~\"a\\r\\nb\"}], "
        "[{option, [{value, ~\"a\\nb\"}], []}]}). "
    ),
    ?assertEqual(
        1, count_selected(iolist_to_binary(arizona_render:render_to_iolist(Static:render(#{}))))
    ),
    ?assert(arizona_template:chosen(arizona_html, <<"a", 0>>, <<"a", 16#FFFD/utf8>>, false)),
    ?assertEqual(
        ~"[\"a\\nb\",\"c\\nd\"]",
        arizona_template:chooser_value(arizona_html, [~"a\r\nb", ~"c\rd"], true)
    ),
    ?assertEqual([~"a\nb"], arizona_template:picks(arizona_html, ~"a\r\nb", false)),
    %% A NUL in text is dropped by the tree builder, on every path that reads text.
    ?assertEqual({text, ~"ab"}, arizona_html:markup_text(<<"a", 0, "b">>, text)),
    ?assertEqual({markup, ~"ab"}, arizona_html:markup_text(<<"<b>a", 0, "</b>b">>, text)),
    ?assertEqual(~"ab", arizona_template:choice_text(arizona_html, [<<"a", 0>>, ~"b"])).

%% A raw a helper returns into an option's value is text, escaped where it renders, so
%% the value the page holds is the one the option is compared by: a select valued by
%% that text selects it, and one valued by what the text would decode to does not.
helper_raw_option_value_is_its_text(Config) when is_list(Config) ->
    Mod = compile_module(
        "-module(sel_helper_raw). -export([render/1]). render(Bindings) -> "
        "arizona_template:target(arizona_html, {select, "
        "[{value, arizona_template:get(v, Bindings)}], "
        "[{option, [{value, h(~\"a&amp;b\")}], [~\"x\"]}, {option, [], [~\"y\"]}]}). "
        "h(X) -> arizona_template:raw(X). "
    ),
    Text = iolist_to_binary(arizona_render:render_to_iolist(Mod:render(#{v => ~"a&amp;b"}))),
    ?assertMatch({match, _}, re:run(Text, ~"value=\"a&amp;amp;b\" selected"), Text),
    ?assertEqual(1, count_selected(Text), Text),
    Decoded = iolist_to_binary(arizona_render:render_to_iolist(Mod:render(#{v => ~"a&b"}))),
    ?assertEqual(0, count_selected(Decoded), Decoded),
    {Live, _Snap, _Views} = arizona_render:render(Mod:render(#{v => ~"a&amp;b"}), #{}),
    ?assertEqual(1, count_selected(iolist_to_binary(Live)), Live).

%% A raw a helper returns is text wherever the value is compared, as it renders: an
%% option or a select valued by one compares as the escaped text the page holds, on
%% the SSR, live and diff paths, in a select that keeps its value for the client, and
%% beside a literal `?raw` part, which is the only part spliced.
helper_raw_value_compares_as_its_text(Config) when is_list(Config) ->
    Opt = compile_module(
        helper_raw_src(
            "sel_helper_raw_opt",
            "{value, arizona_template:get(v, Bindings)}",
            "{option, [{value, h(~\"a&amp;b\")}], []}, {option, [], [~\"y\"]}"
        )
    ),
    assert_picked([~"a&amp;amp;b"], Opt, #{v => ~"a&amp;b"}),
    assert_picked([], Opt, #{v => ~"a&b"}),
    {_HTML, Snap0, V0} = arizona_render:render(Opt:render(#{v => ~"y"}), #{}),
    {Ops, _Snap1, _V1} = arizona_diff:diff(
        Opt:render(#{v => ~"a&amp;b"}), Snap0, V0, #{v => true}
    ),
    ?assertEqual([{remove, ~"selected"}, {set, ~"selected"}], lists:sort(attr_ops(Ops)), Ops),
    {Decoded, _Snap2, _V2} = arizona_diff:diff(Opt:render(#{v => ~"a&b"}), Snap0, V0, #{v => true}),
    ?assertEqual([{remove, ~"selected"}], attr_ops(Decoded), Decoded),
    Sel = compile_module(
        helper_raw_src(
            "sel_helper_raw_sel",
            "{value, h(arizona_template:get(r, Bindings))}",
            "{option, [{value, ~\"a&b\"}], []}, {option, [{value, ~\"a&amp;b\"}], []}, "
            "arizona_template:raw(arizona_template:get(o, Bindings))"
        )
    ),
    B = #{r => ~"a&amp;b", o => ~"<option>a&amp;b</option><option>a&amp;amp;b</option>"},
    HTML = ssr_picked([~"a&amp;amp;b", ~"a&amp;amp;b"], Sel, B),
    ?assertMatch({match, _}, re:run(HTML, ~"<select[^>]* value=\"a&amp;amp;b\""), HTML),
    {Live, Snap3, V3} = arizona_render:render(Sel:render(B), #{}),
    ?assertMatch(
        {match, _}, re:run(iolist_to_binary(Live), ~"<select[^>]* value=\"a&amp;amp;b\"")
    ),
    {SelOps, _Snap4, _V4} = arizona_diff:diff(
        Sel:render(B#{r => ~"&lt;z&gt;"}), Snap3, V3, #{r => true}
    ),
    ?assertMatch(
        [~"&lt;z&gt;"], [V || [?OP_SET_ATTR, _, ~"value", V] <- flatten_ops(SelOps)], SelOps
    ),
    Mixed = compile_module(
        helper_raw_src(
            "sel_helper_raw_mixed",
            "{value, arizona_template:get(v, Bindings)}",
            "{option, [{value, [h(~\"&amp;\"), arizona_template:raw(~\"b\")]}], []}, "
            "{option, [], [~\"y\"]}"
        )
    ),
    assert_picked([~"&amp;amp;b"], Mixed, #{v => ~"&amp;b"}),
    assert_picked([], Mixed, #{v => ~"&b"}),
    %% A helper's raw around a map is not text: the comparison refuses it as an
    %% attribute value refuses one, though the select renders no value of its own.
    MapRaw = compile_module(
        helper_raw_src(
            "sel_helper_raw_map",
            "{value, h(arizona_template:get(m, Bindings))}",
            "{option, [{value, ~\"a\"}], []}"
        )
    ),
    M = #{s => [~"a"], d => []},
    ?assertError(
        {arizona_loc, _, {raw_template, M}}, ssr_html(MapRaw, #{m => M})
    ),
    ?assertError(
        {arizona_loc, _, {raw_template, M}}, arizona_render:render(MapRaw:render(#{m => M}), #{})
    ).

%% A `?raw` attribute value is markup: the page holds it with its character references
%% decoded, so an option valued `?raw(~"a&amp;b")` is the option `a&b` names, on the
%% SSR, live and diff paths, and folded when both sides are literal.
raw_option_value_reads_as_the_page_holds_it(Config) when is_list(Config) ->
    Mod = compile_module(
        raw_src(
            "sel_raw_opt",
            "{value, arizona_template:get(v, Bindings)}",
            "{option, [{value, arizona_template:raw(~\"a&amp;b\")}], [~\"x\"]}, "
            "{option, [], [~\"y\"]}"
        )
    ),
    assert_picked([~"a&amp;b"], Mod, #{v => ~"a&b"}),
    assert_picked([~"y"], Mod, #{v => ~"y"}),
    {_HTML, Snap0, V0} = arizona_render:render(Mod:render(#{v => ~"y"}), #{}),
    {Ops, _Snap1, _V1} = arizona_diff:diff(Mod:render(#{v => ~"a&b"}), Snap0, V0, #{v => true}),
    ?assertEqual([{remove, ~"selected"}, {set, ~"selected"}], lists:sort(attr_ops(Ops)), Ops),
    Static = compile_module(
        raw_src(
            "sel_raw_opt_static",
            "{value, ~\"a&b\"}",
            "{option, [{value, arizona_template:raw(~\"a&amp;b\")}], []}, "
            "{option, [{value, arizona_template:raw(~\"a&amp;amp;b\")}], []}"
        )
    ),
    %% The values render as raw attributes; whether each is picked is static.
    ?assertEqual(
        [], [N || {_, {attr, ~"selected" = N, _}, _} <- maps:get(d, Static:render(#{}))]
    ),
    assert_picked([~"a&amp;b"], Static, #{}),
    %% Read in attribute context: a legacy reference without `;` before an
    %% alphanumeric is text, not a reference.
    ?assertEqual(
        {text, ~"&ampx & x\n"}, arizona_html:markup_text(~"&ampx &amp x\r\n", attribute)
    ).

%% The reverse: a select valued by markup picks the option whose value the page holds
%% equal to it, whether the transform marks the options, the SSR render marks them in
%% a child it cannot see into, or the select keeps its value for the client (as the
%% text the page holds, escaped, and under `multiple` each pick read the same way).
raw_select_value_reads_as_the_page_holds_it(Config) when is_list(Config) ->
    Mod = compile_module(
        raw_src(
            "sel_raw_sel",
            "{value, arizona_template:raw(arizona_template:get(r, Bindings))}",
            "{option, [{value, ~\"a&b\"}], []}, {option, [], [~\"y\"]}"
        )
    ),
    assert_picked([~"a&amp;b"], Mod, #{r => ~"a&amp;b"}),
    Opaque = compile_module(
        raw_src(
            "sel_raw_sel_opaque",
            "{value, arizona_template:raw(arizona_template:get(r, Bindings))}",
            "{option, [{value, ~\"a&b\"}], []}, "
            "arizona_template:raw(arizona_template:get(o, Bindings))"
        )
    ),
    B = #{r => ~"a&amp;b", o => ~"<option>a&amp;b</option><option>z</option>"},
    HTML = ssr_picked([~"a&amp;b", ~"a&amp;b"], Opaque, B),
    ?assertMatch({match, _}, re:run(HTML, ~"<select[^>]* value=\"a&amp;b\""), HTML),
    {Live, Snap0, V0} = arizona_render:render(Opaque:render(B), #{}),
    ?assertMatch({match, _}, re:run(iolist_to_binary(Live), ~"<select[^>]* value=\"a&amp;b\"")),
    {Ops, _Snap1, _V1} = arizona_diff:diff(
        Opaque:render(B#{r => ~"&lt;z&gt;"}), Snap0, V0, #{r => true}
    ),
    ?assertMatch([~"<z>"], [V || [?OP_SET_ATTR, _, ~"value", V] <- flatten_ops(Ops)], Ops),
    Multiple = compile_module(
        raw_src(
            "sel_raw_multi",
            "multiple, {value, [arizona_template:raw(~\"a&amp;b\"), "
            "arizona_template:get(v, Bindings)]}",
            "{option, [{value, ~\"a&b\"}], []}, {option, [], [~\"c\"]}, "
            "arizona_template:raw(arizona_template:get(o, Bindings))"
        )
    ),
    Multi = ssr_picked(
        [~"a&amp;b", ~"c", ~"a&amp;b"], Multiple, #{v => ~"c", o => ~"<option>a&amp;b</option>"}
    ),
    ?assertNotEqual(nomatch, binary:match(Multi, ~"value=\"[&quot;a&amp;b&quot;,&quot;c&quot;]\"")),
    %% What the transform hands the comparison for a literal `?raw` is its markup; a bare
    %% raw is a helper's, which the page holds as its text.
    ?assertEqual(
        ~"[\"a&b\"]",
        arizona_template:chooser_value(
            arizona_html,
            [arizona_template:raw_attr(arizona_html, [arizona_template:raw(~"a&amp;b")])],
            true
        )
    ),
    ?assertEqual(
        ~"[\"a&amp;b\"]",
        arizona_template:chooser_value(arizona_html, [arizona_template:raw(~"a&amp;b")], true)
    ).

%% An interpolated value with a literal `?raw` part is one markup text, the raw part
%% verbatim and the rest escaped: compared as the page holds it, option or select.
interpolated_raw_value_reads_as_one_text(Config) when is_list(Config) ->
    Opt = compile_module(
        raw_src(
            "sel_raw_interp_opt",
            "{value, arizona_template:get(v, Bindings)}",
            "{option, [{value, [~\"a\", arizona_template:raw(~\"&amp;b\")]}], []}, "
            "{option, [{value, [~\"&\", arizona_template:raw(~\"amp;\")]}], []}"
        )
    ),
    assert_picked([~"a&amp;b"], Opt, #{v => ~"a&b"}),
    assert_picked([~"&amp;amp;"], Opt, #{v => ~"&amp;"}),
    Sel = compile_module(
        raw_src(
            "sel_raw_interp_sel",
            "{value, [arizona_template:get(v, Bindings), arizona_template:raw(~\"&amp;b\")]}",
            "{option, [{value, ~\"a&b\"}], []}, "
            "arizona_template:raw(arizona_template:get(o, Bindings))"
        )
    ),
    HTML = ssr_picked(
        [~"a&amp;b", ~"a&amp;b"], Sel, #{v => ~"a", o => ~"<option>a&amp;b</option>"}
    ),
    ?assertMatch({match, _}, re:run(HTML, ~"<select[^>]* value=\"a&amp;b\""), HTML).

%% A `?local` in a list with static parts: one text with them as its affixes, and
%% under `multiple` one pick each, the local its own. A select holding an option the
%% transform cannot see keeps the value; the descriptor hands the client the static
%% parts on either side, each a pick of its own.
interpolated_local_value_compiles(Config) when is_list(Config) ->
    lists:foreach(
        fun({Mod, Multiple, Value, Picked}) ->
            M = compile_module(
                raw_src(
                    Mod,
                    [Multiple, "{value, [~\"a\", arizona_template:local(k, ~\"c\")]}"],
                    local_options(opaque)
                )
            ),
            HTML = ssr_picked(Picked, M, #{}),
            ?assertEqual(Value, select_value(HTML), HTML),
            ?assertEqual(
                #{~"a" => #{~"value" => ~"k"}, ~"ap" => #{~"value" => [[~"a"], []]}},
                local_descriptor(HTML),
                HTML
            )
        end,
        [
            {"sel_interp_local", "", ~"ac", [~"ac"]},
            {"sel_interp_local_m", "multiple, ", ~"[\"a\",\"c\"]", [~"a", ~"c"]}
        ]
    ).

%% `multiple` decided at runtime decides what the parts of a `?local` value are: one
%% text without it, one pick each with it.
local_value_parts_follow_runtime_multiple(Config) when is_list(Config) ->
    M = compile_module(
        raw_src(
            "sel_local_runtime_multiple",
            "{multiple, arizona_template:get(m, Bindings)}, "
            "{value, [~\"a\", arizona_template:local(k, arizona_template:get(x, Bindings))]}",
            local_options(literal)
        )
    ),
    One = ssr_picked([~"ac"], M, #{m => false, x => ~"c"}),
    ?assertEqual(~"ac", select_value(One), One),
    Many = ssr_picked([~"a", ~"c"], M, #{m => true, x => ~"c"}),
    ?assertEqual(~"[\"a\",\"c\"]", select_value(Many), Many),
    Own = ssr_picked([~"a", ~"c", ~"ac"], M, #{m => true, x => [~"c", ~"ac"]}),
    ?assertEqual(~"[\"a\",\"c\",\"ac\"]", select_value(Own), Own),
    None = ssr_picked([~"a"], M, #{m => true, x => false}),
    ?assertEqual(~"[\"a\"]", select_value(None), None),
    %% `multiple` is present whatever it holds but `false`, as the attribute renders.
    Present = ssr_picked([~"a", ~"c"], M, #{m => ~"", x => ~"c"}),
    ?assertEqual(~"[\"a\",\"c\"]", select_value(Present), Present).

%% Whatever the shape of a `?local` value -- whole or interpolated, a select whose
%% options are all marked in the template or one holding an option only the SSR
%% render marks -- the value the select carries names exactly the options the page
%% selects, so the client applying it at load moves nothing.
local_value_names_what_the_page_selects(Config) when is_list(Config) ->
    lists:foreach(
        fun({N, {Multiple, Value, Options, Picked}}) ->
            Mod = "sel_local_" ++ integer_to_list(N),
            M = compile_module(raw_src(Mod, [Multiple, "{value, ", Value, "}"], Options)),
            HTML = ssr_picked(Picked, M, #{x => ~"c", xs => [~"a", ~"c"]}),
            ?assertEqual(Picked, value_picks(HTML), {Mod, HTML})
        end,
        lists:enumerate(
            [
                {"", W, local_options(Kind), Picked}
             || Kind <- [literal, opaque],
                {W, Picked} <- [
                    {"arizona_template:local(k, ~\"ac\")", [~"ac"]},
                    {"[~\"a\", arizona_template:local(k, ~\"c\")]", [~"ac"]},
                    {"[~\"a\", arizona_template:local(k, arizona_template:get(x, Bindings))]", [
                        ~"ac"
                    ]}
                ]
            ] ++
                [
                    {"multiple, ", W, local_options(Kind), Picked}
                 || Kind <- [literal, opaque],
                    {W, Picked} <- [
                        {"arizona_template:local(k, [~\"a\", ~\"c\"])", [~"a", ~"c"]},
                        {"arizona_template:local(k, ~\"ac\")", [~"ac"]},
                        {"[~\"a\", arizona_template:local(k, ~\"c\")]", [~"a", ~"c"]},
                        {"[arizona_template:local(k, ~\"a\"), ~\"c\"]", [~"a", ~"c"]},
                        {"[~\"a\", ~\"c\", arizona_template:local(k, ~\"ac\")]", [
                            ~"a", ~"c", ~"ac"
                        ]},
                        {"[~\"a\", arizona_template:local(k, [~\"c\", ~\"ac\"])]", [
                            ~"a", ~"c", ~"ac"
                        ]},
                        {"[~\"a\", arizona_template:local(k, false)]", [~"a"]},
                        {"arizona_template:local(k, arizona_template:get(xs, Bindings))", [
                            ~"a", ~"c"
                        ]},
                        {"[~\"a\", arizona_template:local(k, arizona_template:get(x, Bindings))]", [
                            ~"a", ~"c"
                        ]}
                    ]
                ]
        )
    ).

%% Under `multiple` the value is the JSON array of the texts it picks on every path:
%% a whole `?local` holds the list of picks (or one pick), and in a list each static
%% part is a pick and the local its own.
multiple_local_value_is_a_json_array(Config) when is_list(Config) ->
    lists:foreach(
        fun({N, {Value, Kind, Json}}) ->
            Mod = "sel_local_json_" ++ integer_to_list(N),
            M = compile_module(
                raw_src(Mod, ["multiple, {value, ", Value, "}"], local_options(Kind))
            ),
            HTML = ssr_html(M, #{}),
            ?assertEqual(Json, select_value(HTML), {Mod, HTML})
        end,
        lists:enumerate([
            {V, Kind, Json}
         || Kind <- [literal, opaque],
            {V, Json} <- [
                {"arizona_template:local(k, [~\"a\", ~\"c\"])", ~"[\"a\",\"c\"]"},
                {"arizona_template:local(k, ~\"a&b\")", ~"[\"a&b\"]"},
                {"[~\"a\", arizona_template:local(k, ~\"c\")]", ~"[\"a\",\"c\"]"},
                {"[~\"a&b\", arizona_template:local(k, [~\"c\", 1])]", ~"[\"a&b\",\"c\",\"1\"]"}
            ]
        ])
    ).

%% A `?local` value is the client's: the options it names are marked as the page
%% loads, from its initial, and never again by the server. A later change of the
%% binding the initial came from ships no `selected` op, so the server never moves a
%% selection the client owns, wherever the options come from.
local_value_marks_render_once(Config) when is_list(Config) ->
    lists:foreach(
        fun({N, {Multiple, Value, Kind}}) ->
            Mod = "sel_local_once_" ++ integer_to_list(N),
            M = compile_module(
                raw_src(Mod, [Multiple, "{value, ", Value, "}"], once_options(Kind))
            ),
            B0 = #{x => ~"c", xs => [~"c"], os => [~"a", ~"c", ~"ac"]},
            {_, Snap0, V0} = arizona_render:render(M:render(B0), #{}),
            B1 = B0#{x => ~"a", xs => [~"a"]},
            {Ops, _Snap1, _} = arizona_diff:diff(M:render(B1), Snap0, V0, #{x => true, xs => true}),
            ?assertEqual([], attr_ops(Ops), {Mod, Ops}),
            {AllOps, _, _} = arizona_diff:diff(M:render(B1), Snap0, V0),
            ?assertEqual([], attr_ops(AllOps), {Mod, AllOps})
        end,
        lists:enumerate([
            {Multiple, Value, Kind}
         || Kind <- [literal, each_list, each_stream],
            {Multiple, Value} <- [
                {"", "arizona_template:local(k, arizona_template:get(x, Bindings))"},
                {"", "[~\"a\", arizona_template:local(k, arizona_template:get(x, Bindings))]"},
                {"multiple, ", "arizona_template:local(k, arizona_template:get(xs, Bindings))"},
                {"multiple, ",
                    "[~\"a\", arizona_template:local(k, arizona_template:get(x, Bindings))]"}
            ]
        ])
    ).

%% Only the page marks the options a `?local` value names, from its initial. What the
%% server renders later -- a live render, an option a list or stream `?each` inserts --
%% carries no `selected` of its own: by then the selection is the client's, which
%% marks a new option from the value it holds, so the binding the initial read cannot
%% take the user's pick over.
local_value_marks_only_the_page(Config) when is_list(Config) ->
    lists:foreach(
        fun({N, {Multiple, Kind}}) ->
            Mod = "sel_local_page_" ++ integer_to_list(N),
            Value = "arizona_template:local(k, arizona_template:get(x, Bindings))",
            M = compile_module(
                raw_src(Mod, [Multiple, "{value, ", Value, "}"], page_options(Kind))
            ),
            B0 = #{x => ~"c", os => [~"a", ~"c", ~"ac"], s => page_stream([~"a", ~"c", ~"ac"])},
            _ = ssr_picked([~"c"], M, B0),
            {Live, Snap0, V0} = arizona_render:render(M:render(B0), #{}),
            ?assertEqual([], selected_values(iolist_to_binary(Live)), {Mod, Live}),
            %% The client owns the selection: no option is the server's.
            ?assertEqual(nomatch, binary:match(iolist_to_binary(Live), ~"az-bound"), {Mod, Live}),
            B1 = B0#{
                x => ~"aa",
                os => [~"a", ~"c", ~"ac", ~"aa"],
                s => arizona_stream:insert(maps:get(s, B0), ~"aa")
            },
            {Ops, _, _} = arizona_diff:diff(M:render(B1), Snap0, V0, #{
                x => true, os => true, s => true
            }),
            ?assertMatch([_ | _], Ops, Mod),
            ?assertEqual(nomatch, binary:match(term_text(Ops), ~"selected"), {Mod, Ops})
        end,
        lists:enumerate([
            {Multiple, Kind}
         || Kind <- [each_list, each_stream], Multiple <- ["", "multiple, "]
        ])
    ),
    %% A mark the transform decides at compile time is the page's alone too.
    Lit = compile_module(
        raw_src(
            "sel_local_page_lit",
            "{value, arizona_template:local(k, ~\"c\")}",
            local_options(literal)
        )
    ),
    _ = ssr_picked([~"c"], Lit, #{}),
    {LitLive, _, _} = arizona_render:render(Lit:render(#{}), #{}),
    ?assertEqual([], selected_values(iolist_to_binary(LitLive)), LitLive).

%% --------------------------------------------------------------------
%% Helpers
%% --------------------------------------------------------------------

page_options(each_list) ->
    "arizona_template:each(fun(O) -> {option, [], [O]} end, arizona_template:get(os, Bindings))";
page_options(each_stream) ->
    "arizona_template:each(fun(O, K) -> {option, [{az_key, K}], [O]} end, "
    "arizona_template:get(s, Bindings))".

page_stream(Items) ->
    arizona_stream:new(fun(O) -> O end, Items).

term_text(Term) ->
    iolist_to_binary(io_lib:format("~0p", [Term])).

once_options(literal) ->
    "{option, [], [~\"a\"]}, {option, [], [~\"c\"]}, {option, [], [~\"ac\"]}, "
    "{option, [], [~\"aa\"]}";
once_options(each_list) ->
    "arizona_template:each(fun(O) -> {option, [], [O]} end, arizona_template:get(os, Bindings))";
once_options(each_stream) ->
    "arizona_template:each(fun(O, K) -> {option, [{az_key, K}], [O]} end, "
    "arizona_stream:new(fun(O) -> O end, arizona_template:get(os, Bindings)))".

%% Options named `a`, `c` and `ac`: written literally, or the last two in raw markup
%% the transform cannot see into.
local_options(literal) ->
    "{option, [], [~\"a\"]}, {option, [], [~\"c\"]}, {option, [], [~\"ac\"]}";
local_options(opaque) ->
    "{option, [], [~\"a\"]}, "
    "arizona_template:raw(~\"<option>c</option><option>ac</option>\")".

%% The text the page's select holds as its `value`, the HTML parser's reading.
select_value(HTML) ->
    case re:run(HTML, ~"<select[^>]* value=\"([^\"]*)\"", [{capture, all_but_first, binary}]) of
        {match, [V]} -> unescape_attr(V);
        nomatch -> none
    end.

%% The options a select's `value` names, read as the client reads it: under
%% `multiple` a JSON array of texts, else one text.
value_picks(HTML) ->
    V = select_value(HTML),
    case re:run(HTML, ~"<select[^>]* multiple[ >]") of
        {match, _} -> json:decode(V);
        nomatch -> [V]
    end.

local_descriptor(HTML) ->
    {match, [D]} = re:run(
        HTML, ~"<select[^>]* az-local=\"([^\"]*)\"", [{capture, all_but_first, binary}]
    ),
    json:decode(unescape_attr(D)).

unescape_attr(V) ->
    lists:foldl(
        fun({From, To}, Acc) -> binary:replace(Acc, From, To, [global]) end,
        V,
        [{~"&quot;", ~"\""}, {~"&lt;", ~"<"}, {~"&gt;", ~">"}, {~"&amp;", ~"&"}]
    ).

shapes() ->
    [
        literal,
        optgroup,
        conditional,
        each_list,
        each_stream,
        each_named,
        helper,
        helper_each,
        helper_returns_each,
        helper_arg_each,
        helper_arg_each_qualified,
        field,
        spelled,
        hoisted,
        nested_each,
        text_options
    ].

opaque_shapes() ->
    [
        component,
        local_component,
        remote_helper,
        runtime_template,
        raw_options,
        each_opaque_item,
        mixed
    ].

opaque_bindings(V) ->
    Opts = [~"a", ~"b", ~"c"],
    #{
        v => V,
        vs => [~"a", ~"c"],
        opts => Opts,
        options_tmpl => arizona_select_value:option_items(#{opts => Opts})
    }.

opaque_ssr(Shape, V) ->
    iolist_to_binary(
        arizona_render:render_to_iolist(arizona_select_value:Shape(opaque_bindings(V)))
    ).

ssr(Shape, V) ->
    iolist_to_binary(arizona_render:render_to_iolist(tmpl(Shape, V))).

tmpl(each_stream, V) ->
    arizona_select_value:each_stream(stream_bindings(V));
tmpl(nested_each, V) ->
    arizona_select_value:nested_each(#{
        v => V, groups => [{~"G1", [~"a", ~"b"]}, {~"G2", [~"c"]}]
    });
tmpl(Shape, V) ->
    arizona_select_value:Shape(#{v => V, more => true, opts => [~"a", ~"b", ~"c"]}).

stream_bindings(V) ->
    Items = [#{id => Id, label => string:uppercase(Id)} || Id <- [~"a", ~"b", ~"c"]],
    #{v => V, items => arizona_stream:new(fun(#{id := Id}) -> Id end, Items)}.

%% The values of the options rendered `selected`, in document order: an option's
%% `value`, else its text.
selected_values(HTML) ->
    Option = ~"<option([^>]*)>(.*?)(?=</?option|</?optgroup|</select|$)",
    case re:run(HTML, Option, [global, caseless, dotall, {capture, all_but_first, binary}]) of
        {match, Opens} ->
            [option_value(Attrs, Text) || [Attrs, Text] <- Opens, is_selected(Attrs)];
        nomatch ->
            []
    end.

option_value(Attrs, Text) ->
    case
        re:run(Attrs, ~"value=[\"']([^\"']*)[\"']", [caseless, {capture, all_but_first, binary}])
    of
        {match, [V]} -> V;
        nomatch -> string:trim(re:replace(Text, ~"<!--.*?-->", ~"", [global, {return, binary}]))
    end.

is_selected(Attrs) ->
    re:run(Attrs, ~" selected(?:$| |=)") =/= nomatch.

strip_tags(HTML) ->
    re:replace(HTML, ~"<[^>]*>", ~"", [global, {return, binary}]).

count_selected(HTML) ->
    length(binary:matches(HTML, ~" selected")).

assert_selection_ops(nested_each, _Ops) ->
    ok;
assert_selection_ops(Shape, Ops) ->
    ?assertEqual(
        [{remove, ~"selected"}, {set, ~"selected"}], lists:sort(attr_ops(Ops)), {Shape, Ops}
    ).

%% Every attribute op, nested ones (item patches, list patches) included.
attr_ops(Ops) ->
    [
        case Op of
            [?OP_SET_ATTR, _Az, Name, _V] -> {set, Name};
            [?OP_REM_ATTR, _Az, Name] -> {remove, Name}
        end
     || Op <- flatten_ops(Ops), hd(Op) =:= ?OP_SET_ATTR orelse hd(Op) =:= ?OP_REM_ATTR
    ].

flatten_ops(Ops) ->
    lists:flatmap(fun flatten_op/1, Ops).

flatten_op([?OP_ITEM_PATCH | Rest]) ->
    flatten_ops(lists:last(Rest));
flatten_op([?OP_LIST_PATCH, _Az, Patches]) ->
    lists:flatmap(fun flatten_op/1, Patches);
flatten_op([Code | _] = Op) when is_integer(Code) ->
    [Op];
flatten_op(Ops) when is_list(Ops) ->
    flatten_ops(Ops).

%% The values of the options `Mod` renders selected, at SSR and in a live render alike.
assert_picked(Picked, Mod, Bindings) ->
    HTML = ssr_html(Mod, Bindings),
    ?assertEqual(Picked, selected_values(HTML), HTML),
    {Live, _Snap, _Views} = arizona_render:render(Mod:render(Bindings), #{}),
    ?assertEqual(Picked, selected_values(iolist_to_binary(Live)), Live).

%% The values of the options the SSR page selects (a live render leaves the options of
%% a child the transform cannot see into to the client); returns the page.
ssr_picked(Picked, Mod, Bindings) ->
    HTML = ssr_html(Mod, Bindings),
    ?assertEqual(Picked, selected_values(HTML), HTML),
    HTML.

ssr_html(Mod, Bindings) ->
    iolist_to_binary(arizona_render:render_to_iolist(Mod:render(Bindings))).

raw_src(Mod, SelectAttrs, Children) ->
    lists:flatten([
        "-module(",
        Mod,
        "). -export([render/1]). render(Bindings) -> "
        "arizona_template:target(arizona_html, {select, [",
        SelectAttrs,
        "], [",
        Children,
        "]}). "
    ]).

helper_raw_src(Mod, SelectAttrs, Children) ->
    raw_src(Mod, SelectAttrs, Children) ++ "h(X) -> arizona_template:raw(X). ".

select_src(Child) ->
    select_src_mod("sel_err", Child).

select_src_mod(Mod, Child) ->
    lists:flatten([
        "-module(",
        Mod,
        "). -export([render/1]). render(Bindings) -> "
        "arizona_template:target(arizona_html, {select, "
        "[{value, arizona_template:get(v, Bindings)}], [",
        Child,
        "]}). opts(B) -> arizona_template:target(arizona_html, [arizona_template:each("
        "fun(O) -> {option, [{value, O}], [O]} end, maps:get(o, B, []))]). "
    ]).

compile_module(Source) ->
    TransformedForms = arizona_parse_transform:parse_transform(parse_forms(Source), []),
    {ok, Mod, Bin} = compile:forms(TransformedForms, [return_errors]),
    {module, Mod} = code:load_binary(Mod, "", Bin),
    Mod.

assert_parse_error(Source, MatchFun) ->
    case arizona_parse_transform:parse_transform(parse_forms(Source), []) of
        {error, [{_File, [{_Line, arizona_parse_transform, Reason}]}], []} ->
            ?assert(MatchFun(Reason), Reason),
            %% Every refusal reads as a sentence, not a crash in format_error/1.
            ?assert(is_list(arizona_parse_transform:format_error(Reason)));
        Other ->
            error({expected_parse_error, Source, Other})
    end.

parse_forms(Source) ->
    {ok, Tokens, _} = erl_scan:string(Source),
    [
        begin
            {ok, F} = erl_parse:parse_form(Toks),
            F
        end
     || Toks <- split_forms(Tokens)
    ].

split_forms(Tokens) ->
    split_forms(Tokens, [], []).

split_forms([], [], Acc) ->
    lists:reverse(Acc);
split_forms([{dot, _} = Dot | Rest], Cur, Acc) ->
    split_forms(Rest, [], [lists:reverse([Dot | Cur]) | Acc]);
split_forms([T | Rest], Cur, Acc) ->
    split_forms(Rest, [T | Cur], Acc).

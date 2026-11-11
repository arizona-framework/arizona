-module(arizona_select_value_SUITE).
-moduledoc """
A `value` on an HTML `<select>` selects the option whose value equals it.

HTML has no such attribute: the parser ignores it and the page loads on the first
option, while a diff writes the `.value` property. So the transform marks the matching
option `selected` instead, at first render and on every diff, wherever the option is
written (see `arizona_select_value`), and refuses an option it cannot see.
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
    nothing_selected_without_a_match/1,
    diff_moves_selected_to_the_new_option/1,
    diff_is_fine_grained/1,
    diff_of_unrelated_binding_is_empty/1,
    stream_item_update_keeps_selection/1,
    static_generation_marks_the_chosen_option/1,
    nodiff_select_marks_the_chosen_option/1,
    local_value_loads_on_its_initial/1,
    select_inside_each_item/1,
    json_value_prop_is_unchanged/1,
    opaque_child_is_refused/1,
    option_needs_a_static_value/1,
    authored_selected_is_refused/1,
    captured_value_is_refused/1,
    static_value_selects_at_compile_time/1
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
            nothing_selected_without_a_match,
            static_generation_marks_the_chosen_option,
            nodiff_select_marks_the_chosen_option,
            local_value_loads_on_its_initial,
            select_inside_each_item,
            json_value_prop_is_unchanged
        ]},
        {diff, [parallel], [
            diff_moves_selected_to_the_new_option,
            diff_is_fine_grained,
            diff_of_unrelated_binding_is_empty,
            stream_item_update_keeps_selection
        ]},
        {compile, [parallel], [
            opaque_child_is_refused,
            option_needs_a_static_value,
            authored_selected_is_refused,
            captured_value_is_refused,
            static_value_selects_at_compile_time
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
    ?assertMatch({match, _}, re:run(HTML, ~"<option[^>]* selected> a </option>"), HTML),
    ?assertEqual(1, count_selected(HTML)).

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

static_generation_marks_the_chosen_option(Config) when is_list(Config) ->
    Dir = filename:join(proplists:get_value(priv_dir, Config), "select_static"),
    {[Path], []} = arizona_static:generate(
        Dir, [{arizona_select_value_page, ~"index.html", #{bindings => #{v => ~"b"}}}]
    ),
    {ok, HTML} = file:read_file(Path),
    %% Literal, list ?each and stream ?each on `b`; the multiple select on its two.
    ?assertEqual([~"b", ~"b", ~"b", ~"a", ~"c"], selected_values(HTML)).

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

%% An inserted item ships as its HTML, or as statics plus values.
payload_html(#{~"s" := [S | Statics], ~"d" := Values}) ->
    iolist_to_binary([S | lists:zipwith(fun(V, St) -> [V, St] end, Values, Statics)]);
payload_html(HTML) ->
    iolist_to_binary(HTML).

%% --------------------------------------------------------------------
%% Compile
%% --------------------------------------------------------------------

%% Options the transform cannot see would render unselected at page load, so the
%% select refuses them: a remote helper, a component, a runtime value, raw markup,
%% and a static fragment spelling an option.
opaque_child_is_refused(Config) when is_list(Config) ->
    Opaque = fun
        ({select_value, {opaque_child, _Tag, _What}}) -> true;
        (_) -> false
    end,
    lists:foreach(
        fun(Child) -> assert_parse_error(select_src(Child), Opaque) end,
        [
            "other_mod:options()",
            "arizona_template:stateless(fun opts/1, #{})",
            "arizona_template:get(opts, Bindings)",
            "arizona_template:raw(~\"<option>a</option>\")",
            "~\"<option value=\\\"a\\\">A</option>\"",
            "{optgroup, [], [~\"<OPTION>a\"]}"
        ]
    ),
    %% An `?each` handed to a helper that renders the select compiles before the
    %% select is seen, and a nested template is built apart from it.
    assert_parse_error(
        "-module(sel_err_field). -export([render/1]). render(Bindings) -> "
        "arizona_template:target(arizona_html, {form, [], [field(arizona_template:get(v, "
        "Bindings), arizona_template:each(fun(O) -> {option, [{value, O}], [O]} end, "
        "arizona_template:get(opts, Bindings)))]}). "
        "field(V, Opts) -> {select, [{value, V}], [Opts]}. ",
        fun
            ({select_value, {opaque_child, ~"select", compiled_each}}) -> true;
            (_) -> false
        end
    ),
    assert_parse_error(
        select_src(
            "{optgroup, [], [arizona_template:target(arizona_html, "
            "{option, [], [~\"a\"]})]}"
        ),
        fun
            ({select_value, {opaque_child, ~"select", template}}) -> true;
            (_) -> false
        end
    ),
    %% A plain-text static and an empty one are no options and pass.
    _ = compile_module(select_src_mod("sel_ws", "~\" \", <<>>")).

option_needs_a_static_value(Config) when is_list(Config) ->
    assert_parse_error(
        select_src("{option, [], [arizona_template:get(label, Bindings)]}"),
        fun
            ({select_value, {option_value, _Tag, _Chooser}}) -> true;
            (_) -> false
        end
    ).

authored_selected_is_refused(Config) when is_list(Config) ->
    Conflict = fun
        ({select_value, {option_selected, _Tag, _Flag, _Chooser}}) -> true;
        (_) -> false
    end,
    assert_parse_error(select_src("{option, [{value, ~\"a\"}, selected], []}"), Conflict),
    assert_parse_error(
        select_src("{option, [{value, ~\"a\"}, {<<\"SELECTED\">>, false}], []}"), Conflict
    ).

%% The value is read inside every option, so an `?each` parameter shadowing a
%% variable it reads would read the item instead.
captured_value_is_refused(Config) when is_list(Config) ->
    assert_parse_error(
        select_src(
            "arizona_template:each(fun(Bindings) -> {option, [{value, "
            "arizona_template:get(id, Bindings)}], []} end, [])"
        ),
        fun
            ({select_value, {captured, ['Bindings']}}) -> true;
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

%% --------------------------------------------------------------------
%% Helpers
%% --------------------------------------------------------------------

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
        nested_each
    ].

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

%% The values of the options rendered `selected`, in document order.
selected_values(HTML) ->
    case re:run(HTML, ~"<option([^>]*)>", [global, caseless, {capture, all_but_first, binary}]) of
        {match, Opens} ->
            [option_value(Attrs) || [Attrs] <- Opens, is_selected(Attrs)];
        nomatch ->
            []
    end.

option_value(Attrs) ->
    case re:run(Attrs, ~"value=\"([^\"]*)\"", [caseless, {capture, all_but_first, binary}]) of
        {match, [V]} -> V;
        nomatch -> no_value
    end.

is_selected(Attrs) ->
    re:run(Attrs, ~" selected(?:$| |=)") =/= nomatch.

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
        "]}). opts(_) -> arizona_template:target(arizona_html, []). "
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

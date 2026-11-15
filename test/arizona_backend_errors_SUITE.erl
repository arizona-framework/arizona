-module(arizona_backend_errors_SUITE).
-moduledoc false.
-behaviour(ct_suite).
-include_lib("stdlib/include/assert.hrl").

%% The render target words its own errors: a compile error a backend words is
%% reported under the backend module (the compiler calls its `format_error/1`), and a
%% runtime one is raised with the backend in its `error_info`. Every backend words
%% every such reason its capability answers let reach it -- `arizona_text_backend`
%% stands in for a target outside core, like `arizona_terminal` -- and the core keeps
%% no wording of its own for them.

-export([all/0]).
-export([groups/0]).

-export([compile_dynamic_in_raw_text/1]).
-export([compile_local_in_raw_text/1]).
-export([compile_markup_in_raw_text/1]).
-export([compile_local_orphaned/1]).
-export([compile_nodiff_value/1]).
-export([compile_nested_nodiff/1]).
-export([compile_effect_in_content/1]).
-export([compile_each_stream_body_control_flow/1]).
-export([compile_option_selected/1]).
-export([compile_missing_callback_is_neutral/1]).
-export([compile_local_needs_markup_text/1]).
-export([compile_choices_need_mark_choices/1]).
-export([compile_local_unsupported/1]).
-export([compile_computed_element_tag/1]).
-export([compile_element_in_comprehension/1]).
-export([compile_each_body_not_element/1]).
-export([compile_each_stream_body_not_element/1]).
-export([compile_each_named_fun_multi_clause/1]).
-export([compile_each_named_fun_undefined/1]).
-export([compile_each_remote_fun_ref/1]).
-export([compile_improper_template_list/1]).
-export([compile_computed_binary_tag/1]).
-export([compile_empty_tag/1]).
-export([compile_binary_tag_runs_nothing/1]).
-export([compile_tag_not_text/1]).
-export([compile_tag_not_text_message/1]).
-export([compile_binary_tag_folds/1]).
-export([compile_binary_tag_not_literal_text/1]).
-export([compile_not_code_point/1]).
-export([compile_attr_name/1]).
-export([compile_name_breaks_markup/1]).
-export([runtime_markup_in_raw_text/1]).
-export([runtime_stream_each_keyless/1]).
-export([runtime_stream_each_multi_root/1]).
-export([runtime_stateful_in_layout/1]).
-export([runtime_cross_target_child_is_neutral/1]).
-export([runtime_effect_in_content_is_neutral/1]).

all() ->
    [{group, compile_time}, {group, runtime}].

groups() ->
    [
        {compile_time, [parallel], [
            compile_dynamic_in_raw_text,
            compile_local_in_raw_text,
            compile_markup_in_raw_text,
            compile_local_orphaned,
            compile_nodiff_value,
            compile_nested_nodiff,
            compile_effect_in_content,
            compile_each_stream_body_control_flow,
            compile_option_selected,
            compile_missing_callback_is_neutral,
            compile_local_needs_markup_text,
            compile_choices_need_mark_choices,
            compile_local_unsupported,
            compile_computed_element_tag,
            compile_element_in_comprehension,
            compile_each_body_not_element,
            compile_each_stream_body_not_element,
            compile_each_named_fun_multi_clause,
            compile_each_named_fun_undefined,
            compile_each_remote_fun_ref,
            compile_improper_template_list,
            compile_computed_binary_tag,
            compile_empty_tag,
            compile_binary_tag_runs_nothing,
            compile_tag_not_text,
            compile_tag_not_text_message,
            compile_binary_tag_folds,
            compile_binary_tag_not_literal_text,
            compile_not_code_point,
            compile_attr_name,
            compile_name_breaks_markup
        ]},
        {runtime, [], [
            runtime_markup_in_raw_text,
            runtime_stream_each_keyless,
            runtime_stream_each_multi_root,
            runtime_stateful_in_layout,
            runtime_cross_target_child_is_neutral,
            runtime_effect_in_content_is_neutral
        ]}
    ].

%% --------------------------------------------------------------------
%% Compile time
%% --------------------------------------------------------------------

compile_dynamic_in_raw_text(Config) when is_list(Config) ->
    {Html, Reason} = compile_error(
        "be_dyn_raw", html("{script, [], [arizona_template:get(x, Bindings)]}")
    ),
    ?assertEqual({arizona_html, dynamic_in_raw_text}, {Html, Reason}),
    ?assert(contains(Html:format_error(Reason), ~"<script>")).

compile_local_in_raw_text(Config) when is_list(Config) ->
    {Html, Reason} = compile_error(
        "be_local_raw", html("{textarea, [], [arizona_template:local(k, <<\"x\">>)]}")
    ),
    ?assertEqual({arizona_html, local_in_raw_text}, {Html, Reason}),
    ?assert(contains(Html:format_error(Reason), ~"textarea")).

compile_markup_in_raw_text(Config) when is_list(Config) ->
    {Html, Reason} = compile_error(
        "be_markup_raw", html("{title, [], [{b, [], [arizona_template:get(t, Bindings)]}]}")
    ),
    ?assertEqual({arizona_html, markup_in_raw_text}, {Html, Reason}),
    ?assert(contains(Html:format_error(Reason), ~"<title>")).

compile_local_orphaned(Config) when is_list(Config) ->
    Orphan = fun(Tag) -> "[{" ++ Tag ++ ", [], []}, arizona_template:local(k, <<\"x\">>)]" end,
    {Html, Reason} = compile_error("be_local_orphan", html(Orphan("p"))),
    ?assertEqual({arizona_html, local_orphaned}, {Html, Reason}),
    ?assert(contains(Html:format_error(Reason), ~"az-local")),
    ?assertEqual(
        {arizona_json, local_orphaned},
        compile_error("be_local_orphan_json", json(Orphan("'Text'")))
    ),
    ?assertNot(contains(arizona_json:format_error(Reason), ~"az-local")),
    ?assertEqual(
        {arizona_text_backend, local_orphaned},
        compile_error("be_local_orphan_text", text(Orphan("line")))
    ).

compile_nodiff_value(Config) when is_list(Config) ->
    {Html, Reason} = compile_error("be_nodiff_value", html("{'div', [{'az-nodiff', true}], []}")),
    ?assertEqual({arizona_html, nodiff_value}, {Html, Reason}),
    ?assert(contains(Html:format_error(Reason), ~"rendered HTML")),
    %% JSON strips the directive: no value of it reaches the document.
    ?assertEqual(
        {arizona_json, nodiff_value},
        compile_error("be_nodiff_value_json", json("{'Column', [{'az-nodiff', true}], []}"))
    ),
    ?assertNot(contains(arizona_json:format_error(Reason), ~"HTML")),
    ?assertEqual(
        {arizona_text_backend, nodiff_value},
        compile_error("be_nodiff_value_text", text("{line, [{'az-nodiff', true}], []}"))
    ).

compile_nested_nodiff(Config) when is_list(Config) ->
    Nested = fun(Outer, Inner) ->
        "{" ++ Outer ++ ", [], [{" ++ Inner ++ ", ['az-nodiff'], []}]}"
    end,
    {Html, Reason} = compile_error("be_nested_nodiff", html(Nested("'div'", "span"))),
    ?assertEqual({arizona_html, nested_nodiff}, {Html, Reason}),
    ?assert(contains(Html:format_error(Reason), ~"DOM")),
    ?assertEqual(
        {arizona_json, nested_nodiff},
        compile_error("be_nested_nodiff_json", json(Nested("'Column'", "'Text'")))
    ),
    ?assertNot(contains(arizona_json:format_error(Reason), ~"DOM")),
    ?assertEqual(
        {arizona_text_backend, nested_nodiff},
        compile_error("be_nested_nodiff_text", text(Nested("line", "span")))
    ).

compile_effect_in_content(Config) when is_list(Config) ->
    {Html, Reason} = compile_error(
        "be_effect_html", html("{'div', [], [arizona_js:hide(<<\"#x\">>)]}")
    ),
    ?assertEqual({arizona_html, effect_in_content}, {Html, Reason}),
    ?assert(contains(Html:format_error(Reason), ~"az_click")),
    ?assertEqual(
        {arizona_json, effect_in_content},
        compile_error(
            "be_effect_json", json("{'Column', [], [arizona_android:push_event(<<\"x\">>)]}")
        )
    ),
    JsonMsg = arizona_json:format_error(Reason),
    ?assert(contains(JsonMsg, ~"on_tap")),
    ?assertNot(contains(JsonMsg, ~"az_click")),
    ?assertEqual(
        {arizona_text_backend, effect_in_content},
        compile_error("be_effect_text", text("{line, [], [arizona_effect:push_event(<<\"x\">>)]}"))
    ).

compile_each_stream_body_control_flow(Config) when is_list(Config) ->
    Each = fun(Tag) ->
        "[arizona_template:each(fun(I, K) -> case I of a -> {" ++ Tag ++
            ", [{az_key, K}], []}; _ -> {" ++ Tag ++
            ", [{az_key, K}], [I]} end end, arizona_template:get(s, Bindings))]"
    end,
    {Html, Reason} = compile_error("be_each_cf_html", html("{ul, [], " ++ Each("li") ++ "}")),
    ?assertEqual({arizona_html, each_stream_body_control_flow}, {Html, Reason}),
    ?assert(contains(Html:format_error(Reason), ~"table/tr")),
    ?assertEqual(
        {arizona_json, Reason},
        compile_error("be_each_cf_json", json("{'Column', [], " ++ Each("'Text'") ++ "}"))
    ),
    JsonMsg = arizona_json:format_error(Reason),
    ?assert(contains(JsonMsg, ~"'Row'")),
    ?assertNot(contains(JsonMsg, ~"table/tr")),
    ?assertEqual(
        {arizona_text_backend, Reason},
        compile_error("be_each_cf_text", text("{col, [], " ++ Each("line") ++ "}"))
    ).

compile_option_selected(Config) when is_list(Config) ->
    {Html, Reason} = compile_error(
        "be_option_selected",
        html(
            "{select, [{value, arizona_template:get(v, Bindings)}], "
            "[{option, [{value, <<\"a\">>}, selected], [<<\"A\">>]}]}"
        )
    ),
    ?assertMatch({arizona_html, {select_value, {option_selected, _, _, _}}}, {Html, Reason}),
    ?assert(contains(Html:format_error(Reason), ~"<option>")).

%% Raised for a backend that lacks a callback, so the core words it, naming no
%% target's syntax.
compile_missing_callback_is_neutral(Config) when is_list(Config) ->
    lists:foreach(
        fun(Reason) ->
            Msg = core(Reason),
            ?assert(contains(Msg, ~"some_backend"), Msg),
            ?assertNot(contains(Msg, ~"<"), Msg)
        end,
        [
            {missing_callback, some_backend, {markup_text, 2}, {choice_text, ~"choice"}},
            {missing_callback, some_backend, {markup_text, 2}, local_affix},
            {missing_callback, some_backend, {mark_choices, 2}, choices}
        ]
    ).

%% A backend supporting `?local` reads the fixed text beside an attribute one through
%% markup_text/2: one without it is refused where the local is written, not as a crash
%% of the transform.
compile_local_needs_markup_text(Config) when is_list(Config) ->
    Backend = backend_without(be_no_markup_text, [{markup_text, 2}]),
    Source =
        "-module(be_local_no_mt).\n-export([render/1]).\nrender(_Bindings) ->\n"
        "    arizona_template:target(be_no_markup_text, {a, [{title, [\n"
        "        <<\"x\">>, arizona_template:local(k, <<\"v\">>)]}], []}).\n",
    ?assertMatch(
        {error,
            [
                {_File, [
                    {5, arizona_parse_transform,
                        {missing_callback, Backend, {markup_text, 2}, local_affix}}
                ]}
            ],
            []},
        arizona_parse_transform:parse_transform(forms(Source), [])
    ),
    %% A whole local has no fixed text to read.
    _ = compile(
        "be_whole_local_no_mt",
        "arizona_template:target(be_no_markup_text, "
        "{a, [{title, arizona_template:local(k, <<\"v\">>)}], []})"
    ).

%% A chooser's child the transform cannot see into is marked by the backend's
%% mark_choices/2 when the page renders: a backend naming the selection roles without
%% it is refused where the child is compiled, not as `undef` at render.
compile_choices_need_mark_choices(Config) when is_list(Config) ->
    Backend = backend_without(be_no_mark_choices, [{mark_choices, 2}]),
    Source =
        "-module(be_choices_no_mc).\n-export([render/1]).\nrender(Bindings) ->\n"
        "    arizona_template:target(be_no_mark_choices, {select,\n"
        "        [{value, arizona_template:get(v, Bindings)}],\n"
        "        [arizona_template:get(opts, Bindings)]}).\n",
    ?assertMatch(
        {error,
            [
                {_File, [
                    {
                        {6, 10},
                        arizona_parse_transform,
                        {missing_callback, Backend, {mark_choices, 2}, choices}
                    }
                ]}
            ],
            []},
        arizona_parse_transform:parse_transform(forms(Source, {1, 1}), [])
    ),
    %% Choices the transform rewrites need no marking at render.
    _ = compile(
        "be_seen_choices_no_mc",
        "arizona_template:target(be_no_mark_choices, {select, "
        "[{value, arizona_template:get(v, Bindings)}], "
        "[{option, [{value, <<\"a\">>}], []}]})"
    ).

compile_local_unsupported(Config) when is_list(Config) ->
    {Json, Reason} = compile_error(
        "be_local_json", json("{'Column', [], [arizona_template:local(k, <<\"x\">>)]}")
    ),
    ?assertEqual({arizona_json, local_unsupported}, {Json, Reason}),
    ?assert(contains(Json:format_error(Reason), ~"native")),
    ?assertEqual(
        {arizona_text_backend, local_unsupported},
        compile_error("be_local_text", text("{line, [], [arizona_template:local(k, <<\"x\">>)]}"))
    ).

%% The element-shape and ?each advice names the target's own elements.
compile_computed_element_tag(Config) when is_list(Config) ->
    Computed = fun(Tag) ->
        "{" ++ Tag ++ ", [], [{arizona_template:get(tag, Bindings), [], [~\"x\"]}]}"
    end,
    assert_tag_advice(
        computed_element_tag,
        {"be_tag_html", html(Computed("'div'"))},
        {"be_tag_json", json(Computed("'Column'"))},
        {"be_tag_text", text(Computed("line"))}
    ),
    %% A literal binary is a tag too.
    ?assert(contains(arizona_html:format_error(computed_element_tag), ~"atom or binary")),
    ?assert(contains(arizona_json:format_error(computed_element_tag), ~"atom or binary")).

compile_element_in_comprehension(Config) when is_list(Config) ->
    Lc = fun(Outer, Inner) ->
        "{" ++ Outer ++ ", [], [[{" ++ Inner ++
            ", [], [X]} || X <- arizona_template:get(xs, Bindings)]]}"
    end,
    assert_tag_advice(
        element_in_comprehension,
        {"be_lc_html", html(Lc("ul", "li"))},
        {"be_lc_json", json(Lc("'Column'", "'Text'"))},
        {"be_lc_text", text(Lc("col", "line"))}
    ).

compile_each_body_not_element(Config) when is_list(Config) ->
    Each = fun(Outer) ->
        "{" ++ Outer ++
            ", [], [arizona_template:each(fun(I) -> I end, "
            "arizona_template:get(xs, Bindings))]}"
    end,
    assert_tag_advice(
        each_body_not_element,
        {"be_body_html", html(Each("ul"))},
        {"be_body_json", json(Each("'Column'"))},
        {"be_body_text", text(Each("col"))}
    ).

compile_each_stream_body_not_element(Config) when is_list(Config) ->
    Each = fun(Outer) ->
        "{" ++ Outer ++
            ", [], [arizona_template:each(fun(I, _K) -> I end, "
            "arizona_template:get(xs, Bindings))]}"
    end,
    assert_tag_advice(
        each_stream_body_not_element,
        {"be_sbody_html", html(Each("ul"))},
        {"be_sbody_json", json(Each("'Column'"))},
        {"be_sbody_text", text(Each("col"))}
    ).

compile_each_named_fun_multi_clause(Config) when is_list(Config) ->
    Each = fun(Outer, Inner) ->
        {
            "{" ++ Outer ++
                ", [], [arizona_template:each(fun row/1, "
                "arizona_template:get(xs, Bindings))]}",
            "row(a) -> {" ++ Inner ++ ", [], []}; row(_) -> {" ++ Inner ++ ", [], [~\"b\"]}."
        }
    end,
    assert_tag_advice(
        each_named_fun_multi_clause,
        with_forms("be_multi_clause_html", Each("ul", "li"), fun html/1),
        with_forms("be_multi_clause_json", Each("'Column'", "'Text'"), fun json/1),
        with_forms("be_multi_clause_text", Each("col", "line"), fun text/1)
    ).

compile_each_named_fun_undefined(Config) when is_list(Config) ->
    Each = fun(Outer) ->
        "{" ++ Outer ++
            ", [], [arizona_template:each(fun missing/1, "
            "arizona_template:get(xs, Bindings))]}"
    end,
    assert_tag_advice(
        each_named_fun_undefined,
        {"be_undef_html", html(Each("ul"))},
        {"be_undef_json", json(Each("'Column'"))},
        {"be_undef_text", text(Each("col"))}
    ).

compile_each_remote_fun_ref(Config) when is_list(Config) ->
    Each = fun(Outer) ->
        "{" ++ Outer ++
            ", [], [arizona_template:each(fun other_mod:row/1, "
            "arizona_template:get(xs, Bindings))]}"
    end,
    assert_tag_advice(
        each_remote_fun_ref,
        {"be_remote_html", html(Each("ul"))},
        {"be_remote_json", json(Each("'Column'"))},
        {"be_remote_text", text(Each("col"))}
    ).

%% A binary tag is a literal only when it is a static binary: one built from a value
%% is chosen at runtime, the same mistake as a computed atom.
compile_computed_binary_tag(Config) when is_list(Config) ->
    Computed = fun(Outer) ->
        "{" ++ Outer ++
            ", [], [{<<\"x-\", (arizona_template:get(k, Bindings))/binary>>, "
            "[], [~\"a\"]}]}"
    end,
    ?assertEqual(
        {arizona_html, computed_element_tag},
        compile_error("be_bin_tag_html", html(Computed("'div'")))
    ),
    ?assertEqual(
        {arizona_json, computed_element_tag},
        compile_error("be_bin_tag_json", json(Computed("'Column'")))
    ).

%% Every way a name is written as text folds: a string, several, `$x` and integer code
%% points, an explicit /utf8.
compile_binary_tag_folds(Config) when is_list(Config) ->
    Cases = [
        {"be_fold_str", "<<\"ab\">>", ~"ab"},
        {"be_fold_parts", "<<\"a\", \"b\">>", ~"ab"},
        {"be_fold_char", "<<$a, $b>>", ~"ab"},
        {"be_fold_int", "<<97, 98>>", ~"ab"},
        {"be_fold_utf8", "<<\"\\x{e9}\"/utf8>>", ~"\x{e9}"}
    ],
    [
        begin
            Html = compile(Name ++ "_html", html("{'div', [], [{" ++ Tag ++ ", [], [~\"x\"]}]}")),
            HtmlOut = iolist_to_binary(arizona_render:render_to_iolist(Html:render(#{}))),
            ?assert(contains(HtmlOut, <<"<", Text/binary, ">x</", Text/binary, ">">>), HtmlOut),
            Json = compile(Name ++ "_json", json("{'Column', [], [{" ++ Tag ++ ", [], []}]}")),
            JsonOut = iolist_to_binary(arizona_render:render_to_iolist(Json:render(#{}))),
            ?assert(contains(JsonOut, <<"\"type\":\"", Text/binary, "\"">>), JsonOut)
        end
     || {Name, Tag, Text} <- Cases
    ].

%% Any other binary of literals lays out bytes rather than spelling a name: refused
%% without being built, so a size that would exhaust memory costs nothing. Under a
%% select too, whose chooser pass reads tags first.
compile_binary_tag_not_literal_text(Config) when is_list(Config) ->
    Tags = [
        "<<97:8>>",
        "<<\"a\":8>>",
        "<<\"ab\"/utf16>>",
        "<<\"ab\"/binary>>",
        "<<1:1>>",
        "<<\"a\", 1:7>>",
        "<<\"a\\tb\":8>>",
        "<<0:(1 bsl 40)>>",
        "<<(<<0:(1 bsl 40)>>):8/bits>>",
        "<<0:(<<0:(1 bsl 40)>>)>>"
    ],
    [
        begin
            {Micros, Result} = timer:tc(fun() ->
                compile_error("be_not_lit", html("{'div', [], [{" ++ Tag ++ ", [], []}]}"))
            end),
            ?assertEqual({arizona_parse_transform, tag_not_literal_text}, Result, Tag),
            ?assert(Micros < 1000000, {Tag, Micros}),
            ?assertEqual(
                {arizona_parse_transform, tag_not_literal_text},
                compile_error("be_not_lit_json", json("{'Column', [], [{" ++ Tag ++ ", [], []}]}")),
                Tag
            )
        end
     || Tag <- Tags
    ],
    ?assertEqual(
        {arizona_parse_transform, tag_not_literal_text},
        compile_error(
            "be_not_lit_select",
            html("{select, [{value, arizona_template:get(v, Bindings)}], [{<<1:1>>, [], []}]}")
        )
    ),
    ?assert(
        contains(arizona_parse_transform:format_error(tag_not_literal_text), ~"not literal text")
    ).

%% An element needs a name: an empty one renders `<>`/`</>` in HTML and an empty
%% type in JSON, which no client reads as an element.
compile_empty_tag(Config) when is_list(Config) ->
    Cases = [{"be_empty_bin", "<<>>"}, {"be_empty_sigil", "~\"\""}, {"be_empty_atom", "''"}],
    [
        begin
            {Formatter, Reason} = compile_error(
                Name ++ "_html", html("{'div', [], [{" ++ Tag ++ ", [], [~\"x\"]}]}")
            ),
            ?assertEqual({arizona_parse_transform, tag_empty}, {Formatter, Reason}, Name),
            ?assert(contains(Formatter:format_error(Reason), ~"empty")),
            ?assertEqual(
                {arizona_parse_transform, tag_empty},
                compile_error(Name ++ "_json", json("{'Column', [], [{" ++ Tag ++ ", [], []}]}")),
                Name
            )
        end
     || {Name, Tag} <- Cases
    ].

%% Nothing in a binary tag is run while the module compiles: a send, a receive or a
%% record field is not text, and is refused unevaluated.
compile_binary_tag_runs_nothing(Config) when is_list(Config) ->
    true = register(be_tag_spy, self()),
    try
        Cases = [
            {"be_tag_send", "<<(be_tag_spy ! 97):8>>", ""},
            {"be_tag_receive", "<<(receive after 3000 -> 97 end):8>>", ""},
            {"be_tag_record", "<<\"h\", (#r.b + 46):8>>", "-record(r, {a, b}). "}
        ],
        [
            begin
                Src =
                    source(Name, html("{'div', [], [{" ++ Tag ++ ", [], [~\"x\"]}]}")) ++ " " ++
                        Extra,
                ?assertEqual(
                    {arizona_parse_transform, tag_not_literal_text}, compile_error_forms(Src), Name
                )
            end
         || {Name, Tag, Extra} <- Cases
        ],
        receive
            Sent -> ct:fail({ran_at_compile_time, Sent})
        after 0 -> ok
        end
    after
        true = unregister(be_tag_spy)
    end.

%% A tag is UTF-8 text with no control character, however it is written.
compile_tag_not_text(Config) when is_list(Config) ->
    Cases = [
        {"be_ctl_sigil", "~\"a\\tb\""},
        {"be_ctl_bin", "<<\"a\\tb\">>"},
        {"be_ctl_atom", "'a\\tb'"},
        {"be_ctl_nul", "<<0>>"}
    ],
    [
        begin
            ?assertMatch(
                {arizona_parse_transform, {tag_not_text, _}},
                compile_error(
                    Name ++ "_html", html("{'div', [], [{" ++ Tag ++ ", [], [~\"x\"]}]}")
                ),
                Name
            ),
            ?assertMatch(
                {arizona_parse_transform, {tag_not_text, _}},
                compile_error(Name ++ "_json", json("{'Column', [], [{" ++ Tag ++ ", [], []}]}")),
                Name
            )
        end
     || {Name, Tag} <- Cases
    ].

%% An integer segment that names no code point is refused wherever a template reads
%% it as a character -- a tag, an attribute name or value, content -- rather than
%% rendered as whatever byte Erlang makes of it. A sized segment lays out bytes.
compile_not_code_point(Config) when is_list(Config) ->
    Cases = [
        {"be_cp_tag_surrogate", "{'div', [], [{<<55296>>, [], []}]}", 55296},
        {"be_cp_tag_past", "{'div', [], [{<<1114112>>, [], []}]}", 1114112},
        {"be_cp_tag_negative", "{'div', [], [{<<-1>>, [], []}]}", -1},
        {"be_cp_content", "{p, [], [<<55296>>]}", 55296},
        {"be_cp_attr_value", "{p, [{class, <<-1>>}], []}", -1},
        {"be_cp_attr_list", "{p, [{class, [<<\"a\">>, <<1114112>>]}], []}", 1114112},
        {"be_cp_attr_name", "{p, [{<<55296>>, ~\"x\"}], []}", 55296},
        {"be_cp_neg_char_content", "{p, [], [<<\"a\", -$a>>]}", -97},
        {"be_cp_neg_char_attr", "{p, [{title, <<\"a\", -$a>>}], []}", -97},
        {"be_cp_neg_char_utf8", "{p, [], [<<\"a\", -$a/utf8>>]}", -97},
        {"be_cp_neg_char_tag", "{'div', [], [{<<-$a>>, [], []}]}", -97},
        {"be_cp_body", "<<\"a\", -1>>", -1},
        {"be_cp_literal_list", "{p, [], [[<<\"a\", -1>>, <<\"b\">>]]}", -1},
        {"be_cp_literal_list_attr", "{p, [{title, [<<\"a\", -1>>, \"b\"]}], []}", -1},
        {"be_cp_fragment_head", "[<<\"a\", -1>>, {p, [], []}]", -1},
        {"be_cp_fragment_tail", "[{p, [], []}, <<\"a\", 55296>>]", 55296}
    ],
    [
        ?assertEqual(
            {arizona_parse_transform, {not_code_point, N}}, compile_error(Name, html(Body)), Name
        )
     || {Name, Body, N} <- Cases
    ],
    ?assert(
        contains(arizona_parse_transform:format_error({not_code_point, 55296}), ~"code point")
    ),
    Sized = compile("be_cp_sized", html("{p, [], [<<255:8>>]}")),
    ?assertMatch(#{}, Sized:render(#{})),
    %% A binary built from a value does not fold: Erlang builds it at runtime, and a
    %% bare integer segment there is a byte, as in any Erlang code.
    Built = compile(
        "be_cp_built",
        html(
            "{p, [{title, <<(arizona_template:get(x, Bindings))/binary, -1>>}], "
            "[<<(arizona_template:get(x, Bindings))/binary, 256>>]}"
        )
    ),
    ?assertMatch(#{}, Built:render(#{x => ~"a"})),
    %% So is one a conditional returns, or one a ?raw wraps: a runtime value, whose
    %% bytes Erlang builds (233 renders as that byte, not as e-acute).
    Runtime = compile(
        "be_cp_runtime",
        html(
            "{p, [{title, case arizona_template:get(x, Bindings) of _ -> <<\"a\", -1>> end}], "
            "[case arizona_template:get(x, Bindings) of _ -> <<\"a\", 55296>> end, "
            "arizona_template:raw(<<\"b\", -1>>)]}"
        )
    ),
    %% A list holding a value read at runtime is one runtime value too.
    ValueList = compile(
        "be_cp_value_list",
        html(
            "{p, [{title, [<<\"a\", -1>>, arizona_template:get(x, Bindings)]}], "
            "[[<<\"a\", -1>>, arizona_template:get(x, Bindings)]]}"
        )
    ),
    ?assertMatch(#{}, ValueList:render(#{x => ~"a"})),
    JsonValueList = compile(
        "be_cp_value_list_json",
        json("{'Text', [{label, [<<\"a\", -1>>, arizona_template:get(x, Bindings)]}], []}")
    ),
    ?assertMatch(#{}, JsonValueList:render(#{x => ~"a"})),
    ?assertMatch(#{}, Runtime:render(#{x => ~"a"})).

%% The message quotes the tag as written, an atom as an atom, and bytes that are not
%% UTF-8 as bytes.
compile_tag_not_text_message(Config) when is_list(Config) ->
    {arizona_parse_transform, Reason} = compile_error(
        "be_ctl_msg", html("{'div', [], [{'a\\x{7f}', [], []}]}")
    ),
    Msg = arizona_parse_transform:format_error(Reason),
    ?assert(contains(Msg, ~"'a")),
    ?assertNot(contains(Msg, ~"<<97")),
    ?assert(contains(Msg, ~"without an ASCII space or control character")),
    ?assert(contains(arizona_parse_transform:format_error({tag_not_text, <<255>>}), ~"<<255>>")).

%% An attribute name is written as text, as a tag is: the forms that fold compile, the
%% rest are refused where the attribute is written.
compile_attr_name(Config) when is_list(Config) ->
    Ok = compile(
        "be_attr_ok",
        html("{p, [{<<\"data-x\">>, ~\"1\"}, {<<$d, $y>>, ~\"2\"}, <<\"hidden\">>], []}")
    ),
    Out = iolist_to_binary(arizona_render:render_to_iolist(Ok:render(#{}))),
    ?assert(contains(Out, ~"data-x=\"1\""), Out),
    ?assert(contains(Out, ~"dy=\"2\""), Out),
    Cases = [
        {"be_an_sized", "{p, [{<<97:8>>, ~\"x\"}], []}", attr_name_not_literal_text},
        {"be_an_bare_huge", "{p, [<<0:(1 bsl 40)>>], []}", attr_name_not_literal_text},
        {"be_an_utf16", "{p, [{<<\"a\"/utf16>>, ~\"x\"}], []}", attr_name_not_literal_text},
        {"be_an_empty_bin", "{p, [{<<>>, ~\"x\"}], []}", attr_name_empty},
        {"be_an_empty_atom", "{p, [{'', ~\"x\"}], []}", attr_name_empty}
    ],
    [
        ?assertEqual({arizona_parse_transform, Reason}, compile_error(Name, html(Body)), Name)
     || {Name, Body, Reason} <- Cases
    ],
    [
        ?assertMatch(
            {arizona_parse_transform, {attr_name_not_text, _}},
            compile_error(Name, html(Body)),
            Name
        )
     || {Name, Body} <- [
            {"be_an_nul", "{p, [{<<\"a\", 0>>, ~\"x\"}], []}"},
            {"be_an_ctl_atom", "{p, [{'a\\tb', ~\"x\"}], []}"}
        ]
    ],
    ?assertEqual(
        {arizona_parse_transform, attr_name_not_literal_text},
        compile_error("be_an_json", json("{'Text', [{<<97:8>>, ~\"x\"}], []}"))
    ),
    [
        ?assert(is_list(arizona_parse_transform:format_error(R)))
     || R <- [attr_name_not_literal_text, attr_name_empty, {attr_name_not_text, ~"a"}]
    ].

%% No target has a name with whitespace in it, so the core refuses one; the
%% characters that end or break a name only in HTML markup are refused by the HTML
%% target, and JSON, where a key is any string, takes them.
compile_name_breaks_markup(Config) when is_list(Config) ->
    [
        begin
            {arizona_parse_transform, {Reason, _} = Error} = compile_error(Name, Target(Body)),
            ?assert(contains(arizona_parse_transform:format_error(Error), ~"ASCII space"), Name)
        end
     || {Name, Target, Body, Reason} <- [
            {"be_ws_tag", fun html/1, "{'div', [], [{<<\"a b\">>, [], []}]}", tag_not_text},
            {"be_ws_tag_json", fun json/1, "{'Column', [], [{<<\"a b\">>, [], []}]}", tag_not_text},
            {"be_ws_attr", fun html/1, "{p, [{<<\"a b\">>, ~\"v\"}], []}", attr_name_not_text},
            {"be_ws_attr_json", fun json/1, "{'Text', [{<<\"a b\">>, ~\"v\"}], []}",
                attr_name_not_text}
        ]
    ],
    [
        begin
            {arizona_parse_transform, {render_reject, Message}} = compile_error(Name, html(Body)),
            ?assert(contains(Message, ~"HTML"), Name)
        end
     || {Name, Body} <- [
            {"be_html_tag_slash", "{'div', [], [{<<\"a/b\">>, [], []}]}"},
            {"be_html_tag_gt", "{'div', [], [{<<\"a>b\">>, [], []}]}"},
            {"be_html_attr_eq", "{p, [{<<\"a=b\">>, ~\"v\"}], []}"},
            {"be_html_attr_quote", "{p, [{<<\"a\\\"b\">>, ~\"v\"}], []}"},
            {"be_html_attr_apos", "{p, [{<<\"a'b\">>, ~\"v\"}], []}"},
            {"be_html_attr_lt", "{p, [{<<\"a<b\">>, ~\"v\"}], []}"},
            {"be_html_attr_atom", "{p, [{'a=b', ~\"v\"}], []}"},
            {"be_html_attr_bare", "{p, [<<\"a/b\">>], []}"},
            {"be_html_attr_dyn", "{p, [{<<\"a=b\">>, arizona_template:get(v, Bindings)}], []}"}
        ]
    ],
    Json = compile("be_json_key", json("{'Text', [{<<\"a=b\">>, ~\"v\"}], []}")),
    ?assertMatch(#{}, Json:render(#{})).

compile_improper_template_list(Config) when is_list(Config) ->
    Improper = fun(Outer, Inner) ->
        "{" ++ Outer ++ ", [], [{" ++ Inner ++ ", [], []} | arizona_template:get(xs, Bindings)]}"
    end,
    assert_tag_advice(
        improper_template_list,
        {"be_improper_html", html(Improper("ul", "li"))},
        {"be_improper_json", json(Improper("'Column'", "'Text'"))},
        {"be_improper_text", text(Improper("col", "line"))}
    ).

%% --------------------------------------------------------------------
%% Runtime
%% --------------------------------------------------------------------

runtime_markup_in_raw_text(Config) when is_list(Config) ->
    M = arizona_conditional_freeze,
    B = #{wrap => true, name => ~"x"},
    {Reason, Msg} = raised(fun() ->
        arizona_render:render_to_iolist(M:script_literal_raw_template(B))
    end),
    {arizona_loc, _, {markup_in_raw_text, ~"script", _} = Inner} = Reason,
    ?assert(contains(Msg, iolist_to_binary(arizona_html:format_error(Inner)))),
    ?assert(contains(Msg, ~"<script>")).

runtime_stream_each_keyless(Config) when is_list(Config) ->
    Stream = arizona_stream:new(fun(X) -> X end, [~"a"]),
    Html = compile(
        "be_keyless_html",
        html(
            "{ul, [], [arizona_template:each(fun(I, _K) -> {li, [], [I]} end, "
            "arizona_template:get(s, Bindings))]}"
        )
    ),
    {stream_each_keyless, HtmlMsg} = raised_render(Html, #{s => Stream}),
    ?assert(contains(HtmlMsg, iolist_to_binary(arizona_html:format_error(stream_each_keyless)))),
    Json = compile(
        "be_keyless_json",
        json(
            "{'Column', [], [arizona_template:each(fun(I, _K) -> {'Text', [], [I]} end, "
            "arizona_template:get(s, Bindings))]}"
        )
    ),
    {stream_each_keyless, JsonMsg} = raised_render(Json, #{s => Stream}),
    ?assert(contains(JsonMsg, iolist_to_binary(arizona_json:format_error(stream_each_keyless)))),
    ?assertNot(contains(JsonMsg, ~"{li,")).

runtime_stream_each_multi_root(Config) when is_list(Config) ->
    Stream = arizona_stream:new(fun(X) -> X end, [~"a"]),
    Each = fun(Tag) ->
        "[arizona_template:each(fun(I, K) -> [{" ++ Tag ++ ", [{az_key, K}], [I]}, {" ++
            Tag ++ ", [], [I]}] end, arizona_template:get(s, Bindings))]"
    end,
    Html = compile("be_multi_html", html("{ul, [], " ++ Each("li") ++ "}")),
    {stream_each_multi_root, HtmlMsg} = raised_render(Html, #{s => Stream}),
    ?assert(
        contains(HtmlMsg, iolist_to_binary(arizona_html:format_error(stream_each_multi_root)))
    ),
    Json = compile("be_multi_json", json("{'Column', [], " ++ Each("'Text'") ++ "}")),
    {stream_each_multi_root, JsonMsg} = raised_render(Json, #{s => Stream}),
    ?assert(
        contains(JsonMsg, iolist_to_binary(arizona_json:format_error(stream_each_multi_root)))
    ),
    %% This target rejects dynamic attributes, az_key included: it keys nothing.
    Text = compile(
        "be_multi_text",
        text(
            "{col, [], [arizona_template:each(fun(I, _K) -> [{line, [], [I]}, {line, [], [I]}] "
            "end, arizona_template:get(s, Bindings))]}"
        )
    ),
    {stream_each_multi_root, TextMsg} = raised_render(Text, #{s => Stream}),
    ?assert(
        contains(
            TextMsg, iolist_to_binary(arizona_text_backend:format_error(stream_each_multi_root))
        )
    ),
    ?assertNot(contains(JsonMsg, ~"{li,")).

runtime_stateful_in_layout(Config) when is_list(Config) ->
    {Reason, Msg} = raised(fun() ->
        arizona_render:render_view_to_iolist(arizona_about, #{
            bindings => #{title => ~"Reject"},
            layouts => [{arizona_stateful_layout, render}]
        })
    end),
    {arizona_loc, _, {stateful_in_layout, _, _, _} = Inner} = Reason,
    ?assert(contains(Msg, iolist_to_binary(arizona_html:format_error(Inner)))),
    ?assert(contains(Msg, ~"az-view")),
    JsonMsg = arizona_json:format_error(Inner),
    ?assertNot(contains(JsonMsg, ~"az-view")),
    ?assertNot(contains(JsonMsg, ~"az_patch")),
    %% A ?json client has no patch: no navigation keeps its view live.
    ?assertNot(contains(JsonMsg, ~"navigating between")),
    ?assert(contains(JsonMsg, ~"instead of navigating")).

runtime_cross_target_child_is_neutral(Config) when is_list(Config) ->
    #{general := Msg} = arizona_render:format_error(
        {cross_target_child, arizona_json, arizona_html}, []
    ),
    ?assert(contains(Msg, ~"?json")),
    ?assertNot(contains(Msg, ~"HTML")).

runtime_effect_in_content_is_neutral(Config) when is_list(Config) ->
    Msg = core_runtime({effect_in_content, {arizona_effect, [0]}}),
    ?assert(contains(Msg, ~"event attribute")),
    ?assertNot(contains(Msg, ~"az_click")).

%% --------------------------------------------------------------------
%% Helpers
%% --------------------------------------------------------------------

html(Body) -> "arizona_template:target(arizona_html, " ++ Body ++ ")".

json(Body) -> "arizona_template:target(arizona_json, " ++ Body ++ ")".

text(Body) -> "arizona_template:target(arizona_text_backend, " ++ Body ++ ")".

source(Mod, Body) ->
    "-module(" ++ Mod ++ "). -export([render/1]). render(Bindings) -> _ = Bindings, " ++
        Body ++ ".".

forms(Source) ->
    forms(Source, 1).

%% `Location` `{1, 1}` gives every form a column too.
forms(Source, Location) ->
    {ok, Tokens, _} = erl_scan:string(Source, Location),
    [
        begin
            {ok, F} = erl_parse:parse_form(Toks),
            F
        end
     || Toks <- split_forms(Tokens, [], [])
    ].

split_forms([], [], Acc) ->
    lists:reverse(Acc);
split_forms([{dot, _} = Dot | Rest], Current, Acc) ->
    split_forms(Rest, [], [lists:reverse([Dot | Current]) | Acc]);
split_forms([Token | Rest], Current, Acc) ->
    split_forms(Rest, [Token | Current], Acc).

%% `Reason` compiled from an `?html`, a `?json` and a test-target page: each is
%% reported under its own backend, HTML advises in `li`, JSON in its nodes.
assert_tag_advice(Reason, {HtmlMod, HtmlSrc}, {JsonMod, JsonSrc}, {TextMod, TextSrc}) ->
    ?assertEqual({arizona_html, Reason}, compile_error_src(HtmlMod, HtmlSrc)),
    ?assert(contains(arizona_html:format_error(Reason), ~"li")),
    ?assertEqual({arizona_json, Reason}, compile_error_src(JsonMod, JsonSrc)),
    JsonMsg = arizona_json:format_error(Reason),
    ?assertNot(contains(JsonMsg, ~"{li")),
    ?assertNot(contains(JsonMsg, ~"'li'")),
    ?assert(contains(JsonMsg, ~"{'")),
    ?assertEqual({arizona_text_backend, Reason}, compile_error_src(TextMod, TextSrc)).

with_forms(Mod, {Body, Forms}, Target) ->
    {Mod, source(Mod, Target(Body)) ++ " " ++ Forms}.

compile_error_src(_Mod, Source) when is_list(Source), hd(Source) =:= $- ->
    compile_error_forms(Source);
compile_error_src(Mod, Body) ->
    compile_error_forms(source(Mod, Body)).

compile_error_forms(Source) ->
    {error, [{_File, [{_Loc, Formatter, Reason}]}], []} =
        arizona_parse_transform:parse_transform(forms(Source), []),
    {Formatter, Reason}.

%% The module the compiler asks to word the error, and the reason it hands it.
compile_error(Mod, Body) ->
    {error, [{_File, [{_Loc, Formatter, Reason}]}], []} =
        arizona_parse_transform:parse_transform(forms(source(Mod, Body)), []),
    {Formatter, Reason}.

compile(Mod, Body) ->
    Forms = arizona_parse_transform:parse_transform(forms(source(Mod, Body)), []),
    {ok, M, Bin} = compile:forms(Forms, [return_errors]),
    {module, M} = code:load_binary(M, "", Bin),
    M.

%% The reason a call raises and the message the shell would print for it.
raised(Fun) ->
    try Fun() of
        Result -> ct:fail({expected_error, Result})
    catch
        error:Reason:ST ->
            {Reason, unicode:characters_to_binary(erl_error:format_exception(error, Reason, ST))}
    end.

%% A stream pairing is refused where the each renders, so the reason is located.
raised_render(Mod, Bindings) ->
    {Reason, Msg} = raised(fun() -> arizona_render:render_to_iolist(Mod:render(Bindings)) end),
    {arizona_loc, {Mod, _}, Inner} = Reason,
    {Inner, Msg}.

%% An HTML backend lacking `Missing`: every other arizona_html export delegated.
backend_without(Name, Missing) ->
    Delegates = [
        io_lib:format("~s(~s) -> arizona_html:~s(~s). ", [F, args(A), F, args(A)])
     || {F, A} = FA <- arizona_html:module_info(exports),
        F =/= module_info,
        not lists:member(FA, Missing)
    ],
    Source = lists:flatten([
        "-module(", atom_to_list(Name), "). -compile([export_all, nowarn_export_all]). ", Delegates
    ]),
    {ok, Name, Bin} = compile:forms(forms(Source), [return_errors]),
    {module, Name} = code:load_binary(Name, "", Bin),
    Name.

args(0) -> "";
args(A) -> lists:join(", ", ["A" ++ integer_to_list(N) || N <- lists:seq(1, A)]).

core(Reason) ->
    unicode:characters_to_binary(arizona_parse_transform:format_error(Reason)).

core_runtime(Reason) ->
    unicode:characters_to_binary(arizona_template:format_error(Reason)).

contains(Chardata, Needle) ->
    binary:match(unicode:characters_to_binary(Chardata), Needle) =/= nomatch.

-module(arizona_parser_SUITE).
-behaviour(ct_suite).
-include_lib("stdlib/include/assert.hrl").
-compile([export_all, nowarn_export_all]).

%% --------------------------------------------------------------------
%% Behaviour (ct_suite) callbacks
%% --------------------------------------------------------------------

all() ->
    [
        {group, basic_parsing},
        {group, static_content},
        {group, dynamic_content},
        {group, mixed_content},
        {group, edge_cases},
        {group, comment_handling}
    ].

groups() ->
    [
        {basic_parsing, [parallel], [
            parse_empty_tokens,
            parse_single_static,
            parse_single_dynamic
        ]},
        {static_content, [parallel], [
            parse_multiple_static,
            parse_static_with_html
        ]},
        {dynamic_content, [parallel], [
            parse_multiple_dynamic,
            parse_dynamic_with_complex_expr
        ]},
        {mixed_content, [parallel], [
            parse_alternating_static_dynamic,
            parse_static_dynamic_static,
            parse_dynamic_static_dynamic,
            parse_complex_template
        ]},
        {edge_cases, [parallel], [
            parse_starts_with_dynamic,
            parse_ends_with_dynamic,
            parse_consecutive_dynamics,
            parse_consecutive_statics,
            parse_only_dynamics,
            parse_only_statics
        ]},
        {comment_handling, [parallel], [
            parse_comments_between_content,
            parse_comments_skipped,
            parse_only_comments
        ]}
    ].

%% --------------------------------------------------------------------
%% Helper Functions
%% --------------------------------------------------------------------

%% Helper to evaluate AST and get template record
eval_template_ast(AST) ->
    {value, Template, _} = erl_eval:expr(erl_syntax:revert(AST), #{}),
    Template.

%% Helper to extract and test static content
assert_static(Template, Expected) ->
    ?assertEqual(Expected, arizona_template:static(Template)).

%% Helper to test dynamic sequence
assert_dynamic_sequence(Template, Expected) ->
    ?assertEqual(Expected, arizona_template:dynamic_sequence(Template)).

%% Helper to test dynamic annotations
assert_dynamic_anno(Template, Expected) ->
    ?assertEqual(Expected, tuple_to_list(arizona_template:dynamic_anno(Template))).

%% Helper to test dynamic callback count
assert_dynamic_count(Template, Expected) ->
    ?assertEqual(Expected, tuple_size(arizona_template:dynamic(Template))).

%% --------------------------------------------------------------------
%% Basic parsing tests
%% --------------------------------------------------------------------

parse_empty_tokens(Config) when is_list(Config) ->
    AST = arizona_parser:parse_tokens([]),
    Template = eval_template_ast(AST),

    assert_static(Template, []),
    assert_dynamic_sequence(Template, []),
    assert_dynamic_anno(Template, []),
    assert_dynamic_count(Template, 0).

parse_single_static(Config) when is_list(Config) ->
    Tokens = [{static, 1, ~"Hello World"}],
    AST = arizona_parser:parse_tokens(Tokens),
    Template = eval_template_ast(AST),

    assert_static(Template, [~"Hello World"]),
    assert_dynamic_sequence(Template, []),
    assert_dynamic_anno(Template, []),
    assert_dynamic_count(Template, 0).

parse_single_dynamic(Config) when is_list(Config) ->
    Tokens = [{dynamic, 1, ~"name"}],
    AST = arizona_parser:parse_tokens(Tokens),
    Template = eval_template_ast(AST),

    assert_static(Template, [~""]),
    assert_dynamic_sequence(Template, [1]),
    assert_dynamic_anno(Template, [1]),
    assert_dynamic_count(Template, 1).

%% --------------------------------------------------------------------
%% Static content tests
%% --------------------------------------------------------------------

parse_multiple_static(Config) when is_list(Config) ->
    Tokens = [
        {static, 1, ~"Hello"},
        {static, 1, ~" "},
        {static, 1, ~"World"}
    ],
    AST = arizona_parser:parse_tokens(Tokens),
    Template = eval_template_ast(AST),

    assert_static(Template, [~"Hello", ~" ", ~"World"]),
    assert_dynamic_sequence(Template, []),
    assert_dynamic_anno(Template, []),
    assert_dynamic_count(Template, 0).

parse_static_with_html(Config) when is_list(Config) ->
    Tokens = [
        {static, 1, ~"<div class=\"container\">"},
        {static, 1, ~"<p>Hello</p>"},
        {static, 1, ~"</div>"}
    ],
    AST = arizona_parser:parse_tokens(Tokens),
    Template = eval_template_ast(AST),

    assert_static(Template, [
        ~"<div class=\"container\">",
        ~"<p>Hello</p>",
        ~"</div>"
    ]),
    assert_dynamic_sequence(Template, []),
    assert_dynamic_anno(Template, []),
    assert_dynamic_count(Template, 0).

%% --------------------------------------------------------------------
%% Dynamic content tests
%% --------------------------------------------------------------------

parse_multiple_dynamic(Config) when is_list(Config) ->
    Tokens = [
        {dynamic, 1, ~"name"},
        {dynamic, 2, ~"age"}
    ],
    AST = arizona_parser:parse_tokens(Tokens),
    Template = eval_template_ast(AST),

    assert_static(Template, [~"", ~""]),
    assert_dynamic_sequence(Template, [1, 2]),
    assert_dynamic_anno(Template, [1, 2]),
    assert_dynamic_count(Template, 2).

parse_dynamic_with_complex_expr(Config) when is_list(Config) ->
    ExprText = ~"maps:get(user_name, #{user_name => foo})",
    Tokens = [{dynamic, 1, ExprText}],
    AST = arizona_parser:parse_tokens(Tokens),
    Template = eval_template_ast(AST),

    assert_static(Template, [~""]),
    assert_dynamic_sequence(Template, [1]),
    assert_dynamic_anno(Template, [1]),
    assert_dynamic_count(Template, 1).

%% --------------------------------------------------------------------
%% Mixed content tests
%% --------------------------------------------------------------------

parse_alternating_static_dynamic(Config) when is_list(Config) ->
    Tokens = [
        {static, 1, ~"Hello "},
        {dynamic, 1, ~"name"},
        {static, 1, ~", you are "},
        {dynamic, 1, ~"age"},
        {static, 1, ~" years old."}
    ],
    AST = arizona_parser:parse_tokens(Tokens),
    Template = eval_template_ast(AST),

    assert_static(Template, [~"Hello ", ~", you are ", ~" years old."]),
    assert_dynamic_sequence(Template, [1, 2]),
    assert_dynamic_anno(Template, [1, 1]),
    assert_dynamic_count(Template, 2).

parse_static_dynamic_static(Config) when is_list(Config) ->
    Tokens = [
        {static, 1, ~"<div>"},
        {dynamic, 1, ~"content"},
        {static, 1, ~"</div>"}
    ],
    AST = arizona_parser:parse_tokens(Tokens),
    Template = eval_template_ast(AST),

    assert_static(Template, [~"<div>", ~"</div>"]),
    assert_dynamic_sequence(Template, [1]),
    assert_dynamic_anno(Template, [1]),
    assert_dynamic_count(Template, 1).

parse_dynamic_static_dynamic(Config) when is_list(Config) ->
    Tokens = [
        {dynamic, 1, ~"first"},
        {static, 1, ~" - "},
        {dynamic, 1, ~"second"}
    ],
    AST = arizona_parser:parse_tokens(Tokens),
    Template = eval_template_ast(AST),

    assert_static(Template, [~"", ~" - "]),
    assert_dynamic_sequence(Template, [1, 2]),
    assert_dynamic_anno(Template, [1, 1]),
    assert_dynamic_count(Template, 2).

parse_complex_template(Config) when is_list(Config) ->
    Tokens = [
        {static, 1, ~"<li class=\"item\">"},
        {dynamic, 1, ~"maps:get(prefix, #{}, none)"},
        {static, 1, ~"_"},
        {dynamic, 1, ~"foo"},
        {static, 1, ~": "},
        {dynamic, 2, ~"bar"},
        {static, 2, ~"</li>"}
    ],
    AST = arizona_parser:parse_tokens(Tokens),
    Template = eval_template_ast(AST),

    assert_static(Template, [~"<li class=\"item\">", ~"_", ~": ", ~"</li>"]),
    assert_dynamic_sequence(Template, [1, 2, 3]),
    assert_dynamic_anno(Template, [1, 1, 2]),
    assert_dynamic_count(Template, 3).

%% --------------------------------------------------------------------
%% Edge case tests
%% --------------------------------------------------------------------

parse_starts_with_dynamic(Config) when is_list(Config) ->
    Tokens = [
        {dynamic, 1, ~"greeting"},
        {static, 1, ~" World!"}
    ],
    AST = arizona_parser:parse_tokens(Tokens),
    Template = eval_template_ast(AST),

    assert_static(Template, [~"", ~" World!"]),
    assert_dynamic_sequence(Template, [1]),
    assert_dynamic_anno(Template, [1]),
    assert_dynamic_count(Template, 1).

parse_ends_with_dynamic(Config) when is_list(Config) ->
    Tokens = [
        {static, 1, ~"Hello "},
        {dynamic, 1, ~"name"}
    ],
    AST = arizona_parser:parse_tokens(Tokens),
    Template = eval_template_ast(AST),

    assert_static(Template, [~"Hello "]),
    assert_dynamic_sequence(Template, [1]),
    assert_dynamic_anno(Template, [1]),
    assert_dynamic_count(Template, 1).

parse_consecutive_dynamics(Config) when is_list(Config) ->
    Tokens = [
        {static, 1, ~"Start: "},
        {dynamic, 1, ~"first"},
        {dynamic, 1, ~"second"},
        {dynamic, 1, ~"third"},
        {static, 1, ~" :End"}
    ],
    AST = arizona_parser:parse_tokens(Tokens),
    Template = eval_template_ast(AST),

    assert_static(Template, [~"Start: ", ~"", ~"", ~" :End"]),
    assert_dynamic_sequence(Template, [1, 2, 3]),
    assert_dynamic_anno(Template, [1, 1, 1]),
    assert_dynamic_count(Template, 3).

parse_consecutive_statics(Config) when is_list(Config) ->
    Tokens = [
        {dynamic, 1, ~"value"},
        {static, 1, ~"part1"},
        {static, 1, ~"part2"},
        {static, 1, ~"part3"},
        {dynamic, 1, ~"'end'"}
    ],
    AST = arizona_parser:parse_tokens(Tokens),
    Template = eval_template_ast(AST),

    assert_static(Template, [~"", ~"part1", ~"part2", ~"part3"]),
    assert_dynamic_sequence(Template, [1, 2]),
    assert_dynamic_anno(Template, [1, 1]),
    assert_dynamic_count(Template, 2).

parse_only_dynamics(Config) when is_list(Config) ->
    Tokens = [
        {dynamic, 1, ~"first"},
        {dynamic, 2, ~"second"},
        {dynamic, 3, ~"third"}
    ],
    AST = arizona_parser:parse_tokens(Tokens),
    Template = eval_template_ast(AST),

    assert_static(Template, [~"", ~"", ~""]),
    assert_dynamic_sequence(Template, [1, 2, 3]),
    assert_dynamic_anno(Template, [1, 2, 3]),
    assert_dynamic_count(Template, 3).

parse_only_statics(Config) when is_list(Config) ->
    Tokens = [
        {static, 1, ~"<div>"},
        {static, 1, ~"Static content"},
        {static, 1, ~"</div>"}
    ],
    AST = arizona_parser:parse_tokens(Tokens),
    Template = eval_template_ast(AST),

    assert_static(Template, [~"<div>", ~"Static content", ~"</div>"]),
    assert_dynamic_sequence(Template, []),
    assert_dynamic_anno(Template, []),
    assert_dynamic_count(Template, 0).

%% --------------------------------------------------------------------
%% Comment handling tests
%% --------------------------------------------------------------------

parse_comments_between_content(Config) when is_list(Config) ->
    Tokens = [
        {static, 1, ~"<div>"},
        {comment, 1, ~" Header comment "},
        {dynamic, 1, ~"content"},
        {comment, 1, ~" Footer comment "},
        {static, 1, ~"</div>"}
    ],
    AST = arizona_parser:parse_tokens(Tokens),
    Template = eval_template_ast(AST),

    assert_static(Template, [~"<div>", ~"</div>"]),
    assert_dynamic_sequence(Template, [1]),
    assert_dynamic_anno(Template, [1]),
    assert_dynamic_count(Template, 1).

parse_comments_skipped(Config) when is_list(Config) ->
    Tokens = [
        {comment, 1, ~" Start comment "},
        {static, 1, ~"Hello "},
        {dynamic, 1, ~"name"},
        {comment, 1, ~" End comment "}
    ],
    AST = arizona_parser:parse_tokens(Tokens),
    Template = eval_template_ast(AST),

    assert_static(Template, [~"Hello "]),
    assert_dynamic_sequence(Template, [1]),
    assert_dynamic_anno(Template, [1]),
    assert_dynamic_count(Template, 1).

parse_only_comments(Config) when is_list(Config) ->
    Tokens = [
        {comment, 1, ~" Comment 1 "},
        {comment, 2, ~" Comment 2 "},
        {comment, 3, ~" Comment 3 "}
    ],
    AST = arizona_parser:parse_tokens(Tokens),
    Template = eval_template_ast(AST),

    assert_static(Template, []),
    assert_dynamic_sequence(Template, []),
    assert_dynamic_anno(Template, []),
    assert_dynamic_count(Template, 0).

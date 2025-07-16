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
        {group, comment_handling},
        {group, access_functions}
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
        ]},
        {access_functions, [parallel], [
            test_static_access,
            test_dynamic_access,
            test_access_empty_template
        ]}
    ].

%% --------------------------------------------------------------------
%% Basic parsing tests
%% --------------------------------------------------------------------

parse_empty_tokens(Config) when is_list(Config) ->
    Result = arizona_parser:parse_tokens([]),
    Static = arizona_parser:static(Result),
    Dynamic = arizona_parser:dynamic(Result),
    ?assertEqual([], Static),
    ?assertEqual([], Dynamic).

parse_single_static(Config) when is_list(Config) ->
    Tokens = [{static, 1, ~"Hello World"}],
    Result = arizona_parser:parse_tokens(Tokens),
    Static = arizona_parser:static(Result),
    Dynamic = arizona_parser:dynamic(Result),
    ?assertEqual([~"Hello World"], Static),
    ?assertEqual([], Dynamic).

parse_single_dynamic(Config) when is_list(Config) ->
    Tokens = [{dynamic, 1, ~"name"}],
    Result = arizona_parser:parse_tokens(Tokens),
    Static = arizona_parser:static(Result),
    Dynamic = arizona_parser:dynamic(Result),
    ?assertEqual([~""], Static),
    ?assertEqual([{1, ~"name"}], Dynamic).

%% --------------------------------------------------------------------
%% Static content tests
%% --------------------------------------------------------------------

parse_multiple_static(Config) when is_list(Config) ->
    Tokens = [
        {static, 1, ~"Hello"},
        {static, 1, ~" "},
        {static, 1, ~"World"}
    ],
    Result = arizona_parser:parse_tokens(Tokens),
    Static = arizona_parser:static(Result),
    Dynamic = arizona_parser:dynamic(Result),
    ?assertEqual([~"Hello", ~" ", ~"World"], Static),
    ?assertEqual([], Dynamic).

parse_static_with_html(Config) when is_list(Config) ->
    Tokens = [
        {static, 1, ~"<div class=\"container\">"},
        {static, 1, ~"<p>Hello</p>"},
        {static, 1, ~"</div>"}
    ],
    Result = arizona_parser:parse_tokens(Tokens),
    Static = arizona_parser:static(Result),
    Dynamic = arizona_parser:dynamic(Result),
    ?assertEqual([~"<div class=\"container\">", ~"<p>Hello</p>", ~"</div>"], Static),
    ?assertEqual([], Dynamic).

%% --------------------------------------------------------------------
%% Dynamic content tests
%% --------------------------------------------------------------------

parse_multiple_dynamic(Config) when is_list(Config) ->
    Tokens = [
        {dynamic, 1, ~"name"},
        {dynamic, 2, ~"age"}
    ],
    Result = arizona_parser:parse_tokens(Tokens),
    Static = arizona_parser:static(Result),
    Dynamic = arizona_parser:dynamic(Result),
    ?assertEqual([~"", ~""], Static),
    ?assertEqual([{1, ~"name"}, {2, ~"age"}], Dynamic).

parse_dynamic_with_complex_expr(Config) when is_list(Config) ->
    ExprText = ~"arizona_socket:get_binding(user_name, Socket)",
    Tokens = [{dynamic, 1, ExprText}],
    Result = arizona_parser:parse_tokens(Tokens),
    Static = arizona_parser:static(Result),
    Dynamic = arizona_parser:dynamic(Result),
    ?assertEqual([~""], Static),
    ?assertEqual([{1, ExprText}], Dynamic).

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
    Result = arizona_parser:parse_tokens(Tokens),
    Static = arizona_parser:static(Result),
    Dynamic = arizona_parser:dynamic(Result),
    ?assertEqual([~"Hello ", ~", you are ", ~" years old."], Static),
    ?assertEqual([{1, ~"name"}, {1, ~"age"}], Dynamic).

parse_static_dynamic_static(Config) when is_list(Config) ->
    Tokens = [
        {static, 1, ~"<div>"},
        {dynamic, 1, ~"content"},
        {static, 1, ~"</div>"}
    ],
    Result = arizona_parser:parse_tokens(Tokens),
    Static = arizona_parser:static(Result),
    Dynamic = arizona_parser:dynamic(Result),
    ?assertEqual([~"<div>", ~"</div>"], Static),
    ?assertEqual([{1, ~"content"}], Dynamic).

parse_dynamic_static_dynamic(Config) when is_list(Config) ->
    Tokens = [
        {dynamic, 1, ~"first"},
        {static, 1, ~" - "},
        {dynamic, 1, ~"second"}
    ],
    Result = arizona_parser:parse_tokens(Tokens),
    Static = arizona_parser:static(Result),
    Dynamic = arizona_parser:dynamic(Result),
    ?assertEqual([~"", ~" - "], Static),
    ?assertEqual([{1, ~"first"}, {1, ~"second"}], Dynamic).

parse_complex_template(Config) when is_list(Config) ->
    Tokens = [
        {static, 1, ~"<li class=\"item\">"},
        {dynamic, 1, ~"arizona_socket:get_binding(prefix, Socket)"},
        {static, 1, ~"_"},
        {dynamic, 1, ~"I"},
        {static, 1, ~": "},
        {dynamic, 2, ~"item.name"},
        {static, 2, ~"</li>"}
    ],
    Result = arizona_parser:parse_tokens(Tokens),
    Static = arizona_parser:static(Result),
    Dynamic = arizona_parser:dynamic(Result),
    ?assertEqual([~"<li class=\"item\">", ~"_", ~": ", ~"</li>"], Static),
    ?assertEqual(
        [
            {1, ~"arizona_socket:get_binding(prefix, Socket)"},
            {1, ~"I"},
            {2, ~"item.name"}
        ],
        Dynamic
    ).

%% --------------------------------------------------------------------
%% Edge case tests
%% --------------------------------------------------------------------

parse_starts_with_dynamic(Config) when is_list(Config) ->
    Tokens = [
        {dynamic, 1, ~"greeting"},
        {static, 1, ~" World!"}
    ],
    Result = arizona_parser:parse_tokens(Tokens),
    Static = arizona_parser:static(Result),
    Dynamic = arizona_parser:dynamic(Result),
    ?assertEqual([~"", ~" World!"], Static),
    ?assertEqual([{1, ~"greeting"}], Dynamic).

parse_ends_with_dynamic(Config) when is_list(Config) ->
    Tokens = [
        {static, 1, ~"Hello "},
        {dynamic, 1, ~"name"}
    ],
    Result = arizona_parser:parse_tokens(Tokens),
    Static = arizona_parser:static(Result),
    Dynamic = arizona_parser:dynamic(Result),
    ?assertEqual([~"Hello "], Static),
    ?assertEqual([{1, ~"name"}], Dynamic).

parse_consecutive_dynamics(Config) when is_list(Config) ->
    Tokens = [
        {static, 1, ~"Start: "},
        {dynamic, 1, ~"first"},
        {dynamic, 1, ~"second"},
        {dynamic, 1, ~"third"},
        {static, 1, ~" :End"}
    ],
    Result = arizona_parser:parse_tokens(Tokens),
    Static = arizona_parser:static(Result),
    Dynamic = arizona_parser:dynamic(Result),
    ?assertEqual([~"Start: ", ~"", ~"", ~" :End"], Static),
    ?assertEqual([{1, ~"first"}, {1, ~"second"}, {1, ~"third"}], Dynamic).

parse_consecutive_statics(Config) when is_list(Config) ->
    Tokens = [
        {dynamic, 1, ~"value"},
        {static, 1, ~"part1"},
        {static, 1, ~"part2"},
        {static, 1, ~"part3"},
        {dynamic, 1, ~"end"}
    ],
    Result = arizona_parser:parse_tokens(Tokens),
    Static = arizona_parser:static(Result),
    Dynamic = arizona_parser:dynamic(Result),
    ?assertEqual([~"", ~"part1", ~"part2", ~"part3"], Static),
    ?assertEqual([{1, ~"value"}, {1, ~"end"}], Dynamic).

parse_only_dynamics(Config) when is_list(Config) ->
    Tokens = [
        {dynamic, 1, ~"first"},
        {dynamic, 2, ~"second"},
        {dynamic, 3, ~"third"}
    ],
    Result = arizona_parser:parse_tokens(Tokens),
    Static = arizona_parser:static(Result),
    Dynamic = arizona_parser:dynamic(Result),
    ?assertEqual([~"", ~"", ~""], Static),
    ?assertEqual([{1, ~"first"}, {2, ~"second"}, {3, ~"third"}], Dynamic).

parse_only_statics(Config) when is_list(Config) ->
    Tokens = [
        {static, 1, ~"<div>"},
        {static, 1, ~"Static content"},
        {static, 1, ~"</div>"}
    ],
    Result = arizona_parser:parse_tokens(Tokens),
    Static = arizona_parser:static(Result),
    Dynamic = arizona_parser:dynamic(Result),
    ?assertEqual([~"<div>", ~"Static content", ~"</div>"], Static),
    ?assertEqual([], Dynamic).

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
    Result = arizona_parser:parse_tokens(Tokens),
    Static = arizona_parser:static(Result),
    Dynamic = arizona_parser:dynamic(Result),
    ?assertEqual([~"<div>", ~"</div>"], Static),
    ?assertEqual([{1, ~"content"}], Dynamic).

parse_comments_skipped(Config) when is_list(Config) ->
    Tokens = [
        {comment, 1, ~" Start comment "},
        {static, 1, ~"Hello "},
        {dynamic, 1, ~"name"},
        {comment, 1, ~" End comment "}
    ],
    Result = arizona_parser:parse_tokens(Tokens),
    Static = arizona_parser:static(Result),
    Dynamic = arizona_parser:dynamic(Result),
    ?assertEqual([~"Hello "], Static),
    ?assertEqual([{1, ~"name"}], Dynamic).

parse_only_comments(Config) when is_list(Config) ->
    Tokens = [
        {comment, 1, ~" Comment 1 "},
        {comment, 2, ~" Comment 2 "},
        {comment, 3, ~" Comment 3 "}
    ],
    Result = arizona_parser:parse_tokens(Tokens),
    Static = arizona_parser:static(Result),
    Dynamic = arizona_parser:dynamic(Result),
    ?assertEqual([], Static),
    ?assertEqual([], Dynamic).

%% --------------------------------------------------------------------
%% Access function tests
%% --------------------------------------------------------------------

test_static_access(Config) when is_list(Config) ->
    Tokens = [
        {static, 1, ~"Hello "},
        {dynamic, 1, ~"name"},
        {static, 1, ~"!"}
    ],
    Result = arizona_parser:parse_tokens(Tokens),
    Static = arizona_parser:static(Result),
    ?assertEqual([~"Hello ", ~"!"], Static).

test_dynamic_access(Config) when is_list(Config) ->
    Tokens = [
        {static, 1, ~"Count: "},
        {dynamic, 2, ~"arizona_socket:get_binding(count, Socket)"}
    ],
    Result = arizona_parser:parse_tokens(Tokens),
    Dynamic = arizona_parser:dynamic(Result),
    ?assertEqual([{2, ~"arizona_socket:get_binding(count, Socket)"}], Dynamic).

test_access_empty_template(Config) when is_list(Config) ->
    Result = arizona_parser:parse_tokens([]),
    Static = arizona_parser:static(Result),
    Dynamic = arizona_parser:dynamic(Result),
    ?assertEqual([], Static),
    ?assertEqual([], Dynamic).

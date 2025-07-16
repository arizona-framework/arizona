-module(arizona_parse_trans_SUITE).
-behaviour(ct_suite).
-include_lib("stdlib/include/assert.hrl").
-compile([export_all, nowarn_export_all]).

%% --------------------------------------------------------------------
%% Behaviour (ct_suite) callbacks
%% --------------------------------------------------------------------

all() ->
    [
        {group, basic_transforms},
        {group, edge_cases},
        {group, error_handling},
        {group, utility_functions}
    ].

groups() ->
    [
        {basic_transforms, [parallel], [
            transform_simple_template,
            transform_dynamic_template,
            transform_mixed_template,
            transform_empty_template,
            transform_html_template
        ]},
        {edge_cases, [parallel], [
            transform_non_string_arg,
            transform_other_function_calls,
            transform_nested_calls,
            transform_multiple_templates,
            transform_no_arizona_calls
        ]},
        {error_handling, [parallel], [
            transform_invalid_template,
            transform_scanner_error,
            transform_parser_error,
            transform_eval_error
        ]},
        {utility_functions, [parallel], [
            test_extract_module_name,
            test_analyze_application,
            test_eval_expr,
            test_error_info
        ]}
    ].

%% --------------------------------------------------------------------
%% Helper Functions
%% --------------------------------------------------------------------

%% Helper to test parse transform with given forms
test_transform(Forms) ->
    arizona_parse_trans:parse_transform(Forms, []).

%% Helper to create a simple module with template call
create_test_module(ModuleName, TemplateContent) ->
    [
        {attribute, 1, module, ModuleName},
        {attribute, 2, export, [{test_func, 0}]},
        {function, 3, test_func, 0, [
            {clause, 3, [], [], [
                {call, 4, {remote, 4, {atom, 4, arizona_template}, {atom, 4, from_string}}, [
                    create_template_arg(TemplateContent),
                    % Empty bindings map
                    {map, 4, []}
                ]}
            ]}
        ]}
    ].

%% Helper to create template argument based on type
create_template_arg(Content) when is_binary(Content) ->
    {bin, 5, [{bin_element, 5, {string, 5, binary_to_list(Content)}, default, default}]};
create_template_arg(Content) when is_list(Content) ->
    {string, 5, Content};
create_template_arg(variable) ->
    {var, 5, 'Template'}.

%% Helper to extract function body from transformed forms
extract_function_body(Forms, FuncName) ->
    FunctionForm = lists:keyfind(
        function,
        1,
        [Form || Form <- Forms, element(1, Form) =:= function, element(3, Form) =:= FuncName]
    ),
    case FunctionForm of
        {function, _, FuncName, 0, [{clause, _, [], [], Body}]} ->
            Body;
        false ->
            error({function_not_found, FuncName})
    end.

%% --------------------------------------------------------------------
%% Basic Transform Tests
%% --------------------------------------------------------------------

transform_simple_template(Config) when is_list(Config) ->
    Forms = create_test_module(test_simple, ~"Hello World"),
    TransformedForms = test_transform(Forms),

    Body = extract_function_body(TransformedForms, test_func),
    ?assertMatch([{tuple, _, [{atom, _, template} | _]}], Body),
    ok.

transform_dynamic_template(Config) when is_list(Config) ->
    Forms = create_test_module(test_dynamic, ~"Hello {name}!"),
    TransformedForms = test_transform(Forms),

    Body = extract_function_body(TransformedForms, test_func),
    ?assertMatch([{tuple, _, [{atom, _, template}, _, {tuple, _, [_]}, _, _]}], Body),
    ok.

transform_mixed_template(Config) when is_list(Config) ->
    Forms = create_test_module(test_mixed, ~"<div>{title}: {content}</div>"),
    TransformedForms = test_transform(Forms),

    Body = extract_function_body(TransformedForms, test_func),
    ?assertMatch([{tuple, _, [{atom, _, template}, _, {tuple, _, [_, _]}, _, _]}], Body),
    ok.

transform_empty_template(Config) when is_list(Config) ->
    Forms = create_test_module(test_empty, ~""),
    TransformedForms = test_transform(Forms),

    Body = extract_function_body(TransformedForms, test_func),
    ?assertMatch(
        [{tuple, _, [{atom, _, template}, {nil, _}, {tuple, _, []}, {nil, _}, {tuple, _, []}]}],
        Body
    ),
    ok.

transform_html_template(Config) when is_list(Config) ->
    Forms = create_test_module(test_html, ~"<div class=\"container\"><p>Hello {user}</p></div>"),
    TransformedForms = test_transform(Forms),

    Body = extract_function_body(TransformedForms, test_func),
    ?assertMatch([{tuple, _, [{atom, _, template} | _]}], Body),
    ok.

%% --------------------------------------------------------------------
%% Edge Case Tests
%% --------------------------------------------------------------------

transform_non_string_arg(Config) when is_list(Config) ->
    % Test with variable argument that can't be evaluated at compile-time
    Forms = [
        {attribute, 1, module, test_variable},
        {attribute, 2, export, [{test_func, 0}]},
        {function, 3, test_func, 0, [
            {clause, 3, [], [], [
                {call, 4, {remote, 4, {atom, 4, arizona_template}, {atom, 4, from_string}}, [
                    create_template_arg(variable),
                    {map, 4, []}
                ]}
            ]}
        ]}
    ],

    % Should throw an error since variable can't be evaluated at compile-time
    ?assertError(arizona_template_extraction_failed, test_transform(Forms)),
    ok.

transform_other_function_calls(Config) when is_list(Config) ->
    % Test that other function calls are not transformed
    Forms = [
        {attribute, 1, module, test_other},
        {attribute, 2, export, [{test_func, 0}]},
        {function, 3, test_func, 0, [
            {clause, 3, [], [], [
                {call, 4, {remote, 4, {atom, 4, other_module}, {atom, 4, other_func}}, [
                    {string, 5, "test"}
                ]}
            ]}
        ]}
    ],

    TransformedForms = test_transform(Forms),
    Body = extract_function_body(TransformedForms, test_func),

    % Should remain unchanged - not a template tuple
    ?assertMatch([{call, _, {remote, _, {atom, _, other_module}, {atom, _, other_func}}, _}], Body),
    ok.

transform_nested_calls(Config) when is_list(Config) ->
    % Test nested arizona_template:from_string calls
    Forms = [
        {attribute, 1, module, test_nested},
        {attribute, 2, export, [{test_func, 0}]},
        {function, 3, test_func, 0, [
            {clause, 3, [], [], [
                {tuple, 4, [
                    {call, 5, {remote, 5, {atom, 5, arizona_template}, {atom, 5, from_string}}, [
                        create_template_arg(~"First {a}"),
                        {map, 5, []}
                    ]},
                    {call, 6, {remote, 6, {atom, 6, arizona_template}, {atom, 6, from_string}}, [
                        create_template_arg(~"Second {b}"),
                        {map, 6, []}
                    ]}
                ]}
            ]}
        ]}
    ],

    TransformedForms = test_transform(Forms),
    Body = extract_function_body(TransformedForms, test_func),

    % Both calls should be transformed
    ?assertMatch(
        [
            {tuple, _, [
                {tuple, _, [{atom, _, template} | _]},
                {tuple, _, [{atom, _, template} | _]}
            ]}
        ],
        Body
    ),
    ok.

transform_multiple_templates(Config) when is_list(Config) ->
    % Test multiple functions with template calls
    Forms = [
        {attribute, 1, module, test_multiple},
        {attribute, 2, export, [{func1, 0}, {func2, 0}]},
        {function, 3, func1, 0, [
            {clause, 3, [], [], [
                {call, 4, {remote, 4, {atom, 4, arizona_template}, {atom, 4, from_string}}, [
                    create_template_arg(~"Template 1"),
                    {map, 4, []}
                ]}
            ]}
        ]},
        {function, 5, func2, 0, [
            {clause, 5, [], [], [
                {call, 6, {remote, 6, {atom, 6, arizona_template}, {atom, 6, from_string}}, [
                    create_template_arg(~"Template 2"),
                    {map, 6, []}
                ]}
            ]}
        ]}
    ],

    TransformedForms = test_transform(Forms),

    Body1 = extract_function_body(TransformedForms, func1),
    Body2 = extract_function_body(TransformedForms, func2),

    ?assertMatch([{tuple, _, [{atom, _, template} | _]}], Body1),
    ?assertMatch([{tuple, _, [{atom, _, template} | _]}], Body2),
    ok.

transform_no_arizona_calls(Config) when is_list(Config) ->
    % Test module with no arizona_template calls
    Forms = [
        {attribute, 1, module, test_no_arizona},
        {attribute, 2, export, [{test_func, 0}]},
        {function, 3, test_func, 0, [{clause, 3, [], [], [{atom, 4, ok}]}]}
    ],

    TransformedForms = test_transform(Forms),

    % Should be identical to input
    ?assertEqual(Forms, TransformedForms),
    ok.

%% --------------------------------------------------------------------
%% Error Handling Tests
%% --------------------------------------------------------------------

transform_invalid_template(Config) when is_list(Config) ->
    % Test with invalid template syntax
    Forms = create_test_module(test_invalid, ~"{unclosed"),

    % Should not crash, but handle gracefully
    ?assertError(arizona_template_extraction_failed, test_transform(Forms)),
    ok.

transform_scanner_error(Config) when is_list(Config) ->
    % Test with content that causes scanner error
    Forms = create_test_module(test_scanner_error, ~"{malformed expression"),

    ?assertError(arizona_template_extraction_failed, test_transform(Forms)),
    ok.

transform_parser_error(Config) when is_list(Config) ->
    % Test with content that causes parser error (this would be caught in transform)
    Forms = create_test_module(test_parser_error, ~"valid static content"),

    % This should work fine as it's valid content
    TransformedForms = test_transform(Forms),
    Body = extract_function_body(TransformedForms, test_func),
    ?assertMatch([{tuple, _, [{atom, _, template} | _]}], Body),
    ok.

transform_eval_error(Config) when is_list(Config) ->
    % Test with invalid expression that causes eval error
    Forms = [
        {attribute, 1, module, test_eval_error},
        {attribute, 2, export, [{test_func, 0}]},
        {function, 3, test_func, 0, [
            {clause, 3, [], [], [
                {call, 4, {remote, 4, {atom, 4, arizona_template}, {atom, 4, from_string}}, [
                    % This will cause eval error
                    {call, 5, {atom, 5, undefined_function}, []},
                    {map, 4, []}
                ]}
            ]}
        ]}
    ],

    ?assertError(arizona_template_extraction_failed, test_transform(Forms)),
    ok.

%% --------------------------------------------------------------------
%% Utility Function Tests
%% --------------------------------------------------------------------

test_extract_module_name(Config) when is_list(Config) ->
    % Test with valid module attribute
    Forms1 = [{attribute, 1, module, test_module}],
    ?assertEqual(test_module, arizona_parse_trans:extract_module_name(Forms1)),

    % Test with module attribute not first
    Forms2 = [{attribute, 1, export, []}, {attribute, 2, module, other_module}],
    ?assertEqual(other_module, arizona_parse_trans:extract_module_name(Forms2)),

    % Test with no module attribute
    Forms3 = [{attribute, 1, export, []}],
    ?assertEqual(undefined, arizona_parse_trans:extract_module_name(Forms3)),

    % Test with empty forms
    ?assertEqual(undefined, arizona_parse_trans:extract_module_name([])),
    ok.

test_analyze_application(Config) when is_list(Config) ->
    % Create application node for arizona_template:from_string/2
    AppNode =
        {call, 1, {remote, 1, {atom, 1, arizona_template}, {atom, 1, from_string}}, [
            {string, 1, "test"},
            {map, 1, []}
        ]},

    Result = arizona_parse_trans:analyze_application(AppNode),
    ?assertEqual({arizona_template, from_string, 2, [{string, 1, "test"}, {map, 1, []}]}, Result),

    % Test with non-remote call
    LocalCall = {call, 1, {atom, 1, local_func}, []},
    ?assertEqual(undefined, arizona_parse_trans:analyze_application(LocalCall)),

    % Test with non-atom module
    InvalidModule = {call, 1, {remote, 1, {var, 1, 'Module'}, {atom, 1, func}}, []},
    ?assertEqual(undefined, arizona_parse_trans:analyze_application(InvalidModule)),
    ok.

test_eval_expr(Config) when is_list(Config) ->
    % Test string evaluation
    StringExpr = {string, 1, "hello"},
    Result = arizona_parse_trans:eval_expr(test_module, StringExpr, #{}),
    ?assertEqual("hello", Result),

    % Test binary evaluation
    BinaryExpr = {bin, 1, [{bin_element, 1, {string, 1, "test"}, default, default}]},
    BinaryResult = arizona_parse_trans:eval_expr(test_module, BinaryExpr, #{}),
    ?assertEqual(~"test", BinaryResult),
    ok.

test_error_info(Config) when is_list(Config) ->
    Cause = {test_module, 42, error, some_reason, []},
    ErrorInfo = arizona_parse_trans:error_info(Cause),

    ?assertMatch(
        [{error_info, #{cause := Cause, module := arizona_parse_trans}}],
        ErrorInfo
    ),
    ok.

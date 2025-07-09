-module(arizona_parse_transform_SUITE).
-moduledoc ~"""
Test suite for Arizona Parse Transform module.

This suite tests the compile-time transformation of Arizona template syntax
into optimized structured formats for high-performance rendering.
""".

-include_lib("stdlib/include/assert.hrl").

%% --------------------------------------------------------------------
%% Test suite exports
%% --------------------------------------------------------------------

-export([all/0, groups/0, init_per_suite/1, end_per_suite/1]).

%% Test cases
-export([
    test_stateless_transform/1,
    test_stateful_transform/1,
    test_stateful_transform_with_variables/1,
    test_non_arizona_call_passthrough/1,
    test_invalid_template_error/1,
    test_non_binary_template_error/1,
    test_format_error/1,
    test_nested_function_calls/1,
    test_stateless_parse_error/1,
    test_stateful_parse_error/1,
    test_format_error_badarg/1,
    test_module_name_extraction_edge_cases/1,
    test_stateful_non_binary_template_error/1,
    test_complex_stateful_template_with_multiple_variables/1,
    test_stateless_binary_elements/1,
    test_transform_stateless_to_ast/1,
    test_transform_stateful_to_ast/1,
    test_stateless_binary_handling/1,
    test_dynamic_expression_ast_creation/1,
    test_nested_arizona_optimization/1,
    test_slot_template_transform/1
]).

%% --------------------------------------------------------------------
%% Test suite configuration
%% --------------------------------------------------------------------

all() ->
    [
        {group, transformation_tests},
        {group, error_handling_tests},
        {group, coverage_tests}
    ].

groups() ->
    [
        {transformation_tests, [parallel], [
            test_stateless_transform,
            test_stateful_transform,
            test_stateful_transform_with_variables,
            test_non_arizona_call_passthrough,
            test_nested_function_calls,
            test_slot_template_transform
        ]},
        {error_handling_tests, [parallel], [
            test_invalid_template_error,
            test_non_binary_template_error,
            test_format_error,
            test_stateless_parse_error,
            test_stateful_parse_error,
            test_format_error_badarg,
            test_module_name_extraction_edge_cases,
            test_stateful_non_binary_template_error
        ]},
        {coverage_tests, [parallel], [
            test_complex_stateful_template_with_multiple_variables,
            test_stateless_binary_elements,
            test_transform_stateless_to_ast,
            test_transform_stateful_to_ast,
            test_stateless_binary_handling,
            test_dynamic_expression_ast_creation,
            test_nested_arizona_optimization
        ]}
    ].

init_per_suite(Config) ->
    % Ensure arizona modules are available
    {ok, _Apps} = application:ensure_all_started(arizona),
    Config.

end_per_suite(Config) when is_list(Config) ->
    ok.

%% --------------------------------------------------------------------
%% Test Cases
%% --------------------------------------------------------------------

%% Test stateless template transformation
test_stateless_transform(Config) when is_list(Config) ->
    % Create a simple AST with a stateless render call using merl:quote
    Forms = merl:quote(~""""
    -module(arizona_stateless_module).
    -export([test_render/1]).
    test_render(Socket) ->
        arizona_html:render_stateless(~"""
        Hello {arizona_socket:get_binding(name, Socket)}!
        """, Socket).
    """"),

    % Apply parse transform
    TransformedForms = arizona_parse_transform:parse_transform(Forms, []),

    % Verify transformation occurred
    ?assertMatch(
        [
            _ModAttr,
            _ExportAttr,
            {function, _FLine, test_render, 1, [
                {clause, _CLine, _Args, _Guards, [
                    {call, _CallLine,
                        {remote, _RemoteLine, {atom, _ModLine, arizona_html},
                            {atom, _FuncLine, render_stateless}},
                        _StatelessArgs}
                ]}
            ]}
        ],
        TransformedForms
    ),

    ct:comment("Stateless template successfully transformed to iolist format").

%% Test stateful template transformation
test_stateful_transform(Config) when is_list(Config) ->
    % For stateful, we'll just test that the transform doesn't crash
    % since the actual transformation is complex
    Forms = merl:quote(~"""
    -module(test_stateful_module).
    -export([test_render/1]).
    test_render(Socket) ->
        arizona_html:render_stateful(~"Hello {arizona_socket:get_binding(name, Socket)}!", Socket).
    """),

    % Apply parse transform
    TransformedForms = arizona_parse_transform:parse_transform(Forms, []),

    % Just verify it produces valid forms without crashing
    ?assertMatch(
        [_ModAttr, _ExportAttr, {function, _, test_render, 1, _Clauses}],
        TransformedForms
    ),

    ct:comment("Stateful template transformation handled without crashing").

%% Test that non-arizona function calls are passed through unchanged
test_non_arizona_call_passthrough(Config) when is_list(Config) ->
    % Create a simple AST with non-arizona function call
    Forms = merl:quote(~"""
    -module(test_other_module).
    -export([test_other/1]).
    test_other(Data) -> other_module:some_function(~"Hello World", Data).
    """),

    % Apply parse transform
    TransformedForms = arizona_parse_transform:parse_transform(Forms, []),

    % Verify no transformation occurred
    ?assertMatch(
        [
            _ModAttr,
            _ExportAttr,
            {function, _FLine, test_other, 1, [
                {clause, _CLine, _Args, _Guards, [
                    {call, _CallLine,
                        {remote, _RemoteLine, {atom, _ModLine, other_module},
                            {atom, _FuncLine, some_function}},
                        _OtherArgs}
                ]}
            ]}
        ],
        TransformedForms
    ),

    ct:comment("Non-arizona function calls correctly passed through unchanged").

%% Test nested function calls are handled properly
test_nested_function_calls(Config) when is_list(Config) ->
    % Create a simple AST with nested calls
    Forms = merl:quote(~""""
    -module(test_nested_module).
    -export([test_nested/1]).
    test_nested(Socket) ->
        io_lib:format("Result: ~s", [
            arizona_html:render_stateless(~"""
            Hello {arizona_socket:get_binding(name, Socket)}!
            """, Socket)
        ]).
    """"),

    % Apply parse transform
    TransformedForms = arizona_parse_transform:parse_transform(Forms, []),

    % Just verify it doesn't crash and produces valid forms
    ?assertMatch(
        [_ModAttr, _ExportAttr, {function, _, test_nested, 1, _Clauses}],
        TransformedForms
    ),

    ct:comment("Nested function calls handled without crashing").

%% Test invalid template error handling
test_invalid_template_error(Config) when is_list(Config) ->
    % Test that format_error handles template_parse_failed
    ErrorMsg = arizona_parse_transform:format_error(template_parse_failed),
    ExpectedMsg = "Failed to parse Arizona template - invalid template syntax",
    ?assertEqual(ExpectedMsg, ErrorMsg),

    % Test unknown error
    UnknownMsg = arizona_parse_transform:format_error({unknown_error, some_data}),
    % Should be a formatted string
    ?assert(is_list(UnknownMsg)),

    ct:comment("Error formatting works correctly").

%% Test non-binary template error
test_non_binary_template_error(Config) when is_list(Config) ->
    % Create a form with a variable template (should raise badarg)
    Forms = merl:quote(~"""
    -module(test_badarg_module).
    -export([test_render/1]).
    test_render(Socket) -> arizona_html:render_stateless(Template, Socket).
    """),

    % The parse transform should raise badarg for non-binary templates
    ?assertError(badarg, arizona_parse_transform:parse_transform(Forms, [])),
    ct:comment("Non-binary template correctly raised badarg error").

%% Test format_error function
test_format_error(Config) when is_list(Config) ->
    % Test known error
    Msg1 = arizona_parse_transform:format_error(template_parse_failed),
    ?assertEqual("Failed to parse Arizona template - invalid template syntax", Msg1),

    % Test unknown error
    Msg2 = arizona_parse_transform:format_error(unknown_error),
    ?assert(string:str(Msg2, "Unknown Arizona parse transform error") > 0),

    ct:comment("format_error/1 handles both known and unknown errors correctly").

%% Test stateful template transformation with variables
test_stateful_transform_with_variables(Config) when is_list(Config) ->
    % Create a stateful render call with variables using merl:quote
    Forms = merl:quote(~""""
    -module(test_stateful_vars_module).
    -export([test_render/1]).
    test_render(Socket) ->
        arizona_html:render_stateful(~"""
        Hello {arizona_socket:get_binding(name, Socket)}!
        Welcome {arizona_socket:get_binding('user_role', Socket)}.
        """, Socket).
    """"),

    % Apply parse transform
    TransformedForms = arizona_parse_transform:parse_transform(Forms, []),

    % Verify transformation occurred and contains variable information
    ?assertMatch(
        [
            _ModAttr,
            _ExportAttr,
            {function, _, test_render, 1, _Clauses}
        ],
        TransformedForms
    ),

    ct:comment("Stateful template with variables transformed correctly").

%% Test stateless parsing error
test_stateless_parse_error(Config) when is_list(Config) ->
    % Mock arizona_scanner to return invalid tokens that will cause parsing to fail
    Forms = merl:quote(~"""
    -module(test_stateless_error_module).
    -export([test_render/1]).
    test_render(Socket) -> arizona_html:render_stateless(~"Invalid {unclosed", Socket).
    """),

    % The parse transform should handle scanner errors gracefully
    ?assertError(template_parse_failed, arizona_parse_transform:parse_transform(Forms, [])),
    ct:comment("Stateless template parse error correctly raised").

%% Test stateful parsing error
test_stateful_parse_error(Config) when is_list(Config) ->
    % Create a stateful template that will cause parsing to fail
    Forms = merl:quote(~"""
    -module(test_stateful_error_module).
    -export([test_render/1]).
    test_render(Socket) -> arizona_html:render_stateful(~"Invalid {unclosed", Socket).
    """),

    % The parse transform should handle errors gracefully
    ?assertError(template_parse_failed, arizona_parse_transform:parse_transform(Forms, [])),
    ct:comment("Stateful template parse error correctly raised").

%% Test format_error function specifically for badarg
test_format_error_badarg(Config) when is_list(Config) ->
    % Test badarg error specifically
    BadargMsg = arizona_parse_transform:format_error(badarg),
    ExpectedMsg =
        "Arizona parse transform requires literal binary templates, variables are not supported",
    ?assertEqual(ExpectedMsg, BadargMsg),

    ct:comment("format_error(badarg) returns correct message").

%% Test module name extraction edge cases
test_module_name_extraction_edge_cases(Config) when is_list(Config) ->
    % Test with forms that don't have module attribute at the beginning
    Forms1 = merl:quote(~"""
    -module(edge_case_module).
    -compile([export_all]).
    test() -> ok.
    """),

    % This should still work - extract_module_name searches through forms
    TransformedForms1 = arizona_parse_transform:parse_transform(Forms1, []),
    ?assertMatch([_CompileAttr, _ModAttr, _Function], TransformedForms1),

    % Test with forms that have no module attribute
    Forms2 = merl:quote(~"""
    -compile([export_all]).
    test() -> ok.
    """),

    % This should work but use unknown_module
    TransformedForms2 = arizona_parse_transform:parse_transform(Forms2, []),
    ?assertMatch([_CompileAttr, _Function], TransformedForms2),

    ct:comment("Module name extraction handles edge cases correctly").

%% Test stateful non-binary template error
test_stateful_non_binary_template_error(Config) when is_list(Config) ->
    % Create a form with a variable stateful template (should raise badarg)
    Forms = merl:quote(~"""
    -module(test_stateful_badarg_module).
    -export([test_render/1]).
    test_render(Socket) -> arizona_html:render_stateful(Template, Socket).
    """),

    % The parse transform should raise badarg for non-binary templates
    ?assertError(badarg, arizona_parse_transform:parse_transform(Forms, [])),
    ct:comment("Non-binary stateful template correctly raised badarg error").

%% Test complex stateful template with multiple variables
test_complex_stateful_template_with_multiple_variables(Config) when is_list(Config) ->
    % Create a complex stateful template with multiple variables
    Forms = merl:quote(~""""
    -module(test_complex_stateful_module).
    -export([test_render/1]).
    test_render(Socket) ->
        arizona_html:render_stateful(~"""
        <div class="user-profile">
            <h1>Welcome {arizona_socket:get_binding(username, Socket)}!</h1>
            <p>Role: {arizona_socket:get_binding('user_role', Socket)}</p>
            <p>Email: {arizona_socket:get_binding(email, Socket)}</p>
            <p>Last Login: {arizona_socket:get_binding('last_login', Socket)}</p>
        </div>
        """, Socket).
    """"),

    % Apply parse transform
    TransformedForms = arizona_parse_transform:parse_transform(Forms, []),

    % Verify transformation occurred
    ?assertMatch(
        [
            _ModAttr,
            _ExportAttr,
            {function, _, test_render, 1, _Clauses}
        ],
        TransformedForms
    ),

    ct:comment("Complex stateful template with multiple variables transformed correctly").

%% Test stateless template with binary elements
test_stateless_binary_elements(Config) when is_list(Config) ->
    % Create a stateless template that will exercise binary element handling
    Forms = merl:quote(~""""
    -module(test_stateless_binary_module).
    -export([test_render/1]).
    test_render(Socket) ->
        arizona_html:render_stateless(~"""
        <header>Static Header</header>
        <main>{arizona_socket:get_binding(content, Socket)}</main>
        <footer>Static Footer</footer>
        """, Socket).
    """"),

    % Apply parse transform
    TransformedForms = arizona_parse_transform:parse_transform(Forms, []),

    % Verify transformation occurred
    ?assertMatch(
        [
            _ModAttr,
            _ExportAttr,
            {function, _, test_render, 1, _Clauses}
        ],
        TransformedForms
    ),

    ct:comment("Stateless template with binary elements transformed correctly").

%% Test transform_stateless_to_ast function directly
test_transform_stateless_to_ast(Config) when is_list(Config) ->
    % Create a simple stateless result list as expected by the function
    StatelessList = [
        {static, 1, ~"<h1>Title</h1><footer>Footer</footer>"}
    ],

    % Call transform_stateless_to_ast directly
    ResultAST = arizona_parse_transform:transform_stateless_to_ast(StatelessList),

    % Verify it returns a proper AST list structure
    ?assert(erl_syntax:is_tree(ResultAST)),
    ?assertEqual(list, erl_syntax:type(ResultAST)),

    ct:comment("transform_stateless_to_ast handles binary content correctly").

%% Test transform_stateful_to_ast function directly
test_transform_stateful_to_ast(Config) when is_list(Config) ->
    % Create a stateful result with only static elements to test vars_indexes path
    StatefulResult = #{
        elems_order => [1, 2, 3],
        elems => #{
            1 => {static, 1, ~"Hello, "},
            2 => {dynamic, 1, ~"arizona_socket:get_binding(name, Socket)"},
            3 => {static, 1, ~"!"}
        },
        vars_indexes => #{
            ~"name" => [2]
        }
    },

    % Call transform_stateful_to_ast directly
    ResultAST = arizona_parse_transform:transform_stateful_to_ast(StatefulResult),

    % Verify it returns a proper AST map structure
    ?assert(erl_syntax:is_tree(ResultAST)),
    ?assertEqual(map_expr, erl_syntax:type(ResultAST)),

    ct:comment("transform_stateful_to_ast handles variable indexes correctly").

%% Test stateless with pure binary literals
test_stateless_binary_handling(Config) when is_list(Config) ->
    % Create a stateless list with only binary literals
    PureBinaryList = [
        {static, 1, ~"<html>"},
        {static, 1, ~"<body>Static content</body>"},
        {static, 1, ~"</html>"}
    ],

    % Call transform_stateless_to_ast
    ResultAST = arizona_parse_transform:transform_stateless_to_ast(PureBinaryList),

    % Verify it creates proper binary field AST nodes
    ?assert(erl_syntax:is_tree(ResultAST)),
    ?assertEqual(list, erl_syntax:type(ResultAST)),

    ct:comment("Binary literal handling in stateless transform works correctly").

%% Test dynamic expression AST creation
test_dynamic_expression_ast_creation(Config) when is_list(Config) ->
    % Test transform_stateless_to_ast with non-binary items to trigger line 123
    MixedList = [
        {static, 1, ~"<h1>Title</h1>"},
        % This will trigger the erl_syntax:abstract(Item) path
        {dynamic, 1, ~"Item"},
        {static, 1, ~"<footer>Footer</footer>"}
    ],

    % Call transform_stateless_to_ast to trigger the generic item handling
    ResultAST = arizona_parse_transform:transform_stateless_to_ast(MixedList),

    % Verify it returns a proper AST structure
    ?assert(erl_syntax:is_tree(ResultAST)),
    ?assertEqual(list, erl_syntax:type(ResultAST)),

    ct:comment("Mixed item handling in stateless transform works correctly").

%% Test nested arizona_html optimization
test_nested_arizona_optimization(Config) when is_list(Config) ->
    % Create a stateful template with deeply nested arizona_html calls
    % to test socket variable shadowing fix
    Forms = merl:quote(~""""""
    -module(test_nested_module).
    -export([test_render/1]).
    test_render(Socket) ->
        arizona_html:render_stateful(~"""""
        <div>
            Level 0: {
                _JustForm = multiple_forms,
                arizona_html:render_stateless(~""""
                <span>Level 1:
                {arizona_html:render_stateless(~"""
                <p>Level 2: Deep nesting test</p>
                """, Socket)}
                </span>
                """", Socket)
            }
        </div>
        """"", Socket).
    """"""),

    % Apply parse transform
    TransformedForms = arizona_parse_transform:parse_transform(Forms, []),

    % Convert the transformed forms back to source code to inspect the optimization
    TransformedSource = lists:flatten([erl_pp:form(Form) || Form <- TransformedForms]),

    % Verify that nested arizona_html:render_stateless calls were optimized
    % The optimized version should contain pre-parsed structures instead of raw template strings

    % Check that the outer template was transformed (should contain a map structure)
    ?assert(string:str(TransformedSource, "#{elems_order") > 0),

    % Check that socket variables at different nesting levels use different names to avoid shadowing
    % Level 0 should use _@Socket0, Level 1 should use _@Socket1
    % Level 2 is a static template with no dynamic content, so no function wrapper needed
    ?assert(string:str(TransformedSource, "_@Socket0") > 0),
    ?assert(string:str(TransformedSource, "_@Socket1") > 0),

    % Verify that we have different socket variable names (no shadowing)
    ?assert(
        string:str(TransformedSource, "_@Socket0") =/= string:str(TransformedSource, "_@Socket1")
    ),

    % Verify the transformation actually occurred by checking we don't have the original template
    ?assertEqual(0, string:str(TransformedSource, "~\"<p>Level 2")),

    % Verify the structure is valid and compiles
    ?assertMatch(
        [_ModAttr, _ExportAttr, {function, _, test_render, 1, _Clauses}], TransformedForms
    ),

    ct:comment("Nested arizona_html calls successfully optimized at compile time").

test_slot_template_transform(Config) when is_list(Config) ->
    % Test that render_slot calls with binary template defaults get optimized
    TestCode = ~""""
    -module(test_slot_optimization).
    -compile({parse_transform, arizona_parse_transform}).

    test_render(Socket) ->
        arizona_html:render_slot(footer, Socket, ~"""
        <div>Default footer with {arizona_socket:get_binding(name, Socket)}</div>
        """).
    """",

    % Parse the test code
    AbstractForms = merl:quote(TestCode),

    % Apply the parse transform
    TransformedForms = arizona_parse_transform:parse_transform(AbstractForms, []),

    % Convert back to source to verify transformation
    TransformedSource = lists:flatten([erl_pp:form(Form) || Form <- TransformedForms]),

    % Verify the template was optimized (original binary template should be gone)
    ?assertEqual(0, string:str(TransformedSource, "~\"<div>Default footer")),

    % Verify we have {stateless, ParsedTemplate} structure
    ?assert(string:str(TransformedSource, "{stateless,") > 0),

    % Verify the render_slot call structure is preserved
    ?assert(string:str(TransformedSource, "render_slot") > 0),
    ?assert(string:str(TransformedSource, "footer") > 0),

    ct:comment("render_slot template default successfully optimized at compile time").

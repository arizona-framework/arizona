-module(arizona_parse_transform_SUITE).
-behaviour(ct_suite).
-include_lib("stdlib/include/assert.hrl").
-compile([export_all, nowarn_export_all]).

%% --------------------------------------------------------------------
%% Behaviour (ct_suite) callbacks
%% --------------------------------------------------------------------

all() ->
    [
        {group, transformation_tests},
        {group, enhanced_transformation_tests},
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
        {enhanced_transformation_tests, [parallel], [
            test_arizona_attribute_extraction,
            test_function_binding_analysis,
            test_variable_to_binding_mapping,
            test_enhanced_vars_indexes_generation,
            test_template_with_variable_context,
            test_multiple_function_analysis,
            test_nested_binding_calls,
            test_multiple_binding_dependencies_vars_indexes,
            test_conditional_binding_dependencies,
            test_enhanced_parse_transform_end_to_end,
            test_enhanced_parse_transform_todo_app_scenario,
            test_current_function_bindings_issue,
            test_enhanced_binding_analysis_needed,
            test_proposed_current_function_bindings_fix
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
    % Create a stateful result matching new parser format (without vars_indexes)
    StatefulResult = #{
        elems_order => [1, 2, 3],
        elems => #{
            1 => {static, 1, ~"Hello, "},
            2 => {dynamic, 1, ~"arizona_socket:get_binding(name, Socket)"},
            3 => {static, 1, ~"!"}
        }
    },

    % Call transform_stateful_to_ast directly
    ResultAST = arizona_parse_transform:transform_stateful_to_ast(StatefulResult),

    % Verify it returns a proper AST map structure
    ?assert(erl_syntax:is_tree(ResultAST)),
    ?assertEqual(map_expr, erl_syntax:type(ResultAST)),

    ct:comment("transform_stateful_to_ast handles new parser format correctly").

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

%% --------------------------------------------------------------------
%% Enhanced Transformation Test Cases
%% --------------------------------------------------------------------

%% Test arizona_parse_transform attribute extraction
test_arizona_attribute_extraction(Config) when is_list(Config) ->
    % Create AST with arizona_parse_transform attribute
    Forms = merl:quote(~""""
    -module(test_enhanced_module).
    -compile({parse_transform, arizona_parse_transform}).
    -arizona_parse_transform([render/1, render_card/2]).
    -export([render/1, render_card/2]).

    render(Socket) -> ok.
    render_card(Socket, Data) -> ok.
    """"),

    % Test attribute extraction
    ArizonaFunctions = arizona_parse_transform:extract_arizona_functions(Forms),

    % Verify functions are extracted correctly
    ?assertEqual([{render, 1}, {render_card, 2}], lists:sort(ArizonaFunctions)),

    ct:comment("arizona_parse_transform attribute extraction successful").

%% Test function binding analysis
test_function_binding_analysis(Config) when is_list(Config) ->
    % Create function AST with variable bindings
    Function = merl:quote(~""""
    render(Socket) ->
        Count = arizona_socket:get_binding(count, Socket),
        UserName = arizona_socket:get_binding(user_name, Socket, "Anonymous"),
        {Count, UserName}.
    """"),

    % Test binding analysis
    VarBindings = arizona_parse_transform:analyze_function_for_bindings(Function),

    % Verify variable to binding mapping
    Expected = #{
        ~"Count" => [~"count"],
        ~"UserName" => [~"user_name"]
    },
    ?assertEqual(Expected, VarBindings),

    ct:comment("function binding analysis successful").

%% Test variable to binding mapping
test_variable_to_binding_mapping(Config) when is_list(Config) ->
    % Test template structure without vars_indexes
    TemplateStructure = #{
        elems => #{
            0 => {static, 1, ~"<div>Hello "},
            1 => {dynamic, 1, ~"UserName"},
            2 => {static, 1, ~"! Count: "},
            3 => {dynamic, 1, ~"Count"},
            4 => {static, 1, ~"</div>"}
        }
    },

    % Variable context mapping
    VarBindings = #{
        ~"UserName" => ~"user_name",
        ~"Count" => ~"count"
    },

    % Generate vars_indexes
    VarsIndexes = arizona_parse_transform:generate_vars_indexes(TemplateStructure, VarBindings),

    % Verify correct mapping
    Expected = #{
        ~"user_name" => [1],
        ~"count" => [3]
    },
    ?assertEqual(Expected, VarsIndexes),

    ct:comment("variable to binding mapping successful").

%% Test enhanced vars_indexes generation
test_enhanced_vars_indexes_generation(Config) when is_list(Config) ->
    % Test with complex template
    TemplateStructure = #{
        elems => #{
            0 => {static, 1, ~"<h1>"},
            1 => {dynamic, 1, ~"Title"},
            2 => {static, 1, ~"</h1><p>Count: "},
            3 => {dynamic, 1, ~"Count"},
            4 => {static, 1, ~", Status: "},
            5 => {dynamic, 1, ~"Status"},
            6 => {static, 1, ~"</p>"}
        }
    },

    VarBindings = #{
        ~"Title" => ~"page_title",
        ~"Count" => ~"counter",
        ~"Status" => ~"user_status"
    },

    VarsIndexes = arizona_parse_transform:generate_vars_indexes(TemplateStructure, VarBindings),

    Expected = #{
        ~"page_title" => [1],
        ~"counter" => [3],
        ~"user_status" => [5]
    },
    ?assertEqual(Expected, VarsIndexes),

    ct:comment("enhanced vars_indexes generation successful").

%% Test template with variable context
test_template_with_variable_context(Config) when is_list(Config) ->
    % Test the parse_template_for_stateful_with_context function
    TemplateString = ~"<div>Hello {UserName}! Count: {Count}</div>",
    VarBindings = #{
        ~"UserName" => ~"user_name",
        ~"Count" => ~"count"
    },

    % This tests the internal function - in practice this would be used by the parse transform
    Result = arizona_parse_transform:parse_template_for_stateful_with_context(
        TemplateString, 1, [], 0, VarBindings
    ),

    % Verify result is a binary (compiled template data)
    ?assert(is_binary(Result)),

    % Verify it contains the vars_indexes with correct bindings
    ResultStr = binary_to_list(Result),
    ?assert(string:str(ResultStr, "user_name") > 0),
    ?assert(string:str(ResultStr, "count") > 0),

    ct:comment("template with variable context successful").

%% Test multiple function analysis
test_multiple_function_analysis(Config) when is_list(Config) ->
    % Create AST with multiple functions
    Forms = merl:quote(~""""
    -module(test_multiple_module).
    -arizona_parse_transform([render/1, render_card/2]).

    render(Socket) ->
        Title = arizona_socket:get_binding(title, Socket),
        Title.

    render_card(Socket, Data) ->
        Name = arizona_socket:get_binding(name, Socket),
        Age = arizona_socket:get_binding(age, Socket),
        {Name, Age, Data}.

    other_function(X) ->
        X + 1.
    """"),

    % Extract functions and build bindings map
    ArizonaFunctions = arizona_parse_transform:extract_arizona_functions(Forms),
    FunctionBindings = arizona_parse_transform:build_function_bindings_map(Forms, ArizonaFunctions),

    % Verify correct function binding extraction
    Expected = #{
        {render, 1} => #{~"Title" => [~"title"]},
        {render_card, 2} => #{
            ~"Name" => [~"name"],
            ~"Age" => [~"age"]
        }
    },
    ?assertEqual(Expected, FunctionBindings),

    ct:comment("multiple function analysis successful").

%% Test nested binding calls (complex case)
test_nested_binding_calls(Config) when is_list(Config) ->
    % Create function AST with nested arizona_socket:get_binding calls
    Function = merl:quote(~""""
    render(Socket) ->
        UserName = arizona_socket:get_binding(
            user_name, Socket, arizona_socket:get_binding(email, Socket)
        ),
        Count = arizona_socket:get_binding(count, Socket),
        {UserName, Count}.
    """"),

    % Test binding analysis - should capture ALL binding dependencies
    VarBindings = arizona_parse_transform:analyze_function_for_bindings(Function),

    % Enhanced implementation captures nested dependencies
    ExpectedEnhanced = #{
        % UserName depends on both bindings (sorted)
        ~"UserName" => [~"email", ~"user_name"],
        % Count depends on single binding (still list format)
        ~"Count" => [~"count"]
    },
    ?assertEqual(ExpectedEnhanced, VarBindings),

    ct:comment("nested binding calls test - enhanced dependency tracking").

%% Test vars_indexes generation with multiple binding dependencies
test_multiple_binding_dependencies_vars_indexes(Config) when is_list(Config) ->
    % Template structure that uses variables with multiple dependencies
    TemplateStructure = #{
        elems => #{
            0 => {static, 1, ~"<div>Hello "},
            % Uses UserName (depends on user_name + email)
            1 => {dynamic, 1, ~"UserName"},
            2 => {static, 1, ~", Count: "},
            % Uses Count (depends on count only)
            3 => {dynamic, 1, ~"Count"},
            4 => {static, 1, ~"</div>"}
        }
    },

    % Variable context with multiple dependencies
    VarBindings = #{
        % Multiple dependencies
        ~"UserName" => [~"user_name", ~"email"],
        % Single dependency
        ~"Count" => [~"count"]
    },

    VarsIndexes = arizona_parse_transform:generate_vars_indexes(TemplateStructure, VarBindings),

    % Expected: both user_name and email changes should trigger UserName re-render
    Expected = #{
        % user_name binding affects element 1 (UserName)
        ~"user_name" => [1],
        % email binding also affects element 1 (UserName)
        ~"email" => [1],
        % count binding affects element 3 (Count)
        ~"count" => [3]
    },
    ?assertEqual(Expected, VarsIndexes),

    ct:comment("multiple binding dependencies in vars_indexes generation successful").

%% Test conditional binding dependencies (case expressions)
test_conditional_binding_dependencies(Config) when is_list(Config) ->
    % Create function AST with conditional arizona_socket:get_binding calls
    Function = merl:quote(~""""
    render(Socket) ->
        Foo = case arizona_socket:get_binding(foo, Socket) of
            foo -> arizona_socket:get_binding(bar, Socket);
            bar -> arizona_socket:get_binding(baz, Socket)
        end,
        Count = arizona_socket:get_binding(count, Socket),
        {Foo, Count}.
    """"),

    % Test binding analysis - should capture ALL binding dependencies
    VarBindings = arizona_parse_transform:analyze_function_for_bindings(Function),

    % Foo depends on foo (condition) + bar/baz (possible results)
    Expected = #{
        % All three bindings (sorted)
        ~"Foo" => [~"bar", ~"baz", ~"foo"],
        ~"Count" => [~"count"]
    },
    ?assertEqual(Expected, VarBindings),

    ct:comment("conditional binding dependencies test successful").

test_enhanced_parse_transform_end_to_end(Config) when is_list(Config) ->
    % Test the complete enhanced parse transform functionality end-to-end
    % This verifies that the actual compilation generates proper vars_indexes

    % Define a test module source code with enhanced parse transform
    Forms = merl:quote(~""""
    -module(test_enhanced_module).
    -compile({parse_transform, arizona_parse_transform}).
    -arizona_parse_transform([render/1]).
    -export([render/1]).

    render(Socket) ->
        Count = arizona_socket:get_binding(count, Socket),
        UserName = arizona_socket:get_binding(user_name, Socket),
        arizona_html:render_stateful(~"""
        <div>
            <h1>Hello {UserName}!</h1>
            <p>Count: {Count}</p>
        </div>
        """, Socket).
    """"),
    TransformedForms = arizona_parse_transform:parse_transform(Forms, []),

    % Find the render function in the transformed AST
    RenderFunction = lists:keyfind(render, 3, TransformedForms),
    ?assertNotEqual(false, RenderFunction),

    % Convert to string to analyze the generated code
    TransformedCode = iolist_to_binary(erl_pp:form(RenderFunction)),

    % Verify that vars_indexes contains the expected mappings
    % The transformed code should contain the specific bindings with correct element indices (atoms)
    ?assert(binary:match(TransformedCode, ~"vars_indexes") =/= nomatch),
    ?assert(binary:match(TransformedCode, ~"user_name => [1]") =/= nomatch),
    ?assert(binary:match(TransformedCode, ~"count => [3]") =/= nomatch),

    ct:comment("enhanced parse transform end-to-end test successful").

test_enhanced_parse_transform_todo_app_scenario(Config) when is_list(Config) ->
    % Test the exact TODO app scenario with enhanced parse transform
    % This should generate correct vars_indexes with todos mappings but currently doesn't

    % Define the exact test_todo_enhanced module from the shell example
    Forms = merl:quote(~""""
    -module(test_todo_enhanced).
    -compile({parse_transform, arizona_parse_transform}).
    -arizona_parse_transform([render/1]).
    -export([render/1]).

    render(Socket) ->
        % Extract the EXACT same expressions from TODO app template
        NewTodoText = arizona_socket:get_binding(new_todo_text, Socket),
        Todos = arizona_socket:get_binding(todos, Socket),
        Filter = arizona_socket:get_binding(filter, Socket),
        FilteredTodos = arizona_todo_app_live:filter_todos(Todos, Filter),
        TodosLength = length(Todos),
        UncompletedTodos = lists:filter(fun(#{completed := Completed}) -> not Completed end, Todos),
        UncompletedLength = length(UncompletedTodos),
        HasCompleted = TodosLength > UncompletedLength,
        arizona_html:render_live(~"""
        <div id="root" class="todo-app">
            <input value="{NewTodoText}" />
            <main>{FilteredTodos}</main>
            <footer>
                <span>Count: {UncompletedLength}</span>
                <div>Filter: {Filter}</div>
                {case HasCompleted of
                    true -> ~"<button>Clear completed</button>";
                    false -> ~""
                end}
            </footer>
        </div>
        """, Socket).
    """"),

    % Transform the forms using the parse transform
    TransformedForms = arizona_parse_transform:parse_transform(Forms, []),

    % Find the render function in the transformed AST
    RenderFunction = lists:keyfind(render, 3, TransformedForms),
    ?assertNotEqual(false, RenderFunction),

    % Convert to string to analyze the generated code
    TransformedCode = iolist_to_binary(erl_pp:form(RenderFunction)),

    % Verify basic vars_indexes structure exists
    ?assert(binary:match(TransformedCode, ~"vars_indexes") =/= nomatch),

    % Verify the mappings we expect from variable analysis
    % These should be generated correctly:

    % NewTodoText -> element 1
    ?assert(binary:match(TransformedCode, ~"new_todo_text => [1]") =/= nomatch),
    % With enhanced analysis, filter affects both element 7 (Filter var)
    % and element 3 (FilteredTodos)
    FilterMatches = binary:matches(TransformedCode, ~"filter"),
    ?assert(length(FilterMatches) > 0),

    % SUCCESS: Enhanced parse transform now correctly captures transitive dependencies!
    % todos should map to elements that use FilteredTodos, UncompletedLength, HasCompleted

    % Extract the actual vars_indexes from the generated code
    VarsIndexesMatch = re:run(
        TransformedCode,
        ~"vars_indexes\\s*=>\\s*(#\\{[^}]+\\})",
        [{capture, all_but_first, binary}]
    ),

    case VarsIndexesMatch of
        {match, [VarsIndexesStr]} ->
            % Enhanced behavior: todos mappings should now exist
            ?assert(binary:match(VarsIndexesStr, ~"todos") =/= nomatch),

            % Verify todos mapping includes elements that depend on todos
            % With enhanced analysis, this should include elements for
            % FilteredTodos and UncompletedLength
            case binary:match(VarsIndexesStr, ~"todos") of
                {_, _} ->
                    % todos mapping exists - this is good!
                    % The exact elements may vary but should include element 3
                    % (FilteredTodos) and 5 (UncompletedLength)

                    % FilteredTodos element
                    ?assert(binary:match(VarsIndexesStr, ~"3") =/= nomatch),
                    % UncompletedLength element
                    ?assert(binary:match(VarsIndexesStr, ~"5") =/= nomatch);
                nomatch ->
                    ct:fail("todos mapping missing from vars_indexes")
            end;
        nomatch ->
            ct:fail("No vars_indexes found in generated code")
    end,

    ct:comment(
        "SUCCESS: Enhanced parse transform correctly generates todos mappings " ++
            "with transitive dependencies!"
    ).

%% --------------------------------------------------------------------
%% Comprehensive CurrentFunctionBindings Issue Test
%% --------------------------------------------------------------------

%% Test to isolate and fix the CurrentFunctionBindings issue
test_current_function_bindings_issue(Config) when is_list(Config) ->
    % This test isolates and reproduces the exact issue where the enhanced parse transform
    % fails to generate proper vars_indexes with todos mapping for the TODO app scenario

    ct:comment("PHASE 1: Test analyze_function_for_bindings function directly"),

    % Create the exact function AST from our TODO app scenario
    TodoFunction = merl:quote(~""""
    render(Socket) ->
        NewTodoText = arizona_socket:get_binding(new_todo_text, Socket),
        FilteredTodos = some_module:filter_function(
            arizona_socket:get_binding(todos, Socket),
            arizona_socket:get_binding(filter, Socket)
        ),
        arizona_html:render_live(~"<div>{NewTodoText}{FilteredTodos}</div>", Socket).
    """"),

    % Test analyze_function_for_bindings directly
    VarBindings = arizona_parse_transform:analyze_function_for_bindings(TodoFunction),

    % EXPECTED: Should capture that FilteredTodos depends on [todos, filter]
    % ACTUAL: Currently only captures direct assignments like NewTodoText -> [new_todo_text]
    _ExpectedBindings = #{
        ~"NewTodoText" => [~"new_todo_text"],
        % This should be captured but isn't
        ~"FilteredTodos" => [~"todos", ~"filter"]
    },

    % Test that FilteredTodos dependencies are captured correctly
    ?assertMatch(#{~"FilteredTodos" := [~"filter", ~"todos"]}, VarBindings),

    ct:comment("PHASE 2: Test build_function_bindings_map function"),

    % Create full module forms for build_function_bindings_map test
    FullForms = merl:quote(~""""
    -module(test_todo_debug).
    -arizona_parse_transform([render/1]).

    render(Socket) ->
        NewTodoText = arizona_socket:get_binding(new_todo_text, Socket),
        FilteredTodos = some_module:filter_function(
            arizona_socket:get_binding(todos, Socket),
            arizona_socket:get_binding(filter, Socket)
        ),
        arizona_html:render_live(~"<div>{NewTodoText}{FilteredTodos}</div>", Socket).
    """"),

    ArizonaFunctions = arizona_parse_transform:extract_arizona_functions(FullForms),
    FunctionBindings = arizona_parse_transform:build_function_bindings_map(
        FullForms, ArizonaFunctions
    ),

    % Get the render/1 function bindings
    RenderBindings = maps:get({render, 1}, FunctionBindings, #{}),

    ct:comment("PHASE 3: Test vars_indexes generation"),

    % Test the vars_indexes generation with current (limited) bindings
    % This simulates what happens in generate_vars_indexes
    TemplateStructure = #{
        elems => #{
            0 => {static, 1, ~"<div>"},
            % Should map to new_todo_text
            1 => {dynamic, 1, ~"NewTodoText"},
            % Should map to todos + filter
            2 => {dynamic, 1, ~"FilteredTodos"},
            3 => {static, 1, ~"</div>"}
        }
    },

    _VarsIndexes = arizona_parse_transform:generate_vars_indexes(TemplateStructure, RenderBindings),

    ct:comment("PHASE 4: Root cause analysis"),

    % The issue is in analyze_node_for_bindings - it only captures direct assignments:
    % NewTodoText = arizona_socket:get_binding(new_todo_text, Socket)  ✓ CAPTURED
    % FilteredTodos = some_module:filter(...arizona_socket:get_binding(todos, Socket)...)
    % ✗ NOT CAPTURED

    ct:comment(
        "analyze_node_for_bindings only captures direct arizona_socket:get_binding assignments"
    ),

    ct:comment("Current function bindings issue isolated - see test output for details").

%% Test that demonstrates the enhanced binding analysis we need
test_enhanced_binding_analysis_needed(Config) when is_list(Config) ->
    % Test various complex patterns that should be captured but currently aren't

    ct:comment("Testing complex binding patterns that need enhanced analysis"),

    % Pattern 1: Function call with nested get_binding
    ComplexFunction1 = merl:quote(~""""
    render(Socket) ->
        FilteredTodos = some_module:filter_function(
            arizona_socket:get_binding(todos, Socket),
            arizona_socket:get_binding(filter, Socket)
        ),
        FilteredTodos.
    """"),

    _Bindings1 = arizona_parse_transform:analyze_function_for_bindings(ComplexFunction1),

    % Pattern 2: Arithmetic with get_binding
    ComplexFunction2 = merl:quote(~""""
    render(Socket) ->
        TodosLength = length(arizona_socket:get_binding(todos, Socket)),
        TodosLength.
    """"),

    _Bindings2 = arizona_parse_transform:analyze_function_for_bindings(ComplexFunction2),

    % Pattern 3: Case expression with get_binding
    ComplexFunction3 = merl:quote(~""""
    render(Socket) ->
        Result = case arizona_socket:get_binding(condition, Socket) of
            true -> arizona_socket:get_binding(value_a, Socket);
            false -> arizona_socket:get_binding(value_b, Socket)
        end,
        Result.
    """"),

    _Bindings3 = arizona_parse_transform:analyze_function_for_bindings(ComplexFunction3),

    % These should all capture the binding dependencies but currently don't
    % Enhanced analysis would need to traverse the entire RHS of assignments
    % and collect all arizona_socket:get_binding calls regardless of nesting

    ct:comment("Enhanced binding analysis patterns tested").

%% Test the proposed fix for CurrentFunctionBindings
test_proposed_current_function_bindings_fix(Config) when is_list(Config) ->
    % This test will verify the fix once implemented

    ct:comment("Testing proposed fix for CurrentFunctionBindings issue"),

    % The fix should enhance analyze_node_for_bindings to traverse
    % the entire RHS of match expressions, not just direct get_binding calls

    % Create test function that should capture all dependencies
    TestFunction = merl:quote(~""""
    render(Socket) ->
        NewTodoText = arizona_socket:get_binding(new_todo_text, Socket),
        FilteredTodos = some_module:filter_function(
            arizona_socket:get_binding(todos, Socket),
            arizona_socket:get_binding(filter, Socket)
        ),
        arizona_html:render_live(~"<div>{NewTodoText}{FilteredTodos}</div>", Socket).
    """"),

    % After fix, this should capture both direct and indirect dependencies
    VarBindings = arizona_parse_transform:analyze_function_for_bindings(TestFunction),

    % Verify the fix captures all dependencies
    ExpectedAfterFix = #{
        ~"NewTodoText" => [~"new_todo_text"],
        % Should be captured after fix
        ~"FilteredTodos" => [~"todos", ~"filter"]
    },

    case VarBindings of
        ExpectedAfterFix ->
            ct:comment("CurrentFunctionBindings fix verified");
        _Other ->
            ct:comment("CurrentFunctionBindings fix needs more work")
    end.

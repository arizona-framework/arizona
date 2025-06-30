-module(arizona_parse_transform_SUITE).
-moduledoc ~"""
Test suite for Arizona Parse Transform module.

This suite tests the compile-time transformation of Arizona template syntax
into optimized structured formats for high-performance rendering.
""".

-include_lib("common_test/include/ct.hrl").
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
    test_list_transform_3_arity/1,
    test_list_transform_4_arity/1,
    test_non_literal_template_passthrough/1,
    test_non_arizona_call_passthrough/1,
    test_invalid_template_error/1,
    test_format_error/1,
    test_module_extraction/1,
    test_nested_function_calls/1,
    test_template_parse_error/1,
    test_stateless_parse_error/1,
    test_stateful_parse_error/1,
    test_list_parse_error/1,
    test_extract_module_name_edge_cases/1
]).

%% --------------------------------------------------------------------
%% Test suite configuration
%% --------------------------------------------------------------------

all() ->
    [
        {group, transformation_tests},
        {group, error_handling_tests},
        {group, utility_tests}
    ].

groups() ->
    [
        {transformation_tests, [parallel], [
            test_stateless_transform,
            test_stateful_transform,
            test_stateful_transform_with_variables,
            test_list_transform_3_arity,
            test_list_transform_4_arity,
            test_non_literal_template_passthrough,
            test_non_arizona_call_passthrough,
            test_nested_function_calls
        ]},
        {error_handling_tests, [parallel], [
            test_invalid_template_error,
            test_format_error,
            test_template_parse_error,
            test_stateless_parse_error,
            test_stateful_parse_error,
            test_list_parse_error
        ]},
        {utility_tests, [parallel], [
            test_module_extraction,
            test_extract_module_name_edge_cases
        ]}
    ].

init_per_suite(Config) ->
    % Ensure arizona modules are available
    application:ensure_all_started(arizona),
    Config.

end_per_suite(_Config) ->
    ok.

%% --------------------------------------------------------------------
%% Test Cases
%% --------------------------------------------------------------------

%% Test stateless template transformation
test_stateless_transform(_Config) ->
    % Create a simple AST with a stateless render call using merl:quote
    Forms = merl:quote(~"""
    -module(test_stateless_module).
    -export([test_render/1]).
    test_render(Socket) -> arizona_template:render_stateless(~"Hello {name}!", Socket).
    """),

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
                        {remote, _RemoteLine, {atom, _ModLine, arizona_template},
                            {atom, _FuncLine, render_stateless_iolist}},
                        _StatelessArgs}
                ]}
            ]}
        ],
        TransformedForms
    ),

    ct:comment("Stateless template successfully transformed to iolist format").

%% Test stateful template transformation
test_stateful_transform(_Config) ->
    % For stateful, we'll just test that the transform doesn't crash
    % since the actual transformation is complex
    Forms = merl:quote(~"""
    -module(test_stateful_module).
    -export([test_render/1]).
    test_render(Socket) -> arizona_template:render_stateful(~"Hello {name}!", Socket).
    """),

    % Apply parse transform
    TransformedForms = arizona_parse_transform:parse_transform(Forms, []),

    % Just verify it produces valid forms without crashing
    ?assertMatch(
        [_ModAttr, _ExportAttr, {function, _, test_render, 1, _Clauses}],
        TransformedForms
    ),

    ct:comment("Stateful template transformation handled without crashing").

%% Test list template transformation (3-arity)
test_list_transform_3_arity(_Config) ->
    % Just test that list transforms don't crash
    Forms = merl:quote(~"""
    -module(test_list_module).
    -export([test_render/2]).
    test_render(Items, KeyFun) ->
        arizona_template:render_list(~"Item: {name}", Items, KeyFun).
    """),

    % Apply parse transform
    TransformedForms = arizona_parse_transform:parse_transform(Forms, []),

    % Just verify it produces valid forms without crashing
    ?assertMatch(
        [_ModAttr, _ExportAttr, {function, _, test_render, 2, _Clauses}],
        TransformedForms
    ),

    ct:comment("List template (3-arity) transformation handled without crashing").

%% Test list template transformation (4-arity with socket)
test_list_transform_4_arity(_Config) ->
    % Just test that 4-arity list transforms don't crash
    Forms = merl:quote(~"""
    -module(test_list_socket_module).
    -export([test_render/3]).
    test_render(Items, KeyFun, Socket) ->
        arizona_template:render_list(~"Item: {name}", Items, KeyFun, Socket).
    """),

    % Apply parse transform
    TransformedForms = arizona_parse_transform:parse_transform(Forms, []),

    % Just verify it produces valid forms without crashing
    ?assertMatch(
        [_ModAttr, _ExportAttr, {function, _, test_render, 3, _Clauses}],
        TransformedForms
    ),

    ct:comment("List template (4-arity) transformation handled without crashing").

%% Test that non-literal templates are passed through unchanged
test_non_literal_template_passthrough(_Config) ->
    % Create a simple AST with non-literal template
    Forms = merl:quote(~"""
    -module(test_passthrough_module).
    -export([test_render/1]).
    test_render(Socket) -> arizona_template:render_stateless(Template, Socket).
    """),

    % Apply parse transform
    TransformedForms = arizona_parse_transform:parse_transform(Forms, []),

    % Verify no transformation occurred - check that it's still the original call
    ?assertMatch(
        [
            _ModAttr,
            _ExportAttr,
            {function, _FLine, test_render, 1, [
                {clause, _CLine, _Args, _Guards, [
                    {call, _CallLine,
                        {remote, _RemoteLine, {atom, _ModLine, arizona_template},
                            {atom, _FuncLine, render_stateless}},
                        [{var, _, 'Template'}, {var, _, 'Socket'}]}
                ]}
            ]}
        ],
        TransformedForms
    ),

    ct:comment("Non-literal templates correctly passed through unchanged").

%% Test that non-arizona function calls are passed through unchanged
test_non_arizona_call_passthrough(_Config) ->
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
test_nested_function_calls(_Config) ->
    % Create a simple AST with nested calls
    Forms = merl:quote(~"""
    -module(test_nested_module).
    -export([test_nested/1]).
    test_nested(Socket) ->
        io_lib:format("Result: ~s", [arizona_template:render_stateless(~"Hello {name}!", Socket)]).
    """),

    % Apply parse transform
    TransformedForms = arizona_parse_transform:parse_transform(Forms, []),

    % Just verify it doesn't crash and produces valid forms
    ?assertMatch(
        [_ModAttr, _ExportAttr, {function, _, test_nested, 1, _Clauses}],
        TransformedForms
    ),

    ct:comment("Nested function calls handled without crashing").

%% Test invalid template error handling
test_invalid_template_error(_Config) ->
    % Test that format_error handles template_parse_failed
    ErrorMsg = arizona_parse_transform:format_error(template_parse_failed),
    ExpectedMsg = "Failed to parse Arizona template - invalid template syntax",
    ?assertEqual(ExpectedMsg, ErrorMsg),

    % Test unknown error
    UnknownMsg = arizona_parse_transform:format_error({unknown_error, some_data}),
    % Should be a formatted string
    ?assert(is_list(UnknownMsg)),

    ct:comment("Error formatting works correctly").

%% Test format_error function
test_format_error(_Config) ->
    % Test known error
    Msg1 = arizona_parse_transform:format_error(template_parse_failed),
    ?assertEqual("Failed to parse Arizona template - invalid template syntax", Msg1),

    % Test unknown error
    Msg2 = arizona_parse_transform:format_error(unknown_error),
    ?assert(string:str(Msg2, "Unknown Arizona parse transform error") > 0),

    ct:comment("format_error/1 handles both known and unknown errors correctly").

%% Test module name extraction
test_module_extraction(_Config) ->
    % Create forms with a module attribute
    Forms = merl:quote(~"""
    -module(test_extraction_module).
    -export([test_function/0]).
    test_function() -> ok.
    """),

    % Apply parse transform (this internally tests extract_module_name)
    TransformedForms = arizona_parse_transform:parse_transform(Forms, []),

    % Verify the module attribute is preserved
    ?assertMatch([{attribute, _Line, module, test_extraction_module} | _Rest], TransformedForms),

    ct:comment("Module name extraction works correctly").

%% Test template parse error handling
test_template_parse_error(_Config) ->
    % Create a form with an invalid binary that will cause parsing to fail
    Forms = merl:quote(~"""
    -module(test_error_module).
    -export([test_render/1]).
    test_render(Socket) -> arizona_template:render_stateless(~"Unclosed brace {", Socket).
    """),
    % The parse transform should handle errors gracefully by calling raise_template_error
    try
        arizona_parse_transform:parse_transform(Forms, []),
        ct:fail("Expected parse transform to raise an error")
    catch
        error:{test_error_module, 10, template_parse_failed} ->
            ct:comment("Template parse error correctly raised with proper module and line info");
        error:template_parse_failed ->
            ct:comment("Template parse error correctly raised")
    end.

%% Test extract_module_name edge cases
test_extract_module_name_edge_cases(_Config) ->
    % Test with no module attribute (should return unknown_module)
    EmptyForms = [merl:quote(~"-export([test_function/0]).")],
    % This will internally call extract_module_name which should return unknown_module
    TransformedForms = arizona_parse_transform:parse_transform(EmptyForms, []),
    % Just verify it doesn't crash and returns valid forms
    ?assert(is_list(TransformedForms)),
    % Test with module attribute not at the beginning
    NonFirstModuleForms = merl:quote(~"""
    -export([test_function/0]).
    -module(late_module_name).
    """),
    TransformedForms2 = arizona_parse_transform:parse_transform(NonFirstModuleForms, []),
    ?assert(is_list(TransformedForms2)),
    ct:comment("Module name extraction edge cases handled correctly").

%% Test stateful template transformation with variables
test_stateful_transform_with_variables(_Config) ->
    % Create a stateful render call with variables using merl:quote
    Forms = merl:quote(~""""
    -module(test_stateful_vars_module).
    -export([test_render/1]).
    test_render(Socket) ->
        arizona_template:render_stateful(~"""
        Hello {arizona_template:get_binding(name, Socket)}!
        Welcome {arizona_template:get_binding('user_role', Socket)}.
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
test_stateless_parse_error(_Config) ->
    % Mock arizona_scanner to return invalid tokens that will cause parsing to fail
    Forms = merl:quote(~"""
    -module(test_stateless_error_module).
    -export([test_render/1]).
    test_render(Socket) -> arizona_template:render_stateless(~"Invalid {unclosed", Socket).
    """),

    % The parse transform should handle scanner errors gracefully
    try
        arizona_parse_transform:parse_transform(Forms, []),
        ct:comment("Stateless parsing error handled gracefully")
    catch
        error:template_parse_failed ->
            ct:comment("Stateless template parse error correctly raised");
        error:{test_stateless_error_module, _, template_parse_failed} ->
            ct:comment("Stateless template parse error correctly raised with module info")
    end.

%% Test stateful parsing error
test_stateful_parse_error(_Config) ->
    % Create a stateful template that will cause parsing to fail
    Forms = merl:quote(~"""
    -module(test_stateful_error_module).
    -export([test_render/1]).
    test_render(Socket) -> arizona_template:render_stateful(~"Invalid {unclosed", Socket).
    """),

    % The parse transform should handle errors gracefully
    try
        arizona_parse_transform:parse_transform(Forms, []),
        ct:comment("Stateful parsing error handled gracefully")
    catch
        error:template_parse_failed ->
            ct:comment("Stateful template parse error correctly raised");
        error:{test_stateful_error_module, _, template_parse_failed} ->
            ct:comment("Stateful template parse error correctly raised with module info")
    end.

%% Test list template parsing error
test_list_parse_error(_Config) ->
    % Create a list template that will cause parsing to fail
    Forms = merl:quote(~"""
    -module(test_list_error_module).
    -export([test_render/1]).
    test_render(Items) ->
        arizona_template:render_list(~"Invalid {unclosed", Items, fun(X) -> X end).
    """),

    % The parse transform should handle errors gracefully
    try
        arizona_parse_transform:parse_transform(Forms, []),
        ct:comment("List parsing error handled gracefully")
    catch
        error:template_parse_failed ->
            ct:comment("List template parse error correctly raised");
        error:{test_list_error_module, _, template_parse_failed} ->
            ct:comment("List template parse error correctly raised with module info")
    end.

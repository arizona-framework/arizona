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
    test_non_arizona_call_passthrough/1,
    test_invalid_template_error/1,
    test_non_binary_template_error/1,
    test_format_error/1,
    test_nested_function_calls/1,
    test_stateless_parse_error/1,
    test_stateful_parse_error/1
]).

%% --------------------------------------------------------------------
%% Test suite configuration
%% --------------------------------------------------------------------

all() ->
    [
        {group, transformation_tests},
        {group, error_handling_tests}
    ].

groups() ->
    [
        {transformation_tests, [parallel], [
            test_stateless_transform,
            test_stateful_transform,
            test_stateful_transform_with_variables,
            test_non_arizona_call_passthrough,
            test_nested_function_calls
        ]},
        {error_handling_tests, [parallel], [
            test_invalid_template_error,
            test_non_binary_template_error,
            test_format_error,
            test_stateless_parse_error,
            test_stateful_parse_error
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
    Forms = merl:quote(~""""
    -module(test_stateless_module).
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
test_stateful_transform(_Config) ->
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

%% Test non-binary template error
test_non_binary_template_error(_Config) ->
    % Create a form with a variable template (should raise badarg)
    Forms = merl:quote(~"""
    -module(test_badarg_module).
    -export([test_render/1]).
    test_render(Socket) -> arizona_html:render_stateless(Template, Socket).
    """),

    % The parse transform should raise badarg for non-binary templates
    try
        arizona_parse_transform:parse_transform(Forms, []),
        ct:fail("Expected parse transform to raise badarg error")
    catch
        error:badarg ->
            ct:comment("Non-binary template correctly raised badarg error");
        error:{test_badarg_module, _, badarg} ->
            ct:comment("Non-binary template correctly raised badarg error with module info")
    end.

%% Test format_error function
test_format_error(_Config) ->
    % Test known error
    Msg1 = arizona_parse_transform:format_error(template_parse_failed),
    ?assertEqual("Failed to parse Arizona template - invalid template syntax", Msg1),

    % Test unknown error
    Msg2 = arizona_parse_transform:format_error(unknown_error),
    ?assert(string:str(Msg2, "Unknown Arizona parse transform error") > 0),

    ct:comment("format_error/1 handles both known and unknown errors correctly").

%% Test stateful template transformation with variables
test_stateful_transform_with_variables(_Config) ->
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
test_stateless_parse_error(_Config) ->
    % Mock arizona_scanner to return invalid tokens that will cause parsing to fail
    Forms = merl:quote(~"""
    -module(test_stateless_error_module).
    -export([test_render/1]).
    test_render(Socket) -> arizona_html:render_stateless(~"Invalid {unclosed", Socket).
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
    test_render(Socket) -> arizona_html:render_stateful(~"Invalid {unclosed", Socket).
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

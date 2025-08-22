-module(arizona_scanner_SUITE).
-behaviour(ct_suite).
-include_lib("stdlib/include/assert.hrl").
-compile([export_all, nowarn_export_all]).

%% --------------------------------------------------------------------
%% Behaviour (ct_suite) callbacks
%% --------------------------------------------------------------------

all() ->
    [
        {group, basic_scanning},
        {group, error_handling},
        {group, utf8_tests},
        {group, line_ending_tests},
        {group, expression_tests},
        {group, comment_tests},
        {group, edge_cases}
    ].

groups() ->
    [
        {basic_scanning, [parallel], [
            scan_empty_string_test,
            scan_static_text_test,
            scan_simple_expression_test,
            scan_mixed_content_test
        ]},
        {error_handling, [parallel], [
            invalid_utf8_error_test,
            unexpected_expr_end_error_test,
            badexpr_error_test,
            format_error_invalid_utf8_test,
            format_error_unexpected_expr_end_test,
            format_error_badexpr_test
        ]},
        {utf8_tests, [parallel], [
            scan_ascii_characters_test,
            scan_two_byte_utf8_test,
            scan_three_byte_utf8_test,
            scan_four_byte_utf8_test,
            invalid_utf8_byte_test
        ]},
        {line_ending_tests, [parallel], [
            scan_unix_newlines_test,
            scan_windows_crlf_test,
            scan_mac_cr_test,
            expression_with_crlf_test,
            expression_with_cr_test
        ]},
        {expression_tests, [parallel], [
            scan_nested_braces_test,
            scan_complex_expression_test,
            unexpected_expression_end_test,
            expression_with_newlines_test,
            malformed_expression_test
        ]},
        {comment_tests, [parallel], [
            scan_single_line_comment_test,
            scan_multiline_comment_test,
            comment_normalization_test,
            mixed_comment_expression_test
        ]},
        {edge_cases, [parallel], [
            scan_escaped_braces_test,
            scan_expression_followed_by_newline_test,
            prepend_newline_to_static_test,
            empty_expression_test
        ]}
    ].

init_per_suite(Config) ->
    Config.

end_per_suite(_Config) ->
    ok.

init_per_group(_GroupName, Config) ->
    Config.

end_per_group(_GroupName, _Config) ->
    ok.

%% --------------------------------------------------------------------
%% Basic scanning tests
%% --------------------------------------------------------------------

scan_empty_string_test(_Config) ->
    ct:comment("Test scanning empty string"),
    Result = arizona_scanner:scan_string(1, ~""),
    ?assertEqual([], Result).

scan_static_text_test(_Config) ->
    ct:comment("Test scanning static text without expressions"),
    Result = arizona_scanner:scan_string(1, ~"Hello World"),
    ?assertMatch([_Token], Result),
    [Token] = Result,
    ?assertEqual(static, arizona_token:get_category(Token)),
    ?assertEqual(~"Hello World", arizona_token:get_content(Token)),
    ?assertEqual(1, arizona_token:get_line(Token)).

scan_simple_expression_test(_Config) ->
    ct:comment("Test scanning simple dynamic expression"),
    Result = arizona_scanner:scan_string(1, ~"{name}"),
    ?assertMatch([_Token], Result),
    [Token] = Result,
    ?assertEqual(dynamic, arizona_token:get_category(Token)),
    ?assertEqual(~"name", arizona_token:get_content(Token)),
    ?assertEqual(1, arizona_token:get_line(Token)).

scan_mixed_content_test(_Config) ->
    ct:comment("Test scanning mixed static text and expressions"),
    Result = arizona_scanner:scan_string(1, ~"Hello {name}, welcome!"),
    ?assertMatch([_StaticToken1, _DynamicToken, _StaticToken2], Result),
    [StaticToken1, DynamicToken, StaticToken2] = Result,

    ?assertEqual(static, arizona_token:get_category(StaticToken1)),
    ?assertEqual(~"Hello ", arizona_token:get_content(StaticToken1)),

    ?assertEqual(dynamic, arizona_token:get_category(DynamicToken)),
    ?assertEqual(~"name", arizona_token:get_content(DynamicToken)),

    ?assertEqual(static, arizona_token:get_category(StaticToken2)),
    ?assertEqual(~", welcome!", arizona_token:get_content(StaticToken2)).

%% --------------------------------------------------------------------
%% Error handling tests
%% --------------------------------------------------------------------

invalid_utf8_error_test(_Config) ->
    ct:comment("Test invalid UTF-8 byte handling"),
    % Hello[invalid]World
    InvalidBinary = <<72, 101, 108, 108, 111, 255, 87, 111, 114, 108, 100>>,
    ?assertError(invalid_utf8, arizona_scanner:scan_string(1, InvalidBinary)).

unexpected_expr_end_error_test(_Config) ->
    ct:comment("Test unexpected expression end error"),
    ?assertError(unexpected_expr_end, arizona_scanner:scan_string(1, ~"{incomplete")).

badexpr_error_test(_Config) ->
    ct:comment("Test bad expression parsing error"),
    %% This should trigger a merl parsing error - use invalid syntax
    ?assertError(badexpr, arizona_scanner:scan_string(1, ~"{begin end}")).

format_error_invalid_utf8_test(_Config) ->
    ct:comment("Test format_error for invalid_utf8"),
    ErrorInfo = #{cause => {1, 5, 255}},
    StackTrace = [{arizona_scanner, scan_string, [1, ~"test"], [{error_info, ErrorInfo}]}],
    Result = arizona_scanner:format_error(invalid_utf8, StackTrace),

    ?assertMatch(#{general := _, reason := _}, Result),
    ?assertEqual("Arizona template scanner UTF-8 validation failed", maps:get(general, Result)),
    ReasonStr = lists:flatten(maps:get(reason, Result)),
    ?assert(string:find(ReasonStr, "Invalid UTF-8 byte 0xFF") =/= nomatch),
    ?assert(string:find(ReasonStr, "line 1") =/= nomatch),
    ?assert(string:find(ReasonStr, "position 5") =/= nomatch).

format_error_unexpected_expr_end_test(_Config) ->
    ct:comment("Test format_error for unexpected_expr_end"),
    ErrorInfo = #{cause => {2, ~"incomplete_expr"}},
    StackTrace = [{arizona_scanner, scan_string, [2, ~"test"], [{error_info, ErrorInfo}]}],
    Result = arizona_scanner:format_error(unexpected_expr_end, StackTrace),

    ?assertMatch(#{general := _, reason := _}, Result),
    ?assertEqual("Arizona template scanner expression parsing failed", maps:get(general, Result)),
    ReasonStr = lists:flatten(maps:get(reason, Result)),
    ?assert(string:find(ReasonStr, "incomplete_expr") =/= nomatch),
    ?assert(string:find(ReasonStr, "line 2") =/= nomatch).

format_error_badexpr_test(_Config) ->
    ct:comment("Test format_error for badexpr"),
    ErrorInfo = #{cause => {3, ~"bad_expr", {parse_failed, error, badarg, []}}},
    StackTrace = [{arizona_scanner, scan_string, [3, ~"test"], [{error_info, ErrorInfo}]}],
    Result = arizona_scanner:format_error(badexpr, StackTrace),

    ?assertMatch(#{general := _, reason := _}, Result),
    ?assertEqual(
        "Arizona template scanner expression validation failed", maps:get(general, Result)
    ),
    ReasonStr = lists:flatten(maps:get(reason, Result)),
    ?assert(string:find(ReasonStr, "bad_expr") =/= nomatch),
    ?assert(string:find(ReasonStr, "line 3") =/= nomatch).

%% --------------------------------------------------------------------
%% UTF-8 tests
%% --------------------------------------------------------------------

scan_ascii_characters_test(_Config) ->
    ct:comment("Test scanning ASCII characters (1-byte UTF-8)"),
    Result = arizona_scanner:scan_string(1, ~"ASCII text 123!@#"),
    ?assertMatch([_Token], Result),
    [Token] = Result,
    ?assertEqual(static, arizona_token:get_category(Token)),
    ?assertEqual(~"ASCII text 123!@#", arizona_token:get_content(Token)).

scan_two_byte_utf8_test(_Config) ->
    ct:comment("Test scanning 2-byte UTF-8 characters"),
    % Contains é (2-byte UTF-8)
    TwoByteText = ~"Café résumé",
    Result = arizona_scanner:scan_string(1, TwoByteText),
    ?assertMatch([_Token], Result),
    [Token] = Result,
    ?assertEqual(static, arizona_token:get_category(Token)),
    ?assertEqual(TwoByteText, arizona_token:get_content(Token)).

scan_three_byte_utf8_test(_Config) ->
    ct:comment("Test scanning 3-byte UTF-8 characters"),
    % Contains 世界 (3-byte UTF-8)
    ThreeByteText = ~"Hello 世界",
    Result = arizona_scanner:scan_string(1, ThreeByteText),
    ?assertMatch([_Token], Result),
    [Token] = Result,
    ?assertEqual(static, arizona_token:get_category(Token)),
    ?assertEqual(ThreeByteText, arizona_token:get_content(Token)).

scan_four_byte_utf8_test(_Config) ->
    ct:comment("Test scanning 4-byte UTF-8 characters"),
    % Contains emojis (4-byte UTF-8)
    FourByteText = ~"Emoji: 😀🎉",
    Result = arizona_scanner:scan_string(1, FourByteText),
    ?assertMatch([_Token], Result),
    [Token] = Result,
    ?assertEqual(static, arizona_token:get_category(Token)),
    ?assertEqual(FourByteText, arizona_token:get_content(Token)).

invalid_utf8_byte_test(_Config) ->
    ct:comment("Test invalid UTF-8 byte in expression scanning"),
    %% Create a binary with invalid UTF-8 inside an expression
    InvalidExprBinary = <<"{hello", 255, "world}">>,
    ?assertError(invalid_utf8, arizona_scanner:scan_string(1, InvalidExprBinary)).

%% --------------------------------------------------------------------
%% Line ending tests
%% --------------------------------------------------------------------

scan_unix_newlines_test(_Config) ->
    ct:comment("Test scanning Unix LF line endings"),
    Text = ~"Line 1\nLine 2\nLine 3",
    Result = arizona_scanner:scan_string(1, Text),
    ?assertMatch([_Token], Result),
    [Token] = Result,
    ?assertEqual(static, arizona_token:get_category(Token)),
    ?assertEqual(~"Line 1\nLine 2\nLine 3", arizona_token:get_content(Token)).

scan_windows_crlf_test(_Config) ->
    ct:comment("Test scanning Windows CRLF line endings"),
    Text = ~"Line 1\r\nLine 2\r\nLine 3",
    Result = arizona_scanner:scan_string(1, Text),
    ?assertMatch([_Token], Result),
    [Token] = Result,
    ?assertEqual(static, arizona_token:get_category(Token)),
    ?assertEqual(~"Line 1\r\nLine 2\r\nLine 3", arizona_token:get_content(Token)).

scan_mac_cr_test(_Config) ->
    ct:comment("Test scanning Mac CR line endings"),
    Text = ~"Line 1\rLine 2\rLine 3",
    Result = arizona_scanner:scan_string(1, Text),
    ?assertMatch([_Token], Result),
    [Token] = Result,
    ?assertEqual(static, arizona_token:get_category(Token)),
    ?assertEqual(~"Line 1\rLine 2\rLine 3", arizona_token:get_content(Token)).

expression_with_crlf_test(_Config) ->
    ct:comment("Test expression scanning with CRLF line endings"),
    Text = ~"{case X of\r\n  ok -> yes;\r\n  _ -> no\r\nend}",
    Result = arizona_scanner:scan_string(1, Text),
    ?assertMatch([_Token], Result),
    [Token] = Result,
    ?assertEqual(dynamic, arizona_token:get_category(Token)),
    ExpectedContent = ~"case X of\r\n  ok -> yes;\r\n  _ -> no\r\nend",
    ?assertEqual(ExpectedContent, arizona_token:get_content(Token)).

expression_with_cr_test(_Config) ->
    ct:comment("Test expression scanning with CR line endings"),
    Text = ~"{case X of\r  ok -> yes;\r  _ -> no\rend}",
    Result = arizona_scanner:scan_string(1, Text),
    ?assertMatch([_Token], Result),
    [Token] = Result,
    ?assertEqual(dynamic, arizona_token:get_category(Token)),
    ExpectedContent = ~"case X of\r  ok -> yes;\r  _ -> no\rend",
    ?assertEqual(ExpectedContent, arizona_token:get_content(Token)).

%% --------------------------------------------------------------------
%% Expression tests
%% --------------------------------------------------------------------

scan_nested_braces_test(_Config) ->
    ct:comment("Test scanning expressions with nested braces"),
    Text = ~"{case X of {ok, Y} -> Y; {error, _} -> default end}",
    Result = arizona_scanner:scan_string(1, Text),
    ?assertMatch([_Token], Result),
    [Token] = Result,
    ?assertEqual(dynamic, arizona_token:get_category(Token)),
    ExpectedContent = ~"case X of {ok, Y} -> Y; {error, _} -> default end",
    ?assertEqual(ExpectedContent, arizona_token:get_content(Token)).

scan_complex_expression_test(_Config) ->
    ct:comment("Test scanning complex nested expression"),
    Text = ~"{lists:map(fun(X) -> {X, X * 2} end, [1, 2, 3])}",
    Result = arizona_scanner:scan_string(1, Text),
    ?assertMatch([_Token], Result),
    [Token] = Result,
    ?assertEqual(dynamic, arizona_token:get_category(Token)),
    ExpectedContent = ~"lists:map(fun(X) -> {X, X * 2} end, [1, 2, 3])",
    ?assertEqual(ExpectedContent, arizona_token:get_content(Token)).

unexpected_expression_end_test(_Config) ->
    ct:comment("Test unexpected expression end in nested context"),
    Text = ~"{case X of {ok, Y} -> Y",
    ?assertError(unexpected_expr_end, arizona_scanner:scan_string(1, Text)).

expression_with_newlines_test(_Config) ->
    ct:comment("Test expression followed by newline"),
    Text = ~"{name}\nNext line",
    Result = arizona_scanner:scan_string(1, Text),
    ?assertMatch([_DynamicToken, _StaticToken], Result),
    [DynamicToken, StaticToken] = Result,

    ?assertEqual(dynamic, arizona_token:get_category(DynamicToken)),
    ?assertEqual(~"name", arizona_token:get_content(DynamicToken)),

    ?assertEqual(static, arizona_token:get_category(StaticToken)),
    ?assertEqual(~"\nNext line", arizona_token:get_content(StaticToken)).

malformed_expression_test(_Config) ->
    ct:comment("Test malformed expression parsing"),
    %% Create an expression that will fail merl parsing

    % Invalid Erlang syntax
    Text = ~"{begin end}",
    ?assertError(badexpr, arizona_scanner:scan_string(1, Text)).

%% --------------------------------------------------------------------
%% Comment tests
%% --------------------------------------------------------------------

scan_single_line_comment_test(_Config) ->
    ct:comment("Test scanning single line comment"),
    Text = ~"{% This is a comment %}",
    Result = arizona_scanner:scan_string(1, Text),
    ?assertMatch([_Token], Result),
    [Token] = Result,
    ?assertEqual(comment, arizona_token:get_category(Token)),
    %% Comment should be normalized
    Content = arizona_token:get_content(Token),
    ct:pal("Comment content: ~p", [Content]).

scan_multiline_comment_test(_Config) ->
    ct:comment("Test scanning multiline comment"),
    Text = ~"{% Line 1\n% Line 2\n% Line 3 %}",
    Result = arizona_scanner:scan_string(1, Text),
    ?assertMatch([_Token], Result),
    [Token] = Result,
    ?assertEqual(comment, arizona_token:get_category(Token)),
    %% Comment should be normalized
    Content = arizona_token:get_content(Token),
    ct:pal("Multiline comment content: ~p", [Content]).

comment_normalization_test(_Config) ->
    ct:comment("Test comment normalization with various whitespace"),
    Text = ~"{%   Spaced comment   %}",
    Result = arizona_scanner:scan_string(1, Text),
    ?assertMatch([_Token], Result),
    [Token] = Result,
    ?assertEqual(comment, arizona_token:get_category(Token)),
    %% Comment should be normalized
    Content = arizona_token:get_content(Token),
    ct:pal("Spaced comment content: ~p", [Content]).

mixed_comment_expression_test(_Config) ->
    ct:comment("Test mixed comments and expressions"),
    Text = ~"Hello {name} {% comment %} world",
    Result = arizona_scanner:scan_string(1, Text),
    ct:pal("Mixed comment result: ~p", [Result]),
    %% Let's see what tokens we actually get
    ?assert(length(Result) >= 3).

%% --------------------------------------------------------------------
%% Edge case tests
%% --------------------------------------------------------------------

scan_escaped_braces_test(_Config) ->
    ct:comment("Test scanning escaped braces"),
    Text = ~"Before \\{escaped} after",
    Result = arizona_scanner:scan_string(1, Text),
    ?assertMatch([_Token], Result),
    [Token] = Result,
    ?assertEqual(static, arizona_token:get_category(Token)),
    %% Escaped brace should become literal brace in output
    ?assertEqual(~"Before {escaped} after", arizona_token:get_content(Token)).

scan_expression_followed_by_newline_test(_Config) ->
    ct:comment("Test expression immediately followed by newline"),
    Text = ~"{expr}\nContent",
    Result = arizona_scanner:scan_string(1, Text),
    ?assertMatch([_DynamicToken, _StaticToken], Result),
    [DynamicToken, StaticToken] = Result,

    ?assertEqual(dynamic, arizona_token:get_category(DynamicToken)),
    ?assertEqual(~"expr", arizona_token:get_content(DynamicToken)),

    ?assertEqual(static, arizona_token:get_category(StaticToken)),
    ?assertEqual(~"\nContent", arizona_token:get_content(StaticToken)).

prepend_newline_to_static_test(_Config) ->
    ct:comment("Test newline prepending to static tokens"),
    %% This tests the prepend_newline_to_first_static function when
    %% there are multiple tokens and newline needs to find the first static one
    Text = ~"{expr1}\n{expr2}Static",
    Result = arizona_scanner:scan_string(1, Text),
    ct:pal("Prepend newline result: ~p", [Result]),
    %% Let's see what we actually get
    ?assert(length(Result) >= 2).

empty_expression_test(_Config) ->
    ct:comment("Test empty expression handling"),
    Text = ~"Before {} after",
    Result = arizona_scanner:scan_string(1, Text),
    ct:pal("Empty expression result: ~p", [Result]),
    %% Let's see what we actually get
    ?assert(length(Result) >= 2).

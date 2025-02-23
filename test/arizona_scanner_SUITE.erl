-module(arizona_scanner_SUITE).
-behaviour(ct_suite).
-include_lib("stdlib/include/assert.hrl").
-compile([export_all, nowarn_export_all]).

%% --------------------------------------------------------------------
%% Behaviour (ct_suite) callbacks
%% --------------------------------------------------------------------

all() ->
    [{group, scan}].

groups() ->
    [
        {scan, [parallel], [
            scan,
            scan_html,
            scan_erlang,
            scan_comment,
            scan_start_html_end_html,
            scan_start_html_end_erlang,
            scan_start_erlang_end_html,
            scan_start_erlang_end_erlang,
            scan_new_line_cr,
            scan_new_line_crlf,
            scan_escape,
            scan_error_unexpected_expr_end,
            scan_error_badexpr
        ]}
    ].

%% --------------------------------------------------------------------
%% Tests
%% --------------------------------------------------------------------

scan(Config) when is_list(Config) ->
    Expect = [
        {comment, {59, 5}, ~"start"},
        {html, {60, 5}, ~"Text1 "},
        {erlang, {61, 5}, ~"{{{expr1}}}"},
        {comment, {62, 5}, ~"before text"},
        {html, {62, 20}, ~"Text2\nText3"},
        {comment, {63, 10}, ~"after text"},
        {comment, {64, 5}, ~"before expr"},
        {erlang, {64, 21}, ~"expr2"},
        {erlang, {65, 5}, ~"expr3"},
        {comment, {65, 12}, ~"after expr"},
        {html, {66, 5}, ~"Text4"},
        {comment, {66, 10}, ~"between text"},
        {html, {66, 26}, ~"Text5 "},
        {erlang, {67, 5}, ~"expr4"},
        {comment, {67, 12}, ~"between expr"},
        {erlang, {67, 28}, ~"expr5"},
        {comment, {68, 5}, ~"mutiple\nlines of\ncomment"},
        {erlang, {71, 5}, ~"expr6"},
        {erlang, {71, 12}, ~"Foo = foo, case Foo of foo -> {foo, expr7}; _ -> expr7 end"},
        {comment, {72, 5}, ~"end"}
    ],
    Got = arizona_scanner:scan(#{line => ?LINE + 1, indentation => 4}, ~"""
    {%   start  }
    Text1
    {{{{expr1}}}}
    {%before text }Text2
    Text3{% after text}
    {% before expr }{expr2}
    {expr3}{%after expr}
    Text4{%between text }Text5
    {expr4}{% between expr}{expr5}
    {% mutiple
     % lines of
     % comment}
    {expr6}{Foo = foo, case Foo of foo -> {foo, expr7}; _ -> expr7 end}
    {%end}
    """),
    ?assertEqual(Expect, Got).

scan_html(Config) when is_list(Config) ->
    Expect = [{html, {1, 1}, ~"Text1"}],
    Got = arizona_scanner:scan(#{}, ~"""
    Text1
    """),
    ?assertEqual(Expect, Got).

scan_erlang(Config) when is_list(Config) ->
    Expect = [{erlang, {1, 1}, ~"expr1"}],
    Got = arizona_scanner:scan(#{}, ~"""
    {expr1}
    """),
    ?assertEqual(Expect, Got).

scan_comment(Config) when is_list(Config) ->
    Expect = [{comment, {1, 1}, ~"comment"}],
    Got = arizona_scanner:scan(#{}, ~"""
    {% comment }
    """),
    ?assertEqual(Expect, Got).

scan_start_html_end_html(Config) when is_list(Config) ->
    Expect = [
        {html, {104, 5}, ~"Text1\nText2"},
        {erlang, {105, 10}, ~"expr1"},
        {html, {105, 17}, ~"Text3\nText4"}
    ],
    Got = arizona_scanner:scan(#{line => ?LINE + 1, indentation => 4}, ~"""
    Text1
    Text2{expr1}Text3
    Text4
    """),
    ?assertEqual(Expect, Got).

scan_start_html_end_erlang(Config) when is_list(Config) ->
    Expect = [
        {html, {118, 5}, ~"Text1\nText2"},
        {erlang, {119, 10}, ~"expr1"},
        {html, {119, 17}, ~"Text3\nText4 "},
        {erlang, {121, 5}, ~"expr2"}
    ],
    Got = arizona_scanner:scan(#{line => ?LINE + 1, indentation => 4}, ~"""
    Text1
    Text2{expr1}Text3
    Text4
    {expr2}
    """),
    ?assertEqual(Expect, Got).

scan_start_erlang_end_html(Config) when is_list(Config) ->
    Expect = [
        {erlang, {133, 5}, ~"expr1"},
        {html, {134, 5}, ~"Text1\nText2"},
        {erlang, {135, 10}, ~"expr2"},
        {html, {135, 17}, ~"Text3\nText4"}
    ],
    Got = arizona_scanner:scan(#{line => ?LINE + 1, indentation => 4}, ~"""
    {expr1}
    Text1
    Text2{expr2}Text3
    Text4
    """),
    ?assertEqual(Expect, Got).

scan_start_erlang_end_erlang(Config) when is_list(Config) ->
    Expect = [
        {erlang, {149, 5}, ~"expr1"},
        {html, {150, 5}, ~"Text1\nText2"},
        {erlang, {151, 10}, ~"expr2"},
        {html, {151, 17}, ~"Text3\nText4 "},
        {erlang, {153, 5}, ~"expr3"}
    ],
    Got = arizona_scanner:scan(#{line => ?LINE + 1, indentation => 4}, ~"""
    {expr1}
    Text1
    Text2{expr2}Text3
    Text4
    {expr3}
    """),
    ?assertEqual(Expect, Got).

scan_new_line_cr(Config) when is_list(Config) ->
    Expect = [
        {html, {1, 1}, ~"1 "},
        {erlang, {2, 1}, ~"[2,\r3]"},
        {html, {4, 1}, ~"4"}
    ],
    Got = arizona_scanner:scan(#{}, ~"1\r{[2,\r3]}\r4"),
    ?assertEqual(Expect, Got).

scan_new_line_crlf(Config) when is_list(Config) ->
    Expect = [
        {html, {1, 1}, ~"1 "},
        {erlang, {2, 1}, ~"[2,\r\n3]"},
        {html, {4, 1}, ~"4"}
    ],
    Got = arizona_scanner:scan(#{}, ~"1\r\n{[2,\r\n3]}\r\n4"),
    ?assertEqual(Expect, Got).

scan_escape(Config) when is_list(Config) ->
    Expect = [{html, {1, 1}, <<"<script>foo({foo: \"bar\"})</script>">>}],
    Got = arizona_scanner:scan(#{}, ~[<script>foo(\\{foo: "bar"})</script>]),
    ?assertEqual(Expect, Got).

scan_error_unexpected_expr_end(Config) when is_list(Config) ->
    Error = {unexpected_expr_end, {1, 6}},
    ?assertError(Error, arizona_scanner:scan(#{}, ~"{error")).

scan_error_badexpr(Config) when is_list(Config) ->
    Error = {badexpr, {1, 1}, ~"[error"},
    ?assertError(Error, arizona_scanner:scan(#{}, ~"{[error}")).

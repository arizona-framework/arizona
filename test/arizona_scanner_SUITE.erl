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
        {comment, {60, 5}, ~"start"},
        {html, {61, 5}, ~"Text1 "},
        {erlang, {62, 5}, ~"{{{expr1}}}"},
        {comment, {63, 5}, ~"before text"},
        {html, {63, 20}, ~"Text2\nText3"},
        {comment, {64, 10}, ~"after text"},
        {comment, {65, 5}, ~"before expr"},
        {erlang, {65, 21}, ~"expr2"},
        {erlang, {66, 5}, ~"expr3"},
        {comment, {66, 12}, ~"after expr"},
        {html, {67, 5}, ~"Text4"},
        {comment, {67, 10}, ~"between text"},
        {html, {67, 26}, ~"Text5 "},
        {erlang, {68, 5}, ~"expr4"},
        {comment, {68, 12}, ~"between expr"},
        {erlang, {68, 28}, ~"expr5"},
        {comment, {69, 5}, ~"mutiple\nlines of\ncomment"},
        {erlang, {72, 5}, ~"expr6"},
        {erlang, {72, 12}, ~"Foo = foo, case Foo of foo -> {foo, expr7}; _ -> expr7 end"},
        {comment, {73, 5}, ~"end"}
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
        {html, {105, 5}, ~"Text1\nText2"},
        {erlang, {106, 10}, ~"expr1"},
        {html, {106, 17}, ~"Text3\nText4"}
    ],
    Got = arizona_scanner:scan(#{line => ?LINE + 1, indentation => 4}, ~"""
    Text1
    Text2{expr1}Text3
    Text4
    """),
    ?assertEqual(Expect, Got).

scan_start_html_end_erlang(Config) when is_list(Config) ->
    Expect = [
        {html, {119, 5}, ~"Text1\nText2"},
        {erlang, {120, 10}, ~"expr1"},
        {html, {120, 17}, ~"Text3\nText4 "},
        {erlang, {122, 5}, ~"expr2"}
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
        {erlang, {134, 5}, ~"expr1"},
        {html, {135, 5}, ~"Text1\nText2"},
        {erlang, {136, 10}, ~"expr2"},
        {html, {136, 17}, ~"Text3\nText4"}
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
        {erlang, {150, 5}, ~"expr1"},
        {html, {151, 5}, ~"Text1\nText2"},
        {erlang, {152, 10}, ~"expr2"},
        {html, {152, 17}, ~"Text3\nText4 "},
        {erlang, {154, 5}, ~"expr3"}
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

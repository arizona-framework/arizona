-module(arizona_token_SUITE).
-behaviour(ct_suite).
-include_lib("stdlib/include/assert.hrl").
-compile([export_all, nowarn_export_all]).

%% --------------------------------------------------------------------
%% Ignore dialyzer warnings
%% --------------------------------------------------------------------

-dialyzer(
    {nowarn_function, [
        new_invalid_category_error/1,
        new_invalid_line_error/1,
        new_invalid_content_error/1,
        set_content_invalid_error/1
    ]}
).

%% --------------------------------------------------------------------
%% Behaviour (ct_suite) callbacks
%% --------------------------------------------------------------------

all() ->
    [
        {group, token_creation},
        {group, token_getters},
        {group, token_setters},
        {group, error_cases}
    ].

groups() ->
    [
        {token_creation, [parallel], [
            new_static_token,
            new_dynamic_token,
            new_comment_token
        ]},
        {token_getters, [parallel], [
            get_category_static,
            get_category_dynamic,
            get_category_comment,
            get_line_number,
            get_content_binary
        ]},
        {token_setters, [parallel], [
            set_content_new_value,
            set_content_empty_binary
        ]},
        {error_cases, [parallel], [
            new_invalid_category_error,
            new_invalid_line_error,
            new_invalid_content_error,
            set_content_invalid_error
        ]}
    ].

init_per_suite(Config) ->
    Config.

end_per_suite(Config) ->
    Config.

%% --------------------------------------------------------------------
%% Token creation tests
%% --------------------------------------------------------------------

new_static_token(Config) when is_list(Config) ->
    ct:comment("new/3 should create static token"),
    Token = arizona_token:new(static, 1, ~"<div>"),
    ?assertEqual(static, arizona_token:get_category(Token)),
    ?assertEqual(1, arizona_token:get_line(Token)),
    ?assertEqual(~"<div>", arizona_token:get_content(Token)).

new_dynamic_token(Config) when is_list(Config) ->
    ct:comment("new/3 should create dynamic token"),
    Token = arizona_token:new(dynamic, 10, ~"dynamic"),
    ?assertEqual(dynamic, arizona_token:get_category(Token)),
    ?assertEqual(10, arizona_token:get_line(Token)),
    ?assertEqual(~"dynamic", arizona_token:get_content(Token)).

new_comment_token(Config) when is_list(Config) ->
    ct:comment("new/3 should create comment token"),
    Token = arizona_token:new(comment, 5, ~"comment"),
    ?assertEqual(comment, arizona_token:get_category(Token)),
    ?assertEqual(5, arizona_token:get_line(Token)),
    ?assertEqual(~"comment", arizona_token:get_content(Token)).

%% --------------------------------------------------------------------
%% Token getters tests
%% --------------------------------------------------------------------

get_category_static(Config) when is_list(Config) ->
    ct:comment("get_category/1 should return static category"),
    Token = arizona_token:new(static, 1, ~"content"),
    ?assertEqual(static, arizona_token:get_category(Token)).

get_category_dynamic(Config) when is_list(Config) ->
    ct:comment("get_category/1 should return dynamic category"),
    Token = arizona_token:new(dynamic, 1, ~"content"),
    ?assertEqual(dynamic, arizona_token:get_category(Token)).

get_category_comment(Config) when is_list(Config) ->
    ct:comment("get_category/1 should return comment category"),
    Token = arizona_token:new(comment, 1, ~"content"),
    ?assertEqual(comment, arizona_token:get_category(Token)).

get_line_number(Config) when is_list(Config) ->
    ct:comment("get_line/1 should return line number"),
    Token = arizona_token:new(static, 42, ~"content"),
    ?assertEqual(42, arizona_token:get_line(Token)).

get_content_binary(Config) when is_list(Config) ->
    ct:comment("get_content/1 should return content binary"),
    Token = arizona_token:new(static, 1, ~"test content"),
    ?assertEqual(~"test content", arizona_token:get_content(Token)).

%% --------------------------------------------------------------------
%% Token setters tests
%% --------------------------------------------------------------------

set_content_new_value(Config) when is_list(Config) ->
    ct:comment("set_content/2 should update content while preserving other fields"),
    Token = arizona_token:new(static, 1, ~"original"),
    NewToken = arizona_token:set_content(~"updated", Token),
    ?assertEqual(static, arizona_token:get_category(NewToken)),
    ?assertEqual(1, arizona_token:get_line(NewToken)),
    ?assertEqual(~"updated", arizona_token:get_content(NewToken)).

set_content_empty_binary(Config) when is_list(Config) ->
    ct:comment("set_content/2 should handle empty binary"),
    Token = arizona_token:new(static, 1, ~"original"),
    NewToken = arizona_token:set_content(~"", Token),
    ?assertEqual(~"", arizona_token:get_content(NewToken)).

%% --------------------------------------------------------------------
%% Error cases tests
%% --------------------------------------------------------------------

new_invalid_category_error(Config) when is_list(Config) ->
    ct:comment("new/3 should cause function_clause error for invalid category"),
    ?assertError(function_clause, arizona_token:new(invalid, 1, ~"content")).

new_invalid_line_error(Config) when is_list(Config) ->
    ct:comment("new/3 should cause function_clause error for invalid line"),
    ?assertError(function_clause, arizona_token:new(static, -1, ~"content")).

new_invalid_content_error(Config) when is_list(Config) ->
    ct:comment("new/3 should cause function_clause error for non-binary content"),
    ?assertError(function_clause, arizona_token:new(static, 1, "string_not_binary")).

set_content_invalid_error(Config) when is_list(Config) ->
    ct:comment("set_content/2 should cause function_clause error for non-binary content"),
    Token = arizona_token:new(static, 1, ~"content"),
    ?assertError(function_clause, arizona_token:set_content("not_binary", Token)).

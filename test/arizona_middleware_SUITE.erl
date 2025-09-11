-module(arizona_middleware_SUITE).
-behaviour(ct_suite).
-include_lib("stdlib/include/assert.hrl").
-compile([export_all, nowarn_export_all]).

%% --------------------------------------------------------------------
%% Ignore dialyzer warnings for test functions
%% --------------------------------------------------------------------

%% Dialyzer warnings are suppressed for test functions because:
%% 1. Test middleware modules are created dynamically at runtime using merl
%% 2. Mock cowboy_req() doesn't exactly match the opaque cowboy_req:req() type
%% 3. These are controlled test scenarios, not production code
-dialyzer(
    {nowarn_function, [
        process_middlewares_empty_list_test/1,
        process_middlewares_single_continue_test/1,
        process_middlewares_single_halt_test/1,
        process_middlewares_chain_continue_test/1,
        process_middlewares_chain_halt_test/1,
        process_middlewares_with_opts_test/1
    ]}
).

%% --------------------------------------------------------------------
%% Behaviour (ct_suite) callbacks
%% --------------------------------------------------------------------

all() ->
    [
        {group, middleware_processing_tests}
    ].

groups() ->
    [
        {middleware_processing_tests, [parallel], [
            process_middlewares_empty_list_test,
            process_middlewares_single_continue_test,
            process_middlewares_single_halt_test,
            process_middlewares_chain_continue_test,
            process_middlewares_chain_halt_test,
            process_middlewares_with_opts_test
        ]}
    ].

init_per_suite(Config) ->
    % Create test middleware modules
    ok = create_test_middleware_modules(),
    Config.

end_per_suite(_Config) ->
    % Clean up test modules
    ok = cleanup_test_middleware_modules(),
    ok.

%% --------------------------------------------------------------------
%% Middleware processing tests
%% --------------------------------------------------------------------

process_middlewares_empty_list_test(_Config) ->
    ct:comment("Test processing empty middleware list"),
    MockReq = #{},
    Result = arizona_middleware:process_middlewares([], MockReq),
    ?assertEqual({continue, MockReq}, Result).

process_middlewares_single_continue_test(_Config) ->
    ct:comment("Test processing single middleware that continues"),
    MockReq = #{},
    Middlewares = [{test_middleware_continue, #{}}],
    Result = arizona_middleware:process_middlewares(Middlewares, MockReq),
    ?assertEqual({continue, MockReq}, Result).

process_middlewares_single_halt_test(_Config) ->
    ct:comment("Test processing single middleware that halts"),
    MockReq = #{},
    Middlewares = [{test_middleware_halt, #{}}],
    Result = arizona_middleware:process_middlewares(Middlewares, MockReq),
    ?assertEqual({halt, MockReq}, Result).

process_middlewares_chain_continue_test(_Config) ->
    ct:comment("Test processing middleware chain that all continue"),
    MockReq = #{},
    Middlewares = [{test_middleware_continue, #{}}, {test_middleware_continue, #{}}],
    Result = arizona_middleware:process_middlewares(Middlewares, MockReq),
    ?assertEqual({continue, MockReq}, Result).

process_middlewares_chain_halt_test(_Config) ->
    ct:comment("Test processing middleware chain where second halts"),
    MockReq = #{},
    Middlewares = [{test_middleware_continue, #{}}, {test_middleware_halt, #{}}],
    Result = arizona_middleware:process_middlewares(Middlewares, MockReq),
    ?assertEqual({halt, MockReq}, Result).

process_middlewares_with_opts_test(_Config) ->
    ct:comment("Test processing middleware with options"),
    MockReq = #{},
    Middlewares = [{test_middleware_opts, #{key => value}}],
    Result = arizona_middleware:process_middlewares(Middlewares, MockReq),
    ?assertEqual({continue, MockReq}, Result).

%% --------------------------------------------------------------------
%% Helper Functions
%% --------------------------------------------------------------------

create_test_middleware_modules() ->
    % Create test middleware that continues
    TestMiddlewareContinueCode = merl:quote(~"""
    -module('test_middleware_continue').
    -behaviour(arizona_middleware).
    -export([execute/2]).

    execute(Req, _Opts) ->
        {continue, Req}.
    """),

    % Create test middleware that halts
    TestMiddlewareHaltCode = merl:quote(~"""
    -module('test_middleware_halt').
    -behaviour(arizona_middleware).
    -export([execute/2]).

    execute(Req, _Opts) ->
        {halt, Req}.
    """),

    % Create test middleware that uses opts
    TestMiddlewareOptsCode = merl:quote(~"""
    -module('test_middleware_opts').
    -behaviour(arizona_middleware).
    -export([execute/2]).

    % Just verify opts is a map and continue
    execute(Req, Opts) when is_map(Opts) ->
        {continue, Req}.
    """),

    % Compile and load modules
    {ok, _} = merl:compile_and_load(TestMiddlewareContinueCode),
    {ok, _} = merl:compile_and_load(TestMiddlewareHaltCode),
    {ok, _} = merl:compile_and_load(TestMiddlewareOptsCode),
    ok.

cleanup_test_middleware_modules() ->
    Modules = [
        test_middleware_continue,
        test_middleware_halt,
        test_middleware_opts
    ],

    lists:foreach(
        fun(Module) ->
            code:purge(Module),
            code:delete(Module)
        end,
        Modules
    ).

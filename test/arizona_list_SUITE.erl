-module(arizona_list_SUITE).
-moduledoc ~"""
Test suite for Arizona List module.

This suite tests the function call utilities for list rendering,
including item function calls and element function calls.
""".

-include_lib("common_test/include/ct.hrl").
-include_lib("stdlib/include/assert.hrl").

%% --------------------------------------------------------------------
%% Test suite exports
%% --------------------------------------------------------------------

-export([all/0, groups/0]).

%% Test cases
-export([
    test_call_item_function_basic/1,
    test_call_item_function_binary_result/1,
    test_call_item_function_iolist_result/1,
    test_call_item_function_error_propagation/1,
    test_call_element_function_basic/1,
    test_call_element_function_with_socket/1,
    test_call_element_function_error_propagation/1,
    test_function_arity_validation/1
]).

%% --------------------------------------------------------------------
%% Test suite configuration
%% --------------------------------------------------------------------

all() ->
    [
        {group, item_function_tests},
        {group, element_function_tests},
        {group, error_handling_tests}
    ].

groups() ->
    [
        {item_function_tests, [parallel], [
            test_call_item_function_basic,
            test_call_item_function_binary_result,
            test_call_item_function_iolist_result
        ]},
        {element_function_tests, [parallel], [
            test_call_element_function_basic,
            test_call_element_function_with_socket
        ]},
        {error_handling_tests, [parallel], [
            test_call_item_function_error_propagation,
            test_call_element_function_error_propagation,
            test_function_arity_validation
        ]}
    ].

%% --------------------------------------------------------------------
%% Item Function Tests
%% --------------------------------------------------------------------

test_call_item_function_basic(Config) when is_list(Config) ->
    Item = #{id => 123, name => <<"test">>},

    %% Simple field access
    Fun = fun(I) -> maps:get(name, I) end,
    Result = arizona_list:call_item_function(Fun, Item),

    ?assertEqual(<<"test">>, Result).

test_call_item_function_binary_result(Config) when is_list(Config) ->
    Item = #{title => <<"Hello">>, count => 5},

    %% Function returning binary
    Fun = fun(I) ->
        Title = maps:get(title, I),
        Count = integer_to_binary(maps:get(count, I)),
        <<Title/binary, " (", Count/binary, ")">>
    end,

    Result = arizona_list:call_item_function(Fun, Item),
    ?assertEqual(<<"Hello (5)">>, Result).

test_call_item_function_iolist_result(Config) when is_list(Config) ->
    Item = #{product => <<"Widget">>, price => 25},

    %% Function returning iolist
    Fun = fun(I) ->
        [
            <<"<span>">>,
            maps:get(product, I),
            <<"</span>">>,
            <<"<span>$">>,
            integer_to_binary(maps:get(price, I)),
            <<"</span>">>
        ]
    end,

    Result = arizona_list:call_item_function(Fun, Item),
    Expected = [
        <<"<span>">>,
        <<"Widget">>,
        <<"</span>">>,
        <<"<span>$">>,
        <<"25">>,
        <<"</span>">>
    ],

    ?assertEqual(Expected, Result).

%% --------------------------------------------------------------------
%% Element Function Tests
%% --------------------------------------------------------------------

test_call_element_function_basic(Config) when is_list(Config) ->
    Item = #{field => <<"value">>},
    Socket = arizona_socket:new(#{}),

    %% Function using item data
    Fun = fun(I, _S) -> maps:get(field, I) end,
    Result = arizona_list:call_element_function(Fun, Item, Socket),

    ?assertEqual(<<"value">>, Result).

test_call_element_function_with_socket(Config) when is_list(Config) ->
    Item = #{id => 42},
    Socket = arizona_socket:new(#{}),

    %% Function using both item and socket
    Fun = fun(I, S) ->
        Id = maps:get(id, I),
        {item_processed, Id, S}
    end,

    Result = arizona_list:call_element_function(Fun, Item, Socket),
    ?assertEqual({item_processed, 42, Socket}, Result).

%% --------------------------------------------------------------------
%% Error Handling Tests
%% --------------------------------------------------------------------

test_call_item_function_error_propagation(Config) when is_list(Config) ->
    Item = #{id => 1},

    %% Function that throws
    ThrowFun = fun(_I) -> throw(test_error) end,
    ?assertThrow(test_error, arizona_list:call_item_function(ThrowFun, Item)),

    %% Function that errors
    ErrorFun = fun(_I) -> error(test_error) end,
    ?assertError(test_error, arizona_list:call_item_function(ErrorFun, Item)),

    %% Function that exits
    ExitFun = fun(_I) -> exit(test_error) end,
    ?assertExit(test_error, arizona_list:call_item_function(ExitFun, Item)),

    %% Bad key access
    BadKeyFun = fun(I) -> maps:get(nonexistent, I) end,
    ?assertError({badkey, nonexistent}, arizona_list:call_item_function(BadKeyFun, Item)).

test_call_element_function_error_propagation(Config) when is_list(Config) ->
    Item = #{id => 1},
    Socket = arizona_socket:new(#{}),

    %% Function that throws binding error
    BindingFun = fun(_I, _S) -> throw({binding_not_found, test_key}) end,
    ?assertThrow(
        {binding_not_found, test_key},
        arizona_list:call_element_function(BindingFun, Item, Socket)
    ),

    %% Function that errors
    ErrorFun = fun(_I, _S) -> error(badarg) end,
    ?assertError(badarg, arizona_list:call_element_function(ErrorFun, Item, Socket)),

    %% Function that exits
    ExitFun = fun(_I, _S) -> exit(normal) end,
    ?assertExit(normal, arizona_list:call_element_function(ExitFun, Item, Socket)).

test_function_arity_validation(Config) when is_list(Config) ->
    Item = #{test => value},
    Socket = arizona_socket:new(#{}),

    %% Test correct arities work
    ItemFun1 = fun(I) -> I end,
    ElementFun2 = fun(I, S) -> {I, S} end,

    ?assertMatch(#{test := value}, arizona_list:call_item_function(ItemFun1, Item)),
    ?assertMatch(
        {#{test := value}, _Socket},
        arizona_list:call_element_function(ElementFun2, Item, Socket)
    ),

    %% Test wrong arities fail with badarity
    WrongArityFun0 = fun() -> ok end,
    WrongArityFun3 = fun(_A, _B, _C) -> ok end,

    ?assertError({badarity, _}, arizona_list:call_item_function(WrongArityFun0, Item)),
    ?assertError({badarity, _}, arizona_list:call_item_function(WrongArityFun3, Item)),
    ?assertError({badarity, _}, arizona_list:call_element_function(WrongArityFun0, Item, Socket)),
    ?assertError({badarity, _}, arizona_list:call_element_function(ItemFun1, Item, Socket)).

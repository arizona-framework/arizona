-module(arizona_jsonrpc_SUITE).
-include_lib("stdlib/include/assert.hrl").

-export([all/0]).
-export([
    decode_request/1,
    decode_notification/1,
    decode_null_id_is_notification/1,
    decode_absent_params_defaults_empty/1,
    decode_parse_error/1,
    decode_invalid_non_object/1,
    decode_invalid_missing_method/1,
    decode_invalid_wrong_version/1,
    decode_invalid_params_not_object/1,
    decode_invalid_batch_array/1,
    decode_invalid_fractional_id/1,
    decode_invalid_non_scalar_id/1,
    result_shape/1,
    error_shape/1
]).

all() ->
    [
        decode_request,
        decode_notification,
        decode_null_id_is_notification,
        decode_absent_params_defaults_empty,
        decode_parse_error,
        decode_invalid_non_object,
        decode_invalid_missing_method,
        decode_invalid_wrong_version,
        decode_invalid_params_not_object,
        decode_invalid_batch_array,
        decode_invalid_fractional_id,
        decode_invalid_non_scalar_id,
        result_shape,
        error_shape
    ].

decode_request(_Config) ->
    Body =
        ~"""
    {"jsonrpc":"2.0","id":1,"method":"tools/call","params":{"name":"add"}}
    """,
    ?assertEqual(
        {ok, #{method => ~"tools/call", params => #{~"name" => ~"add"}, id => 1}},
        arizona_jsonrpc:decode(Body)
    ).

decode_notification(_Config) ->
    Body =
        ~"""
    {"jsonrpc":"2.0","method":"notifications/initialized"}
    """,
    ?assertEqual(
        {ok, #{method => ~"notifications/initialized", params => #{}, id => undefined}},
        arizona_jsonrpc:decode(Body)
    ).

decode_null_id_is_notification(_Config) ->
    Body =
        ~"""
    {"jsonrpc":"2.0","method":"ping","id":null}
    """,
    ?assertMatch({ok, #{id := undefined}}, arizona_jsonrpc:decode(Body)).

decode_absent_params_defaults_empty(_Config) ->
    Body =
        ~"""
    {"jsonrpc":"2.0","id":2,"method":"tools/list"}
    """,
    ?assertMatch({ok, #{params := #{}}}, arizona_jsonrpc:decode(Body)).

decode_parse_error(_Config) ->
    ?assertEqual({error, parse_error}, arizona_jsonrpc:decode(~"not json at all {")),
    ?assertEqual({error, parse_error}, arizona_jsonrpc:decode(<<>>)).

decode_invalid_non_object(_Config) ->
    ?assertEqual({error, invalid_request}, arizona_jsonrpc:decode(~"42")).

decode_invalid_missing_method(_Config) ->
    Body =
        ~"""
    {"jsonrpc":"2.0","id":1}
    """,
    ?assertEqual({error, invalid_request}, arizona_jsonrpc:decode(Body)).

decode_invalid_wrong_version(_Config) ->
    Body =
        ~"""
    {"jsonrpc":"1.0","id":1,"method":"ping"}
    """,
    ?assertEqual({error, invalid_request}, arizona_jsonrpc:decode(Body)).

decode_invalid_params_not_object(_Config) ->
    Body =
        ~"""
    {"jsonrpc":"2.0","id":1,"method":"ping","params":[1,2,3]}
    """,
    ?assertEqual({error, invalid_request}, arizona_jsonrpc:decode(Body)).

decode_invalid_batch_array(_Config) ->
    Body =
        ~"""
    [{"jsonrpc":"2.0","id":1,"method":"ping"}]
    """,
    ?assertEqual({error, invalid_request}, arizona_jsonrpc:decode(Body)).

decode_invalid_fractional_id(_Config) ->
    %% MCP request ids are string-or-integer, so a fractional id is invalid --
    %% and must be reported as such rather than silently read as a notification
    %% (which answers 202 with no body, hanging the client that sent it).
    Fractional =
        ~"""
    {"jsonrpc":"2.0","id":2.5,"method":"ping"}
    """,
    ?assertEqual({error, invalid_request}, arizona_jsonrpc:decode(Fractional)),
    %% Exponent notation decodes to a float too, so it is invalid the same way.
    Exponent =
        ~"""
    {"jsonrpc":"2.0","id":1e2,"method":"ping"}
    """,
    ?assertEqual({error, invalid_request}, arizona_jsonrpc:decode(Exponent)).

decode_invalid_non_scalar_id(_Config) ->
    %% Neither is a boolean, an array, nor an object a request id.
    Boolean =
        ~"""
    {"jsonrpc":"2.0","id":true,"method":"ping"}
    """,
    ?assertEqual({error, invalid_request}, arizona_jsonrpc:decode(Boolean)),
    Array =
        ~"""
    {"jsonrpc":"2.0","id":[1],"method":"ping"}
    """,
    ?assertEqual({error, invalid_request}, arizona_jsonrpc:decode(Array)),
    Object =
        ~"""
    {"jsonrpc":"2.0","id":{"n":1},"method":"ping"}
    """,
    ?assertEqual({error, invalid_request}, arizona_jsonrpc:decode(Object)).

result_shape(_Config) ->
    ?assertEqual(
        #{~"jsonrpc" => ~"2.0", ~"id" => 7, ~"result" => #{~"ok" => true}},
        arizona_jsonrpc:result(7, #{~"ok" => true})
    ).

error_shape(_Config) ->
    ?assertEqual(
        #{
            ~"jsonrpc" => ~"2.0",
            ~"id" => 7,
            ~"error" => #{~"code" => -32601, ~"message" => ~"Method not found"}
        },
        arizona_jsonrpc:error(7, -32601, ~"Method not found")
    ).

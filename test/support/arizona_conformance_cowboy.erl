-module(arizona_conformance_cowboy).
-moduledoc """
**TEST SUPPORT.** The Cowboy driver for `arizona_server_conformance_SUITE`.
""".

-behaviour(arizona_conformance_driver).

-export([start/3]).
-export([stop/1]).
-export([drain/2]).
-export([supports_route/1]).

-spec start(Name, Routes, Opts) -> {ok, inet:port_number()} | {error, term()} when
    Name :: atom(),
    Routes :: list(),
    Opts :: map().
start(Name, Routes, Opts) ->
    Port = arizona_test_port:pick(),
    ServerOpts = #{
        transport_opts => [{port, Port}],
        routes => Routes,
        compress => maps:get(compress, Opts, true)
    },
    case arizona_cowboy_server:start(Name, ServerOpts) of
        {ok, _Pid} -> {ok, Port};
        {error, _} = Error -> Error
    end.

-spec stop(Name) -> ok | {error, not_found} when
    Name :: atom().
stop(Name) ->
    arizona_cowboy_server:stop(Name).

-spec drain(Name, TimeoutMs) -> ok | {error, not_found} when
    Name :: atom(),
    TimeoutMs :: non_neg_integer().
drain(Name, TimeoutMs) ->
    arizona_cowboy_server:drain(Name, TimeoutMs).

-spec supports_route(Kind) -> boolean() when
    Kind :: mcp | reload.
supports_route(Kind) ->
    #{routes := Routes} = arizona_cowboy_server:capabilities(),
    lists:member(Kind, Routes).

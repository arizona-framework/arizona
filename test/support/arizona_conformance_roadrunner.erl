-module(arizona_conformance_roadrunner).
-moduledoc """
**TEST SUPPORT.** The roadrunner driver for `arizona_server_conformance_SUITE`.
""".

-behaviour(arizona_conformance_driver).

-export([start/3]).
-export([stop/1]).
-export([drain/2]).
-export([notify_drain/2]).

-spec start(Name, Routes, Opts) -> {ok, inet:port_number()} | {error, term()} when
    Name :: atom(),
    Routes :: list(),
    Opts :: map().
start(Name, Routes, Opts) ->
    {ok, _} = application:ensure_all_started(roadrunner),
    Port = arizona_test_port:pick(),
    ServerOpts = #{
        transport_opts => [{port, Port}],
        routes => Routes,
        compress => maps:get(compress, Opts, true)
    },
    case arizona_roadrunner_server:start(Name, ServerOpts) of
        {ok, _Pid} -> {ok, Port};
        {error, _} = Error -> Error
    end.

-spec stop(Name) -> ok | {error, not_found} when
    Name :: atom().
stop(Name) ->
    arizona_roadrunner_server:stop(Name).

-spec drain(Name, TimeoutMs) -> ok | {error, not_found} when
    Name :: atom(),
    TimeoutMs :: non_neg_integer().
drain(Name, TimeoutMs) ->
    arizona_roadrunner_server:drain(Name, TimeoutMs).

-spec notify_drain(Name, TimeoutMs) -> ok | {error, not_found} when
    Name :: atom(),
    TimeoutMs :: non_neg_integer().
notify_drain(Name, TimeoutMs) ->
    arizona_roadrunner_server:notify_drain(Name, TimeoutMs).

-module(arizona_conformance_cowboy_mounted).
-moduledoc """
**TEST SUPPORT.** The Cowboy driver for `arizona_server_conformance_SUITE` in
mounted mode: the listener is a plain Cowboy one this driver starts as its owner,
with `arizona_cowboy_middleware` in front of the owner's own router, and the
Arizona routes are mounted on it (`arizona_cowboy_server:mount/2`) -- how Arizona
runs inside a framework that owns the listener, such as Nova.
""".

-behaviour(arizona_conformance_driver).

-export([start/3]).
-export([stop/1]).
-export([drain/2]).
-export([notify_drain/2]).
-export([supports_route/1]).

-spec start(Name, Routes, Opts) -> {ok, inet:port_number()} | {error, term()} when
    Name :: atom(),
    Routes :: list(),
    Opts :: map().
start(Name, Routes, Opts) ->
    %% Mounted first: it compiles (and so validates) the routes, the way a
    %% refused route table fails `arizona_cowboy_server:start/2` before it binds.
    case
        arizona_cowboy_server:mount(Name, #{
            routes => Routes, compress => maps:get(compress, Opts, true)
        })
    of
        ok -> start_owner(Name);
        {error, _} = Error -> Error
    end.

-spec stop(Name) -> ok | {error, not_found} when
    Name :: atom().
stop(Name) ->
    case arizona_cowboy_server:unmount(Name) of
        ok -> cowboy:stop_listener(Name);
        {error, not_found} = NotFound -> NotFound
    end.

-spec drain(Name, TimeoutMs) -> ok | {error, not_found} when
    Name :: atom(),
    TimeoutMs :: non_neg_integer().
drain(Name, TimeoutMs) ->
    %% Arizona winds its sessions and requests down and unmounts; closing the
    %% listener is the owner's part.
    case arizona_cowboy_server:drain(Name, TimeoutMs) of
        ok -> cowboy:stop_listener(Name);
        {error, not_found} = NotFound -> NotFound
    end.

-spec notify_drain(Name, TimeoutMs) -> ok | {error, not_found} when
    Name :: atom(),
    TimeoutMs :: non_neg_integer().
notify_drain(Name, TimeoutMs) ->
    arizona_cowboy_server:notify_drain(Name, TimeoutMs).

-spec supports_route(Kind) -> boolean() when
    Kind :: mcp | reload.
supports_route(Kind) ->
    #{routes := Routes} = arizona_cowboy_server:capabilities(),
    lists:member(Kind, Routes).

start_owner(Name) ->
    {ok, _} = application:ensure_all_started(cowboy),
    Port = arizona_test_port:pick(),
    %% The owner's router has nothing Arizona serves, so a path no Arizona route
    %% matches gets the owner's answer.
    Dispatch = cowboy_router:compile([{'_', []}]),
    case
        cowboy:start_clear(Name, [{port, Port}], #{
            middlewares => [arizona_cowboy_middleware, cowboy_router, cowboy_handler],
            env => #{dispatch => Dispatch},
            enable_connect_protocol => true
        })
    of
        {ok, _Pid} ->
            {ok, Port};
        {error, _} = Error ->
            ok = arizona_cowboy_server:unmount(Name),
            Error
    end.

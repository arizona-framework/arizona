-module(arizona_conformance_driver).
-moduledoc """
**TEST SUPPORT.** The one server-specific seam of `arizona_server_conformance_SUITE`.

A driver boots a listener serving Arizona `Routes` on some server and reports the
port; the suite then speaks only raw HTTP/1.1 and WebSocket to it. An adapter for
another server implements these callbacks and runs the suite with
`ARIZONA_CONFORMANCE_DRIVER=<its module>`, e.g.

```sh
ARIZONA_CONFORMANCE_DRIVER=arizona_cowboy_conformance \
    rebar3 ct --suite=arizona_server_conformance_SUITE
```

`stop/1` and `drain/2` answer `{error, not_found}` for a listener that is not running.

`Opts` may carry `compress => boolean()` (the listener-wide default compression).
`start/3` must fail -- an error tuple or an exception -- on a route table it
refuses, such as a route the earlier ones fully shadow. `drain/2` winds the
listener down: live views run `handle_drain/2` and their sockets close `1001`.
""".

-export([start/4]).
-export([stop/2]).
-export([drain/3]).
-export([supports_reply/2]).

-callback start(Name :: atom(), Routes :: list(), Opts :: map()) ->
    {ok, Port :: inet:port_number()} | {error, term()}.
-callback stop(Name :: atom()) -> ok | {error, not_found}.
-callback drain(Name :: atom(), TimeoutMs :: non_neg_integer()) -> ok | {error, not_found}.

%% Whether the adapter can send a non-buffered controller reply kind. The suite
%% skips the cases of a kind the adapter does not support; omitted, every kind
%% is expected.
-callback supports_reply(Kind :: stream | sendfile | loop) -> boolean().

-optional_callbacks([supports_reply/1]).

-spec start(Driver, Name, Routes, Opts) -> {ok, inet:port_number()} | {error, term()} when
    Driver :: module(),
    Name :: atom(),
    Routes :: list(),
    Opts :: map().
start(Driver, Name, Routes, Opts) ->
    Driver:start(Name, Routes, Opts).

-spec stop(Driver, Name) -> ok | {error, not_found} when
    Driver :: module(),
    Name :: atom().
stop(Driver, Name) ->
    Driver:stop(Name).

-spec drain(Driver, Name, TimeoutMs) -> ok | {error, not_found} when
    Driver :: module(),
    Name :: atom(),
    TimeoutMs :: non_neg_integer().
drain(Driver, Name, TimeoutMs) ->
    Driver:drain(Name, TimeoutMs).

-spec supports_reply(Driver, Kind) -> boolean() when
    Driver :: module(),
    Kind :: stream | sendfile | loop.
supports_reply(Driver, Kind) ->
    case erlang:function_exported(Driver, supports_reply, 1) of
        true -> Driver:supports_reply(Kind);
        false -> true
    end.

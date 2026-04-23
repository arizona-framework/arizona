-module(arizona_adapter).
-moduledoc """
Behaviour that resolves a request path to a handler module and
associated options.

Concrete adapters wrap whatever routing table the transport already
has. The Cowboy adapter (`arizona_cowboy_router`) walks the compiled
Cowboy dispatch table; alternative adapters could front a different
HTTP server, an in-memory map, or a config file.

`arizona_socket` is one consumer -- it calls `call_resolve_route/4`
to look up the handler for a new path -- but nothing about the
behaviour is specific to that caller.

## Callback contract

```erlang
resolve_route(Path :: path(), Qs :: qs(), State :: term()) ->
    {module(), route_opts(), arizona_req:request()}.
```

`State` is opaque to the caller -- whatever the adapter needs to
perform the lookup (e.g. a compiled dispatch table reference). The
returned `request()` is a navigate-scoped `arizona_req` carrying
the new path and query.
""".

%% --------------------------------------------------------------------
%% API function exports
%% --------------------------------------------------------------------

-export([call_resolve_route/4]).

%% --------------------------------------------------------------------
%% Behaviour callbacks
%% --------------------------------------------------------------------

-callback resolve_route(Path :: path(), Qs :: qs(), State :: term()) ->
    {module(), route_opts(), arizona_req:request()}.

%% --------------------------------------------------------------------
%% Types exports
%% --------------------------------------------------------------------

-export_type([path/0]).
-export_type([qs/0]).
-export_type([route_opts/0]).

%% --------------------------------------------------------------------
%% Types definitions
%% --------------------------------------------------------------------

-nominal path() :: binary().
-nominal qs() :: binary().

-nominal route_opts() :: #{
    bindings => map(),
    on_mount => arizona_live:on_mount(),
    layout => {module(), atom()},
    _ => term()
}.

%% --------------------------------------------------------------------
%% API Functions
%% --------------------------------------------------------------------

-doc """
Invokes the `resolve_route/3` callback on an adapter module.
""".
-spec call_resolve_route(Adapter, Path, Qs, State) ->
    {module(), route_opts(), arizona_req:request()}
when
    Adapter :: module(),
    Path :: path(),
    Qs :: qs(),
    State :: term().
call_resolve_route(Adapter, Path, Qs, State) ->
    Adapter:resolve_route(Path, Qs, State).

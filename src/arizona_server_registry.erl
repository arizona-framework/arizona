-module(arizona_server_registry).
-moduledoc """
The `arizona_server` adapters that have run a listener in this node.

An adapter registers itself once a listener is up; the dev reloader then asks
each registered adapter to recompile its routes (`recompile_routes/0`). That is
how a listener started outside the `server` env -- by hand, by a test, or by
another framework that owns it -- still gets its routes refreshed on reload.

The set lives in a `persistent_term`, so it needs no process: a node that never
starts a server pays nothing, and the reloader reads it without a call. An
adapter is never unregistered; one with no listener left recompiles nothing.
""".

%% --------------------------------------------------------------------
%% API function exports
%% --------------------------------------------------------------------

-export([register/1]).
-export([adapters/0]).
-export([recompile_routes/0]).

%% --------------------------------------------------------------------
%% Ignore xref warnings
%% --------------------------------------------------------------------

%% Introspection for apps and tests; in-tree code only calls it internally.
-ignore_xref([adapters/0]).

%% --------------------------------------------------------------------
%% Macros
%% --------------------------------------------------------------------

-define(KEY, arizona_server_adapters).

%% --------------------------------------------------------------------
%% API Functions
%% --------------------------------------------------------------------

-doc """
Registers `Adapter` (an `arizona_server` module). Idempotent: writing the
term only when the set changes keeps repeated starts from paying a
`persistent_term` update each.
""".
-spec register(Adapter) -> ok when
    Adapter :: module().
register(Adapter) when is_atom(Adapter) ->
    Adapters = adapters(),
    case lists:member(Adapter, Adapters) of
        true -> ok;
        false -> persistent_term:put(?KEY, [Adapter | Adapters])
    end.

-doc "The registered adapters, most recently registered first.".
-spec adapters() -> [module()].
adapters() ->
    persistent_term:get(?KEY, []).

-doc "Asks every registered adapter to recompile its listeners' routes.".
-spec recompile_routes() -> ok.
recompile_routes() ->
    lists:foreach(fun(Adapter) -> ok = Adapter:recompile_routes() end, adapters()).

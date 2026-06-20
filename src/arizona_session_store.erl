-module(arizona_session_store).
-moduledoc """
Behaviour for a server-side session store.

In store mode (the `session_store` application env names an implementing module),
the `az_session` cookie carries only a signed opaque id and the session map lives
here -- enabling revocation (`delete/1`), large/secret state, and data the client
never sees. The framework generates the id (`arizona_session:new_id/0`) and passes
it in; an implementation only persists by id.

`arizona_session_store_ets` is the default in-memory implementation; apps can
provide their own (Redis, Mnesia, a database) for multi-node deployments. Modelled
on `Plug.Session.Store`.

An implementation may export `child_spec/0` to be supervised under `arizona_sup`
(the ETS store returns its `gen_server` spec); a backend that manages its own
process (e.g. an external connection pool) can omit it.
""".

%% --------------------------------------------------------------------
%% Callbacks
%% --------------------------------------------------------------------

-doc "Fetches the session for `Id`, or `error` when absent or expired.".
-callback get(Id :: binary()) -> {ok, arizona_req:session()} | error.

-doc "Stores `Session` under `Id` with a `TtlSecs` lifetime, overwriting any prior value.".
-callback put(Id :: binary(), Session :: arizona_req:session(), TtlSecs :: pos_integer()) -> ok.

-doc "Removes the session for `Id` (revocation/logout). A no-op when absent.".
-callback delete(Id :: binary()) -> ok.

-doc "Optional child spec so `arizona_sup` supervises the backend (e.g. an ETS owner).".
-callback child_spec() -> supervisor:child_spec().

-optional_callbacks([child_spec/0]).

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

-doc """
Fetches the session for `Id`.

- `{ok, Session}` -- found.
- `no_session` -- there is no valid session: no such id, expired, or revoked (return
  `no_session` for any of these, **not** `{error, ...}` -- a tagged error is read as a store
  failure). Reads as a signed-out request; a later write mints a fresh id rather than
  resurrecting a revoked one.
- `{error, Reason}` -- the backing store could not be reached (a database outage, a
  connection-pool timeout). Still reads as an empty session (a failed read must never
  grant access), but the framework records `Reason` on the request so an app can observe
  it (`arizona_req:session_error/1`) -- distinct from absence.

`no_session` names the *outcome* (no session), not the cause, so the same answer covers a
never-stored id, an expired one, and a revoked one. A backend with an infallible read (like
the in-memory `arizona_session_store_ets`) only ever returns `{ok, _} | no_session`; the
`{error, Reason}` answer is for backends with a fallible transport.
""".
-callback get(Id :: binary()) ->
    {ok, arizona_req:session()} | no_session | {error, Reason :: term()}.

-doc "Stores `Session` under `Id` with a `TtlSecs` lifetime, overwriting any prior value.".
-callback put(Id :: binary(), Session :: arizona_req:session(), TtlSecs :: pos_integer()) -> ok.

-doc "Removes the session for `Id` (revocation/logout). A no-op when absent.".
-callback delete(Id :: binary()) -> ok.

-doc "Optional child spec so `arizona_sup` supervises the backend (e.g. an ETS owner).".
-callback child_spec() -> supervisor:child_spec().

-optional_callbacks([child_spec/0]).

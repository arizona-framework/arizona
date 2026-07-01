-module(arizona_session_store_ets).
-moduledoc """
In-memory `arizona_session_store` backed by a public ETS table.

A supervised `gen_server` owns the table (created in `init/1`, so it outlives
request churn) and runs a periodic sweep that reaps expired rows; `get/1`/`put/3`/
`delete/1` are direct ETS operations from the calling request process, so the owner
is never on the hot path. Rows are `{Id, Session, ExpiresAt}` with `ExpiresAt` in
absolute seconds; `get/1` also lazily drops a row it finds past its deadline.

Single-node and in-memory: the table is local and owned by this process, so a restart
of the owner drops every session. For multi-node or restart-survival, provide an
`arizona_session_store` backed by a shared/persistent store (Redis, Mnesia). Config:
`session_store_sweep_ms` (default 60000).
""".
-behaviour(arizona_session_store).
-behaviour(gen_server).

%% `get/1` is the store API, not the `erlang:get/1` BIF.
-compile({no_auto_import, [get/1]}).

%% --------------------------------------------------------------------
%% API function exports
%% --------------------------------------------------------------------

-export([child_spec/0]).
-export([start_link/0]).
-export([get/1]).
-export([put/3]).
-export([delete/1]).

%% --------------------------------------------------------------------
%% gen_server callback exports
%% --------------------------------------------------------------------

-export([init/1]).
-export([handle_call/3]).
-export([handle_cast/2]).
-export([handle_info/2]).

%% --------------------------------------------------------------------
%% Ignore xref warnings
%% --------------------------------------------------------------------

%% Started via the supervisor child spec (an MFA tuple), not a direct call.
-ignore_xref([start_link/0]).

%% init/1 is the gen_server callback and must return {ok, State}; the app-defined
%% arizona_session_store behaviour alongside it hides that callback status from elvis's
%% consistent_ok_error_spec, which then misreads the required wrap as unnecessary.
-elvis([{elvis_style, consistent_ok_error_spec, disable}]).

%% --------------------------------------------------------------------
%% Macros
%% --------------------------------------------------------------------

-define(TABLE, ?MODULE).
-define(DEFAULT_SWEEP_MS, 60000).

%% --------------------------------------------------------------------
%% Types
%% --------------------------------------------------------------------

%% The owner is stateless beyond holding the table; the sweep reschedules itself.
-record(state, {}).
-type state() :: #state{}.

%% --------------------------------------------------------------------
%% API Functions
%% --------------------------------------------------------------------

-doc "The supervised child spec (the table-owning sweeper). Used by `arizona_sup`.".
-spec child_spec() -> supervisor:child_spec().
child_spec() ->
    #{
        id => ?MODULE,
        start => {?MODULE, start_link, []},
        type => worker
    }.

-doc "Starts the table owner / sweeper under the well-known name `arizona_session_store_ets`.".
-spec start_link() -> gen_server:start_ret().
start_link() ->
    gen_server:start_link({local, ?MODULE}, ?MODULE, #{}, []).

-doc "Fetches the session for `Id`, dropping and reporting `error` for an expired row.".
-spec get(Id) -> {ok, arizona_req:session()} | error when Id :: binary().
get(Id) ->
    Now = erlang:system_time(second),
    case ets:lookup(?TABLE, Id) of
        [{_, Session, ExpiresAt}] when ExpiresAt >= Now ->
            {ok, Session};
        [{_, _, _}] ->
            ok = delete(Id),
            error;
        [] ->
            error
    end.

-doc "Stores `Session` under `Id` for `TtlSecs` seconds, overwriting any prior value.".
-spec put(Id, Session, TtlSecs) -> ok when
    Id :: binary(),
    Session :: arizona_req:session(),
    TtlSecs :: pos_integer().
put(Id, Session, TtlSecs) ->
    ExpiresAt = erlang:system_time(second) + TtlSecs,
    true = ets:insert(?TABLE, {Id, Session, ExpiresAt}),
    ok.

-doc "Removes the session for `Id`.".
-spec delete(Id) -> ok when Id :: binary().
delete(Id) ->
    true = ets:delete(?TABLE, Id),
    ok.

%% --------------------------------------------------------------------
%% gen_server Callbacks
%% --------------------------------------------------------------------

-spec init(map()) -> {ok, state()}.
init(#{}) ->
    _ = ets:new(?TABLE, [named_table, public, {read_concurrency, true}]),
    {ok, schedule_sweep(#state{})}.

-spec handle_call(term(), gen_server:from(), state()) -> {reply, ok, state()}.
handle_call(_Request, _From, State) ->
    {reply, ok, State}.

-spec handle_cast(term(), state()) -> {noreply, state()}.
handle_cast(_Msg, State) ->
    {noreply, State}.

-spec handle_info(term(), state()) -> {noreply, state()}.
handle_info(sweep, State) ->
    Now = erlang:system_time(second),
    _ = ets:select_delete(?TABLE, [{{'_', '_', '$1'}, [{'<', '$1', Now}], [true]}]),
    {noreply, schedule_sweep(State)};
handle_info(_Info, State) ->
    {noreply, State}.

%% --------------------------------------------------------------------
%% Internal functions
%% --------------------------------------------------------------------

schedule_sweep(State) ->
    _ = erlang:send_after(sweep_ms(), self(), sweep),
    State.

sweep_ms() ->
    arizona_config:get_env(session_store_sweep_ms, ?DEFAULT_SWEEP_MS).

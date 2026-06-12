-module(arizona_mcp_session).
-moduledoc """
A per-session process for a stateful (`Mcp-Session-Id`) MCP connection.

Started by `arizona_mcp_sup` when an `initialize` request opens a session,
and **not** linked to the connection process that created it -- it is owned
by the supervisor so it outlives any single HTTP request, which is the
point of an MCP session. It holds the session record `arizona_mcp_handler` builds at initialize (the
handler module, its state, and the negotiated capabilities) and serves one
method at a time via `dispatch/4`, so a session's requests never race on its
state.

The session registers itself in `arizona_mcp_session_registry` on start and
removes itself on terminate. An idle timer tears the session down after a
configurable quiet period, so an abandoned session (a client that never
sends `DELETE`) does not leak; every served request resets it.
""".

-behaviour(gen_server).

%% --------------------------------------------------------------------
%% API function exports
%% --------------------------------------------------------------------

-export([start_link/3]).
-export([dispatch/4]).
-export([stop/1]).

%% --------------------------------------------------------------------
%% Ignore xref warnings
%% --------------------------------------------------------------------

%% Referenced only by `arizona_mcp_sup`'s child spec (an MFA tuple, not a call).
-ignore_xref([start_link/3]).

%% --------------------------------------------------------------------
%% gen_server callback exports
%% --------------------------------------------------------------------

-export([init/1]).
-export([handle_call/3]).
-export([handle_cast/2]).
-export([handle_info/2]).
-export([terminate/2]).

%% --------------------------------------------------------------------
%% Types definitions
%% --------------------------------------------------------------------

-record(state, {
    id :: binary(),
    session :: arizona_mcp_handler:session(),
    ttl_ms :: pos_integer(),
    ttl_timer :: reference() | undefined
}).

-type state() :: #state{}.

%% --------------------------------------------------------------------
%% API Functions
%% --------------------------------------------------------------------

-doc """
Start a session process holding `Session`, registered under `SessionId`,
and tearing itself down after `TtlMs` of inactivity. Called by
`arizona_mcp_sup:start_session/3`.
""".
-spec start_link(SessionId, Session, TtlMs) -> gen_server:start_ret() when
    SessionId :: binary(),
    Session :: arizona_mcp_handler:session(),
    TtlMs :: pos_integer().
start_link(SessionId, Session, TtlMs) ->
    gen_server:start_link(?MODULE, {SessionId, Session, TtlMs}, []).

-doc """
Run one non-initialize MCP method against the session, returning the
JSON-RPC outcome (`{reply, Object}` | `{error, Object}`). Serialized
through the session process and resets the idle timer.
""".
-spec dispatch(Pid, Method, Params, Id) -> {reply, map()} | {error, map()} when
    Pid :: pid(),
    Method :: binary(),
    Params :: map(),
    Id :: arizona_jsonrpc:id().
dispatch(Pid, Method, Params, Id) ->
    %% `infinity`: a tool may legitimately run long, and the calling roadrunner
    %% connection is already dedicated to this one request. (Bounding this and
    %% cancelling on client disconnect is a later-phase hardening.)
    gen_server:call(Pid, {dispatch, Method, Params, Id}, infinity).

-doc "Terminate the session (the `DELETE` teardown).".
-spec stop(Pid) -> ok when
    Pid :: pid().
stop(Pid) ->
    gen_server:stop(Pid).

%% --------------------------------------------------------------------
%% gen_server Callbacks
%% --------------------------------------------------------------------

-spec init({SessionId, Session, TtlMs}) -> {ok, state()} when
    SessionId :: binary(),
    Session :: arizona_mcp_handler:session(),
    TtlMs :: pos_integer().
init({SessionId, Session, TtlMs}) ->
    proc_lib:set_label({arizona_mcp_session, SessionId}),
    ok = arizona_mcp_session_registry:add(SessionId, self()),
    State = #state{id = SessionId, session = Session, ttl_ms = TtlMs},
    {ok, arm_ttl(State)}.

-spec handle_call(Request, gen_server:from(), state()) -> {reply, Reply, state()} when
    Request :: {dispatch, binary(), map(), arizona_jsonrpc:id()},
    Reply :: {reply, map()} | {error, map()}.
handle_call({dispatch, Method, Params, Id}, _From, #state{session = Session} = State) ->
    Outcome = safe_handle_method(Method, Params, Id, Session),
    {reply, Outcome, arm_ttl(State)}.

-spec handle_cast(term(), state()) -> {noreply, state()}.
handle_cast(_Msg, State) ->
    {noreply, State}.

-spec handle_info(term(), state()) -> {noreply, state()} | {stop, normal, state()}.
handle_info(session_ttl_expired, State) ->
    {stop, normal, State};
handle_info(_Info, State) ->
    {noreply, State}.

-spec terminate(term(), state()) -> ok.
terminate(_Reason, #state{id = SessionId}) ->
    ok = arizona_mcp_session_registry:remove(SessionId),
    ok.

%% --------------------------------------------------------------------
%% Internal functions
%% --------------------------------------------------------------------

%% Guard a method against a crashing tool/resource/prompt: the session must
%% survive a bad request, answering with a JSON-RPC `-32603` rather than
%% dying (which would silently drop the whole session).
safe_handle_method(Method, Params, Id, Session) ->
    try
        arizona_mcp_handler:handle_method(Method, Params, Id, Session)
    catch
        Class:Reason:Stacktrace ->
            logger:error("MCP session dispatch crashed: ~ts:~tp~n~tp", [Class, Reason, Stacktrace]),
            {error, arizona_jsonrpc:error(Id, -32603, ~"Internal error")}
    end.

%% (Re)arm the idle timer: cancel the previous one and schedule a fresh
%% teardown TtlMs from now. Called on start and after every served request.
arm_ttl(#state{ttl_timer = Old, ttl_ms = TtlMs} = State) ->
    cancel_ttl(Old),
    Ref = erlang:send_after(TtlMs, self(), session_ttl_expired),
    State#state{ttl_timer = Ref}.

cancel_ttl(undefined) ->
    ok;
cancel_ttl(Ref) ->
    _ = erlang:cancel_timer(Ref),
    ok.

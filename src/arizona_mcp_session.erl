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
-export([attach_channel/3]).
-export([detach_channel/2]).
-export([notify/3]).
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
    ttl_timer :: reference() | undefined,
    %% The attached server-to-client SSE channel (a roadrunner loop process),
    %% monitored so a racing conn death still detaches.
    channel :: pid() | undefined,
    channel_mon :: reference() | undefined,
    %% Pubsub channels subscribed for server-initiated notifications.
    channels :: [arizona_mcp:channel()],
    %% Resumability: a monotonic event id (never resets) and a bounded buffer
    %% of recent framed events, replayed past a client's Last-Event-ID.
    next_event_id :: pos_integer(),
    buffer :: queue:queue({pos_integer(), iodata()}),
    buffer_len :: non_neg_integer(),
    buffer_max :: pos_integer()
}).

-type state() :: #state{}.
-type session_opts() :: #{ttl_ms := pos_integer(), buffer_max := pos_integer()}.

-export_type([session_opts/0]).

%% --------------------------------------------------------------------
%% API Functions
%% --------------------------------------------------------------------

-doc """
Start a session process holding `Session`, registered under `SessionId`.
`SessionOpts` carries the idle `ttl_ms` and the resumability `buffer_max`.
Called by `arizona_mcp_sup:start_session/3`.
""".
-spec start_link(SessionId, Session, SessionOpts) -> gen_server:start_ret() when
    SessionId :: binary(),
    Session :: arizona_mcp_handler:session(),
    SessionOpts :: session_opts().
start_link(SessionId, Session, SessionOpts) ->
    gen_server:start_link(?MODULE, {SessionId, Session, SessionOpts}, []).

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

-doc """
Attach a server-to-client SSE channel (a roadrunner loop process) to the
session. Buffered events newer than `LastEventId` (a `Last-Event-ID` header
value, or `undefined` for a fresh stream) are replayed to the channel.
Returns `{error, already_attached}` if one is already attached -- the spec
allows only one such stream per session.
""".
-spec attach_channel(Pid, ChannelPid, LastEventId) -> ok | {error, already_attached} when
    Pid :: pid(),
    ChannelPid :: pid(),
    LastEventId :: binary() | undefined.
attach_channel(Pid, ChannelPid, LastEventId) ->
    gen_server:call(Pid, {attach_channel, ChannelPid, LastEventId}, 5000).

-doc "Detach the SSE channel (on client disconnect). Fire-and-forget.".
-spec detach_channel(Pid, ChannelPid) -> ok when
    Pid :: pid(),
    ChannelPid :: pid().
detach_channel(Pid, ChannelPid) ->
    gen_server:cast(Pid, {detach_channel, ChannelPid}).

-doc """
Push a server-initiated `notifications/<Method>` with `Params` to the
session's SSE channel. A no-op if no channel is attached. Fire-and-forget.
""".
-spec notify(Pid, Method, Params) -> ok when
    Pid :: pid(),
    Method :: binary(),
    Params :: map().
notify(Pid, Method, Params) ->
    gen_server:cast(Pid, {notify, Method, Params}).

-doc "Terminate the session (the `DELETE` teardown).".
-spec stop(Pid) -> ok when
    Pid :: pid().
stop(Pid) ->
    gen_server:stop(Pid).

%% --------------------------------------------------------------------
%% gen_server Callbacks
%% --------------------------------------------------------------------

-spec init({SessionId, Session, SessionOpts}) -> {ok, state()} when
    SessionId :: binary(),
    Session :: arizona_mcp_handler:session(),
    SessionOpts :: session_opts().
init({SessionId, Session, #{ttl_ms := TtlMs, buffer_max := BufferMax}}) ->
    proc_lib:set_label({arizona_mcp_session, SessionId}),
    ok = arizona_mcp_session_registry:add(SessionId, self()),
    Channels = subscribe_channels(Session),
    State = #state{
        id = SessionId,
        session = Session,
        ttl_ms = TtlMs,
        channels = Channels,
        next_event_id = 1,
        buffer = queue:new(),
        buffer_len = 0,
        buffer_max = BufferMax
    },
    {ok, refresh_ttl(State)}.

-spec handle_call(Request, gen_server:from(), state()) -> {reply, Reply, state()} when
    Request ::
        {dispatch, binary(), map(), arizona_jsonrpc:id()}
        | {attach_channel, pid(), binary() | undefined},
    Reply :: {reply, map()} | {error, map()} | ok | {error, already_attached}.
handle_call({dispatch, Method, Params, Id}, _From, #state{session = Session} = State) ->
    Outcome = safe_handle_method(Method, Params, Id, Session),
    {reply, Outcome, refresh_ttl(State)};
handle_call(
    {attach_channel, ChannelPid, LastEventId}, _From, #state{channel = undefined} = State
) ->
    Mon = erlang:monitor(process, ChannelPid),
    Attached = State#state{channel = ChannelPid, channel_mon = Mon},
    Replayed = replay(parse_last_event_id(LastEventId), Attached),
    {reply, ok, refresh_ttl(Replayed)};
handle_call({attach_channel, _ChannelPid, _LastEventId}, _From, State) ->
    {reply, {error, already_attached}, State}.

-spec handle_cast(term(), state()) -> {noreply, state()}.
handle_cast({detach_channel, ChannelPid}, #state{channel = ChannelPid} = State) ->
    {noreply, do_detach(State)};
handle_cast({notify, Method, Params}, State) ->
    {noreply, emit(Method, Params, State)};
handle_cast(_Msg, State) ->
    {noreply, State}.

-spec handle_info(term(), state()) -> {noreply, state()} | {stop, normal, state()}.
handle_info({arizona_mcp_notification, Method, Params}, State) ->
    {noreply, emit(Method, Params, State)};
handle_info(
    {'DOWN', Mon, process, ChannelPid, _Reason},
    #state{channel_mon = Mon, channel = ChannelPid} = State
) ->
    %% The channel's connection died without a clean detach.
    {noreply, do_detach(State)};
handle_info(session_ttl_expired, State) ->
    {stop, normal, State};
handle_info(_Info, State) ->
    {noreply, State}.

-spec terminate(term(), state()) -> ok.
terminate(_Reason, #state{id = SessionId}) ->
    %% pg auto-removes the session from its subscribed channels on exit, so
    %% only the registry row needs explicit cleanup.
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

%% Subscribe to the handler's declared pubsub channels (if it exports
%% channels/1) so broadcasts reach this session. pg auto-removes the session
%% from them when it exits, so terminate needs no explicit unsubscribe.
subscribe_channels(#{mod := Mod, state := HandlerState}) ->
    case erlang:function_exported(Mod, channels, 1) of
        true ->
            Channels = Mod:channels(HandlerState),
            lists:foreach(fun ensure_subscribed/1, Channels),
            Channels;
        false ->
            []
    end.

ensure_subscribed(Channel) ->
    case arizona_pubsub:subscribe(Channel, self()) of
        ok -> ok;
        {error, already_joined} -> ok
    end.

%% Frame a server-initiated notification with a monotonic event id, buffer it
%% for possible replay, and forward it to the channel if one is attached.
emit(Method, Params, #state{next_event_id = EventId} = State) ->
    Notification = arizona_jsonrpc:notification(Method, Params),
    Data = iolist_to_binary(json:encode(Notification)),
    Frame = roadrunner_sse:event(~"message", Data, integer_to_binary(EventId)),
    push_frame(Frame, State),
    buffer_event(EventId, Frame, State#state{next_event_id = EventId + 1}).

push_frame(_Frame, #state{channel = undefined}) ->
    ok;
push_frame(Frame, #state{channel = ChannelPid}) ->
    ChannelPid ! {mcp_event, Frame},
    ok.

%% Append a framed event to the replay buffer, evicting the oldest when the
%% bound is reached. An evicted event is no longer resumable.
buffer_event(EventId, Frame, #state{buffer = Q, buffer_len = Len, buffer_max = Max} = State) ->
    Q1 = queue:in({EventId, Frame}, Q),
    case Len + 1 > Max of
        true ->
            {_Dropped, Q2} = queue:out(Q1),
            State#state{buffer = Q2, buffer_len = Max};
        false ->
            State#state{buffer = Q1, buffer_len = Len + 1}
    end.

%% Replay buffered events newer than `LastEventId` to the freshly attached
%% channel, in order. `undefined` (a fresh stream) replays nothing.
replay(undefined, State) ->
    State;
replay(LastEventId, #state{buffer = Q, channel = ChannelPid} = State) ->
    Frames = [Frame || {EventId, Frame} <:- queue:to_list(Q), EventId > LastEventId],
    lists:foreach(fun(Frame) -> ChannelPid ! {mcp_event, Frame} end, Frames),
    State.

parse_last_event_id(undefined) ->
    undefined;
parse_last_event_id(Bin) ->
    try binary_to_integer(Bin) of
        EventId -> EventId
    catch
        error:badarg -> undefined
    end.

%% Detach the channel: stop monitoring it, forget it, and re-arm the idle
%% timer (the session is connectionless again and may be abandoned).
do_detach(#state{channel_mon = Mon} = State) ->
    demonitor_channel(Mon),
    refresh_ttl(State#state{channel = undefined, channel_mon = undefined}).

demonitor_channel(undefined) ->
    ok;
demonitor_channel(Mon) ->
    true = erlang:demonitor(Mon, [flush]),
    ok.

%% A session with a live SSE channel is kept alive by it; a connectionless
%% session counts down to teardown. Cancel any pending timer, then arm a
%% fresh one only when no channel is attached.
refresh_ttl(#state{ttl_timer = Old, ttl_ms = TtlMs, channel = Channel} = State) ->
    cancel_ttl(Old),
    case Channel of
        undefined ->
            Ref = erlang:send_after(TtlMs, self(), session_ttl_expired),
            State#state{ttl_timer = Ref};
        _ ->
            State#state{ttl_timer = undefined}
    end.

cancel_ttl(undefined) ->
    ok;
cancel_ttl(Ref) ->
    _ = erlang:cancel_timer(Ref),
    ok.

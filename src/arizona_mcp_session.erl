-module(arizona_mcp_session).
-moduledoc """
A per-session process for a stateful (`Mcp-Session-Id`) MCP connection.

Started by `arizona_mcp_sup` when an `initialize` request opens a session,
and **not** linked to the connection process that created it -- it is owned
by the supervisor so it outlives any single HTTP request, which is the
point of an MCP session. It holds the session record `arizona_mcp_handler` builds at initialize (the
handler module, its state, and the negotiated capabilities) and serves its
requests one at a time in a worker, so app code never blocks the session
process and a session's requests never race on its state.

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
-export([dispatch/5]).
-export([start_streaming_tool/6]).
-export([cancel/2]).
-export([attach_channel/3]).
-export([detach_channel/2]).
-export([notify/3]).
-export([log/4]).
-export([stop/1]).

%% --------------------------------------------------------------------
%% Ignore xref warnings
%% --------------------------------------------------------------------

%% Referenced only by `arizona_mcp_sup`'s child spec (an MFA tuple, not a call).
-ignore_xref([start_link/3]).

%% A test-only convenience wrapper (the release dispatches via `dispatch/5`).
-ignore_xref([dispatch/4]).

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
    buffer_max :: pos_integer(),
    %% In-flight streaming `tools/call` workers, by their request id, so a
    %% `notifications/cancelled` (or a client disconnect) can kill one.
    streams :: #{arizona_jsonrpc:id() => {pid(), reference(), pid()}},
    %% Buffered (non-streaming) dispatch runs in a worker so the session process
    %% never blocks on app code. One worker at a time keeps the state threading
    %% serialized; the rest wait their turn in `dispatch_q`.
    dispatch_active :: active_dispatch() | undefined,
    dispatch_q :: queue:queue(pending_dispatch()),
    %% Cap on queued (not-yet-running) buffered requests; a dispatch arriving
    %% with the queue already full is rejected rather than enqueued.
    max_pending :: pos_integer(),
    %% Periodic SSE keep-alive comment over an attached channel, so idle proxies
    %% don't close the stream. `infinity` disables it.
    keepalive_ms :: pos_integer() | infinity,
    keepalive_timer :: reference() | undefined
}).

-type state() :: #state{}.
-type session_opts() :: #{
    ttl_ms := pos_integer(),
    buffer_max := pos_integer(),
    max_pending := pos_integer(),
    keepalive_ms := pos_integer() | infinity,
    %% The request path the session was opened on, for the per-route session cap.
    route_key := binary()
}.
%% A queued buffered request awaiting its turn: the caller to reply to, its
%% request id, and the method/params to run.
-type pending_dispatch() :: {gen_server:from(), arizona_jsonrpc:id(), binary(), map()}.
%% The running buffered worker: its pid, monitor, caller to reply, request id.
-type active_dispatch() :: {pid(), reference(), gen_server:from(), arizona_jsonrpc:id()}.

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
JSON-RPC outcome (`{reply, Object}` | `{error, Object}`). The method runs
in a worker (replied to asynchronously), so a slow tool never blocks the
session process; requests are still served one at a time, so they never
race on the session's state. Resets the idle timer.
""".
-spec dispatch(Pid, Method, Params, Id) -> {reply, map()} | {error, map()} when
    Pid :: pid(),
    Method :: binary(),
    Params :: map(),
    Id :: arizona_jsonrpc:id().
dispatch(Pid, Method, Params, Id) ->
    dispatch(Pid, Method, Params, Id, infinity).

-spec dispatch(Pid, Method, Params, Id, Timeout) -> {reply, map()} | {error, map()} when
    Pid :: pid(),
    Method :: binary(),
    Params :: map(),
    Id :: arizona_jsonrpc:id(),
    Timeout :: timeout().
dispatch(Pid, Method, Params, Id, Timeout) ->
    %% Bounded by the route's `request_timeout_ms`: a buffered tool that runs
    %% past it frees the client with a -32603 (the session stays responsive and
    %% keeps running the call in its worker). A session that ends mid-wait (idle
    %% reap / DELETE) frees the caller the same way rather than crashing it.
    try
        gen_server:call(Pid, {dispatch, Method, Params, Id}, Timeout)
    catch
        exit:{timeout, _} ->
            {error, arizona_jsonrpc:error(Id, -32603, ~"Request timed out")};
        exit:_ ->
            {error, arizona_jsonrpc:error(Id, -32603, ~"Session ended")}
    end.

-doc """
Start a streaming `tools/call` in a worker process tracked by `Id`, relaying the
result and any `notifications/progress` to `ConnPid` (the POST's loop process).
The worker reads a snapshot of the session state and does **not** thread state
back -- only the serialized buffered queue mutates session state, so nothing
races on it. It frees the session to handle a `cancel/2` for the same `Id`.
""".
-spec start_streaming_tool(Pid, Name, Args, Id, Token, ConnPid) -> ok when
    Pid :: pid(),
    Name :: binary(),
    Args :: map(),
    Id :: arizona_jsonrpc:id(),
    Token :: binary() | integer(),
    ConnPid :: pid().
start_streaming_tool(Pid, Name, Args, Id, Token, ConnPid) ->
    gen_server:cast(Pid, {start_streaming_tool, Name, Args, Id, Token, ConnPid}).

-doc """
Cancel an in-flight request by its `Id` (a `notifications/cancelled`, or a
client disconnect). A streaming `tools/call` has its worker killed and its POST
loop told to stop; a buffered request has its worker killed (or is dropped from
the queue) and its caller freed with a -32603. A no-op for an unknown or
already-finished id.
""".
-spec cancel(Pid, Id) -> ok when
    Pid :: pid(),
    Id :: arizona_jsonrpc:id().
cancel(Pid, Id) ->
    gen_server:cast(Pid, {cancel, Id}).

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

-doc """
Send a `notifications/message` log to the session, dropped if `Severity` is
below the session's set minimum (`logging/setLevel`). `Level` is the wire level
atom and `Data` the message body. Fire-and-forget.
""".
-spec log(Pid, Severity, Level, Data) -> ok when
    Pid :: pid(),
    Severity :: 0..7,
    Level :: arizona_mcp:log_level(),
    Data :: term().
log(Pid, Severity, Level, Data) ->
    gen_server:cast(Pid, {log, Severity, Level, Data}).

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
init({SessionId, Session, SessionOpts}) ->
    #{
        ttl_ms := TtlMs,
        buffer_max := BufferMax,
        max_pending := MaxPending,
        keepalive_ms := KeepaliveMs,
        route_key := RouteKey
    } = SessionOpts,
    proc_lib:set_label({arizona_mcp_session, SessionId}),
    ok = arizona_mcp_session_registry:add(SessionId, self(), RouteKey),
    Channels = subscribe_channels(Session),
    State = #state{
        id = SessionId,
        session = Session,
        ttl_ms = TtlMs,
        channels = Channels,
        next_event_id = 1,
        buffer = queue:new(),
        buffer_len = 0,
        buffer_max = BufferMax,
        streams = #{},
        dispatch_active = undefined,
        dispatch_q = queue:new(),
        max_pending = MaxPending,
        keepalive_ms = KeepaliveMs,
        keepalive_timer = undefined
    },
    {ok, refresh_ttl(State)}.

-spec handle_call(Request, gen_server:from(), state()) -> Return when
    Request ::
        {dispatch, binary(), map(), arizona_jsonrpc:id()}
        | {attach_channel, pid(), binary() | undefined},
    Return :: {noreply, state()} | {reply, Reply, state()},
    Reply :: {reply, map()} | {error, map()} | ok | {error, already_attached}.
handle_call({dispatch, Method, Params, Id}, From, State) ->
    case session_coupled(Method) of
        true ->
            %% `resources/subscribe`/`unsubscribe` own a pubsub subscription
            %% keyed to THIS process, so they must run in the session process,
            %% not a worker. They run no app code (a pg join/leave), so running
            %% them inline never blocks.
            #state{session = Session} = State,
            {Outcome, Session1} = safe_handle_method(Method, Params, Id, Session),
            {reply, Outcome, refresh_ttl(State#state{session = Session1})};
        false ->
            %% Everything else runs in a worker, replied to asynchronously, so a
            %% slow tool never blocks the session process (it stays responsive to
            %% cancels, idle-reap, and channel pushes). One worker at a time keeps
            %% the state threading serialized; the rest wait in the queue, up to
            %% `max_pending` -- past that the session is overloaded and rejects.
            #state{dispatch_q = Q, max_pending = MaxPending} = State,
            case queue:len(Q) >= MaxPending of
                true ->
                    Error = arizona_jsonrpc:error(Id, -32603, ~"Session overloaded"),
                    {reply, {error, Error}, State};
                false ->
                    Q1 = queue:in({From, Id, Method, Params}, Q),
                    {noreply, refresh_ttl(start_next_dispatch(State#state{dispatch_q = Q1}))}
            end
    end;
handle_call({attach_channel, ChannelPid, LastEventId}, _From, State) ->
    case channel_alive(State) of
        true ->
            {reply, {error, already_attached}, State};
        false ->
            %% No channel, or the old one is dead -- a fast reconnect can beat
            %% the disconnect cleanup, so replace a dead channel rather than
            %% rejecting the resume with a spurious 409.
            Cleared = clear_channel(State),
            Mon = erlang:monitor(process, ChannelPid),
            Attached = Cleared#state{channel = ChannelPid, channel_mon = Mon},
            Replayed = replay(parse_last_event_id(LastEventId), Attached),
            {reply, ok, arm_keepalive(refresh_ttl(Replayed))}
    end.

-spec handle_cast(term(), state()) -> {noreply, state()}.
handle_cast({detach_channel, ChannelPid}, #state{channel = ChannelPid} = State) ->
    {noreply, do_detach(State)};
handle_cast({notify, Method, Params}, State) ->
    {noreply, emit(Method, Params, State)};
handle_cast({log, Severity, Level, Data}, #state{session = Session} = State) ->
    %% Deliver only when at or above the level the client set via setLevel.
    case Severity >= maps:get(log_min_severity, Session) of
        true ->
            Params = #{~"level" => atom_to_binary(Level), ~"data" => Data},
            {noreply, emit(~"notifications/message", Params, State)};
        false ->
            {noreply, State}
    end;
handle_cast(
    {start_streaming_tool, Name, Args, Id, Token, ConnPid},
    #state{session = Session, streams = Streams} = State
) ->
    %% Run the tool in a monitored worker so the session stays free to handle a
    %% cancel. The worker reads a snapshot of `Session`, relays progress + result
    %% to `ConnPid`, and threads no state back (only the buffered queue mutates
    %% session state). It signals completion so we can drop the tracking.
    SessionPid = self(),
    Ctx = #{token => Token, to => ConnPid},
    WorkerPid = spawn(fun() ->
        ok = arizona_mcp_handler:run_streaming_tool(Session, Name, Args, Id, Ctx, ConnPid),
        SessionPid ! {streaming_done, Id}
    end),
    MonRef = erlang:monitor(process, WorkerPid),
    Streams1 = Streams#{Id => {WorkerPid, MonRef, ConnPid}},
    {noreply, refresh_ttl(State#state{streams = Streams1})};
handle_cast({cancel, Id}, State) ->
    {noreply, cancel_request(Id, State)};
handle_cast(_Msg, State) ->
    {noreply, State}.

-spec handle_info(term(), state()) -> {noreply, state()} | {stop, normal, state()}.
handle_info({arizona_mcp_notification, Method, Params}, State) ->
    {noreply, emit(Method, Params, State)};
handle_info(
    {dispatch_done, WorkerPid, Outcome, Session1}, #state{dispatch_active = Active} = State
) ->
    %% The active buffered worker finished: reply to its caller, store the
    %% threaded-back session, and start the next queued request. A stale message
    %% (the worker was cancelled just before completion) is ignored.
    case Active of
        {WorkerPid, MonRef, From, _Id} ->
            true = erlang:demonitor(MonRef, [flush]),
            gen_server:reply(From, Outcome),
            State1 = State#state{session = Session1, dispatch_active = undefined},
            {noreply, refresh_ttl(start_next_dispatch(State1))};
        _ ->
            {noreply, State}
    end;
handle_info(
    {'DOWN', Mon, process, ChannelPid, _Reason},
    #state{channel_mon = Mon, channel = ChannelPid} = State
) ->
    %% The channel's connection died without a clean detach.
    {noreply, do_detach(State)};
handle_info(
    {'DOWN', Mon, process, _Pid, _Reason}, #state{dispatch_active = {_W, Mon, From, Id}} = State
) ->
    %% The active buffered worker died without reporting done (an untrappable
    %% kill -- safe_handle_method catches everything else). Free its caller.
    gen_server:reply(From, {error, arizona_jsonrpc:error(Id, -32603, ~"Internal error")}),
    {noreply, refresh_ttl(start_next_dispatch(State#state{dispatch_active = undefined}))};
handle_info({streaming_done, Id}, #state{streams = Streams} = State) ->
    %% The streaming worker finished (it already sent the result to the POST
    %% loop). Drop the tracking; its state snapshot is discarded. Ignored if the
    %% request was cancelled just before completion.
    case Streams of
        #{Id := {_WorkerPid, MonRef, _ConnPid}} ->
            true = erlang:demonitor(MonRef, [flush]),
            {noreply, refresh_ttl(State#state{streams = maps:remove(Id, Streams)})};
        _ ->
            {noreply, State}
    end;
handle_info({'DOWN', MonRef, process, _WorkerPid, _Reason}, #state{streams = Streams} = State) ->
    %% A streaming worker died without reporting done -- a crash. Answer the POST
    %% loop with -32603 and drop the tracking.
    case stream_by_mon(MonRef, Streams) of
        {Id, ConnPid} ->
            Error = arizona_jsonrpc:error(Id, -32603, ~"Internal error"),
            ConnPid ! {mcp_result, arizona_mcp_handler:message_frame(Error)},
            {noreply, refresh_ttl(State#state{streams = maps:remove(Id, Streams)})};
        error ->
            {noreply, State}
    end;
handle_info(mcp_keepalive, #state{channel = undefined} = State) ->
    %% The channel detached before this tick; nothing to keep alive.
    {noreply, State};
handle_info(mcp_keepalive, #state{channel = ChannelPid} = State) ->
    %% A comment line keeps idle proxies from closing the SSE stream. It carries
    %% no event id, so it is never buffered for resumption.
    ChannelPid ! {mcp_event, roadrunner_sse:comment(~"keepalive")},
    {noreply, arm_keepalive(State)};
handle_info(session_ttl_expired, State) ->
    {stop, normal, State};
handle_info(_Info, State) ->
    {noreply, State}.

-spec terminate(term(), state()) -> ok.
terminate(
    Reason,
    #state{
        id = SessionId,
        session = #{mod := Mod, state := HandlerState},
        streams = Streams,
        dispatch_active = Active
    }
) ->
    %% Kill any in-flight workers (streaming + the active buffered one) so none
    %% outlive the session (a blocking tool would otherwise run forever, orphaned).
    maps:foreach(fun(_Id, {WorkerPid, _MonRef, _ConnPid}) -> exit(WorkerPid, kill) end, Streams),
    kill_active_dispatch(Active),
    %% Run the app's optional cleanup hook, then drop the registry row. (pg
    %% auto-removes the session from its subscribed channels on exit.)
    erlang:function_exported(Mod, terminate, 2) andalso Mod:terminate(Reason, HandlerState),
    ok = arizona_mcp_session_registry:remove(SessionId),
    ok.

%% --------------------------------------------------------------------
%% Internal functions
%% --------------------------------------------------------------------

%% Guard a method against a crashing tool/resource/prompt: the session must
%% survive a bad request, answering with a JSON-RPC `-32603` rather than
%% dying (which would silently drop the whole session). A crash discards any
%% partial state, so the unchanged session is threaded back.
safe_handle_method(Method, Params, Id, Session) ->
    try
        arizona_mcp_handler:handle_method(Method, Params, Id, Session)
    catch
        Class:Reason:Stacktrace ->
            logger:error("MCP session dispatch crashed: ~ts:~tp~n~tp", [Class, Reason, Stacktrace]),
            {{error, arizona_jsonrpc:error(Id, -32603, ~"Internal error")}, Session}
    end.

%% Find the streaming request (its id and POST loop) whose worker monitor died.
stream_by_mon(MonRef, Streams) ->
    case [{Id, Conn} || {Id, {_W, Mon, Conn}} <:- maps:to_list(Streams), Mon =:= MonRef] of
        [{Id, Conn} | _] -> {Id, Conn};
        [] -> error
    end.

%% Methods that own a pubsub subscription keyed to the session process, so they
%% must run there rather than in a worker (they run no app code, so they don't
%% block). Everything else is offloaded.
session_coupled(~"resources/subscribe") -> true;
session_coupled(~"resources/unsubscribe") -> true;
session_coupled(_Method) -> false.

%% Spawn a monitored worker for the head of the dispatch queue if none is
%% active. The worker runs the method against the current session snapshot and
%% reports `{dispatch_done, ...}`; one-at-a-time keeps the state threading
%% serialized (the next starts only once this one completes or is cancelled).
start_next_dispatch(#state{dispatch_active = Active} = State) when Active =/= undefined ->
    State;
start_next_dispatch(#state{dispatch_q = Q, session = Session} = State) ->
    case queue:out(Q) of
        {empty, _} ->
            State;
        {{value, {From, Id, Method, Params}}, Q1} ->
            SessionPid = self(),
            WorkerPid = spawn(fun() ->
                {Outcome, Session1} = safe_handle_method(Method, Params, Id, Session),
                SessionPid ! {dispatch_done, self(), Outcome, Session1}
            end),
            MonRef = erlang:monitor(process, WorkerPid),
            State#state{dispatch_active = {WorkerPid, MonRef, From, Id}, dispatch_q = Q1}
    end.

%% Cancel by request id: a streaming worker, the active buffered worker, or a
%% still-queued buffered request -- whichever holds the id (at most one). A no-op
%% if none does.
cancel_request(Id, #state{streams = Streams} = State) ->
    case Streams of
        #{Id := {WorkerPid, MonRef, ConnPid}} ->
            true = erlang:demonitor(MonRef, [flush]),
            true = exit(WorkerPid, kill),
            %% Tell the POST loop to stop -- the spec says a cancelled request
            %% gets no response. Dropping the last stream re-arms the idle timer.
            ConnPid ! mcp_cancelled,
            refresh_ttl(State#state{streams = maps:remove(Id, Streams)});
        _ ->
            cancel_buffered(Id, State)
    end.

%% Cancel a buffered request: kill the active worker (replying a -32603 to its
%% caller so the held HTTP POST closes), or drop a still-queued one.
cancel_buffered(Id, #state{dispatch_active = {WorkerPid, MonRef, From, Id}} = State) ->
    true = erlang:demonitor(MonRef, [flush]),
    true = exit(WorkerPid, kill),
    gen_server:reply(From, dispatch_cancelled(Id)),
    refresh_ttl(start_next_dispatch(State#state{dispatch_active = undefined}));
cancel_buffered(Id, #state{dispatch_q = Q} = State) ->
    case drop_queued(Id, Q) of
        {From, Q1} ->
            gen_server:reply(From, dispatch_cancelled(Id)),
            refresh_ttl(State#state{dispatch_q = Q1});
        error ->
            State
    end.

%% Remove the queued request with the given id, returning its caller. Queues are
%% short, so a list round-trip is fine.
drop_queued(Id, Q) ->
    case lists:partition(fun({_From, QId, _M, _P}) -> QId =:= Id end, queue:to_list(Q)) of
        {[], _} -> error;
        {[{From, _, _, _} | _], Rest} -> {From, queue:from_list(Rest)}
    end.

dispatch_cancelled(Id) ->
    {error, arizona_jsonrpc:error(Id, -32603, ~"Request cancelled")}.

kill_active_dispatch({WorkerPid, _MonRef, _From, _Id}) ->
    true = exit(WorkerPid, kill),
    ok;
kill_active_dispatch(undefined) ->
    ok.

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
%%
%% `json:encode/1` rejects params a callback made unencodable (a binary that is
%% not valid UTF-8). This runs in the session process, so crashing would take the
%% session -- and every request in flight on it -- down over one notification.
%% A notification carries no id and so cannot be answered: log it and drop it,
%% leaving `next_event_id` untouched (no event reached the channel to replay).
emit(Method, Params, #state{next_event_id = EventId} = State) ->
    Notification = arizona_jsonrpc:notification(Method, Params),
    try iolist_to_binary(json:encode(Notification)) of
        Data ->
            Frame = roadrunner_sse:event(~"message", Data, integer_to_binary(EventId)),
            push_frame(Frame, State),
            buffer_event(EventId, Frame, State#state{next_event_id = EventId + 1})
    catch
        Class:Reason:Stacktrace ->
            logger:error(
                "MCP notification dropped, ~ts params are not encodable: ~ts:~tp~n~tp",
                [Method, Class, Reason, Stacktrace]
            ),
            State
    end.

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

channel_alive(#state{channel = undefined}) ->
    false;
channel_alive(#state{channel = Pid}) ->
    is_process_alive(Pid).

%% Forget the current channel, demonitoring it (and flushing any pending DOWN)
%% so a stale signal can't detach a freshly attached replacement.
clear_channel(#state{channel = undefined} = State) ->
    State;
clear_channel(#state{channel_mon = Mon} = State) ->
    demonitor_channel(Mon),
    %% No channel left to keep alive.
    cancel_keepalive(State#state{channel = undefined, channel_mon = undefined}).

%% Detach the channel: stop monitoring it, forget it, and re-arm the idle
%% timer (the session is connectionless again and may be abandoned).
do_detach(State) ->
    refresh_ttl(clear_channel(State)).

demonitor_channel(undefined) ->
    ok;
demonitor_channel(Mon) ->
    true = erlang:demonitor(Mon, [flush]),
    ok.

%% A session with a live SSE channel or an in-flight streaming request is kept
%% alive by it; an otherwise connectionless session counts down to teardown.
%% Cancel any pending timer, then arm a fresh one only when nothing holds it
%% (so a streaming tool is never idle-reaped mid-run).
refresh_ttl(#state{ttl_timer = Old, ttl_ms = TtlMs} = State) ->
    cancel_timer(Old),
    case held_open(State) of
        true ->
            State#state{ttl_timer = undefined};
        false ->
            Ref = erlang:send_after(TtlMs, self(), session_ttl_expired),
            State#state{ttl_timer = Ref}
    end.

%% An attached channel or any in-flight streaming worker holds the session open.
held_open(#state{channel = Channel, streams = Streams}) ->
    Channel =/= undefined orelse Streams =/= #{}.

%% Arm the periodic keep-alive timer, but only when enabled and a channel is
%% attached (there is nothing to keep alive otherwise).
arm_keepalive(#state{keepalive_ms = infinity} = State) ->
    State;
arm_keepalive(#state{channel = undefined} = State) ->
    State;
arm_keepalive(#state{keepalive_ms = Ms, keepalive_timer = Old} = State) ->
    cancel_timer(Old),
    Ref = erlang:send_after(Ms, self(), mcp_keepalive),
    State#state{keepalive_timer = Ref}.

cancel_keepalive(#state{keepalive_timer = Old} = State) ->
    cancel_timer(Old),
    State#state{keepalive_timer = undefined}.

cancel_timer(undefined) ->
    ok;
cancel_timer(Ref) ->
    _ = erlang:cancel_timer(Ref),
    ok.

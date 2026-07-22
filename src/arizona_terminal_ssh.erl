-module(arizona_terminal_ssh).
-moduledoc """
Serves `?terminal` live views over SSH -- a network transport for the terminal
render target.

This is the Erlang analog of an SSH-served TUI: `ssh -p <port> <user>@host` drops
the client into a live, server-rendered `?terminal` view. The server runs the live
process and pushes full ANSI frames; the SSH client is a real terminal that paints
them and sends keystrokes back. The SSH protocol also carries the terminal size
(`pty-req` + `window-change`), which a local serverless TTY can't read
(`io:rows/0` returns `enotsup`).

The module is both the **daemon launcher** (`start/1`) and the
`m:ssh_server_channel` callback installed via the daemon's `ssh_cli` option. Each
accepted session channel mounts a live view through `arizona_terminal_session`,
supplying an output function that writes to the SSH channel; key bytes, terminal
resizes, and the live process's `{arizona_push, _, _}` updates are fed to the
session.

```erlang
{ok, _Daemon} = arizona_terminal_ssh:start(#{
    port => 2222,
    handler => my_terminal_view,
    driver => my_terminal_driver,
    system_dir => "/path/to/host/keys",
    daemon_opts => [{auth_methods, "password"}, {pwdfun, fun(_, _) -> true end}]
}).
```

Host keys (`system_dir`) and authentication (`daemon_opts`) are the caller's
responsibility: a demo passes a throwaway generated host key and accept-all auth;
production passes real keys and real auth.
""".
-behaviour(ssh_server_channel).

%% --------------------------------------------------------------------
%% API function exports
%% --------------------------------------------------------------------

-export([start/1]).
-export([daemon_opts/1]).

%% --------------------------------------------------------------------
%% ssh_server_channel callback exports
%% --------------------------------------------------------------------

-export([init/1]).
-export([handle_msg/2]).
-export([handle_ssh_msg/2]).
-export([terminate/2]).

-ignore_xref([start/1]).
-ignore_xref([daemon_opts/1]).

%% init/1 is the ssh_server_channel callback and must return {ok, State}; elvis's
%% consistent_ok_error_spec doesn't know this behaviour, so it misreads the
%% required wrap as unnecessary.
-elvis([{elvis_style, consistent_ok_error_spec, disable}]).

%% --------------------------------------------------------------------
%% Types
%% --------------------------------------------------------------------

-record(state, {
    handler :: module(),
    driver :: module(),
    conn :: ssh:connection_ref() | undefined,
    channel :: ssh:channel_id() | undefined,
    rows :: pos_integer(),
    cols :: pos_integer(),
    session :: arizona_terminal_session:session() | undefined
}).

-opaque state() :: #state{}.
-export_type([state/0]).

-type opts() :: #{
    port := inet:port_number(),
    handler := module(),
    driver := module(),
    system_dir := file:filename(),
    daemon_opts => [term()]
}.
-export_type([opts/0]).

%% Pty geometry until a pty-req or window-change supplies the real size.
-define(DEFAULT_ROWS, 24).
-define(DEFAULT_COLS, 80).

%% Bound the concurrent session channels a single daemon accepts, so a client
%% cannot mount unlimited live views (each is a process + pubsub subscription).
%% A caller's own `max_sessions` in `daemon_opts` overrides this default.
-define(DEFAULT_MAX_SESSIONS, 100).

%% Finite per-frame send timeout. With the ssh default of `infinity`, a client that
%% stops extending its flow-control window blocks the channel forever while pushes
%% accumulate (unbounded memory). A finite timeout bounds each send instead.
-define(SEND_TIMEOUT, 5000).

%% --------------------------------------------------------------------
%% API Functions
%% --------------------------------------------------------------------

-doc """
Starts an SSH daemon that serves the given `?terminal` `handler` as its
interactive shell. Returns the daemon reference (stop it with
`ssh:stop_daemon/1`). `system_dir` must hold a host key; `daemon_opts` are extra
`m:ssh` daemon options (typically authentication).
""".
-spec start(opts()) -> {ok, ssh:daemon_ref()} | {error, term()}.
start(#{port := Port} = Opts) ->
    {ok, _Started} = application:ensure_all_started(ssh),
    ssh:daemon(Port, daemon_opts(Opts)).

-doc """
Builds the `m:ssh` daemon options for `start/1`: the framework's `ssh_cli` (this
module as the shell) and `system_dir`, a bounded `max_sessions` default (so a client
cannot mount unlimited live views), and the caller's `daemon_opts` (typically
authentication). A caller's own `max_sessions` overrides the default.
""".
-spec daemon_opts(opts()) -> [term()].
daemon_opts(#{handler := Handler, driver := Driver, system_dir := SystemDir} = Opts) ->
    CallerOpts = maps:get(daemon_opts, Opts, []),
    [
        {ssh_cli, {?MODULE, [#{handler => Handler, driver => Driver}]}},
        {system_dir, SystemDir}
        | max_sessions_default(CallerOpts) ++ CallerOpts
    ].

%% Prepend the max_sessions cap unless the caller already set one (their value then
%% wins). Prepending a default the caller can override keeps the cap opt-out.
max_sessions_default(CallerOpts) ->
    case proplists:is_defined(max_sessions, CallerOpts) of
        true -> [];
        false -> [{max_sessions, ?DEFAULT_MAX_SESSIONS}]
    end.

%% --------------------------------------------------------------------
%% ssh_server_channel Callbacks
%% --------------------------------------------------------------------

-spec init([#{handler := module(), driver := module()}]) -> {ok, state()}.
init([#{handler := Handler, driver := Driver}]) ->
    {ok, #state{handler = Handler, driver = Driver, rows = ?DEFAULT_ROWS, cols = ?DEFAULT_COLS}}.

-doc """
Handles non-SSH messages: the one-shot `ssh_channel_up` (records the connection
and channel), the live process's `{arizona_push, _, Effects}` updates (timer ticks,
pubsub) which the session turns into a repaint or a stop, and the linked live
view's `{'EXIT', _, _}` (ssh channels trap exits, so a crash surfaces as a message
rather than killing the channel) which restores the client's terminal and stops
the channel. Any other message is ignored, mirroring the local driver's selective
receive.
""".
-spec handle_msg(term(), state()) -> {ok, state()} | {stop, ssh:channel_id(), state()}.
handle_msg({ssh_channel_up, ChannelId, ConnectionRef}, State) ->
    {ok, State#state{conn = ConnectionRef, channel = ChannelId}};
handle_msg({arizona_push, _Ops, Effects}, #state{session = Session, channel = ChannelId} = State) ->
    apply_session_result(arizona_terminal_session:handle_push(Session, Effects), ChannelId, State);
handle_msg(
    {'EXIT', _Pid, _Reason}, #state{session = Session, channel = ChannelId} = State
) when Session =/= undefined ->
    %% The linked live view died abnormally. Without this the exit is swallowed
    %% and the client is left with a frozen screen (no teardown) until the next
    %% keystroke crashes with noproc. Restore the terminal via the driver teardown,
    %% then stop the channel.
    ok = arizona_terminal_session:teardown(Session),
    {stop, ChannelId, State};
handle_msg(_Msg, State) ->
    {ok, State}.

-doc """
Handles SSH connection-protocol events: `pty-req` stashes the terminal size,
`shell` mounts the live view, `data` feeds key bytes to the session,
`window-change` resizes it, and `closed` stops the channel. Other events (env,
exec, eof, signal) are ignored -- the TUI only needs pty + shell + data.
""".
-spec handle_ssh_msg(ssh_connection:event(), state()) ->
    {ok, state()} | {stop, ssh:channel_id(), state()}.
handle_ssh_msg(
    {ssh_cm, Conn, {pty, ChannelId, WantReply, {_Term, Cols, Rows, _PixW, _PixH, _Modes}}}, State
) ->
    ok = reply(Conn, ChannelId, WantReply),
    {ok, State#state{rows = Rows, cols = Cols}};
handle_ssh_msg({ssh_cm, Conn, {shell, ChannelId, WantReply}}, State) ->
    ok = reply(Conn, ChannelId, WantReply),
    {ok, start_session(Conn, ChannelId, State)};
handle_ssh_msg({ssh_cm, _Conn, {data, ChannelId, 0, Data}}, #state{session = Session} = State) ->
    apply_session_result(arizona_terminal_session:handle_key(Session, Data), ChannelId, State);
handle_ssh_msg(
    {ssh_cm, _Conn, {window_change, _ChannelId, Cols, Rows, _PixW, _PixH}},
    #state{session = Session} = State
) ->
    ok = arizona_terminal_session:resize(Session, Rows, Cols),
    {ok, State#state{rows = Rows, cols = Cols}};
handle_ssh_msg({ssh_cm, _Conn, {closed, ChannelId}}, State) ->
    {stop, ChannelId, State};
handle_ssh_msg({ssh_cm, _Conn, _Msg}, State) ->
    {ok, State}.

-spec terminate(term(), state()) -> ok.
terminate(_Reason, #state{session = Session}) when Session =/= undefined ->
    %% Stop the live view so it doesn't outlive the channel and linger as a
    %% pubsub subscriber -- the start_link link doesn't fire on a normal stop.
    %% Tolerate a session whose live view already died (the {'EXIT', _, _} path),
    %% where arizona_live:stop would otherwise raise noproc.
    stop_session(Session);
terminate(_Reason, _State) ->
    ok.

%% --------------------------------------------------------------------
%% Internal functions
%% --------------------------------------------------------------------

%% Turn a session step (`handle_key`/`handle_push`) into a channel callback reply:
%% a `quit` stops the channel (the session already wrote the driver's teardown --
%% e.g. the cursor restore -- via the output fun before returning quit, while the
%% channel was still open), a `{cont, _}` stores the advanced session.
apply_session_result(quit, ChannelId, State) ->
    {stop, ChannelId, State};
apply_session_result({cont, Session}, _ChannelId, State) ->
    {ok, State#state{session = Session}}.

%% Mount the live view with the channel as its transport. The output function
%% writes frames to the SSH channel; the driver owns cursor/screen setup+teardown.
start_session(
    Conn, ChannelId, #state{handler = Handler, driver = Driver, rows = Rows, cols = Cols} = State
) ->
    Out = fun(Iodata) -> send(Conn, ChannelId, Iodata) end,
    Bindings = #{term_rows => Rows, term_cols => Cols},
    {ok, Session} = arizona_terminal_session:start(Handler, Bindings, Driver, [], Out),
    State#state{session = Session}.

%% Stop the live view, tolerating one already gone (the {'EXIT', _, _} teardown
%% path is where the live process is what died); arizona_live:stop would otherwise
%% raise noproc.
stop_session(Session) ->
    try
        arizona_terminal_session:stop(Session)
    catch
        exit:noproc -> ok
    end.

%% Write to the SSH channel under a finite timeout so a stalled client (one that
%% stops extending its flow-control window) cannot block the channel forever.
%% Type 0 is normal channel data. A send error (a closing channel, or a timeout on
%% a stalled peer) is swallowed -- the matching `closed` event stops the session.
send(Conn, ChannelId, Iodata) ->
    case ssh_connection:send(Conn, ChannelId, 0, Iodata, ?SEND_TIMEOUT) of
        ok -> ok;
        {error, _Reason} -> ok
    end.

reply(_Conn, _ChannelId, false) ->
    ok;
reply(Conn, ChannelId, true) ->
    ssh_connection:reply_request(Conn, true, success, ChannelId).

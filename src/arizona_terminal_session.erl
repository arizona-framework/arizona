-module(arizona_terminal_session).
-moduledoc """
Transport-agnostic orchestrator for `?terminal` live views.

Mounts a `?terminal` view (the calling process is its transport), then delegates
the terminal *policy* -- key mapping, paint model, screen setup/teardown -- to an
`m:arizona_terminal_driver` module. It holds no UX opinion of its own, so it serves
the local TTY driver (`arizona_terminal_tty`) and the SSH transport (`arizona_terminal_ssh`)
unchanged, with whatever terminal style the driver implements. Each callback
dispatches to the given driver if it exports it, otherwise to
`m:arizona_terminal_default_driver`'s default -- so a driver overrides only what it
changes.

It is parameterized by a driver module and an output function `t:out/0`
(`fun((iodata()) -> ok)`). The owning process becomes the live view's transport: it
must forward each `{arizona_push, _, _, Effects, _}` it receives to `handle_push/2`, key
reads to `handle_key/2`, terminal resizes to `resize/3`, and `{arizona_reloader, reload}`
to `handle_reload/1,2`.

In dev mode (`arizona_reloader:enabled/0`) the owning process joins the reloader topic,
and `handle_reload/1,2` remounts the view on fresh code in place: a fun held in the view's
state points into code a second reload purges, and a terminal client has no reconnect to
fall back on.
""".

-export([start/5]).
-export([handle_key/2]).
-export([handle_push/2]).
-export([handle_reload/1]).
-export([handle_reload/2]).
-export([resize/3]).
-export([stop/1]).
-export([teardown/1]).
-export([pid/1]).

-ignore_xref([resize/3]).
-ignore_xref([teardown/1]).
-ignore_xref([pid/1]).

-record(session, {
    pid :: pid(),
    view_id :: binary(),
    handler :: module(),
    bindings :: arizona_template:bindings(),
    %% Joined the reloader topic at start, so stop/1 leaves it: a local TTY's
    %% owner is the caller's process, which outlives the session.
    reloader :: boolean(),
    driver :: module(),
    dstate :: term(),
    out :: out()
}).

-opaque session() :: #session{}.
-type out() :: fun((iodata()) -> ok).

-export_type([session/0]).
-export_type([out/0]).

%% --------------------------------------------------------------------
%% API Functions
%% --------------------------------------------------------------------

-doc """
Mounts `Handler` (request-free `mount/1`) as a live view whose transport is the
calling process, creates the `Driver` state from `DriverArg`, and paints the
initial frame via `Out` (after the driver's `setup`). `Bindings` carries any
connection context (e.g. `term_rows`/`term_cols`).

The initial paint decides `continue`/`stop` like every other paint: a driver that
stops on the very first frame gets `quit` instead of a session, having already
emitted its teardown. Since the caller never receives a session it could stop,
`start/5` also stops the live process itself on that path.
""".
-spec start(Handler, Bindings, Driver, DriverArg, Out) -> {ok, session()} | quit when
    Handler :: module(),
    Bindings :: arizona_template:bindings(),
    Driver :: module(),
    DriverArg :: term(),
    Out :: out().
start(Handler, Bindings, Driver, DriverArg, Out) ->
    %% Load the driver once so the function_exported/3 fallbacks below are accurate.
    {module, Driver} = code:ensure_loaded(Driver),
    %% Joined before the mount, so a reload landing during it still remounts.
    Reloader = arizona_reloader:enabled(),
    ok = join_reloader(Reloader),
    {Pid, ViewId} = mount(Handler, Bindings),
    DState0 = call_init(Driver, DriverArg),
    {Setup, DState1} = call_setup(Driver, DState0),
    {ok, Frame, _} = arizona_live:render_current(Pid),
    {Paint, Next, DState2} = call_paint(Driver, Frame, [], DState1),
    ok = Out([Setup, Paint]),
    Session = #session{
        pid = Pid,
        view_id = ViewId,
        handler = Handler,
        bindings = Bindings,
        reloader = Reloader,
        driver = Driver,
        dstate = DState2,
        out = Out
    },
    case Next of
        continue ->
            {ok, Session};
        stop ->
            ok = do_teardown(Session),
            ok = stop(Session),
            quit
    end.

-doc """
Handles a raw key read (a character list from a local TTY, or a byte binary from a
network transport). The driver maps it to commands: a `stop` command tears down and
returns `quit`; `event` commands are dispatched to the view and the result is
repainted. A read that maps to nothing returns `{cont, Session}` without repainting.
""".
-spec handle_key(session(), string() | binary()) -> {cont, session()} | quit.
handle_key(#session{driver = Driver, dstate = DState} = Session, Input) ->
    {Commands, DState1} = call_keys(Driver, to_binary(Input), DState),
    Session1 = Session#session{dstate = DState1},
    case lists:member(stop, Commands) of
        true ->
            do_teardown(Session1),
            quit;
        false ->
            case [Command || {event, _, _} = Command <- Commands] of
                [] ->
                    {cont, Session1};
                Events ->
                    repaint(Session1, dispatch_events(Session1, Events))
            end
    end.

-doc """
Handles a live-process push (the `Effects` from an `{arizona_push, _, _, Effects, _}` the
owning process received): repaints via the driver, returning `quit` if the driver
stops.
""".
-spec handle_push(session(), [arizona_effect:cmd()]) -> {cont, session()} | quit.
handle_push(Session, Effects) ->
    repaint(Session, Effects).

-doc """
Handles a dev reload (`{arizona_reloader, reload}`): remounts the view with the bindings
it was mounted with. See `handle_reload/2`.
""".
-spec handle_reload(session()) -> {cont, session()} | quit.
handle_reload(#session{bindings = Bindings} = Session) ->
    handle_reload(Session, Bindings).

-doc """
Handles a dev reload (`{arizona_reloader, reload}`): stops the live view and mounts a
fresh one from `Bindings` (a transport passes its current connection context, e.g. the
latest `term_rows`/`term_cols`), then repaints it. The view's state resets, as a browser
page's does on reload. Further reloads already queued are folded into this one, and so
are pushes the old view queued, which describe a view that no longer exists. Returns
`quit` if the driver stops on the repaint. A session that did not join the reloader
topic (the reloader was not enabled when it started) ignores it.
""".
-spec handle_reload(session(), arizona_template:bindings()) -> {cont, session()} | quit.
handle_reload(#session{reloader = false} = Session, _Bindings) ->
    {cont, Session};
handle_reload(#session{pid = OldPid, handler = Handler} = Session, Bindings) ->
    %% Unlinked first: transports trap exits and end the session on any 'EXIT'.
    true = unlink(OldPid),
    ok = stop_live(OldPid),
    ok = flush_reload(OldPid),
    {Pid, ViewId} = mount(Handler, Bindings),
    repaint(Session#session{pid = Pid, view_id = ViewId, bindings = Bindings}, []).

-doc """
Notifies the view of a terminal resize. Sends `{term_resize, Rows, Cols}` to the
view's `handle_info/2` (which updates bindings and re-renders); the resulting push
repaints. Sent straight to the live pid (not `arizona_live:send/2`, which targets
the live process's *own* mailbox).
""".
-spec resize(session(), pos_integer(), pos_integer()) -> ok.
resize(#session{pid = Pid, view_id = ViewId}, Rows, Cols) ->
    Pid ! {arizona_view, ViewId, {term_resize, Rows, Cols}},
    ok.

-doc """
Stops the live process backing the session. A transport calls this when its
connection ends, so the view doesn't outlive the transport (and unsubscribes from
any pubsub channels). Teardown output is emitted on the quit path, not here -- on a
disconnect the client is gone, so there's nothing to write to.
""".
-spec stop(session()) -> ok.
stop(#session{pid = Pid, reloader = Reloader}) ->
    ok = leave_reloader(Reloader),
    arizona_live:stop(Pid).

-doc """
Emits the driver's teardown output (e.g. restoring the cursor) via the session's
output function, without touching the live process. A transport calls this to
restore the client's terminal when the live view has died asynchronously -- the
quit path already tears down before returning `quit`, but an out-of-band live-view
crash never reaches that path.
""".
-spec teardown(session()) -> ok.
teardown(Session) ->
    do_teardown(Session).

-doc "The live view process backing the session.".
-spec pid(session()) -> pid().
pid(#session{pid = Pid}) ->
    Pid.

%% --------------------------------------------------------------------
%% Internal functions
%% --------------------------------------------------------------------

mount(Handler, Bindings) ->
    {ok, Pid} = arizona_live:start_link(Handler, Bindings, self(), []),
    {ok, ViewId, _} = arizona_live:mount(Pid),
    {Pid, ViewId}.

join_reloader(true) -> arizona_reloader:join(self());
join_reloader(false) -> ok.

leave_reloader(true) -> arizona_reloader:leave(self());
leave_reloader(false) -> ok.

%% A view that already died (its 'EXIT' then sits in the mailbox) is not an error.
stop_live(Pid) ->
    try
        arizona_live:stop(Pid)
    catch
        exit:noproc -> ok
    end.

flush_reload(OldPid) ->
    receive
        {arizona_reloader, reload} -> flush_reload(OldPid);
        {'EXIT', OldPid, _Reason} -> flush_reload(OldPid);
        {arizona_push, _ViewId, _Ops, _Effects, _Observed} -> flush_reload(OldPid)
    after 0 -> ok
    end.

%% Normalize a raw key read to a byte binary. A network transport delivers a byte
%% binary already in the wire encoding, so pass it through untouched (the driver's
%% decoder handles any non-UTF-8 byte). A local TTY in unicode mode delivers a list
%% of codepoints (possibly > 255, e.g. "€" is 8364), which iolist_to_binary rejects
%% with badarg -- encode those to UTF-8 so a non-Latin-1 keystroke can't crash the
%% session.
to_binary(Input) when is_binary(Input) ->
    Input;
to_binary(Input) when is_list(Input) ->
    unicode:characters_to_binary(Input).

dispatch_events(#session{pid = Pid, view_id = ViewId}, Events) ->
    lists:flatmap(
        fun({event, Name, Payload}) ->
            {ok, _Ops, Effects, _Observed} = arizona_live:handle_event(Pid, ViewId, Name, Payload),
            Effects
        end,
        Events
    ).

repaint(#session{pid = Pid, driver = Driver, dstate = DState, out = Out} = Session, Effects) ->
    {ok, Frame, _Observed} = arizona_live:render_current(Pid),
    {Output, Next, DState1} = call_paint(Driver, Frame, Effects, DState),
    ok = Out(Output),
    Session1 = Session#session{dstate = DState1},
    case Next of
        stop ->
            do_teardown(Session1),
            quit;
        continue ->
            {cont, Session1}
    end.

do_teardown(#session{driver = Driver, dstate = DState, out = Out}) ->
    ok = Out(call_teardown(Driver, DState)).

%% Dispatch each driver callback to the driver if it implements it, else to
%% arizona_terminal_default_driver's default. The driver is loaded once in start/5,
%% so function_exported/3 is accurate here.
call_init(Driver, Arg) ->
    case erlang:function_exported(Driver, init, 1) of
        true -> Driver:init(Arg);
        false -> arizona_terminal_default_driver:init(Arg)
    end.

call_keys(Driver, Bytes, DState) ->
    case erlang:function_exported(Driver, keys, 2) of
        true -> Driver:keys(Bytes, DState);
        false -> arizona_terminal_default_driver:keys(Bytes, DState)
    end.

call_paint(Driver, Frame, Effects, DState) ->
    case erlang:function_exported(Driver, paint, 3) of
        true -> Driver:paint(Frame, Effects, DState);
        false -> arizona_terminal_default_driver:paint(Frame, Effects, DState)
    end.

call_setup(Driver, DState) ->
    case erlang:function_exported(Driver, setup, 1) of
        true -> Driver:setup(DState);
        false -> arizona_terminal_default_driver:setup(DState)
    end.

call_teardown(Driver, DState) ->
    case erlang:function_exported(Driver, teardown, 1) of
        true -> Driver:teardown(DState);
        false -> arizona_terminal_default_driver:teardown(DState)
    end.

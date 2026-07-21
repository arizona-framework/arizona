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
must forward each `{arizona_push, _, Effects}` it receives to `handle_push/2`, key
reads to `handle_key/2`, and terminal resizes to `resize/3`.
""".

-export([start/5]).
-export([handle_key/2]).
-export([handle_push/2]).
-export([resize/3]).
-export([stop/1]).
-export([pid/1]).

-ignore_xref([resize/3]).
-ignore_xref([pid/1]).

-record(session, {
    pid :: pid(),
    view_id :: binary(),
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
""".
-spec start(Handler, Bindings, Driver, DriverArg, Out) -> {ok, session()} when
    Handler :: module(),
    Bindings :: arizona_template:bindings(),
    Driver :: module(),
    DriverArg :: term(),
    Out :: out().
start(Handler, Bindings, Driver, DriverArg, Out) ->
    %% Load the driver once so the function_exported/3 fallbacks below are accurate.
    {module, Driver} = code:ensure_loaded(Driver),
    {ok, Pid} = arizona_live:start_link(Handler, Bindings, self(), []),
    {ok, ViewId} = arizona_live:mount(Pid),
    DState0 = call_init(Driver, DriverArg),
    {Setup, DState1} = call_setup(Driver, DState0),
    {ok, Frame} = arizona_live:render_current(Pid),
    {Paint, _Next, DState2} = call_paint(Driver, Frame, [], DState1),
    ok = Out([Setup, Paint]),
    {ok, #session{pid = Pid, view_id = ViewId, driver = Driver, dstate = DState2, out = Out}}.

-doc """
Handles a raw key read (a character list from a local TTY, or a byte binary from a
network transport). The driver maps it to commands: a `stop` command tears down and
returns `quit`; `event` commands are dispatched to the view and the result is
repainted. A read that maps to nothing returns `{cont, Session}` without repainting.
""".
-spec handle_key(session(), string() | binary()) -> {cont, session()} | quit.
handle_key(#session{driver = Driver, dstate = DState} = Session, Input) ->
    {Commands, DState1} = call_keys(Driver, iolist_to_binary(Input), DState),
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
Handles a live-process push (the `Effects` from an `{arizona_push, _, Effects}` the
owning process received): repaints via the driver, returning `quit` if the driver
stops.
""".
-spec handle_push(session(), [arizona_effect:cmd()]) -> {cont, session()} | quit.
handle_push(Session, Effects) ->
    repaint(Session, Effects).

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
stop(#session{pid = Pid}) ->
    arizona_live:stop(Pid).

-doc "The live view process backing the session.".
-spec pid(session()) -> pid().
pid(#session{pid = Pid}) ->
    Pid.

%% --------------------------------------------------------------------
%% Internal functions
%% --------------------------------------------------------------------

dispatch_events(#session{pid = Pid, view_id = ViewId}, Events) ->
    lists:flatmap(
        fun({event, Name, Payload}) ->
            {ok, _Ops, Effects} = arizona_live:handle_event(Pid, ViewId, Name, Payload),
            Effects
        end,
        Events
    ).

repaint(#session{pid = Pid, driver = Driver, dstate = DState, out = Out} = Session, Effects) ->
    {ok, Frame} = arizona_live:render_current(Pid),
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

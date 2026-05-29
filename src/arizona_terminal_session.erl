-module(arizona_terminal_session).
-moduledoc """
Transport-agnostic driver for `?terminal` live views.

Mounts a `?terminal` view, renders it as a **scrolling log above a pinned status
block**, turns key input into `~"key"` events, and interprets `arizona_tty`
effects (`log` / `quit`). It is parameterized by an output function
`t:out/0` (`fun((iodata()) -> ok)`), so it serves both the local TTY driver
(`arizona_terminal_app`, `Out = fun io:put_chars/1`) and a network transport
(`arizona_ssh`, `Out` writes to the SSH channel).

The process that calls `start/3` becomes the live view's transport: the live
process sends it `{arizona_push, _, Effects}` for async updates (timer ticks,
pubsub), which the owner must forward to `handle_push/2`. Key input is fed to
`handle_key/2` and terminal resizes to `resize/3`.
""".

-export([start/3]).
-export([handle_key/2]).
-export([handle_push/2]).
-export([resize/3]).
-export([normalize_key/1]).
-export([to_crlf/1]).
-export([count_lines/1]).
-export([log_lines/1]).
-export([has_quit/1]).

-ignore_xref([resize/3]).
-ignore_xref([normalize_key/1]).
-ignore_xref([to_crlf/1]).
-ignore_xref([count_lines/1]).
-ignore_xref([log_lines/1]).
-ignore_xref([has_quit/1]).

-record(session, {
    pid :: pid(),
    view_id :: binary(),
    lines :: non_neg_integer(),
    out :: out()
}).

-opaque session() :: #session{}.
-type out() :: fun((iodata()) -> ok).

-export_type([session/0]).
-export_type([out/0]).

-doc """
Mounts `Handler` (request-free `mount/1`) as a live view whose transport is the
calling process, paints the initial frame via `Out`, and returns the session.
`Bindings` carries any connection context (e.g. `term_rows`/`term_cols`).
""".
-spec start(Handler, Bindings, Out) -> {ok, session()} when
    Handler :: module(),
    Bindings :: arizona_template:bindings(),
    Out :: out().
start(Handler, Bindings, Out) ->
    {ok, Pid} = arizona_live:start_link(Handler, Bindings, self(), [], undefined),
    {ok, ViewId} = arizona_live:mount(Pid),
    {ok, Status} = arizona_live:render_current(Pid),
    ok = Out(to_crlf(Status)),
    {ok, #session{pid = Pid, view_id = ViewId, lines = count_lines(Status), out = Out}}.

-doc """
Handles a raw key read -- a character list (local TTY) or a byte binary (a
network transport's `{data, ...}`). Returns `quit` when the key (`q`, Ctrl-C/D)
or a resulting `arizona_tty:quit/0` effect asks to stop, otherwise `{cont,
Session}` after dispatching a `~"key"` event and repainting.
""".
-spec handle_key(session(), string() | binary()) -> {cont, session()} | quit.
handle_key(#session{pid = Pid, view_id = ViewId} = Session, Input) ->
    case normalize_key(to_chars(Input)) of
        quit ->
            quit;
        ignore ->
            {cont, Session};
        Key ->
            {ok, _Ops, Effects} = arizona_live:handle_event(
                Pid, ViewId, ~"key", #{~"key" => Key}
            ),
            apply_effects(Session, Effects)
    end.

-doc """
Handles a live-process push (the `Effects` from an `{arizona_push, _, Effects}`
the owning process received): `quit` on an `arizona_tty:quit/0` effect, else
streams any `log` lines above the status block and repaints.
""".
-spec handle_push(session(), [arizona_effect:cmd()]) -> {cont, session()} | quit.
handle_push(Session, Effects) ->
    apply_effects(Session, Effects).

-doc """
Notifies the view of a terminal resize. Sends `{term_resize, Rows, Cols}` to the
view's `handle_info/2` (which updates bindings and re-renders); the resulting
push repaints. Sends straight to the live pid (not `arizona_live:send/2`, which
targets the live process's *own* mailbox).
""".
-spec resize(session(), pos_integer(), pos_integer()) -> ok.
resize(#session{pid = Pid, view_id = ViewId}, Rows, Cols) ->
    Pid ! {arizona_view, ViewId, {term_resize, Rows, Cols}},
    ok.

%% --------------------------------------------------------------------
%% Internal functions
%% --------------------------------------------------------------------

%% Local TTY reads arrive as a character list, network transports as a byte
%% binary; normalize to the character list the key clauses match on.
to_chars(Input) when is_binary(Input) -> binary_to_list(Input);
to_chars(Input) when is_list(Input) -> Input.

apply_effects(Session, Effects) ->
    case has_quit(Effects) of
        true -> quit;
        false -> {cont, redraw(Session, log_lines(Effects))}
    end.

%% Erase the previous status block, emit any new log lines (they stay above and
%% scroll into history), then reprint the status block at the bottom via `Out`.
redraw(#session{pid = Pid, lines = PrevLines, out = Out} = Session, LogLines) ->
    {ok, Status} = arizona_live:render_current(Pid),
    Logs = [to_crlf(<<Line/binary, "\n">>) || Line <- LogLines],
    ok = Out([erase_block(PrevLines), Logs, to_crlf(Status)]),
    Session#session{lines = count_lines(Status)}.

erase_block(0) ->
    [];
erase_block(Lines) ->
    %% Move to the top of the previous status block and clear from there down.
    [io_ansi:cursor_up(Lines), io_ansi:erase_display()].

-doc """
Number of terminal rows a rendered status block occupies: one per `\\n` (each
`?terminal` line ends in a newline). Used to move the cursor back to the top of
the block before redrawing it.
""".
-spec count_lines(binary()) -> non_neg_integer().
count_lines(Frame) ->
    length(binary:matches(Frame, ~"\n")).

-doc """
Extracts the lines carried by `arizona_tty:log/1` effects, dropping any other
effects.
""".
-spec log_lines([arizona_effect:cmd()]) -> [binary()].
log_lines(Effects) ->
    [Line || {arizona_effect, [log, Line]} <- Effects].

-doc """
Whether the effects include an `arizona_tty:quit/0` request, signalling the
runtime to stop.
""".
-spec has_quit([arizona_effect:cmd()]) -> boolean().
has_quit(Effects) ->
    lists:member({arizona_effect, [quit]}, Effects).

-doc """
Translates bare LF line breaks to CRLF. In raw mode the terminal does not map
`\\n` to `\\r\\n`, so without this each line would start where the previous one
ended (a staircase).
""".
-spec to_crlf(binary()) -> binary().
to_crlf(Frame) ->
    binary:replace(Frame, ~"\n", ~"\r\n", [global]).

-doc """
Maps a raw key read into a quit request, a `~"key"` event payload value, or
`ignore` for sequences the loop should drop without repainting.
""".
-spec normalize_key(string()) -> binary() | quit | ignore.
normalize_key("q") -> quit;
normalize_key([3]) -> quit;
normalize_key([4]) -> quit;
normalize_key("\e[A") -> ~"up";
normalize_key("\e[B") -> ~"down";
normalize_key("\e[C") -> ~"right";
normalize_key("\e[D") -> ~"left";
normalize_key("\r") -> ~"enter";
normalize_key("\n") -> ~"enter";
normalize_key([Char]) when Char >= 32, Char < 127 -> <<Char>>;
normalize_key(_Other) -> ignore.

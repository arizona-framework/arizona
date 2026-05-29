-module(arizona_terminal_app).
-moduledoc """
Headless terminal runtime for `?terminal` views -- no HTTP server.

Renders a view into a real terminal as a **scrolling log above a pinned status
block**. The view's `render/1` is the status block (menu, counters, footer); the
runtime prints it at the bottom and redraws it in place (cursor up + erase) on
every update. Append-only output -- `arizona_tty:log/1` effects from the view --
is printed *above* the status block, so it scrolls into the terminal's native
scrollback (the user can scroll back through it). There is no alternate screen,
so the session stays in the shell after quit.

A linked **input reader** process blocks on `io:get_chars/2` and forwards each
keypress as `{term_input, Key}`, so a blocking read never starves the live
process's asynchronous `{arizona_push, _, _}` updates (timer ticks, pubsub
broadcasts). Keys become ordinary Arizona events: a `~"key"` event with the
normalized key in the payload. `q` / Ctrl-C / Ctrl-D quit, restoring the cursor.

```erlang
arizona_terminal_app:start(my_terminal_view, #{}).
```
""".

-export([start/1]).
-export([start/2]).
-export([normalize_key/1]).
-export([to_crlf/1]).
-export([count_lines/1]).
-export([log_lines/1]).
-export([has_quit/1]).

-ignore_xref([start/1]).
-ignore_xref([start/2]).
-ignore_xref([normalize_key/1]).
-ignore_xref([to_crlf/1]).
-ignore_xref([count_lines/1]).
-ignore_xref([log_lines/1]).
-ignore_xref([has_quit/1]).

%% Terminal IO and a blocking event loop are this module's whole job, not stray
%% debug leftovers: it writes frames with io:put_chars / io:format, and its loop
%% waits on receive with no timeout until a key or a push arrives.
-elvis([
    {elvis_style, no_debug_call, disable},
    {elvis_style, no_receive_without_timeout, disable}
]).

%% shell:start_interactive/1's success typing omits {error, enotsup} (returned
%% with no controlling TTY), so dialyzer thinks start/2's bail clause is dead.
%% It is reachable at runtime, so keep it and silence the false no_match.
-dialyzer({no_match, [start/2]}).

-doc "Equivalent to `start(Handler, #{})`.".
-spec start(module()) -> ok.
start(Handler) ->
    start(Handler, #{}).

-doc """
Mounts `Handler` and runs the terminal event loop until the user quits,
restoring the cursor on exit.
""".
-spec start(module(), arizona_template:bindings()) -> ok.
start(Handler, Bindings) ->
    case enter_raw_mode() of
        ok ->
            run_terminal(Handler, Bindings);
        {error, already_started} ->
            run_terminal(Handler, Bindings);
        {error, Reason} ->
            %% No controlling TTY (e.g. output is piped) -- raw mode is required.
            io:format(
                "arizona terminal: cannot enter raw mode (~p); run in a real terminal~n",
                [Reason]
            ),
            ok
    end.

%% --------------------------------------------------------------------
%% Internal functions
%% --------------------------------------------------------------------

%% shell:start_interactive/1's success typing is `ok | {error, already_started}`,
%% but without a controlling TTY it actually returns `{error, enotsup}`. Wrap it
%% with an honest wider spec so the graceful-bail clause in start/2 is reachable.
-spec enter_raw_mode() -> ok | {error, term()}.
enter_raw_mode() ->
    shell:start_interactive({noshell, raw}).

run_terminal(Handler, Bindings) ->
    ok = io:put_chars(io_ansi:cursor_hide()),
    try
        run(Handler, Bindings)
    after
        %% No alternate screen: leave the log + final status block in the shell;
        %% just restore the cursor and move below the block for the next prompt.
        io:put_chars([io_ansi:cursor_show(), ~"\r\n"])
    end.

run(Handler, Bindings) ->
    %% Trap exits so a dying live/reader process becomes a message the loop
    %% handles (and the cursor is restored), not a signal that kills us mid-frame.
    process_flag(trap_exit, true),
    %% Serverless transport: no HTTP request, so mount with `undefined`
    %% (the demo view uses the request-free mount/1).
    {ok, Pid} = arizona_live:start_link(Handler, Bindings, self(), [], undefined),
    {ok, ViewId} = arizona_live:mount(Pid),
    {ok, Status} = arizona_live:render_current(Pid),
    ok = io:put_chars(to_crlf(Status)),
    Driver = self(),
    _Reader = spawn_link(fun() -> read_loop(Driver) end),
    loop(Pid, ViewId, count_lines(Status)).

loop(Pid, ViewId, PrevLines) ->
    receive
        {term_input, quit} ->
            ok;
        {term_input, ignore} ->
            loop(Pid, ViewId, PrevLines);
        {term_input, Key} ->
            {ok, _Ops, Effects} = arizona_live:handle_event(
                Pid, ViewId, ~"key", #{~"key" => Key}
            ),
            continue(Pid, ViewId, PrevLines, Effects);
        {arizona_push, _Ops, Effects} ->
            %% Timer tick or pubsub broadcast updated the view -- redraw the status
            %% block, streaming any log lines above it.
            continue(Pid, ViewId, PrevLines, Effects);
        {'EXIT', _From, _Reason} ->
            ok
    end.

%% A view can request shutdown with an `arizona_tty:quit/0` effect (e.g. a "Quit"
%% menu item); otherwise stream any log lines, redraw, and keep looping.
continue(Pid, ViewId, PrevLines, Effects) ->
    case has_quit(Effects) of
        true ->
            ok;
        false ->
            loop(Pid, ViewId, redraw(Pid, PrevLines, log_lines(Effects)))
    end.

%% Erase the previous status block, emit any new log lines (they stay above and
%% scroll into history), then reprint the status block at the bottom. Returns the
%% new status-block line count for the next redraw.
redraw(Pid, PrevLines, LogLines) ->
    {ok, Status} = arizona_live:render_current(Pid),
    Logs = [to_crlf(<<Line/binary, "\n">>) || Line <- LogLines],
    ok = io:put_chars([erase_block(PrevLines), Logs, to_crlf(Status)]),
    count_lines(Status).

erase_block(0) ->
    [];
erase_block(Lines) ->
    %% Move to the top of the previous status block and clear from there down.
    [io_ansi:cursor_up(Lines), io_ansi:erase_display()].

read_loop(Driver) ->
    case io:get_chars("", 8) of
        eof ->
            Driver ! {term_input, quit};
        {error, _Reason} ->
            Driver ! {term_input, quit};
        Chars when is_list(Chars) ->
            %% Raw mode delivers keys as a character list.
            forward(Driver, Chars)
    end.

forward(Driver, Chars) ->
    case normalize_key(Chars) of
        quit ->
            Driver ! {term_input, quit};
        Key ->
            Driver ! {term_input, Key},
            read_loop(Driver)
    end.

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

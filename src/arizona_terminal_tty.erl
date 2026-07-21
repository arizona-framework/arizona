-module(arizona_terminal_tty).
-moduledoc """
Local TTY transport for `?terminal` live views -- no HTTP server.

A thin transport over `arizona_terminal_session`: it owns the parts specific to a
real local terminal (raw mode, an `io:get_chars/2` reader, `fun io:put_chars/1` as
the output) and leaves the terminal UX -- the key map, paint model, and cursor
handling -- to the `arizona_terminal_driver` it is given.

A linked **input reader** process blocks on `io:get_chars/2` and forwards each
read as `{term_input, Chars}`, so a blocking read never starves the live process's
asynchronous `{arizona_push, _, _}` updates (timer ticks, pubsub broadcasts).

```erlang
arizona_terminal_tty:start(my_terminal_view, #{}, my_terminal_driver).
```
""".

-export([start/3]).
%% Exported for the transport-teardown test (it drives serve/2 directly, without
%% a controlling TTY); not part of the public API.
-export([serve/2]).

-ignore_xref([start/3]).
-ignore_xref([serve/2]).

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
-dialyzer({no_match, [start/3]}).

-doc """
Mounts `Handler` as a `?terminal` live view driven by `Driver`
(an `arizona_terminal_driver`) and runs the local terminal event loop until the
user quits.
""".
-spec start(module(), arizona_template:bindings(), module()) -> ok.
start(Handler, Bindings, Driver) ->
    case enter_raw_mode() of
        ok ->
            run_terminal(Handler, Bindings, Driver);
        {error, already_started} ->
            run_terminal(Handler, Bindings, Driver);
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

run_terminal(Handler, Bindings, Driver) ->
    try
        run(Handler, Bindings, Driver)
    after
        %% No alternate screen: leave the log + final status block in the shell.
        %% Restore the cursor (a crash-safety net beyond the driver's teardown) and
        %% move below the block for the next prompt.
        io:put_chars([io_ansi:cursor_show(), ~"\r\n"])
    end.

run(Handler, Bindings, Driver) ->
    %% Trap exits so a dying live/reader process becomes a message the loop
    %% handles (and the cursor is restored), not a signal that kills us mid-frame.
    %% `start/3` runs in the caller's process, so restore the caller's flag on the
    %% way out rather than leaving it flipped.
    Prev = process_flag(trap_exit, true),
    try
        {ok, Session} = arizona_terminal_session:start(
            Handler, Bindings, Driver, [], fun io:put_chars/1
        ),
        Owner = self(),
        Reader = spawn_link(fun() -> read_loop(Owner) end),
        serve(Session, Reader)
    after
        process_flag(trap_exit, Prev)
    end.

%% Run the event loop, then reap the session and the input reader on EVERY exit
%% path (quit, eof, or a linked process dying). Without this the live view
%% outlives the transport -- it keeps ticking, stays subscribed to pubsub, and
%% floods the mailbox with `{arizona_push, _, _}` -- and the reader stays blocked
%% in `io:get_chars/2` forever, stealing subsequent stdin bytes.
-spec serve(arizona_terminal_session:session(), pid()) -> ok.
serve(Session, Reader) ->
    try
        loop(Session)
    after
        ok = stop_session(Session),
        true = exit(Reader, kill)
    end.

%% Stop the live view backing the session, tolerating one that has already exited
%% (the `{'EXIT', _, _}` loop path is where the live process is what died);
%% `gen_server:stop/1` would otherwise raise `noproc`.
-spec stop_session(arizona_terminal_session:session()) -> ok.
stop_session(Session) ->
    try
        arizona_terminal_session:stop(Session)
    catch
        exit:noproc -> ok
    end.

loop(Session) ->
    receive
        {term_input, eof} ->
            ok;
        {term_input, Chars} ->
            case arizona_terminal_session:handle_key(Session, Chars) of
                quit -> ok;
                {cont, Session1} -> loop(Session1)
            end;
        {arizona_push, _Ops, Effects} ->
            %% Timer tick or pubsub broadcast updated the view -- redraw the status
            %% block, streaming any log lines above it.
            case arizona_terminal_session:handle_push(Session, Effects) of
                quit -> ok;
                {cont, Session1} -> loop(Session1)
            end;
        {'EXIT', _From, _Reason} ->
            ok
    end.

read_loop(Owner) ->
    case io:get_chars("", 8) of
        eof ->
            Owner ! {term_input, eof};
        {error, _Reason} ->
            Owner ! {term_input, eof};
        Chars when is_list(Chars) ->
            %% Raw mode delivers keys as a character list; the driver normalizes.
            Owner ! {term_input, Chars},
            read_loop(Owner)
    end.

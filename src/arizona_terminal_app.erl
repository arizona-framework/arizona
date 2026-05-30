-module(arizona_terminal_app).
-moduledoc """
Local TTY driver for `?terminal` views -- no HTTP server.

A thin transport over `arizona_terminal_session`: it owns the parts specific to a
real local terminal (raw mode, cursor hide/show, an `io:get_chars/2` reader) and
delegates the rendering, key handling, and `arizona_term_demo_effects` effect interpretation to
the shared session, supplying `fun io:put_chars/1` as the session's output.

The view renders as a **scrolling log above a pinned status block**: the session
redraws the status block in place (cursor up + erase) on every update, while
`arizona_term_demo_effects:log/1` output scrolls into the terminal's native scrollback. There is
no alternate screen, so the session stays in the shell after quit.

A linked **input reader** process blocks on `io:get_chars/2` and forwards each
read as `{term_input, Chars}`, so a blocking read never starves the live process's
asynchronous `{arizona_push, _, _}` updates (timer ticks, pubsub broadcasts).

```erlang
arizona_terminal_app:start(my_terminal_view, #{}).
```
""".

-export([start/1]).
-export([start/2]).

-ignore_xref([start/1]).
-ignore_xref([start/2]).

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
    {ok, Session} = arizona_terminal_session:start(Handler, Bindings, fun io:put_chars/1),
    Driver = self(),
    _Reader = spawn_link(fun() -> read_loop(Driver) end),
    loop(Session).

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

read_loop(Driver) ->
    case io:get_chars("", 8) of
        eof ->
            Driver ! {term_input, eof};
        {error, _Reason} ->
            Driver ! {term_input, eof};
        Chars when is_list(Chars) ->
            %% Raw mode delivers keys as a character list; the session normalizes.
            Driver ! {term_input, Chars},
            read_loop(Driver)
    end.

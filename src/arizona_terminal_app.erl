-module(arizona_terminal_app).
-moduledoc """
Headless terminal runtime for `?terminal` views -- no HTTP server.

Drives an `arizona_live` process from a real terminal: it puts the terminal in
raw mode on the alternate screen, mounts the view, and paints the full ANSI
frame produced by `arizona_live:render_current/1`. A linked **input reader**
process blocks on `io:get_chars/2` and forwards each keypress as a
`{term_input, Key}` message, so a blocking read never starves the live
process's asynchronous `{arizona_push, _, _}` updates (timer ticks, pubsub
broadcasts). Both kinds of message trigger a full repaint -- terminals repaint
cheaply, so the diff ops are ignored.

Keys become ordinary Arizona events: each keypress is dispatched as a `~"key"`
event with the normalized key in the payload (`#{~"key" => Key}`), so the view
handles them in `handle_event/3`. `q` / Ctrl-C / Ctrl-D quit, restoring the
terminal on the way out.

```erlang
arizona_terminal_app:start(my_terminal_view, #{}).
```
""".

-export([start/1]).
-export([start/2]).
-export([normalize_key/1]).

-ignore_xref([start/1]).
-ignore_xref([start/2]).
-ignore_xref([normalize_key/1]).

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
restoring the terminal (cursor, main screen) on exit.
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
    ok = io:put_chars([io_ansi:alternate_screen(), io_ansi:cursor_hide()]),
    try
        run(Handler, Bindings)
    after
        io:put_chars([io_ansi:cursor_show(), io_ansi:alternate_screen_off()])
    end.

run(Handler, Bindings) ->
    %% Trap exits so a dying live/reader process becomes a message the loop
    %% handles (and the terminal is restored), not a signal that kills us mid-frame.
    process_flag(trap_exit, true),
    {ok, Pid} = arizona_live:start_link(
        Handler, Bindings, self(), [], arizona_terminal_req:new()
    ),
    {ok, ViewId} = arizona_live:mount(Pid),
    ok = paint(Pid),
    Driver = self(),
    _Reader = spawn_link(fun() -> read_loop(Driver) end),
    loop(Pid, ViewId).

loop(Pid, ViewId) ->
    receive
        {term_input, quit} ->
            ok;
        {term_input, ignore} ->
            loop(Pid, ViewId);
        {term_input, Key} ->
            {ok, _Ops, _Effects} = arizona_live:handle_event(
                Pid, ViewId, ~"key", #{~"key" => Key}
            ),
            ok = paint(Pid),
            loop(Pid, ViewId);
        {arizona_push, _Ops, _Effects} ->
            %% Timer tick or pubsub broadcast updated the view -- repaint.
            ok = paint(Pid),
            loop(Pid, ViewId);
        {'EXIT', _From, _Reason} ->
            ok
    end.

paint(Pid) ->
    {ok, Frame} = arizona_live:render_current(Pid),
    ok = io:put_chars([io_ansi:clear(), Frame]).

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
normalize_key([Char]) when Char >= 32, Char < 127 -> <<Char>>;
normalize_key(_Other) -> ignore.

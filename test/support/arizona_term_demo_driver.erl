-module(arizona_term_demo_driver).
-moduledoc """
The demo's terminal UI driver: a **scrolling log above a pinned status block**.

Implements `arizona_terminal_driver`. The view's `render/1` is the status block,
redrawn in place (cursor up + erase) on every update; `arizona_term_demo_effects:log/1`
output streams *above* it into the terminal's native scrollback. No alternate
screen, so the session stays in the shell after quit. The cursor is hidden for the
session and restored on quit.

This is a demo-scoped policy, not framework code -- a different terminal style
(full-screen, alternate-screen, different keys) is a different driver.
""".
-behaviour(arizona_terminal_driver).

-export([init/1]).
-export([keys/2]).
-export([paint/3]).
-export([setup/1]).
-export([teardown/1]).
%% Exported for the terminal suite's unit tests.
-export([count_lines/1]).
-export([log_lines/1]).
-export([has_quit/1]).
-export([to_crlf/1]).

-ignore_xref([count_lines/1]).
-ignore_xref([log_lines/1]).
-ignore_xref([has_quit/1]).
-ignore_xref([to_crlf/1]).

%% --------------------------------------------------------------------
%% arizona_terminal_driver callbacks
%% --------------------------------------------------------------------

%% State is the line count of the last status block, so the next paint can move
%% the cursor back to its top before redrawing.
init(_Arg) ->
    #{prev_lines => 0}.

-doc "Hide the cursor for the session.".
setup(State) ->
    {io_ansi:cursor_hide(), State}.

-doc "Restore the cursor on quit.".
teardown(_State) ->
    io_ansi:cursor_show().

-doc """
Map a raw key read into commands. `q` is an ordinary character (the view decides
whether it quits); only Ctrl-C (`[3]`) / Ctrl-D (`[4]`) stop at this layer.
Backspace and bare ESC reach the view for text editing.
""".
keys(Bytes, State) ->
    {normalize_key(binary_to_list(Bytes)), State}.

-doc """
Erase the previous status block, stream any `log` lines above it (they scroll into
history), then CRLF + repaint the block. A `quit` effect stops the session.
""".
paint(Frame, Effects, #{prev_lines := PrevLines} = State) ->
    Logs = [to_crlf(<<Line/binary, "\n">>) || Line <- log_lines(Effects)],
    Output = [erase_block(PrevLines), Logs, to_crlf(Frame)],
    Next =
        case has_quit(Effects) of
            true -> stop;
            false -> continue
        end,
    {Output, Next, State#{prev_lines => count_lines(Frame)}}.

%% --------------------------------------------------------------------
%% Internal functions
%% --------------------------------------------------------------------

normalize_key([3]) -> [stop];
normalize_key([4]) -> [stop];
normalize_key("\e[A") -> key(~"up");
normalize_key("\e[B") -> key(~"down");
normalize_key("\e[C") -> key(~"right");
normalize_key("\e[D") -> key(~"left");
normalize_key("\r") -> key(~"enter");
normalize_key("\n") -> key(~"enter");
normalize_key([127]) -> key(~"backspace");
normalize_key([8]) -> key(~"backspace");
normalize_key([27]) -> key(~"esc");
normalize_key([Char]) when Char >= 32, Char < 127 -> key(<<Char>>);
normalize_key(_Other) -> [].

key(K) ->
    [{event, ~"key", #{~"key" => K}}].

erase_block(0) ->
    [];
erase_block(Lines) ->
    %% Move to the top of the previous status block and clear from there down.
    [io_ansi:cursor_up(Lines), io_ansi:erase_display()].

-doc """
Number of terminal rows a rendered status block occupies: one per `\\n` (each
`?terminal` line ends in a newline).
""".
-spec count_lines(binary()) -> non_neg_integer().
count_lines(Frame) ->
    length(binary:matches(Frame, ~"\n")).

-doc "Extracts the lines carried by `log` effects, dropping any others.".
-spec log_lines([arizona_effect:cmd()]) -> [binary()].
log_lines(Effects) ->
    [Line || {arizona_effect, [log, Line]} <- Effects].

-doc "Whether the effects include a `quit` request.".
-spec has_quit([arizona_effect:cmd()]) -> boolean().
has_quit(Effects) ->
    lists:member({arizona_effect, [quit]}, Effects).

-doc """
Translates bare LF to CRLF. In raw mode the terminal does not map `\\n` to
`\\r\\n`, so without this each line would start where the previous one ended.
""".
-spec to_crlf(binary()) -> binary().
to_crlf(Frame) ->
    binary:replace(Frame, ~"\n", ~"\r\n", [global]).

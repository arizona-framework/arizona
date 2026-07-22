-module(arizona_terminal_default_driver).
-moduledoc """
The framework's default `m:arizona_terminal_driver` implementation -- a sensible
terminal experience any `?terminal` app gets for free.

`arizona_terminal_session` falls back to these for any callback a driver doesn't
implement, so a custom driver overrides only what it changes; passing this module
itself as the driver uses all the defaults.

The defaults:

- `keys/2` translates the read with `arizona_terminal_io:keys/1` (printable -> `$a`,
  named keys -> atoms, `{ctrl, _}` / `{alt, _}`). Ctrl-D (EOF) quits -- the one
  always-available escape, so any app is quittable. Every other key, Ctrl-C included,
  becomes a `~"key"` event carrying the idiomatic key value, so a view matches
  `handle_event(~"key", #{key := $j}, B)`, `#{key := up}`, `#{key := {ctrl, $c}}`,
  etc. An app decides what Ctrl-C does (cancel, confirm-quit, ...) by handling that
  event; nothing happens by default.
- `setup/1` hides the cursor for the session; `teardown/1` restores it on quit.
- `paint/3` repaints the frame in place (cursor home, clear-to-end-of-line per line,
  then clear below) so it does not flicker, and interprets the framework terminal
  effects (`m:arizona_terminal_effect`): `quit` stops the session, `set_title` sets
  the terminal title, `bell` rings it. Unknown effects are ignored.

> #### Frame must fit the terminal {: .warning}
>
> The in-place repaint homes with `\e[H` (absolute top-left) and writes every line,
> so it assumes the frame **fits within the terminal's visible rows**. A frame taller
> than the window scrolls on each paint: `\e[H` no longer addresses the true top and
> each repaint appends another screenful to the scrollback (a continuous flood under
> the periodic tick). Clamping to the visible rows would need the terminal geometry,
> which this default driver is not given (the SSH transport knows it, but a local TTY
> cannot read its own size). Keep a `?terminal` view's rendered height within the
> smallest terminal you target, or supply a custom driver that clamps against a size
> it tracks.
""".

-behaviour(arizona_terminal_driver).

%% BEL (0x07). Erlang has no `\a` escape (unlike C -- `~"\a"` is the literal byte
%% `a`, 0x61, not the alert char), so BEL is written as control-G. Used as the
%% OSC string terminator for `set_title` and as the `bell` effect's byte.
-define(BEL, ~"\^g").

-export([init/1]).
-export([keys/2]).
-export([paint/3]).
-export([setup/1]).
-export([teardown/1]).

%% --------------------------------------------------------------------
%% Callbacks
%% --------------------------------------------------------------------

-doc "The default driver is stateless; its state is an empty map.".
-spec init(term()) -> map().
init(_Arg) ->
    #{}.

-doc """
Translate the read and turn keys into events; Ctrl-D (EOF) quits. An escape
sequence split across reads (a transport delivers bytes in chunks) would otherwise
mis-decode -- a trailing `\\e[` dropped, the next read's `A` surfacing as a spurious
`$a`. So the incomplete trailing escape prefix is buffered in the driver state
(`pending`) and prepended to the next read.
""".
-spec keys(binary(), State) -> {[arizona_terminal_driver:command()], State}.
keys(Bytes, State) ->
    Pending = maps:get(pending, State, <<>>),
    {Complete, Pending1} = arizona_terminal_io:take_incomplete(<<Pending/binary, Bytes/binary>>),
    Commands = [to_command(Key) || Key <- arizona_terminal_io:keys(Complete)],
    {Commands, State#{pending => Pending1}}.

-doc """
Repaint the frame in place and interpret framework terminal effects: a `quit`
effect stops the session; `set_title` / `bell` emit their escape sequences; unknown
effects are ignored. Repaints with home + per-line clear-to-EOL + clear-below rather
than a `\\e[2J` full clear, so the screen does not flicker.
""".
-spec paint(binary(), [arizona_effect:cmd()], State) -> {iodata(), continue | stop, State}.
paint(Frame, Effects, State) ->
    Output = [repaint(Frame), [effect_bytes(Effect) || Effect <- Effects]],
    Next =
        case lists:member(arizona_terminal_effect:quit(), Effects) of
            true -> stop;
            false -> continue
        end,
    {Output, Next, State}.

-doc "Hide the cursor for the session.".
-spec setup(State) -> {iodata(), State}.
setup(State) ->
    {io_ansi:cursor_hide(), State}.

-doc "Restore the cursor on quit.".
-spec teardown(term()) -> iodata().
teardown(_State) ->
    io_ansi:cursor_show().

%% --------------------------------------------------------------------
%% Internal functions
%% --------------------------------------------------------------------

%% Ctrl-D (EOF) is the always-available escape; every other key, Ctrl-C included,
%% is a view event the app decides what to do with.
to_command({ctrl, $d}) -> stop;
to_command(Key) -> {event, ~"key", #{key => Key}}.

%% Repaint in place: home, write each line clearing to end-of-line (so a now-shorter
%% line erases its old tail) then CRLF, and finally clear everything below (leftover
%% from a previously longer frame). Avoids the full-screen flash of \e[2J. Assumes
%% the frame fits the terminal's visible rows -- a taller frame scrolls and floods
%% the scrollback (see the "Frame must fit the terminal" note in the moduledoc).
repaint(Frame) ->
    [~"\e[H", binary:replace(Frame, ~"\n", ~"\e[K\r\n", [global]), ~"\e[J"].

%% Frame-orthogonal effects -> escape sequences emitted with the repaint. quit is
%% handled by the stop result in paint/3; unknown effects produce no output.
effect_bytes({arizona_effect, [set_title, Title]}) ->
    %% Sanitize the (user-influenced) title so a BEL/ESC byte cannot terminate
    %% the OSC string early and inject escape sequences.
    [~"\e]0;", arizona_terminal:escape(Title), ?BEL];
effect_bytes({arizona_effect, [bell]}) ->
    ?BEL;
effect_bytes(_Effect) ->
    [].

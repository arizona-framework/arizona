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
- `paint/3` (**draft**) clears the screen and repaints the frame (CRLF for raw
  mode). Effects are not interpreted yet -- a smarter, non-flickering paint and
  effect handling are next.
""".

-behaviour(arizona_terminal_driver).

-export([init/1]).
-export([keys/2]).
-export([paint/3]).
-export([setup/1]).
-export([teardown/1]).

%% --------------------------------------------------------------------
%% Callbacks
%% --------------------------------------------------------------------

-doc "No paint state yet (grows when the default paint is refined).".
-spec init(term()) -> map().
init(_Arg) ->
    #{}.

-doc "Translate the read and turn keys into events; Ctrl-D (EOF) quits.".
-spec keys(binary(), State) -> {[arizona_terminal_driver:command()], State}.
keys(Bytes, State) ->
    {[to_command(Key) || Key <- arizona_terminal_io:keys(Bytes)], State}.

-doc "Clear the screen and repaint the frame (CRLF for raw mode).".
-spec paint(binary(), [arizona_effect:cmd()], State) -> {iodata(), continue, State}.
paint(Frame, _Effects, State) ->
    {[~"\e[2J", ~"\e[H", crlf(Frame)], continue, State}.

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

%% Raw mode does not map \n to \r\n; add the \r so lines don't stair-step.
crlf(Frame) ->
    binary:replace(Frame, ~"\n", ~"\r\n", [global]).

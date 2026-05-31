-module(arizona_terminal_effect).
-moduledoc """
Framework terminal effect builders for `?terminal` views -- the terminal analog of
`m:arizona_js` (web) and `m:arizona_android` (native).

A view returns these from `handle_event/3` / `handle_info/2`, and the terminal
driver interprets them. `m:arizona_terminal_default_driver` handles all three; a
custom driver may handle more (or fewer). Each builds the neutral
`t:arizona_effect:cmd/0` tuple the other targets use.

These are *frame-orthogonal* -- they don't depend on the paint model, so they work
with the default full-frame repaint. Append-only output like a scrolling log is a
paint-model concern and stays the responsibility of a custom driver.

```erlang
handle_event(~"key", #{key := $q}, Bindings) ->
    {Bindings, #{}, [arizona_terminal_effect:quit()]}.
```
""".

-export([quit/0]).
-export([set_title/1]).
-export([bell/0]).

-ignore_xref([quit/0]).
-ignore_xref([set_title/1]).
-ignore_xref([bell/0]).

-doc "Stop the terminal session: the driver tears down (restores the terminal) and exits.".
-spec quit() -> arizona_effect:cmd().
quit() ->
    {arizona_effect, [quit]}.

-doc "Set the terminal window/icon title.".
-spec set_title(Title) -> arizona_effect:cmd() when
    Title :: iodata().
set_title(Title) ->
    {arizona_effect, [set_title, iolist_to_binary(Title)]}.

-doc "Ring the terminal bell.".
-spec bell() -> arizona_effect:cmd().
bell() ->
    {arizona_effect, [bell]}.

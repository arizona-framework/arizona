-module(arizona_tty).
-moduledoc """
Terminal effect builders for `?terminal` views -- the terminal analog of
`arizona_js` (web) and `arizona_android` (native).

Effects are returned from a view's `handle_event/3` / `handle_info/2` and
interpreted by the terminal runtime (`arizona_terminal_app`). They build the
same neutral `t:arizona_effect:cmd/0` tuple the other targets use.
""".

-export([log/1]).
-export([quit/0]).

-ignore_xref([log/1]).
-ignore_xref([quit/0]).

-doc """
Appends a line to the terminal's scrolling log -- the region above the pinned
status block, which the user can scroll back through. Use it for append-only
output (chat, notifications, events) as opposed to in-place state, which the
view shows through its `render/1`.
""".
-spec log(Line) -> arizona_effect:cmd() when
    Line :: iodata().
log(Line) ->
    {arizona_effect, [log, iolist_to_binary(Line)]}.

-doc """
Requests that the terminal runtime stop and restore the terminal -- e.g. from a
"Quit" menu item. Equivalent to the user pressing `q`.
""".
-spec quit() -> arizona_effect:cmd().
quit() ->
    {arizona_effect, [quit]}.

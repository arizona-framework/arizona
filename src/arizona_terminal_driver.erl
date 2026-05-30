-module(arizona_terminal_driver).
-moduledoc """
Behaviour for a terminal UI driver -- the app-supplied policy that turns a
`?terminal` live view into a concrete terminal experience.

`arizona_terminal_session` is generic: it mounts the view, dispatches events, and
re-renders, but it holds no opinion about *how* the terminal looks or which keys
mean what. A driver supplies that policy:

- `keys/2` maps raw input bytes to view events (or a stop request) -- the key map.
- `paint/3` turns a freshly rendered ANSI frame plus the view's effects into the
  bytes to write, and decides whether to keep going or stop -- the paint model
  (full repaint, a scrolling log above a pinned block, an alternate-screen TUI,
  ...).
- the optional `setup/1` / `teardown/1` emit one-shot bytes at start and on quit
  (e.g. hide/show the cursor, enter/leave the alternate screen).

The driver carries its own state across these calls. Different terminal styles are
different drivers; the framework ships none -- see the demo's
`arizona_term_demo_driver` (a scrolling-log driver) for an example.

A transport (`arizona_terminal_app`, `arizona_ssh`) selects a driver and passes it
to `arizona_terminal_session:start/5`.
""".

-export_type([command/0]).

%% --------------------------------------------------------------------
%% Behaviour callbacks
%% --------------------------------------------------------------------

-doc "An action a key read produces: dispatch a view event, or stop the session.".
-nominal command() :: {event, Name :: binary(), Payload :: map()} | stop.

-doc "Create the driver's initial state. `Arg` is transport-supplied context.".
-callback init(Arg :: term()) -> State :: term().

-doc """
Map a raw input read (a byte binary) to commands and the next state. `[]` ignores
the input. May keep state to buffer escape sequences across reads.
""".
-callback keys(Bytes :: binary(), State :: term()) ->
    {[command()], NewState :: term()}.

-doc """
Turn the freshly rendered ANSI `Frame` and the view's `Effects` into the bytes to
write, whether to `continue` or `stop`, and the next state.
""".
-callback paint(Frame :: binary(), Effects :: [arizona_effect:cmd()], State :: term()) ->
    {iodata(), continue | stop, NewState :: term()}.

-doc "One-shot output emitted before the first paint (e.g. hide the cursor).".
-callback setup(State :: term()) -> {iodata(), NewState :: term()}.

-doc "One-shot output emitted on quit, before stopping (e.g. show the cursor).".
-callback teardown(State :: term()) -> iodata().

-optional_callbacks([setup/1, teardown/1]).

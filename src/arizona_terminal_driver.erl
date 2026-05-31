-module(arizona_terminal_driver).
-moduledoc """
Behaviour for a terminal UI driver -- the policy that turns a `?terminal` live view
into a concrete terminal experience.

`arizona_terminal_session` is generic: it mounts the view, dispatches events, and
re-renders, but holds no opinion about *how* the terminal looks or which keys mean
what. A driver supplies that policy:

- `keys/2` maps raw input bytes to view events (or a `stop` request) -- the key map.
- `paint/3` turns a freshly rendered ANSI frame plus the view's effects into the
  bytes to write, and decides whether to keep going or stop -- the paint model.
- `setup/1` / `teardown/1` emit one-shot bytes at start and on quit (e.g. hide /
  show the cursor, enter / leave the alternate screen).
- `init/1` creates the driver state threaded through the calls above.

Every callback is optional: `m:arizona_terminal_default_driver` ships a sensible
default for each, and the session uses a driver's callback when it exports one, else
that default. So a driver implements only the callbacks it changes, and passing
`arizona_terminal_default_driver` itself uses all the defaults.
""".

-export_type([command/0]).

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

%% Every callback has a default in arizona_terminal_default_driver, which the
%% session falls back to, so all are optional.
-optional_callbacks([init/1, keys/2, paint/3, setup/1, teardown/1]).

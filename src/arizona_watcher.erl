-module(arizona_watcher).
-moduledoc """
Behaviour for a file-system watcher: the source of the file changes the dev
reloader acts on.

Arizona ships no implementation, so the file-system dependency stays out of
every consumer that does not reload code. `arizona_watcher_fs` (a separate
package, built on the `fs` library) is the reference one; add it to a dev-only
rebar3 profile and name it in the `reloader` application env:

```erlang
{arizona, [
    {reloader, #{
        enabled => true,
        watcher => arizona_watcher_fs,
        rules => [#{directory => "src", patterns => [".*\\.erl$"],
                    callback => fun arizona_reloader:reload_erl/1}]
    }}
]}
```

`watcher` is a module, or `{Module, Opts}` to hand the implementation its own
options (`Opts` is a map; a bare module gets `#{}`).

## Contract

An implementation is a callback module, not a process. Each reloader rule runs
in its own `arizona_reloader_rule` process, which calls `init/2` once and then
hands every message it does not own to `handle_info/2`, so whatever the
implementation subscribes to must deliver its events to the **calling**
process. The rule process owns everything above the raw events -- the rule's
`patterns`, debouncing, the callback, and the `arizona_watcher` pubsub
broadcast -- so every implementation behaves the same from the configuration's
side.

`handle_info/2` reports **content changes** only: a file created, modified in
place, deleted, or renamed into the directory (the atomic-rename save most
editors use). A directory rename and a file moved out of the directory are not
content changes. Paths are absolute strings, as the implementation's watch
reports them (one that resolves symlinks reports resolved paths, so patterns
should not depend on the directory prefix).

A watch may need a moment after `init/2` returns before it reports anything
(the `fs` implementation starts an OS process that sets its watches up
asynchronously), so a change made in that window can be missed.
""".

%% --------------------------------------------------------------------
%% Callbacks
%% --------------------------------------------------------------------

-doc """
Starts watching `Dir` (an absolute path to an existing directory) recursively,
delivering the events to the calling process. `Opts` is the map given as
`{Module, Opts}` in the `reloader` env (`#{}` for a bare module).

Returning `{error, Reason}` stops the rule process with `Reason`.
""".
-callback init(Dir :: string(), Opts :: map()) ->
    {ok, State :: term()} | {error, Reason :: term()}.

-doc """
Translates one message the rule process received into the paths it changed.

Called with every message the rule process does not own, so a message that is
not one of this watcher's events -- or is one but not a content change --
returns `{ok, [], State}`. `{stop, Reason, State}` reports a watch that can no
longer deliver events (its backend gave up): the rule process stops with
`Reason`, running `terminate/1`, and its supervisor restarts it.
""".
-callback handle_info(Info :: term(), State :: term()) ->
    {ok, Paths :: [string()], NewState :: term()}
    | {stop, Reason :: term(), NewState :: term()}.

-doc """
Stops watching. Called whenever the rule process stops -- a supervisor
shutdown, an orderly `gen_server:stop/1`, or a crash -- so an implementation
can release what it holds and wait for it to go. It must return within the
rule's shutdown timeout (5 seconds), or the supervisor kills the process.

The rule process traps exits, so a process the implementation links to it
that exits abnormally stops the rule with that reason (and its supervisor
restarts it); such `'EXIT'` messages never reach `handle_info/2`. Optional: an
implementation with nothing to release can omit it.
""".
-callback terminate(State :: term()) -> ok.

-optional_callbacks([terminate/1]).

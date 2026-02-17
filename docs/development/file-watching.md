# File Watching

- [Architecture](#architecture)
  - [Watcher GenServer](#watcher-genserver)
  - [Supervisor](#supervisor)
- [Reloader Behaviour](#reloader-behaviour)
- [Custom Handler](#custom-handler)
- [Config](#config)

## Architecture

Arizona's file watching system monitors source files for changes during development and triggers
reload actions. It is built on the `fs` library, which uses native OS file system events (inotify on
Linux, FSEvents on macOS, ReadDirectoryChangesW on Windows) for efficient, low-latency change
detection.

The system is composed of two layers: watchers that detect file changes, and the reloader that
coordinates what happens when changes are detected. Each watcher monitors a set of directories and
filters events by regex patterns, while the reloader dispatches matched changes to the appropriate
handler.

### Watcher GenServer

Each watcher is a GenServer (`arizona_watcher`) that monitors a set of directories for file changes.
It uses the `fs` library to receive native file system events, then matches changed files against
configured regex patterns. Regex patterns are precompiled during initialization for efficient
matching, and directories are precomputed to absolute paths to avoid redundant path resolution on
every event.

A debounce mechanism (configurable via `debounce_ms`) prevents rapid re-triggers when multiple files
are saved in quick succession. This is particularly important during batch operations such as
search-and-replace across files or when an editor writes backup files alongside the main save. When
the debounce timer expires, the watcher invokes its configured callback function with the
accumulated list of changed files.

### Supervisor

Watchers are managed by `arizona_watcher_sup`, a dynamic supervisor using a `simple_one_for_one`
strategy. This allows creating and stopping watchers at runtime:

```erlang
{ok, Pid} = arizona_watcher_sup:start_child(WatcherConfig).
arizona_watcher_sup:stop_child(Pid).
```

The watcher supervisor is started as part of the main `arizona_sup` supervision tree. Each reloader
rule spawns its own watcher child, so they can be independently started, stopped, and restarted
without affecting each other.

## Reloader Behaviour

The `arizona_reloader` module coordinates file watching with reload logic. It defines a behaviour
with a single callback:

```erlang
-callback reload(Files, HandlerOptions) -> Result.
```

When a watcher detects changes, the reloader calls the handler's `reload/2` callback with the list
of changed files and the handler's options. The handler is responsible for performing the actual
reload work (recompiling, rebuilding assets, etc.) and notifying connected clients.

The reloader is started and stopped with:

```erlang
arizona_reloader:start(ReloaderConfig).
arizona_reloader:stop().
```

You can also invoke a reload handler manually, which is useful for testing:

```erlang
arizona_reloader:call_reload_callback({HandlerModule, HandlerOptions}, Files).
```

## Custom Handler

Implement the `arizona_reloader` behaviour to create custom reload handlers. A handler receives the
list of changed files and its configured options, then decides what to do:

```erlang
-module(my_erl_reloader).
-behaviour(arizona_reloader).
-export([reload/2]).

reload(Files, _Options) ->
    %% Recompile changed modules
    os:cmd("rebar3 compile"),
    %% Reload modules in the VM
    [code:purge(M) || M <- changed_modules(Files)],
    [code:load_file(M) || M <- changed_modules(Files)],
    %% Notify connected clients
    arizona_pubsub:broadcast(~"arizona:reload", #{type => erl}),
    ok.
```

A JavaScript asset handler might look like this:

```erlang
-module(my_js_reloader).
-behaviour(arizona_reloader).
-export([reload/2]).

reload(_Files, _Options) ->
    os:cmd("npm run build"),
    arizona_pubsub:broadcast(~"arizona:reload", #{type => js}),
    ok.
```

The handler is free to perform any logic needed -- recompile code, rebuild assets, run linters,
clear caches, or trigger external tools. The only requirement is that it broadcasts to the
`arizona:reload` PubSub topic so connected clients are notified.

## Config

Configure the reloader in `sys.config`:

```erlang
[{arizona, #{
    reloader => #{
        enabled => true,
        rules => [
            #{
                handler => {my_erl_reloader, #{}},
                watcher => #{
                    directories => ["src", "include"],
                    patterns => ["\\.erl$", "\\.hrl$"],
                    debounce_ms => 200
                }
            },
            #{
                handler => {my_js_reloader, #{}},
                watcher => #{
                    directories => ["assets/js"],
                    patterns => ["\\.js$"],
                    debounce_ms => 300
                }
            }
        ]
    }
}}].
```

Each rule pairs a handler with a watcher config. Multiple rules allow different reload strategies
for different file types. The watcher config accepts the following options:

| Option | Type | Description |
| -------- | ------ | ------------- |
| `directories` | `[path()]` | List of directories to watch for changes |
| `patterns` | `[regex()]` | Regex patterns to filter changed files |
| `callback` | `fun()` | Callback function invoked on matched changes |
| `debounce_ms` | `integer()` | Milliseconds to wait before triggering after the last change |

The handler tuple `{Module, Options}` specifies the module implementing the `arizona_reloader`
behaviour and any options to pass to its `reload/2` callback. The options map is opaque to the
reloader -- its structure is defined by the handler module.

See also: [Smart Reload](smart-reload.md), [Configuration](../server/configuration.md)

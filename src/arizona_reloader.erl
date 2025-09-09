-module(arizona_reloader).
-moduledoc ~"""
File reloader coordination system for development workflow automation.

Manages multiple reload handlers with individual file system watchers.
Each rule defines a specialized handler module and its file watching configuration.
Handlers implement domain-specific reload logic (compile, build, etc.).

## Configuration

```erlang
Config = #{
    enabled => true,
    rules => [
        #{
            handler => my_erl_reloader,     % -behaviour(arizona_reloader)
            watcher => #{                   % arizona_watcher:config()
                directories => ["src"],
                patterns => [".*\\.erl$"],
                debounce_ms => 100
            }
        },
        #{
            handler => my_assets_reloader,
            watcher => #{
                directories => ["assets"],
                patterns => [".*\\.js$"],
                debounce_ms => 200
            }
        }
    ]
}.
```

## Handler Behavior

Reload handlers must implement the arizona_reloader behavior:

```erlang
-module(my_erl_reloader).
-behaviour(arizona_reloader).
-export([reload/1]).

reload(Files) ->
    % Custom reload logic for Erlang files
    lists:foreach(fun compile:file/1, Files),
    ok.
```

## Integration

The reloader coordinates with arizona_watcher_sup to manage individual
watcher instances. Each rule creates a dedicated watcher process that
monitors specific directories and delegates to the configured handler.

## Usage Pattern

1. **Configuration**: Define rules with handler modules and watcher configs
2. **Startup**: Start reloader with rule set during application boot
3. **Monitoring**: Individual watchers detect file changes
4. **Delegation**: Changes are forwarded to appropriate handler modules
5. **Execution**: Handlers perform domain-specific reload operations
""".

%% --------------------------------------------------------------------
%% API function exports
%% --------------------------------------------------------------------

-export([call_reload_callback/2]).
-export([start/1]).
-export([stop/0]).

%% --------------------------------------------------------------------
%% Ignore xref warnings
%% --------------------------------------------------------------------

-ignore_xref([call_reload_callback/2]).
-ignore_xref([start/1]).
-ignore_xref([stop/0]).

%% --------------------------------------------------------------------
%% Types exports
%% --------------------------------------------------------------------

-export_type([config/0]).
-export_type([rule/0]).
-export_type([reload_files/0]).
-export_type([reload_result/0]).

%% --------------------------------------------------------------------
%% Types definitions
%% --------------------------------------------------------------------

-nominal config() :: #{
    enabled := boolean(),
    rules := [rule()]
}.
-nominal rule() :: #{
    handler := module(),
    watcher := arizona_watcher:config()
}.
-nominal reload_files() :: [arizona_watcher:filename()].
-nominal reload_result() :: arizona_watcher:watcher_callback_result().

%% --------------------------------------------------------------------
%% Behavior definition
%% --------------------------------------------------------------------

-doc ~"""
Callback for handling file reload operations.

Invoked when files matching the watcher configuration are changed.
Handlers receive a list of changed files and should perform their
specific reload logic (compilation, building, etc.).
""".
-callback reload(Files) -> Result when
    Files :: reload_files(),
    Result :: reload_result().

%% --------------------------------------------------------------------
%% API function definitions
%% --------------------------------------------------------------------

-doc ~"""
Executes a reload handler callback.

Calls the handler module's `reload/1` function with the list of changed files.
Used by the watcher callback to delegate to appropriate reload handlers.
""".
-spec call_reload_callback(Handler, Files) -> Result when
    Handler :: module(),
    Files :: reload_files(),
    Result :: reload_result().
call_reload_callback(Handler, Files) ->
    apply(Handler, reload, [Files]).

-doc ~"""
Starts the reloader system with the given configuration.

Creates individual file watchers for each rule, with each watcher
configured to call the appropriate reload handler when files change.
Returns `ok` on success or `{error, Reason}` on failure.
""".
-spec start(Config) -> Result when
    Config :: config(),
    Result :: ok | {error, Reason},
    Reason :: dynamic().
start(#{enabled := true, rules := Rules}) ->
    start_reloader_instances(Rules);
start(_Config) ->
    ok.

-doc ~"""
Stops the reloader system.

Note: Individual watcher processes are managed by arizona_watcher_sup
and will be terminated when the supervisor shuts down.
""".
-spec stop() -> ok.
stop() ->
    ok.

%% --------------------------------------------------------------------
%% Internal functions
%% --------------------------------------------------------------------

start_reloader_instances([]) ->
    ok;
start_reloader_instances([Rule | Rules]) ->
    case start_reloader_instance(Rule) of
        {ok, _Pid} ->
            start_reloader_instances(Rules);
        {error, Reason} ->
            {error, {reloader_instance_failed, Reason}}
    end.

start_reloader_instance(#{handler := Handler, watcher := WatcherConfig}) ->
    % Create callback that delegates to the handler module via call_reload_callback
    ReloadCallback = fun(Files) -> call_reload_callback(Handler, Files) end,
    % Update watcher config with our callback
    FinalWatcherConfig = WatcherConfig#{callback => ReloadCallback},
    % Start watcher instance
    arizona_watcher_sup:start_child(FinalWatcherConfig).

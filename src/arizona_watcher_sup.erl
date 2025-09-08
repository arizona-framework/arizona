-module(arizona_watcher_sup).
-moduledoc ~"""
Dynamic supervisor for arizona_watcher processes.

Provides a simple_one_for_one supervisor that allows users to dynamically
start and stop arizona_watcher instances at runtime. Useful for managing
multiple file watchers with automatic restart capabilities.

## Features

- **Dynamic child management**: Start watchers on-demand with start_child/1
- **Fault tolerance**: Automatic restart of crashed watchers
- **Process cleanup**: Proper termination of watcher processes
- **Scalable**: Handle multiple independent watcher instances

## Usage

```erlang
% Start the supervisor
{ok, SupPid} = arizona_watcher_sup:start_link().

% Start individual watchers dynamically
ErlangConfig = #{
    directories => ["src"],
    patterns => [".*\\.erl$"],
    callback => fun(Files) -> compile_files(Files) end
},
{ok, WatcherPid1} = arizona_watcher_sup:start_child(ErlangConfig),

AssetConfig = #{
    directories => ["assets"],
    callback => fun(Files) -> build_assets(Files) end
},
{ok, WatcherPid2} = arizona_watcher_sup:start_child(AssetConfig),

% Stop specific watchers when done
ok = arizona_watcher_sup:stop_child(WatcherPid1).
```

## Integration

Can be integrated into application supervision trees:

```erlang
% In your application supervisor
#{
    id => arizona_watcher_sup,
    start => {arizona_watcher_sup, start_link, []},
    type => supervisor
}
```
""".
-behaviour(supervisor).

%% --------------------------------------------------------------------
%% API function exports
%% --------------------------------------------------------------------

-export([start_link/0]).
-export([start_child/1]).
-export([stop_child/1]).

%% --------------------------------------------------------------------
%% Behaviour (supervisor) exports
%% --------------------------------------------------------------------

-export([init/1]).

%% --------------------------------------------------------------------
%% API function definitions
%% --------------------------------------------------------------------

-doc ~"""
Starts the arizona_watcher supervisor.

Returns {ok, Pid} where Pid can be used to start child watcher processes.
""".
-spec start_link() -> supervisor:startlink_ret().
start_link() ->
    supervisor:start_link({local, ?MODULE}, ?MODULE, []).

-doc ~"""
Starts a new arizona_watcher child process with the given configuration.

Returns {ok, ChildPid} on success. The child will be automatically
restarted by the supervisor if it crashes.
""".
-spec start_child(arizona_watcher:config()) -> supervisor:startchild_ret().
start_child(Config) ->
    supervisor:start_child(?MODULE, [Config]).

-doc ~"""
Stops a specific arizona_watcher child process.

The child process will be terminated gracefully and removed from
the supervisor's child list.
""".
-spec stop_child(pid()) -> ok | {error, ErrReason} when
    ErrReason :: running | restarting | not_found | simple_one_for_one.
stop_child(ChildPid) ->
    case supervisor:terminate_child(?MODULE, ChildPid) of
        ok ->
            supervisor:delete_child(?MODULE, ChildPid);
        Error ->
            Error
    end.

%% --------------------------------------------------------------------
%% Behaviour (supervisor) callbacks
%% --------------------------------------------------------------------

-spec init([]) -> {ok, {supervisor:sup_flags(), [supervisor:child_spec()]}}.
init([]) ->
    SupFlags = #{
        strategy => simple_one_for_one,
        % Allow 5 restarts
        intensity => 5,
        % Within 60 seconds
        period => 60
    },

    ChildSpec = #{
        id => arizona_watcher,
        start => {arizona_watcher, start_link, []},
        % Always restart crashed watchers
        restart => permanent,
        % 5 second shutdown timeout
        shutdown => 5000,
        type => worker,
        modules => [arizona_watcher]
    },

    {ok, {SupFlags, [ChildSpec]}}.

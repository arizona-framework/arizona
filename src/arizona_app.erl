-module(arizona_app).
-moduledoc ~"""
Arizona LiveView Application Behavior Module.

## Overview

This module implements the OTP application behavior for the Arizona LiveView
framework. It provides the necessary callbacks for starting and stopping the
Arizona application within an OTP supervision tree.

## Features

- **Application Startup**: Handles Arizona application initialization
- **Supervision Tree**: Integrates with OTP supervision architecture
- **Clean Shutdown**: Proper application termination handling
- **OTP Compliance**: Full adherence to OTP application behavior standards

## Key Functions

- `start/2`: Start the Arizona application and supervision tree
- `stop/1`: Stop the Arizona application cleanly

## Application Lifecycle

### Startup Process

1. **Application Start**: OTP calls start/2 with start type and arguments
2. **Supervisor Launch**: Starts the main Arizona supervisor
3. **Service Initialization**: All Arizona services are started under supervision
4. **Ready State**: Application is ready to handle LiveView requests

### Shutdown Process

1. **Stop Signal**: OTP calls stop/1 during application shutdown
2. **Clean Termination**: Ensures all resources are properly cleaned up
3. **Supervision Tree**: Supervisor handles ordered shutdown of all processes

This module serves as the entry point for the Arizona LiveView framework
within the OTP application architecture.
""".
-behaviour(application).

%% --------------------------------------------------------------------
%% Behaviour (application) exports
%% --------------------------------------------------------------------

-export([start/2]).
-export([stop/1]).

%% --------------------------------------------------------------------
%% Behaviour (application) callbacks
%% --------------------------------------------------------------------

-doc ~"""
Start the Arizona application and supervision tree.

This function is called by the OTP application controller when the Arizona
application is started. It initializes the main supervisor and returns the
supervisor PID for integration with the OTP supervision architecture.

## Examples

```erlang
% Started automatically by OTP
{ok, Pid} = arizona_app:start(normal, []).
```

## Error Handling

Returns `{error, Reason}` if the supervisor fails to start, which will
cause the application startup to fail.
""".
-spec start(StartType, StartArgs) -> StartRet when
    StartType :: application:start_type(),
    StartArgs :: term(),
    StartRet :: {ok, pid()} | {error, term()}.
start(_StartType, _StartArgs) ->
    arizona_sup:start_link().

-doc ~"""
Stop the Arizona application.

This function is called by the OTP application controller when the Arizona
application is being stopped. It performs any necessary cleanup operations
before the application terminates.

## Examples

```erlang
% Called automatically by OTP during shutdown
ok = arizona_app:stop(State).
```

## Cleanup

Currently performs no special cleanup as the supervision tree handles
proper termination of all child processes.
""".
-spec stop(State) -> ok when
    State :: term().
stop(_State) ->
    ok.

-module(arizona_sup).
-moduledoc ~"""
Arizona LiveView Application Supervisor.

## Overview

This module implements the OTP supervisor behavior for the Arizona LiveView
framework. It provides supervision for all Arizona processes and ensures
system reliability through proper fault tolerance and recovery mechanisms.

## Features

- **OTP Supervision**: Full adherence to OTP supervisor behavior standards
- **Fault Tolerance**: Configurable restart strategies for system reliability
- **Process Management**: Manages Arizona service processes
- **One-for-All Strategy**: Restart all processes if any critical process fails
- **Minimal Configuration**: Simple, focused supervisor implementation

## Key Functions

- `start_link/0`: Start the supervisor and register it locally
- `init/1`: Initialize supervisor with configuration and child specifications

## Supervision Strategy

### One-for-All Strategy

The supervisor uses a one-for-all restart strategy, meaning if any child
process terminates, all other child processes are terminated and restarted.
This ensures system consistency across all Arizona components.

### Restart Intensity

- **Strategy**: `one_for_all`
- **Intensity**: 0 (no automatic restarts)
- **Period**: 1 second

The minimal restart intensity prevents cascading failures and allows
for controlled system shutdown during critical errors.

## Child Processes

Currently, the supervisor manages no child processes, serving as a
placeholder for future Arizona service processes that may require
supervision.

This supervisor serves as the root of the Arizona LiveView framework's
supervision tree within the OTP application architecture.
""".
-behaviour(supervisor).

%% --------------------------------------------------------------------
%% API function exports
%% --------------------------------------------------------------------

-export([start_link/0]).

%% --------------------------------------------------------------------
%% Behaviour (supervisor) exports
%% --------------------------------------------------------------------

-export([init/1]).

%% --------------------------------------------------------------------
%% API function definitions
%% --------------------------------------------------------------------

-doc ~"""
Start the Arizona supervisor and register it locally.

Creates a new supervisor process registered locally as this module and
initialized with an empty argument list. The supervisor will manage
Arizona service processes according to the configured supervision strategy.

## Examples

```erlang
% Started automatically by arizona_app
{ok, Pid} = arizona_sup:start_link().
```

## Error Handling

Returns `{error, Reason}` if the supervisor fails to start, which will
propagate to the application startup process.
""".
-spec start_link() -> supervisor:startlink_ret().
start_link() ->
    supervisor:start_link({local, ?MODULE}, ?MODULE, []).

%% --------------------------------------------------------------------
%% Behaviour (supervisor) callbacks
%% --------------------------------------------------------------------

-doc ~"""
Initialize the supervisor with configuration and child specifications.

Sets up the supervisor with a one-for-all restart strategy and provides
child specifications for any Arizona service processes that need supervision.

## Supervision Configuration

- **Strategy**: `one_for_all` - restart all children if any child fails
- **Intensity**: 0 - no automatic restarts after failure
- **Period**: 1 second - time window for restart intensity

## Child Specifications

Currently returns an empty list of child specifications, serving as a
placeholder for future Arizona services that may require supervision.

## Examples

```erlang
% Called automatically by supervisor:start_link/3
{ok, {SupFlags, ChildSpecs}} = arizona_sup:init([]).
```
""".
-spec init(Args) -> {ok, {SupFlags, [ChildSpec]}} when
    Args :: term(),
    SupFlags :: supervisor:sup_flags(),
    ChildSpec :: supervisor:child_spec().
init(_Args) ->
    SupFlags = #{
        strategy => one_for_all,
        intensity => 0,
        period => 1
    },
    ChildSpecs = [],
    {ok, {SupFlags, ChildSpecs}}.

-module(arizona_live).
-moduledoc ~"""
Provides LiveView process management and behavior for Arizona applications.

## Overview

The live module implements the gen_server behavior for managing LiveView processes,
providing a stateful process for each LiveView component. It defines the LiveView
behavior callbacks and manages the LiveView lifecycle including mounting, rendering,
and event handling.

## Features

- **Process Management**: Gen_server implementation for LiveView process lifecycle
- **Behavior Definition**: Defines callbacks for mount, render, and event handling
- **State Management**: Manages LiveView state and socket throughout lifecycle
- **Event Handling**: Processes client events and updates LiveView state
- **Callback Delegation**: Safe callback invocation with optional callback support
- **Mode Management**: Supports different rendering modes (render, diff, hierarchical)

## Key Functions

- `start_link/2`: Start a new LiveView process
- `mount/2`: Mount a LiveView with request context
- `render/1`: Render the LiveView to generate HTML/structure
- `handle_event/3`: Handle client events and update state
- `set_mode/2`: Set the rendering mode for the LiveView
- `call_mount_callback/3`, `call_render_callback/2`: Callback wrapper functions

## LiveView Lifecycle

1. **Process Start**: `start_link/2` creates a new gen_server process
2. **Mount**: `mount/2` calls the LiveView's mount callback with request context
3. **Render**: `render/1` calls the LiveView's render callback to generate output
4. **Event Loop**: `handle_event/3` processes client events and updates state
5. **Info Handling**: `handle_info/2` processes Erlang messages sent to the process

## Callback Behaviors

LiveView modules must implement the arizona_live behavior:
- `mount/2`: Initialize LiveView state with request context
- `render/1`: Generate template output from current state
- `handle_event/3`: Process client events (optional)
- `handle_info/2`: Process Erlang messages (optional)

The module provides safe callback invocation with fallback behavior for optional
callbacks, ensuring robust error handling throughout the LiveView lifecycle.
""".

-behaviour(gen_server).

%% --------------------------------------------------------------------
%% API function exports
%% --------------------------------------------------------------------

-export([start_link/2]).
-export([mount/2]).
-export([render/1]).
-export([handle_event/3]).
-export([set_mode/2]).
-export([call_mount_callback/3]).
-export([call_render_callback/2]).
-export([call_handle_event_callback/4]).
-export([call_handle_info_callback/3]).

%% --------------------------------------------------------------------
%% Gen_server callback exports
%% --------------------------------------------------------------------

-export([init/1]).
-export([handle_call/3]).
-export([handle_cast/2]).
-export([handle_info/2]).

%% --------------------------------------------------------------------
%% Types exports
%% --------------------------------------------------------------------

-export_type([state/0]).

%% --------------------------------------------------------------------
%% Types definitions
%% --------------------------------------------------------------------

%% Internal state record for LiveView processes
-record(state, {
    module :: atom(),
    socket :: arizona_socket:socket()
}).

-doc ~"""
Opaque state type for LiveView gen_server processes.

Contains the LiveView module and current socket state, managed internally
by the gen_server implementation.
""".
-opaque state() :: #state{}.

%% --------------------------------------------------------------------
%% Behavior callback definitions
%% --------------------------------------------------------------------

-doc ~"""
Mount callback for initializing LiveView state.

Called when the LiveView is first mounted with the HTTP request context.
Should initialize the LiveView's state and return the updated socket.
""".
-callback mount(Req, Socket) -> Socket when
    Req :: arizona_request:request(),
    Socket :: arizona_socket:socket().

-doc ~"""
Render callback for generating LiveView output.

Called to render the LiveView's template with the current socket state.
Should return the socket with accumulated HTML or hierarchical structure.
""".
-callback render(Socket) -> Socket when
    Socket :: arizona_socket:socket().

-doc ~"""
Handle event callback for processing client events.

Optional callback for handling client-side events such as clicks, form
submissions, and other user interactions. Returns either a noreply or
reply response with the updated socket.
""".
-callback handle_event(Event, Params, Socket) -> {noreply, Socket} | {reply, Reply, Socket} when
    Event :: binary(),
    Params :: map(),
    Socket :: arizona_socket:socket(),
    Reply :: term().

-doc ~"""
Handle info callback for processing Erlang messages.

Optional callback for handling Erlang messages sent to the LiveView process.
Useful for handling timers, notifications, and other system messages.
""".
-callback handle_info(Info, Socket) -> {noreply, Socket} when
    Info :: term(),
    Socket :: arizona_socket:socket().

-optional_callbacks([handle_event/3, handle_info/2]).

%% --------------------------------------------------------------------
%% API function definitions
%% --------------------------------------------------------------------

-doc ~"""
Start a new LiveView process.

Creates a new gen_server process for the specified LiveView module with
the initial socket state. The process will handle the LiveView lifecycle
and maintain state throughout its lifetime.

## Examples

```erlang
1> Socket = arizona_socket:new(#{}).
#socket{...}
2> arizona_live:start_link(my_live_view, Socket).
{ok, <0.123.0>}
```
""".
-spec start_link(Module, Socket) -> {ok, Pid} | {error, Reason} when
    Module :: atom(),
    Socket :: arizona_socket:socket(),
    Pid :: pid(),
    Reason :: term().
start_link(Module, Socket) ->
    gen_server:start_link(?MODULE, {Module, Socket}, []).

-doc ~"""
Mount a LiveView with request context.

Calls the LiveView's mount callback with the HTTP request context,
allowing the LiveView to initialize its state based on the request.
Returns the updated socket after mounting.

## Examples

```erlang
1> {ok, Pid} = arizona_live:start_link(my_live_view, Socket).
{ok, <0.123.0>}
2> Req = arizona_request:new(#{method => get, path => "/users"}).
#request{...}
3> arizona_live:mount(Pid, Req).
#socket{...}
```
""".
-spec mount(Pid, Req) -> Socket when
    Pid :: pid(),
    Req :: arizona_request:request(),
    Socket :: arizona_socket:socket().
mount(Pid, Req) ->
    gen_server:call(Pid, {mount, Req}).

-doc ~"""
Render the LiveView to generate output.

Calls the LiveView's render callback to generate HTML output or hierarchical
structure based on the current socket state. Returns the updated socket
after rendering.

## Examples

```erlang
1> arizona_live:render(Pid).
#socket{html_acc = [<<"<div>Hello World</div>">>], ...}
```
""".
-spec render(Pid) -> Socket when
    Pid :: pid(),
    Socket :: arizona_socket:socket().
render(Pid) ->
    gen_server:call(Pid, render).

-doc ~"""
Handle a client event in the LiveView.

Processes a client event such as a click or form submission, calling the
LiveView's handle_event callback if implemented. Returns either a noreply
or reply response with the updated socket.

## Examples

```erlang
1> arizona_live:handle_event(Pid, <<"click">>, #{<<"target">> => <<"button">>}).
{noreply, #socket{...}}
2> arizona_live:handle_event(Pid, <<"submit">>, #{<<"form">> => #{<<"name">> => <<"John">>}}).
{reply, <<"success">>, #socket{...}}
```
""".
-spec handle_event(Pid, Event, Params) -> {noreply, Socket} | {reply, Reply, Socket} when
    Pid :: pid(),
    Event :: binary(),
    Params :: map(),
    Socket :: arizona_socket:socket(),
    Reply :: term().
handle_event(Pid, Event, Params) ->
    gen_server:call(Pid, {handle_event, Event, Params}).

-doc ~"""
Set the rendering mode for the LiveView.

Updates the LiveView's socket to use the specified rendering mode, which
affects how templates are processed and what output is generated.

## Examples

```erlang
1> arizona_live:set_mode(Pid, hierarchical).
ok
2> arizona_live:set_mode(Pid, diff).
ok
```
""".
-spec set_mode(Pid, Mode) -> ok when
    Pid :: pid(),
    Mode :: arizona_socket:mode().
set_mode(Pid, Mode) ->
    gen_server:cast(Pid, {set_mode, Mode}).

-doc ~"""
Call the mount callback for a LiveView module.

Wrapper function for safely invoking the LiveView's mount callback with
the provided request and socket. Used internally by the LiveView process.

## Examples

```erlang
1> arizona_live:call_mount_callback(my_live_view, Req, Socket).
#socket{...}
```
""".
-spec call_mount_callback(Module, Req, Socket) -> Socket when
    Module :: atom(),
    Req :: arizona_request:request(),
    Socket :: arizona_socket:socket().
call_mount_callback(Module, Req, Socket) ->
    apply(Module, mount, [Req, Socket]).

-doc ~"""
Call the render callback for a LiveView module.

Wrapper function for safely invoking the LiveView's render callback with
the provided socket. Used internally by the LiveView process.

## Examples

```erlang
1> arizona_live:call_render_callback(my_live_view, Socket).
#socket{...}
```
""".
-spec call_render_callback(Module, Socket) -> Socket when
    Module :: atom(),
    Socket :: arizona_socket:socket().
call_render_callback(Module, Socket) ->
    apply(Module, render, [Socket]).

-doc ~"""
Call the handle_event callback for a LiveView module if it exists.

Wrapper function for safely invoking the LiveView's handle_event callback
if it's implemented, otherwise returns a noreply response. Used internally
by the LiveView process.

## Examples

```erlang
1> arizona_live:call_handle_event_callback(my_live_view, <<"click">>, #{}, Socket).
{noreply, #socket{...}}
```
""".
-spec call_handle_event_callback(Module, Event, Params, Socket) ->
    {noreply, Socket} | {reply, Reply, Socket}
when
    Module :: atom(),
    Event :: binary(),
    Params :: map(),
    Socket :: arizona_socket:socket(),
    Reply :: term().
call_handle_event_callback(Module, Event, Params, Socket) ->
    case erlang:function_exported(Module, handle_event, 3) of
        true ->
            apply(Module, handle_event, [Event, Params, Socket]);
        false ->
            {noreply, Socket}
    end.

-doc ~"""
Call the handle_info callback for a LiveView module if it exists.

Wrapper function for safely invoking the LiveView's handle_info callback
if it's implemented, otherwise returns a noreply response. Used internally
by the LiveView process.

## Examples

```erlang
1> arizona_live:call_handle_info_callback(my_live_view, timeout, Socket).
{noreply, #socket{...}}
```
""".
-spec call_handle_info_callback(Module, Info, Socket) -> {noreply, Socket} when
    Module :: atom(),
    Info :: term(),
    Socket :: arizona_socket:socket().
call_handle_info_callback(Module, Info, Socket) ->
    case erlang:function_exported(Module, handle_info, 2) of
        true ->
            apply(Module, handle_info, [Info, Socket]);
        false ->
            {noreply, Socket}
    end.

%% --------------------------------------------------------------------
%% Gen_server callback implementations
%% --------------------------------------------------------------------

-doc ~"""
Initialize the LiveView gen_server process.

Called when the LiveView process is started, initializing the internal
state with the LiveView module and socket.
""".
-spec init(InitArgs) -> {ok, State} when
    InitArgs :: {Module, Socket},
    Module :: atom(),
    Socket :: arizona_socket:socket(),
    State :: state().
init({Module, Socket}) ->
    {ok, #state{module = Module, socket = Socket}}.

-doc ~"""
Handle synchronous calls to the LiveView process.

Processes mount, render, and handle_event requests, delegating to the
appropriate callback functions and updating the process state.
""".
-spec handle_call(Message, From, State) -> {reply, Reply, State} when
    Message :: {mount, arizona_request:request()} | render | {handle_event, binary(), map()},
    From :: {pid(), term()},
    State :: state(),
    Reply :: term().
handle_call({mount, Req}, _From, #state{module = Module, socket = Socket} = State) ->
    UpdatedSocket = call_mount_callback(Module, Req, Socket),
    {reply, UpdatedSocket, State#state{socket = UpdatedSocket}};
handle_call(render, _From, #state{module = Module, socket = Socket} = State) ->
    UpdatedSocket = call_render_callback(Module, Socket),
    {reply, UpdatedSocket, State#state{socket = UpdatedSocket}};
handle_call(
    {handle_event, Event, Params},
    _From,
    #state{module = Module, socket = Socket} = State
) ->
    case call_handle_event_callback(Module, Event, Params, Socket) of
        {noreply, UpdatedSocket} ->
            {reply, {noreply, UpdatedSocket}, State#state{socket = UpdatedSocket}};
        {reply, Reply, UpdatedSocket} ->
            {reply, {reply, Reply, UpdatedSocket}, State#state{socket = UpdatedSocket}}
    end.

-doc ~"""
Handle asynchronous casts to the LiveView process.

Processes set_mode requests and other asynchronous operations without
blocking the caller.
""".
-spec handle_cast(Request, State) -> {noreply, State} when
    Request :: term(),
    State :: state().
handle_cast({set_mode, Mode}, #state{socket = Socket} = State) ->
    Socket1 = arizona_socket:set_mode(Mode, Socket),
    {noreply, State#state{socket = Socket1}};
handle_cast(_Request, State) ->
    {noreply, State}.

-doc ~"""
Handle Erlang messages sent to the LiveView process.

Delegates to the LiveView's handle_info callback if implemented,
updating the process state with the returned socket.
""".
-spec handle_info(Info, State) -> {noreply, State} when
    Info :: term(),
    State :: state().
handle_info(Info, #state{module = Module, socket = Socket} = State) ->
    {noreply, UpdatedSocket} = call_handle_info_callback(Module, Info, Socket),
    {noreply, State#state{socket = UpdatedSocket}}.

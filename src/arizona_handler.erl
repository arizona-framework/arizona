-module(arizona_handler).
-moduledoc ~"""
Provides HTTP handler functionality for Arizona LiveView requests.

## Overview

The handler module implements the Cowboy HTTP handler behavior for processing
Arizona LiveView requests. It serves as the entry point for HTTP requests,
creating the necessary Arizona abstractions and orchestrating the LiveView
lifecycle for request handling.

## Features

- **Cowboy Integration**: Implements cowboy_handler behavior for HTTP request processing
- **LiveView Orchestration**: Manages the complete LiveView request lifecycle
- **Request Abstraction**: Converts Cowboy requests to Arizona request format
- **Socket Management**: Creates and manages Arizona sockets for LiveView processing
- **Error Handling**: Comprehensive error handling with proper HTTP status codes
- **Stateful Component Support**: Initializes stateful components with proper state management

## Key Functions

- `init/2`: Cowboy handler entry point for processing HTTP requests
- `handle_live_request/3`: Core LiveView request processing logic

## Request Flow

1. **Request Reception**: Cowboy invokes init/2 with HTTP request
2. **Route Resolution**: Extract LiveView module from routing state
3. **Request Conversion**: Convert Cowboy request to Arizona request format
4. **Socket Creation**: Initialize Arizona socket with stateful state
5. **Mount Callback**: Invoke LiveView mount callback for initialization
6. **Render Callback**: Invoke LiveView render callback for HTML generation
7. **Response Generation**: Return HTML response with proper headers

The handler provides a clean abstraction layer between Cowboy's HTTP handling
and Arizona's LiveView processing, ensuring proper error handling and state
management throughout the request lifecycle.
""".

-behaviour(cowboy_handler).

%% --------------------------------------------------------------------
%% API function exports
%% --------------------------------------------------------------------

-export([init/2]).

%% --------------------------------------------------------------------
%% API function definitions
%% --------------------------------------------------------------------

-doc ~"""
Cowboy handler entry point for processing HTTP requests.

Called by Cowboy when an HTTP request matches the Arizona handler route.
Extracts the LiveView module from the routing state and delegates to
the LiveView request processing logic.

## Examples

```erlang
% Called automatically by Cowboy - not typically invoked directly
1> Req = #{method => <<"GET">>, path => "/users", ...}.
#{...}
2> State = #{handler => user_live}.
#{...}
3> arizona_handler:init(Req, State).
{ok, UpdatedReq, State}
```
""".
-spec init(Req, State) -> {ok, Req1, State} when
    Req :: cowboy_req:req(),
    State :: map(),
    Req1 :: cowboy_req:req().
init(Req, State) when is_map(State) ->
    % Extract route information from state
    Handler = maps:get(handler, State),
    handle_live_request(Handler, Req, State).

%% --------------------------------------------------------------------
%% Private functions
%% --------------------------------------------------------------------

%% Handle LiveView requests with complete lifecycle management
-spec handle_live_request(LiveModule, Req, State) -> {ok, Req1, State} when
    LiveModule :: atom(),
    Req :: cowboy_req:req(),
    State :: map(),
    Req1 :: cowboy_req:req().
handle_live_request(LiveModule, Req, State) when is_atom(LiveModule), is_map(State) ->
    try
        % Create arizona request abstraction
        ArizonaReq = arizona_request:from_cowboy(Req),

        % Create Arizona socket and call mount via arizona_live callback wrapper
        Socket = arizona_socket:new(#{}),
        StatefulState = arizona_stateful:new(root, LiveModule, #{}),
        Socket1 = arizona_socket:put_stateful_state(StatefulState, Socket),
        Socket2 = arizona_live:call_mount_callback(LiveModule, ArizonaReq, Socket1),

        % Render the LiveView via arizona_live callback wrapper
        Socket3 = arizona_live:call_render_callback(LiveModule, Socket2),

        % Get final HTML
        Html = arizona_socket:get_html(Socket3),
        Req1 = cowboy_req:reply(200, #{~"content-type" => ~"text/html"}, Html, Req),
        {ok, Req1, State}
    catch
        Error:Reason:Stacktrace ->
            ErrorMsg = io_lib:format("LiveView Error: ~p:~p~nStacktrace: ~p", [
                Error, Reason, Stacktrace
            ]),
            Req2 = cowboy_req:reply(500, #{}, iolist_to_binary(ErrorMsg), Req),
            {ok, Req2, State}
    end.

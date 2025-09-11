-module(arizona_view_handler).
-moduledoc ~"""
Cowboy HTTP handler for serving static HTML pages from Arizona views.

Implements the `cowboy_handler` behavior to handle HTTP requests by
mounting Arizona views and rendering them to static HTML. Used for
initial page loads.

## Request Flow

1. Receive HTTP request with view module, mount arguments, and middlewares
2. Process middleware chain - continue or halt early
3. Create Arizona request wrapper (if continued)
4. Call view's `mount/2` callback
5. Render view through layout system
6. Return HTML response with 200 status
7. Handle errors with 500 status and error details

## Use Cases

- **Initial Page Loads**: First HTML before WebSocket upgrade
- **Server-Side Rendering**: Complete HTML generation on server

## Error Handling

All rendering errors are caught and converted to 500 responses
with detailed error information for debugging.

## Example Configuration

```erlang
%% In Arizona server routes
{view, ~"/users/[:id]", users_view, #{}, [
    {auth_middleware, #{jwt_secret => ~"secret123"}}
]}
```
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
Handles HTTP request with middleware processing and renders view to static HTML.

Processes middleware chain first, then mounts the view, renders it through
the layout system, and returns the HTML response. Provides comprehensive
error handling with stacktrace information for debugging.
""".
-spec init(CowboyRequest, State) -> {ok, CowboyRequest1, State} when
    CowboyRequest :: cowboy_req:req(),
    State :: {ViewModule, MountArg},
    ViewModule :: module(),
    MountArg :: arizona_view:mount_arg(),
    CowboyRequest1 :: cowboy_req:req().
init(CowboyRequest, {ViewModule, MountArg} = State) ->
    try
        ArizonaRequest = arizona_cowboy_request:new(CowboyRequest),
        View = arizona_view:call_mount_callback(ViewModule, MountArg, ArizonaRequest),
        {Html, _RenderView} = arizona_renderer:render_layout(View),
        CowboyRequest1 = cowboy_req:reply(
            200, #{~"content-type" => ~"text/html; charset=utf-8"}, Html, CowboyRequest
        ),
        {ok, CowboyRequest1, State}
    catch
        Error:Reason:Stacktrace ->
            ErrorMsg = io_lib:format("Error: ~p:~p~nStacktrace: ~p", [
                Error, Reason, Stacktrace
            ]),
            CowboyRequest2 = cowboy_req:reply(500, #{}, iolist_to_binary(ErrorMsg), CowboyRequest),
            {ok, CowboyRequest2, State}
    end.

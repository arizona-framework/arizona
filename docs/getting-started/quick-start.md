# Quick Start

- [Creating Your First View](#creating-your-first-view)
- [Configuring the Server](#configuring-the-server)
- [Starting the Application](#starting-the-application)

## Creating Your First View

A view in Arizona is an Erlang module that implements the `arizona_view`
behaviour. Each view must export three callbacks:

- `mount/2` -- initializes the view state when a user visits the page.
- `render/1` -- returns a template describing the HTML to display.
- `handle_event/3` -- responds to user interactions sent from the browser.

Here is a minimal counter view that increments a number each time the user
clicks a button:

```erlang
-module(my_counter_view).
-behaviour(arizona_view).
-export([mount/2, render/1, handle_event/3]).

mount(_MountArg, Req) ->
    View = arizona_view:new(?MODULE, #{count => 0}, none),
    View.

render(Bindings) ->
    arizona_template:from_html(<<"
        <div>
            <h1>Count: {get_binding(count, Bindings)}</h1>
            <button az-click=\"increment\">+</button>
        </div>
    ">>).

handle_event(~"increment", _Params, View) ->
    State = arizona_view:get_state(View),
    Count = arizona_stateful:get_binding(count, State),
    State1 = arizona_stateful:put_binding(count, Count + 1, State),
    View1 = arizona_view:update_state(State1, View),
    {[], View1}.
```

Walking through this module:

1. **`mount/2`** receives the mount argument (configured in the route, covered
   below) and the current request. It creates a new view with an initial state
   where `count` is `0`.
2. **`render/1`** receives the current bindings and produces an HTML template
   using `arizona_template:from_html/1`. The expression
   `{get_binding(count, Bindings)}` interpolates the `count` value into the
   rendered output. The `az-click` attribute tells the client to send an
   `"increment"` event to the server when the button is clicked.
3. **`handle_event/3`** receives the event name, any parameters from the
   client, and the current view. It reads the current count from the view
   state, increments it, and returns the updated view. Arizona then computes
   the minimal diff and pushes only the changed parts to the browser.

## Configuring the Server

Arizona uses [Cowboy](https://github.com/ninenines/cowboy) as its HTTP server.
Configuration is provided through a standard Erlang `sys.config` file. At a
minimum, you need to define the routes that map URL paths to your views.

Create (or update) your `config/sys.config` file:

```erlang
[{arizona, #{
    server => #{
        routes => [
            {view, ~"/", my_counter_view, #{}, []}
        ]
    }
}}].
```

The route tuple follows the format
`{view, Path, Module, MountArg, Middlewares}`:

- **`view`** -- the route type (other types include `controller`, `websocket`,
  and `asset`).
- **`"/"`** -- the URL path to match.
- **`my_counter_view`** -- the view module that handles requests for this path.
- **`#{}`** -- a map passed as the first argument to `mount/2`, useful for
  passing configuration or initial data to the view.
- **`[]`** -- a list of middleware modules to run before the view mounts.

By default, the server listens on port **1912**. You can override this and
other server options in the same configuration map. Configuration values are
also accessible at runtime through `arizona_config:get/0`.

## Starting the Application

With your view module compiled and your `sys.config` in place, start the
application using the rebar3 shell:

```shell
rebar3 shell
```

This compiles your project, boots the Arizona application (which starts the
`arizona_app` module, its supervisor tree, the file reloader, and the Cowboy
server), and drops you into an interactive Erlang shell.

Once the shell is running, open your browser and navigate to:

```text
http://localhost:1912
```

You should see the counter view rendered on the page. Clicking the **+** button
will increment the count in real time without a full page reload -- the
WebSocket connection handles the event round-trip and differential DOM update
automatically.

If you need to retrieve the server address programmatically (for example, in
tests or scripts), call:

```erlang
{ok, IpAddress, Port} = arizona_server:get_address().
```

This returns the IP address and port where the server is listening.

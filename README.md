# Arizona

![arizona_256x256](https://github.com/arizona-framework/arizona/assets/35941533/88b76a0c-0dfc-4f99-8608-b0ebd9c9fbd9)

Arizona is a modern, real-time web framework for Erlang that delivers high-performance web applications
through compile-time optimization and differential rendering. Built for scalability and developer
experience, it combines the reliability of Erlang/OTP with modern web development paradigms.

## ⚠️ Warning

Work in progress.

Use it at your own risk, as the API may change at any time.

## Architecture Overview

Arizona follows a component-based architecture with three main layers:

### **Template System**

- **Compile-time optimization** via parse transforms for maximum performance
- **Multiple syntax options**: HTML strings, Erlang terms, or Markdown with template expressions
- **Template DSL** with plain HTML, `{}` Erlang expressions, and `\{` escaping for literal braces
- **Erlang term syntax** for type-safe template construction with full editor support
- **GitHub Flavored Markdown support** with template syntax preservation for content-driven applications
- **Differential rendering** for minimal DOM updates
- **Static site generation** support for SEO and deployment flexibility

### **Component Types**

- **Views** - Top-level page components with full lifecycle management (require `id` field)
- **Stateful Components** - Interactive components with persistent state (require `id` field)
- **Stateless Components** - Pure rendering functions for simple UI elements
- **Layouts** - Document wrappers with slot-based content insertion
- **Collections** - List and map rendering with optimized iteration patterns

### **Real-time Infrastructure**

- **WebSocket transport** with JSON message protocol
- **Live processes** managing connected clients with state synchronization
- **PubSub system** for decoupled inter-process communication
- **Differential updates** minimizing network traffic through smart diffing

## Key Features

- **Real-time WebSocket updates** with automatic client synchronization
- **Hierarchical component rendering** supporting complex nested structures
- **Compile-time template optimization** for maximum runtime performance
- **Type-safe stateful and stateless components** with behavior validation
- **Efficient differential DOM updates** reducing browser workload
- **Simple template syntax** using plain HTML with `{}` Erlang expressions
- **GitHub Flavored Markdown processing** with Arizona template integration
- **Static site generation** for deployment flexibility and SEO optimization
- **File watching infrastructure** for custom development automation
- **Unified middleware system** for request processing across all route types
- **Simple plugin system** for configuration transformation and extensibility

## Quick Start

### 1. Add Dependency

In your `rebar.config`:

```erlang
{deps, [
    {arizona, {git, "https://github.com/arizona-framework/arizona", {branch, "main"}}}
]}.
```

### 2. Create Your First View

```erlang
-module(home_view).
-behaviour(arizona_view).
-compile({parse_transform, arizona_parse_transform}).
-export([mount/2, render/1, handle_event/3]).

mount(_Arg, _Request) ->
    arizona_view:new(
        ?MODULE,
        #{
            id => ~"home",
            greeting => ~"Hello, Arizona!",
            count => 0
        },
        none  % No layout
    ).

render(Bindings) ->
    arizona_template:from_html(~"""
    <div id="{arizona_template:get_binding(id, Bindings)}">
        <h1>{arizona_template:get_binding(greeting, Bindings)}</h1>
        <p>Clicked: {arizona_template:get_binding(count, Bindings)} times</p>
        <button onclick="arizona.pushEvent('increment')">Click me</button>
    </div>
    """).

handle_event(~"increment", _Params, View) ->
    State = arizona_view:get_state(View),
    Count = arizona_stateful:get_binding(count, State),
    NewState = arizona_stateful:put_binding(count, Count + 1, State),
    {[], arizona_view:update_state(NewState, View)}.
```

### 3. Configure and Start Server

#### Using sys.config (Recommended)

Create `config/sys.config`:

```erlang
[
    {arizona, [
        {server, #{
            enabled => true,
            scheme => http,  % or https
            transport_opts => [{port, 1912}],  % Cowboy/Ranch transport options
            proto_opts => #{env => #{custom_option => value}},  % Optional Cowboy protocol options
            routes => [
                {view, ~"/", home_view, #{}, []},  % Empty middleware list
                {websocket, ~"/live", #{}, []},    % WebSocket options + middleware list
                {controller, ~"/api/presence", my_api_controller, #{}, []},  % Plain Cowboy handler
                {asset, ~"/assets", {priv_dir, arizona, ~"static/assets"}, []}  % Required for live features
            ]
        }},
        {reloader, #{
            enabled => true,  % Enable file watcher coordination
            rules => [
                #{
                    handler => {my_erlang_handler, #{profile => dev}},  % {Module, Options}
                    watcher => #{
                        directories => ["src"],
                        patterns => [".*\\.erl$"],
                        debounce_ms => 100
                    }
                }
            ]
        }}
    ]}
].
```

Then start the application:

```erlang
{ok, _Started} = application:ensure_all_started(arizona).
```

> [!Note]
>
> Add to your `rebar.config` to automatically load the config with `rebar3 shell`:
>
> ```erlang
> {shell, [{config, "config/sys.config"}]}.
> ```

#### Alternative: Programmatic Configuration

```erlang
% Configure the application environment
application:set_env([{arizona, [
    {server, #{
        enabled => true,
        scheme => http,
        transport_opts => [{port, 1912}],
        routes => [
            {view, ~"/", home_view, #{}, []},
            {websocket, ~"/live", #{}, []},
            {asset, ~"/assets", {priv_dir, arizona, ~"static/assets"}, []}
        ]
    }},
    {reloader, #{enabled => false}}  % Typically disabled when using programmatic config
]}]),

% Start the application
{ok, _Started} = application:ensure_all_started(arizona).

% You must implement your own handler modules:

% Example:
-module(my_erlang_handler).
-behaviour(arizona_reloader).
-export([reload/2]).

reload(Files, Options) ->
    Profile = maps:get(profile, Options, default),
    io:format("Compiling ~p with profile ~p~n", [Files, Profile]),
    % Your custom build logic here
    ok.
```

> **Note:** The asset route `{asset, ~"/assets", {priv_dir, arizona, ~"static/assets"}}` is required
for Arizona's WebSocket/live functionality to work as it serves the necessary JavaScript client files.

## Live Demo

Try the complete working examples:

```bash
# Clone and run the test server
$ git clone https://github.com/arizona-framework/arizona
$ cd arizona
$ ./scripts/start_test_server.sh
```

Then visit:

- **<http://localhost:8080/counter>** - Simple stateful interactions
- **<http://localhost:8080/todo>** - CRUD operations and list management
- **<http://localhost:8080/modal>** - Dynamic overlays and slot composition
- **<http://localhost:8080/datagrid>** - Sortable tables with complex data
- **<http://localhost:8080/realtime>** - Real-time PubSub updates and live data
- **<http://localhost:8080/blog>** - Markdown template processing with Arizona syntax
- **<http://localhost:8080/>** - Static blog home page
- **<http://localhost:8080/about>** - Static about page
- **<http://localhost:8080/post/hello-world>** - Dynamic blog post routing
- **<http://localhost:8080/post/arizona-static>** - Blog post about Arizona static generation

### Explore the Code

Each demo corresponds to complete source code in [`test/support/e2e/`](https://github.com/arizona-framework/arizona/tree/main/test/support/e2e/):

- **[Counter App](https://github.com/arizona-framework/arizona/tree/main/test/support/e2e/counter/)**
  - Layout + View with event handling and PubSub integration
- **[Todo App](https://github.com/arizona-framework/arizona/tree/main/test/support/e2e/todo/)**
  - Complex state management and CRUD operations
- **[Modal System](https://github.com/arizona-framework/arizona/tree/main/test/support/e2e/modal/)**
  - Component composition, slots, and dynamic overlays
- **[Data Grid](https://github.com/arizona-framework/arizona/tree/main/test/support/e2e/datagrid/)**
  - Advanced data presentation and sorting functionality
- **[Real-time App](https://github.com/arizona-framework/arizona/tree/main/test/support/e2e/realtime/)**
  - Live data updates and WebSocket communication
- **[Blog App](https://github.com/arizona-framework/arizona/tree/main/test/support/e2e/blog/)**
  - Markdown template processing with Arizona syntax integration
- **[Static Blog](https://github.com/arizona-framework/arizona/tree/main/test/support/e2e/static/)**
  - Static site generation with layouts and dynamic routing

### More Examples

For additional examples and real-world usage patterns, check out the dedicated example repository:

- **[Arizona Example](https://github.com/arizona-framework/arizona_example)**
  - Complete application examples and patterns

## Template Syntax

Arizona templates combine HTML with Erlang expressions using a simple and intuitive syntax:

### Basic Expressions

```erlang
% Inline HTML template
arizona_template:from_html(~"""
<div>
    <h1>{arizona_template:get_binding(title, Bindings)}</h1>
    <p>User: {arizona_template:get_binding(username, Bindings)}</p>
</div>
""")

% File-based template with compile-time optimization
arizona_template:from_html({file, "templates/user.html"})
arizona_template:from_html({priv_file, myapp, "templates/user.html"})
```

### Erlang Term-Based Templates

As an alternative to HTML strings, use pure Erlang terms for type-safe template construction:

```erlang
% Erlang term syntax with full editor support
arizona_template:from_erl([
    {'div', [], [
        {h1, [], [arizona_template:get_binding(title, Bindings)]},
        {p, [], [
            ~"User: ",
            arizona_template:get_binding(username, Bindings)
        ]}
    ]}
])

% Elements are tuples: {Tag, Attributes, Children}
% Note: 'div' must be quoted since it's the division operator in Erlang
arizona_template:from_erl([
    {'div', [{id, ~"main"}, {class, ~"container"}], [
        {h1, [], [~"Welcome"]}  % Other tags don't need quotes
    ]}
])

% Boolean attributes and void elements (self-closing)
arizona_template:from_erl([
    % Renders as: <input disabled type="text" hidden />
    {input, [disabled, {type, ~"text"}, hidden], []}
])

arizona_template:from_erl([
    {br, [], []},           % <br />
    {hr, [], []},           % <hr />
    {img, [{src, ~"logo.png"}, {alt, ~"Logo"}], []}  % <img src="logo.png" alt="Logo" />
])

% Nested components work seamlessly
arizona_template:from_erl([
    {'div', [], [
        arizona_template:render_stateful(counter_component, #{
            id => ~"counter",
            count => 0
        })
    ]}
])
```

**Benefits:**

- Full editor support with syntax highlighting and auto-completion
- Compile-time validation of element structure
- Automatic self-closing for void elements (XHTML compatible)
- No string escaping issues for dynamic attributes
- Type-safe component composition

### Stateful Component Rendering

Render interactive components that maintain their own state between updates:

```erlang
arizona_template:from_html(~"""
<div>
    {arizona_template:render_stateful(counter_component, #{
        id => ~"my_counter",
        count => 0
    })}
</div>
""")
```

### Stateless Component Rendering

Render pure functions that return templates based solely on their input:

```erlang
arizona_template:from_html(~"""
<div>
    {arizona_template:render_stateless(my_module, render_header, #{
        title => ~"Welcome",
        user => ~"John"
    })}
</div>
""")
```

### List Rendering

Iterate over lists with automatic compile-time optimization via parse transforms:

```erlang
arizona_template:from_html(~"""
<ul>
    {arizona_template:render_list(fun(Item) ->
        arizona_template:from_html(~"""
        <li>{arizona_template:get_binding(name, Item)}</li>
        """)
    end, arizona_template:get_binding(items, Bindings))}
</ul>
""")
```

### Map Rendering

Iterate over key-value pairs with optimized rendering performance:

```erlang
arizona_template:from_html(~"""
<div>
    {arizona_template:render_map(fun({Key, Value}) ->
        arizona_template:from_html(~"""
        <p>{Key}: {Value}</p>
        """)
    end, #{~"name" => ~"Arizona", ~"type" => ~"Framework"})}
</div>
""")
```

### Slot Rendering

Insert dynamic content into predefined slots for flexible component composition:

```erlang
arizona_template:from_html(~"""
<div class="modal">
    <h1>{arizona_template:render_slot(arizona_template:get_binding(header, Bindings))}</h1>
    <div class="content">
        {arizona_template:render_slot(arizona_template:get_binding(inner_block, Bindings))}
    </div>
</div>
""")
```

### Comments and Escaping

Use template comments and escape braces for literal output in CSS or JavaScript:

```erlang
arizona_template:from_html(~"""
<div>
    {% This is a comment and is not rendered }
    <h1>{arizona_template:get_binding(title, Bindings)}</h1>
    <style>
        .css-rule \{ color: blue; }  /* \{ renders as literal { */
    </style>
</div>
""")
```

## Markdown Template Support

Arizona includes GitHub Flavored Markdown processing with full template syntax preservation for
content-driven applications.

### **Features**

- **Full GFM**: Tables, autolinks, strikethrough, task lists, tag filtering
- **Template Integration**: `{expressions}` and `%` comments work within markdown
- **High Performance**: Built on `cmark-gfm` C library via NIF
- **Production Ready**: Safety limits, error handling, comprehensive tests

### **Basic Usage**

```erlang
% Pure markdown to HTML
{ok, Html} = arizona_markdown:to_html(~"# Hello **World**")
% Returns: ~"<h1>Hello <strong>World</strong></h1>\n"

% Inline markdown template
arizona_template:from_markdown(~"""
# {arizona_template:get_binding(title, Bindings)}
Welcome **{arizona_template:get_binding(user, Bindings)}**!

{% Template comment - not rendered %}

{arizona_template:render_stateful(my_widget_component, #{
    id => ~"widget",
    data => arizona_template:get_binding(widget_data, Bindings)
})}
""")

% File-based markdown template with compile-time optimization
arizona_template:from_markdown({file, "content/blog-post.md"})
arizona_template:from_markdown({priv_file, myapp, "content/blog-post.md"})

% With markdown options
{ok, Html} = arizona_markdown:to_html(~"# Hello", [source_pos, smart])
```

### **Blog Example**

```erlang
render(Bindings) ->
    arizona_template:from_markdown(~"""
    # {arizona_template:get_binding(title, Bindings)}

    {arizona_template:get_binding(content, Bindings)}

    {arizona_template:render_stateful(comment_section, #{
        id => ~"comments",
        post_id => arizona_template:get_binding(id, Bindings)
    })}
    """).
```

## Component Architecture

Arizona follows a hierarchical component model with clear separation of concerns:

### **Views** (`arizona_view`)

Top-level page components that represent complete routes. Views:

- **Require an `id` field** in their bindings for internal state management and component tracking
- Initialize from mount arguments and HTTP requests via `mount/2`
- Manage their own state plus nested stateful components
- Handle WebSocket events via `handle_event/3`
- Support optional layout wrapping
- Generate templates via `render/1`

### **Stateful Components** (`arizona_stateful`)

Interactive components with persistent internal state and full lifecycle management. They:

- **Require an `id` field** in their bindings for state management, event routing, and lifecycle tracking
- Mount with initial bindings via `mount/1`
- Maintain state between renders for efficient diff updates
- Handle events independently via `handle_event/3`
- Track changes for differential DOM updates
- Support automatic cleanup via optional `unmount/1` callback when removed from component tree

#### **Component Lifecycle**

1. **Mount**: Component initialized with `mount/1` using initial bindings
2. **Render**: Template generated with `render/1` using current state
3. **Events**: User interactions processed via `handle_event/3`
4. **Updates**: State changes trigger re-rendering with minimal DOM updates
5. **Unmount**: Optional cleanup via `unmount/1` when component removed from template

The `unmount/1` callback is automatically called when:

- Parent template changes and component is no longer rendered
- Component is replaced with a different component at the same location
- View navigation removes the entire component tree

Use `unmount/1` for cleanup tasks such as:

- Canceling timers and intervals
- Unsubscribing from PubSub topics
- Closing network connections
- Releasing GenServer references
- Cleaning up ETS tables or other shared resources

```erlang
-module(timer_component).
-behaviour(arizona_stateful).
-export([mount/1, render/1, handle_event/3, unmount/1]).

mount(Bindings) ->
    % Start a timer when component mounts
    {ok, TimerRef} = timer:send_interval(1000, tick),
    arizona_pubsub:join(~"time_updates", self()),
    NewBindings = Bindings#{timer_ref => TimerRef, seconds => 0},
    arizona_stateful:new(?MODULE, NewBindings).

handle_event(~"tick", _Params, State) ->
    Seconds = arizona_stateful:get_binding(seconds, State),
    NewState = arizona_stateful:put_binding(seconds, Seconds + 1, State),
    {[], NewState}.

% Automatic cleanup when component is unmounted
unmount(State) ->
    % Cancel timer to prevent memory leaks
    case arizona_stateful:get_binding(timer_ref, State) of
        undefined -> ok;
        TimerRef -> timer:cancel(TimerRef)
    end,
    % Unsubscribe from PubSub
    arizona_pubsub:leave(~"time_updates", self()),
    ok.
```

### **Stateless Components**

Pure rendering functions that:

- Accept bindings and return templates
- Have no internal state or lifecycle
- Are deterministic based solely on input
- Provide reusable UI elements

### **Layouts**

Document-level wrappers that:

- Define HTML structure and metadata
- Enable consistent page structure across views
- Handle head section and asset loading
- Use slots for dynamic content insertion
- **Never re-rendered** - layouts are static and no diffs are generated for them

Example layout with JavaScript client setup:

```erlang
-module(my_layout).
-compile({parse_transform, arizona_parse_transform}).
-export([render/1]).

render(Bindings) ->
    arizona_template:from_html(~"""
    <!DOCTYPE html>
    <html>
    <head>
        <title>My Arizona App</title>
        <script type="module" async>
            import { Arizona, ArizonaConsoleLogger, LOG_LEVELS } from '@arizona-framework/client';

            // Create client with optional logger
            const logger = new ArizonaConsoleLogger({ logLevel: LOG_LEVELS.info });

            globalThis.arizona = new Arizona({ logger });
            arizona.connect('/live');
        </script>
    </head>
    <body>
        {arizona_template:render_slot(arizona_template:get_binding(main_content, Bindings))}
    </body>
    </html>
    """).
```

### **Slots**

All components support slot-based composition:

- Accept dynamic content via `arizona_template:render_slot/1`
- Support view references, templates, or HTML values
- Enable flexible component composition and reuse
- Used in layouts for content insertion and views for dynamic sections

## Action System

Arizona uses a flexible action system for handling callback responses. All callbacks return
`{Actions, State}` where `Actions` is a list of action tuples.

### **Available Actions**

```erlang
% No action - just update state
{[], NewState}

% Dispatch custom events - subscribe using arizona.on('event_name', callback)
{[{dispatch, ~"dataLoaded", #{status => success, data => Value}}], NewState}
{[{dispatch, ~"notification:show", #{message => ~"Success!"}}], NewState}
{[{dispatch, ~"counter_123:update", #{count => 5}}], NewState}

% Redirect to new URL
{[{redirect, ~"/new-page", #{target => ~"_self"}}], NewState}     % Same tab
{[{redirect, ~"/external", #{target => ~"_blank"}}], NewState}    % New tab

% Redirect with window features
{[{redirect, ~"/popup", #{
    target => ~"popup_window",
    window_features => ~"width=600,height=400,resizable=yes"
}}], NewState}

% Reload the current page
{[reload], NewState}

% Multiple actions - executed in sequence
{[
    {dispatch, ~"taskCompleted", #{message => ~"Saved successfully!"}},
    {redirect, ~"/dashboard", #{target => ~"_self"}}
], NewState}
```

### **Benefits of the Action System**

- **Multiple Responses**: Send multiple actions per callback
- **Built-in Functionality**: No need to implement redirects or reloads manually
- **Custom Event Dispatching**: Integrate with any JavaScript framework or library via client events
- **Consistent API**: Same pattern across views and stateful components
- **Type Safety**: All actions are validated and processed uniformly
- **Future Extensible**: Easy to add new action types as needed

## Middleware System

Arizona includes a unified middleware system that works consistently across all route types
(view, controller, websocket, and asset). Middlewares process requests before they reach the
main handler, enabling powerful features like authentication, authorization, CORS handling,
request modification, and custom response generation.

> [!NOTE]
>
> Arizona does not provide any built-in middleware implementations. All middleware examples
> shown below are for demonstration purposes and must be implemented by you according to your
> application's specific requirements.

### **Route Configuration with Middlewares**

All routes support middleware lists as their final parameter:

```erlang
routes => [
    % View routes with authentication middleware
    {view, ~"/admin", admin_view, #{}, [
        {auth_middleware, #{jwt_secret => ~"secret123", redirect_on_fail => true}},
        {role_middleware, #{required_role => admin}}
    ]},

    % Controller routes with CORS and auth
    {controller, ~"/api/users", users_controller, #{}, [
        {cors_middleware, #{origins => [~"https://app.com"]}},
        {auth_middleware, #{jwt_secret => ~"secret123"}}
    ]},

    % WebSocket routes with authentication
    {websocket, ~"/live", #{idle_timeout => 60000}, [
        {auth_middleware, #{jwt_secret => ~"secret123"}}
    ]},

    % Asset routes with authentication (for protected files)
    {asset, ~"/private", {priv_dir, myapp, ~"private"}, [
        {auth_middleware, #{jwt_secret => ~"secret123"}}
    ]},

    % Public routes with no middlewares
    {view, ~"/public", public_view, #{}, []}
]
```

### **Creating Custom Middlewares**

Implement the `arizona_middleware` behavior:

```erlang
-module(auth_middleware).
-behaviour(arizona_middleware).
-export([execute/2]).

execute(Req, #{jwt_secret := Secret} = Opts) ->
    case cowboy_req:header(~"authorization", Req) of
        <<"Bearer ", Token/binary>> ->
            case validate_jwt(Token, Secret) of
                {ok, Claims} ->
                    % Add user info for downstream middlewares/handlers
                    Req1 = cowboy_req:set_meta(user_claims, Claims, Req),
                    {continue, Req1};
                {error, _} ->
                    Req1 = cowboy_req:reply(401, #{}, ~"Invalid token", Req),
                    {halt, Req1}
            end;
        undefined ->
            case maps:get(redirect_on_fail, Opts, false) of
                true ->
                    % Redirect to login for views
                    Req1 = cowboy_req:reply(302, #{~"location" => ~"/login"}, ~"", Req),
                    {halt, Req1};
                false ->
                    % JSON error for APIs
                    Req1 = cowboy_req:reply(401, #{}, ~"Unauthorized", Req),
                    {halt, Req1}
            end
    end.

validate_jwt(_Token, _Secret) ->
    % Your JWT validation logic
    {ok, #{user_id => ~"123", role => ~"admin"}}.
```

### **Middleware Chain Processing**

- Middlewares execute **sequentially** in the order specified
- Each middleware returns `{continue, Req1}` or `{halt, Req1}`
- If any middleware returns `{halt, Req1}`, processing stops (response already sent)
- If all middlewares return `{continue, Req1}`, the request reaches the main handler

### **Architecture Integration**

Arizona integrates middlewares into Cowboy's pipeline as a generic middleware:

```text
cowboy_router -> arizona_middleware_cowboy -> cowboy_handler
```

This ensures proper integration with Cowboy's request processing while maintaining Arizona's
flexibility and performance.

## Plugin System

Arizona includes a simple plugin system that allows you to transform the server configuration
before startup. Plugins are perfect for adding middleware, modifying routes, or customizing
server behavior across your application.

> [!NOTE]
>
> Arizona does not provide any built-in plugins. All plugin examples shown below are for
> demonstration purposes and must be implemented according to your application's requirements.

### **Creating a Plugin**

Implement the `arizona_plugin` behavior:

```erlang
-module(my_auth_plugin).
-behaviour(arizona_plugin).
-export([transform_config/2]).

transform_config(Config, PluginConfig) ->
    JwtSecret = maps:get(jwt_secret, PluginConfig),

    % Transform routes to add auth middleware to protected paths
    ServerConfig = maps:get(server, Config),
    Routes = maps:get(routes, ServerConfig, []),
    NewRoutes = lists:map(fun(Route) ->
        case Route of
            {view, Path, ViewModule, MountArg, Middlewares} when
                binary:match(Path, <<"admin">>) =/= nomatch ->
                % Add auth middleware to admin routes
                AuthMiddleware = {auth_middleware, #{jwt_secret => JwtSecret}},
                {view, Path, ViewModule, MountArg, [AuthMiddleware | Middlewares]};
            Other ->
                Other
        end
    end, Routes),

    UpdatedServerConfig = ServerConfig#{routes => NewRoutes},
    Config#{server => UpdatedServerConfig}.
```

### **Using Plugins**

Configure plugins in your `sys.config`:

```erlang
[
    {arizona, [
        {server, #{
            enabled => true,
            transport_opts => [{port, 1912}],
            routes => [
                {view, ~"/", home_view, #{}, []},
                {view, ~"/admin", admin_view, #{}, []},  % Plugin will add auth middleware
                {controller, ~"/api/users", users_controller, #{}, []}
            ]
        }},

        % Configure plugins with their options
        {plugins, [
            {my_auth_plugin, #{jwt_secret => "secret123"}},
            {my_cors_plugin, #{origins => ["*"]}},
            {my_logging_plugin, true}  % Plugin config can be any term
        ]}
    ]}
].
```

### **Plugin Execution**

- Plugins execute **before server startup** in the order specified
- Each plugin receives the current config and returns the transformed config
- Plugin configuration can be any Erlang term (maps, lists, atoms, tuples, etc.)
- If a plugin fails, Arizona startup fails with a clear error message

### **Example Use Cases**

- **Authentication**: Automatically add auth middleware to protected routes
- **CORS**: Add CORS headers to API routes
- **Logging**: Add request logging middleware to all routes
- **Rate Limiting**: Add rate limiting middleware based on route patterns
- **Environment Configuration**: Modify routes or options based on deployment environment

### **Distribution**

Plugins are distributed as standard Erlang applications via Hex.pm:

```erlang
% rebar.config
{deps, [
    {arizona, "~> 1.0"},
    {arizona_auth_plugin, "~> 1.0"}  % Add plugin dependency
]}.

% sys.config - just reference by name
{plugins, [
    {arizona_auth_plugin, #{jwt_secret => "secret123"}}
]}
```

## Event Handling & Real-time Updates

Arizona provides multiple ways to handle user interactions and real-time updates:

### **WebSocket Events**

```erlang
% In templates - send events to current view
<button onclick="arizona.pushEvent('my_event')">Click</button>

% Send events to specific components
<button onclick="arizona.pushEventTo('component_id', 'increment', \{amount: 5})">+5</button>

% In view/component modules - handle events
handle_event(~"my_event", Params, State) ->
    % Update state and return {Actions, NewState} where Actions is a list
    {[], arizona_stateful:put_binding(updated, true, State)}.

% Example with actions - dispatch custom event to client
handle_event(~"save_data", Params, State) ->
    % Process data and dispatch event to client
    {[{dispatch, ~"dataSaved", #{status => success, id => 123}}], UpdatedState}.

% Example with redirect action
handle_event(~"login_success", _Params, State) ->
    % Redirect user to dashboard after login
    {[{redirect, ~"/dashboard", #{target => ~"_self"}}], State}.

% Example with reload action
handle_event(~"reset_app", _Params, State) ->
    % Reload the entire page
    {[reload], State}.

% Example with multiple actions
handle_event(~"complete_task", _Params, State) ->
    % Dispatch event and then redirect
    Actions = [
        {dispatch, ~"taskCompleted", #{message => ~"Task completed!"}},
        {redirect, ~"/tasks", #{target => ~"_self"}}
    ],
    {Actions, State}.
```

### **Request-Reply Pattern with callEvent**

For operations that need to return data to the client (like fetching data, validations, etc.),
use `callEvent` which returns a Promise that resolves when the server sends a reply:

**Client-side:**

```javascript
// Call event and wait for reply from view
const user = await arizona.callEvent('get_user', {id: 123});
console.log(user); // {name: "Alice", email: "alice@example.com"}

// Call event on specific component and wait for reply
const data = await arizona.callEventFrom('data-grid', 'fetch_rows', {page: 1});
console.log(data); // {rows: [...], total: 100}

// With custom timeout (default: 10000ms)
try {
  const result = await arizona.callEvent('slow_operation', {}, {timeout: 30000});
} catch (error) {
  console.error('Timeout or error:', error);
}
```

**Server-side:**

```erlang
% In view or stateful component - handle with reply action
handle_event(~"get_user", {Ref, Params}, State) ->
    % Extract params
    UserId = maps:get(~"id", Params),

    % Fetch data
    User = fetch_user(UserId),
    UserData = #{
        ~"name" => User#user.name,
        ~"email" => User#user.email
    },

    % Return reply action with the ref
    {[{reply, Ref, UserData}], State};

% Fire-and-forget events (without ref) work as before
handle_event(~"increment", Params, State) ->
    Count = arizona_stateful:get_binding(count, State),
    {[], arizona_stateful:put_binding(count, Count + 1, State)}.
```

**Key differences:**

- **pushEvent** / **pushEventTo**: Fire-and-forget, no reply expected
  - Server receives `Params` (map)
  - Returns `{[Actions], State}` with optional actions

- **callEvent** / **callEventFrom**: Request-reply pattern
  - Server receives `{Ref, Params}` (tuple with ref ID)
  - Must include `{reply, Ref, Data}` action to resolve the promise
  - Client awaits the response

### **PubSub Messaging**

Subscribe to topics, broadcast messages, and handle real-time updates:

```erlang
% Subscribe to topics during mount
mount(_Arg, _Request) ->
    case arizona_live:is_connected(self()) of
        true -> arizona_pubsub:join(~"time_update", self());
        false -> ok
    end,
    arizona_view:new(?MODULE, #{current_time => ~"Loading..."}).

% Publish messages from external processes
arizona_pubsub:broadcast(~"time_update", #{~"time" => TimeString}),

% Handle PubSub messages in views (treated as events)
handle_event(~"time_update", Data, View) ->
    NewTime = maps:get(~"time", Data),
    State = arizona_view:get_state(View),
    UpdatedState = arizona_stateful:put_binding(current_time, NewTime, State),
    {[], arizona_view:update_state(UpdatedState, View)}.
```

### **Process Messages**

```erlang
% Views can handle arbitrary Erlang messages
handle_info({timer, update}, View) ->
    % Handle timer or other process messages
    {[], UpdatedView}.
```

### **Client-Side Event Listening**

Arizona provides a built-in event subscription system that allows seamless integration with any
JavaScript framework or library.

Subscribe to events triggered by `dispatch` actions from `handle_event/3`:

```javascript
// Subscribe to custom events dispatched from the server
const unsubscribe = arizona.on('dataSaved', (data) => {
    // Handle data saved event with custom data
    console.log('Data saved:', data);
    showNotification('Saved successfully!');
});

// Component-scoped events using namespace pattern
arizona.on('notification:show', (data) => {
    const notification = document.querySelector('#notification');
    notification.textContent = data.message;
    notification.classList.add('visible');
});

// Multiple component instances with unique IDs
arizona.on('counter_123:update', (data) => {
    document.querySelector('#counter_123 .count').textContent = data.count;
});

arizona.on('counter_456:update', (data) => {
    document.querySelector('#counter_456 .count').textContent = data.count;
});

// Unsubscribe when no longer needed
const cleanup = arizona.on('myEvent', handleEvent);
cleanup(); // Removes the event listener

// Subscribe to Arizona framework events
arizona.on('connected', (data) => {
    showConnectionIndicator('online');
});

arizona.on('disconnected', (data) => {
    showConnectionIndicator('offline');
});

arizona.on('error', (data) => {
    console.error('Server error:', data.error);
    showErrorMessage(data.error);
});

// Example: Handle form submission with custom event feedback
function submitForm(formData) {
    // Send event to server
    arizona.pushEvent('submit_form', formData);

    // Listen for response (one-time subscription)
    const cleanup = arizona.on('formSubmitted', (data) => {
        if (data.success) {
            showSuccess('Form submitted successfully!');
        }
        cleanup(); // Clean up after handling
    });
}
```

## Development Features

### **File Watching Infrastructure**

Arizona provides file watching tools that you can use to build custom development automation:

- **Generic File Watcher**: `arizona_watcher` GenServer for monitoring directories
- **Watcher Supervisor**: `arizona_watcher_sup` for managing multiple watcher instances
- **Reloader Coordination**: `arizona_reloader` system for organizing multiple handlers
- **Custom Handler Pattern**: Implement your own reload/build/compilation logic

**Arizona does not provide any built-in reloading functionality.** You must implement your own
handler modules with custom logic for compilation, hot-loading, asset building, etc.

Example handler implementing the `arizona_reloader` behavior:

```erlang
-module(my_custom_handler).
-behaviour(arizona_reloader).
-export([reload/2]).

reload(Files, Options) ->
    % Your custom logic: compile, build, reload, etc.
    io:format("Files changed: ~p with options: ~p~n", [Files, Options]),
    % You implement what happens here
    ok.
```

**Key Point**: The reloader system is just infrastructure. All actual reloading, compilation,
and automation logic is your responsibility to implement in handler modules.

### **Smart Reload System**

Arizona's JavaScript client includes intelligent reload handling that optimizes the development experience:

- **CSS-only reloading**: When CSS files change, only stylesheets are refreshed without full page reload
- **Application state preservation**: Form inputs, scroll position, and component state remain intact
  during CSS updates
- **Automatic fallback**: Non-CSS file changes trigger full page reload as expected
- **File type detection**: Reload handlers can specify `file_type` to control client behavior

When implementing custom reload handlers, you can leverage this feature:

```erlang
% In your reload handler, specify file type for smart client handling
reload(_Files, _Options) ->
    % Compile logic here
    FileType = css,
    arizona_pubsub:broadcast(~"arizona:reload", FileType).
```

This enhancement significantly improves the development workflow by avoiding unnecessary page
reloads during CSS development.

### **JavaScript Client Logging**

Arizona's JavaScript client provides a pluggable logging system with log levels aligned with
Erlang's logger for consistent debugging across the stack.

**Basic Usage:**

```javascript
import { Arizona, ArizonaConsoleLogger, LOG_LEVELS } from '@arizona-framework/client';

// Production - no logger (silent by default)
const arizona = new Arizona();

// Development - console logger with info level
const arizona = new Arizona({
    logger: new ArizonaConsoleLogger({ logLevel: LOG_LEVELS.info })
});

// Full debugging - all internal operations
const arizona = new Arizona({
    logger: new ArizonaConsoleLogger({ logLevel: LOG_LEVELS.debug })
});

// Programmatic control
const arizona = new Arizona({
    logger: process.env.NODE_ENV === 'development'
        ? new ArizonaConsoleLogger({ logLevel: LOG_LEVELS.debug })
        : null
});
```

**Available Log Levels** (aligned with Erlang logger):

- `LOG_LEVELS.error` (3): Errors only
- `LOG_LEVELS.warning` (4): Warnings and errors
- `LOG_LEVELS.info` (6): Connection status, reload notifications, redirects (default)
- `LOG_LEVELS.debug` (7): All internal operations and message details

**Custom Logger:**

Implement your own logger by extending `ArizonaLogger`:

```javascript
import { ArizonaLogger, LOG_LEVELS } from '@arizona-framework/client';

class MyCustomLogger extends ArizonaLogger {
    handleLog(level, message, ...args) {
        // Send to your logging service, format differently, etc.
        fetch('/api/logs', {
            method: 'POST',
            body: JSON.stringify({ level, message, args })
        });
    }
}

const arizona = new Arizona({
    logger: new MyCustomLogger({ logLevel: LOG_LEVELS.warning })
});
```

**Benefits:**

- **Optional**: Logger is completely optional - production apps can omit it entirely
- **Pluggable**: Use built-in console logger or implement custom logging backends
- **Flexible development**: Choose appropriate verbosity for debugging
- **Erlang alignment**: Log levels match backend for consistent configuration
- **Zero overhead**: No logger means zero logging overhead

### **Static Site Generation**

Generate static HTML for deployment:

```erlang
arizona_static:generate(#{
    route_paths => #{
        ~"/" => #{},
        ~"/about" => #{parallel => true},
        ~"/posts/123" => #{parallel => false}
    },
    output_dir => ~"_site"
}).
```

## Production Deployment

### **Server Configuration**

Configure your production `config/sys.config`:

```erlang
[
    {arizona, [
        {server, #{
            enabled => true,
            scheme => https,  % Use HTTPS in production
            transport_opts => #{
                socket_opts => [{port, 443}],
                % Add SSL certificates and options
                ssl_opts => [
                    {certfile, "/path/to/cert.pem"},
                    {keyfile, "/path/to/key.pem"}
                ]
            },
            proto_opts => #{
                env => #{
                    max_keepalive => 100,
                    timeout => 60000
                }
            },
            routes => YourRoutes
        }},
        {reloader, #{enabled => false}}  % Disable file watchers in production
    ]}
].
```

Then start normally:

```erlang
{ok, _Started} = application:ensure_all_started(arizona).
```

### **JavaScript Client Setup**

Install the Arizona client via npm:

```bash
npm install @arizona-framework/client
```

**Import Options:**

```javascript
// Recommended: Import everything from main entry point
import { Arizona, ArizonaConsoleLogger, LOG_LEVELS } from '@arizona-framework/client';

// Or: Import from specific subpaths
import { Arizona } from '@arizona-framework/client';
import { ArizonaConsoleLogger } from '@arizona-framework/client/logger';
import ArizonaConsoleLogger from '@arizona-framework/client/logger/console';
```

**Asset Route (Optional):**

If you're **not** using the npm package and serving Arizona's JavaScript files directly from
the `priv/static/assets` directory, you need this asset route:

```erlang
{asset, ~"/assets", {priv_dir, arizona, ~"static/assets"}, []}
```

When using the npm package with a bundler (Vite, Webpack, esbuild, etc.), this route is not required
as your bundler will handle the JavaScript files.

**Static Site Generation:**

Use `arizona_static:generate/1` to generate static HTML files from your views.
Static generation creates SEO-friendly HTML that can be deployed to any web server.

### **Performance Considerations**

- Arizona templates are compile-time optimized via parse transforms
- Differential rendering minimizes WebSocket traffic
- Live processes are lightweight Erlang processes
- PubSub uses Erlang's built-in `pg` for efficient message routing

## Developer Tools

Arizona provides additional tools to enhance the development experience:

### **Editor Support**

- **[arizona.nvim](https://github.com/arizona-framework/arizona.nvim)** - Neovim plugin for Arizona development
- **[tree-sitter-arizona](https://github.com/arizona-framework/tree-sitter-arizona)** - Tree-sitter
  grammar for Arizona templates

## Requirements

- Erlang/OTP 28+

## Sponsors

If you like this tool, please consider [sponsoring me](https://github.com/sponsors/williamthome).
I'm thankful for your never-ending support :heart:

I also accept coffees :coffee:

[!["Buy Me A Coffee"](https://www.buymeacoffee.com/assets/img/custom_images/orange_img.png)](https://www.buymeacoffee.com/williamthome)

## Contributing

Contributions are welcome! Please see [CONTRIBUTING.md](CONTRIBUTING.md) for development setup,
testing guidelines, and contribution workflow.

### Contributors

<a href="https://github.com/arizona-framework/arizona/graphs/contributors">
  <img
    src="https://contrib.rocks/image?repo=arizona-framework/arizona&max=100&columns=10"
    width="15%"
    alt="Contributors"
  />
</a>

## Star History

<a href="https://star-history.com/#arizona-framework/arizona">
  <picture>
    <source
      media="(prefers-color-scheme: dark)"
      srcset="https://api.star-history.com/svg?repos=arizona-framework/arizona&type=Date&theme=dark"
    />
    <source
      media="(prefers-color-scheme: light)"
      srcset="https://api.star-history.com/svg?repos=arizona-framework/arizona&type=Date"
    />
    <img
      src="https://api.star-history.com/svg?repos=arizona-framework/arizona&type=Date"
      alt="Star History Chart"
      width="100%"
    />
  </picture>
</a>

## License

Copyright (c) 2023-2025 [William Fank Thomé](https://github.com/williamthome)

Arizona is 100% open-source and community-driven. All components are
available under the Apache 2 License on [GitHub](https://github.com/arizona-framework/arizona).

See [LICENSE.md](LICENSE.md) for more information.

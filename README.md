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
- **Template DSL** with plain HTML, `{}` Erlang expressions, and `\{` escaping for literal braces
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
- **Static site generation** for deployment flexibility and SEO optimization
- **File watching infrastructure** for custom development automation

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
    arizona_template:from_string(~"""
    <div id="{arizona_template:get_binding(id, Bindings)}">
        <h1>{arizona_template:get_binding(greeting, Bindings)}</h1>
        <p>Clicked: {arizona_template:get_binding(count, Bindings)} times</p>
        <button onclick="arizona.sendEvent('increment')">Click me</button>
    </div>
    """).

handle_event(~"increment", _Params, View) ->
    State = arizona_view:get_state(View),
    Count = arizona_stateful:get_binding(count, State),
    NewState = arizona_stateful:put_binding(count, Count + 1, State),
    {noreply, arizona_view:update_state(NewState, View)}.
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
                {view, ~"/", home_view, #{}},
                {websocket, ~"/live"},
                {controller, ~"/api/presence", my_api_controller, #{}},  % Plain Cowboy REST handler
                {asset, ~"/assets", {priv_dir, arizona, ~"static/assets"}}  % Required for live features
            ]
        }},
        {reloader, #{
            enabled => true,  % Enable file watcher coordination
            rules => [
                #{
                    handler => my_erlang_handler,  % Your custom handler module
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
            {view, ~"/", home_view, #{}},
            {websocket, ~"/live"},
            {asset, ~"/assets", {priv_dir, arizona, ~"static/assets"}}
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
-export([reload/1]).

reload(Files) ->
    io:format("Compiling ~p~n", [Files]),
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
arizona_template:from_string(~"""
<div>
    <h1>{arizona_template:get_binding(title, Bindings)}</h1>
    <p>User: {arizona_template:get_binding(username, Bindings)}</p>
</div>
""")
```

### Stateful Component Rendering

Render interactive components that maintain their own state between updates:

```erlang
arizona_template:from_string(~"""
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
arizona_template:from_string(~"""
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
arizona_template:from_string(~"""
<ul>
    {arizona_template:render_list(fun(Item) ->
        arizona_template:from_string(~"""
        <li>{arizona_template:get_binding(name, Item)}</li>
        """)
    end, arizona_template:get_binding(items, Bindings))}
</ul>
""")
```

### Map Rendering

Iterate over key-value pairs with optimized rendering performance:

```erlang
arizona_template:from_string(~"""
<div>
    {arizona_template:render_map(fun({Key, Value}) ->
        arizona_template:from_string(~"""
        <p>{Key}: {Value}</p>
        """)
    end, #{~"name" => ~"Arizona", ~"type" => ~"Framework"})}
</div>
""")
```

### Slot Rendering

Insert dynamic content into predefined slots for flexible component composition:

```erlang
arizona_template:from_string(~"""
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
arizona_template:from_string(~"""
<div>
    {% This is a comment and is not rendered }
    <h1>{arizona_template:get_binding(title, Bindings)}</h1>
    <style>
        .css-rule \{ color: blue; }  /* \{ renders as literal { */
    </style>
</div>
""")
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

Interactive components with persistent internal state. They:

- **Require an `id` field** in their bindings for state management, event routing, and lifecycle tracking
- Mount with initial bindings via `mount/1`
- Maintain state between renders for efficient diff updates
- Handle events independently via `handle_event/3`
- Track changes for differential DOM updates

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
    arizona_template:from_string(~"""
    <!DOCTYPE html>
    <html>
    <head>
        <title>My Arizona App</title>
        <script type="module" async>
            import Arizona from '/assets/js/arizona.min.js';
            globalThis.arizona = new Arizona();
            arizona.connect();
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

## Event Handling & Real-time Updates

Arizona provides multiple ways to handle user interactions and real-time updates:

### **WebSocket Events**

```erlang
% In templates - send events to current view
<button onclick="arizona.sendEvent('my_event')">Click</button>

% Send events to specific components
<button onclick="arizona.sendEventTo('component_id', 'increment', \{amount: 5})">+5</button>

% In view/component modules - handle events
handle_event(~"my_event", Params, State) ->
    % Update state and return {noreply, NewState}
    {noreply, arizona_stateful:put_binding(updated, true, State)}.
```

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
    {noreply, arizona_view:update_state(UpdatedState, View)}.
```

### **Process Messages**

```erlang
% Views can handle arbitrary Erlang messages
handle_info({timer, update}, View) ->
    % Handle timer or other process messages
    {noreply, UpdatedView}.
```

### **Client-Side Event Listening**

Arizona automatically dispatches custom events for meaningful server interactions,
allowing application code to react to server responses.

The `reply` event is triggered when `handle_event/3` returns `{reply, Reply, View}`
(in views) or `{reply, Reply, StatefulState}` (in stateful components):

```javascript
// Listen for server events in your application
document.addEventListener('arizonaEvent', (event) => {
    const { type, data } = event.detail;

    if (type === 'reply') {
        // Handle server replies to user actions
        console.log('Server replied:', data);
        showNotification('Action completed successfully');
    }

    if (type === 'error') {
        // Handle server errors
        console.error('Server error:', data.error);
        showErrorMessage(data.error);
    }

    if (type === 'status') {
        // Track connection status
        if (data.status === 'connected') {
            showConnectionIndicator('online');
        } else if (data.status === 'disconnected') {
            showConnectionIndicator('offline');
        }
    }
});

// Example: Handle form submission with server feedback
function submitForm(formData) {
    arizona.sendEvent('submit_form', formData);

    // The arizonaEvent will fire when the server responds
    // allowing you to show success/error messages or update UI
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
-export([reload/1]).

reload(Files) ->
    % Your custom logic: compile, build, reload, etc.
    io:format("Files changed: ~p~n", [Files]),
    % You implement what happens here
    ok.
```

**Key Point**: The reloader system is just infrastructure. All actual reloading, compilation,
and automation logic is your responsibility to implement in handler modules.

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

### **Static Assets**

- Arizona provides required JavaScript client files (`arizona.min.js`, `arizona-worker.min.js`)
- Asset route `{asset, ~"/assets", {priv_dir, arizona, ~"static/assets"}}` is required for WebSocket
- Use `arizona_static:generate/1` to generate static HTML files from your views
- Static generation creates SEO-friendly HTML that can be deployed to any web server

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

- Erlang/OTP 27+

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

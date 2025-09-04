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

- **Views** - Top-level page components with full lifecycle management
- **Stateful Components** - Interactive components with persistent state
- **Stateless Components** - Pure rendering functions for simple UI elements
- **Layouts** - Document wrappers with slot-based content insertion

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
- **Development-time file reloader** for fast iteration cycles

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

### 3. Start Server

```erlang
arizona:start(#{
    server => #{
        transport_opts => [{port, 1912}],
        routes => [
            {view, ~"/", home_view, #{}},
            {websocket, ~"/live"},
            {controller, ~"/api/presence", my_api_controller, #{}},
            {asset, ~"/assets", {priv_dir, arizona, ~"static/assets"}}  % Required for live features
        ]
    },
    reloader => #{enabled => true}  % Development mode
}).
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

- Initialize from mount arguments and HTTP requests via `mount/2`
- Manage their own state plus nested stateful components
- Handle WebSocket events via `handle_event/3`
- Support optional layout wrapping
- Generate templates via `render/1`

### **Stateful Components** (`arizona_stateful`)

Interactive components with persistent internal state. They:

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

```erlang
% Subscribe to topics
arizona_pubsub:join(~"notifications", self()),

% Publish messages
arizona_pubsub:broadcast(~"notifications", #{type => alert, msg => ~"Hello"}),

% Handle PubSub in views (treated as events)
handle_event({pubsub_message, ~"notifications", Data}, _Params, View) ->
    % Process real-time message
    {noreply, UpdatedView}.
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

### **Hot Code Reloading**

Arizona includes a development-time file watcher that:

- Monitors source files for changes
- Automatically recompiles modified modules
- Hot-loads updated code without server restart
- Rebuilds client assets when JS files change

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

For production, disable development features and configure appropriate settings:

```erlang
arizona:start(#{
    server => #{
        scheme => https,  % Use HTTPS in production
        transport_opts => #{
            socket_opts => [{port, 443}],
            % Add SSL certificates and options
        },
        routes => YourRoutes
    },
    reloader => #{enabled => false}  % Disable in production
}).
```

### **Static Assets**

- Use `arizona_static:generate/1` for pre-generated static sites
- Serve Arizona's JavaScript assets via CDN for better performance
- Enable gzip compression at the reverse proxy level

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

## License

Copyright (c) 2023-2025 [William Fank Thomé](https://github.com/williamthome)

Arizona is 100% open-source and community-driven. All components are
available under the Apache 2 License on [GitHub](https://github.com/williamthome/arizona).

See [LICENSE.md](LICENSE.md) for more information.

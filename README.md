# Arizona

![arizona_256x256](https://github.com/arizona-framework/arizona/assets/35941533/88b76a0c-0dfc-4f99-8608-b0ebd9c9fbd9)

Arizona is a modern LiveView web framework for Erlang with real-time capabilities and optimized performance.

## ⚠️ Warning

Work in progress.

Use it at your own risk, as the API may change at any time.

## Overview

Arizona provides a complete LiveView framework with hierarchical rendering, real-time WebSocket
updates, and stateful/stateless component architecture. It features compile-time template
optimization and surgical DOM updates for optimal performance.

### Key Features

- **📡 Real-time LiveView**: WebSocket-based live updates with minimal payload
- **🏗️ Hierarchical Rendering**: Efficient differential rendering with surgical DOM updates
- **⚡ Performance Optimized**: Compile-time template processing with enhanced parse transforms
- **🧩 Component Architecture**: Stateful and stateless components with lifecycle management
- **🎯 Type Safe**: Comprehensive Dialyzer support with proper type contracts
- **🔧 OTP Integration**: Full OTP compliance with gen_server and supervisor patterns
- **🚀 Enhanced Templates**: Variable assignment support with automatic change detection

## Template Syntax

Arizona supports two template patterns for different use cases:

### Standard Templates (Original API)

Arizona uses the `~"""` sigil for template compilation with embedded Erlang expressions:

```erlang
render(Socket) ->
    arizona_html:render_stateful(~"""
    <div class="counter">
        <h1>Count: {arizona_socket:get_binding(count, Socket)}</h1>
        <button onclick="increment">+</button>
        <button onclick="decrement">-</button>
    </div>
    """, Socket).
```

### Enhanced Templates (Variable Assignment Support)

Use the enhanced parse transform to enable variable assignments before templates for a more
"Erlanger" API:

```erlang
-module(my_live).
-compile({parse_transform, arizona_parse_transform}).
-arizona_parse_transform([render/1]).

render(Socket) ->
    Count = arizona_socket:get_binding(count, Socket),
    UserName = arizona_socket:get_binding(user_name, Socket),
    arizona_html:render_stateful(~"""
    <div class="counter">
        <h1>Hello {UserName}! Count: {Count}</h1>
        <button onclick="increment">+</button>
        <button onclick="decrement">-</button>
    </div>
    """, Socket).
```

**Enhanced Features:**

- **Variable Assignment Support**: Extract variables before templates
- **Automatic Change Detection**: Generates optimized `vars_indexes` for surgical DOM updates
- **Multi-Function Support**: Different variable contexts per function
- **Dependency Tracking**: Handles nested and conditional binding calls
- **Full Backward Compatibility**: Existing code continues to work unchanged

## Basic Usage

### 1. Create a new rebar3 application

```bash
# Create a new rebar3 application
$ rebar3 new app arizona_example

# Navigate to the project directory
$ cd arizona_example
```

### 2. Add Arizona dependency

Add Arizona as a dependency in `rebar.config`:

```erlang
{deps, [
    {arizona, {git, "https://github.com/arizona-framework/arizona", {branch, "main"}}}
]}.
```

Update the application dependencies in `src/arizona_example.app.src`:

```erlang
{application, arizona_example, [
    {description, "Arizona Example Application"},
    {applications, [
        kernel,
        stdlib,
        arizona
    ]}
]}.
```

### 3. Create a LiveView module

Create `src/arizona_example_counter.erl` using the enhanced parse transform:

```erlang
-module(arizona_example_counter).
-behaviour(arizona_live).
-compile({parse_transform, arizona_parse_transform}).
-arizona_parse_transform([render/1]).

-export([mount/2, render/1, handle_event/3]).

mount(_Req, Socket) ->
    Socket1 = arizona_socket:put_binding(count, 0, Socket),
    {ok, Socket1}.

render(Socket) ->
    Count = arizona_socket:get_binding(count, Socket),
    arizona_html:render_stateful(~"""
    <div class="counter">
        <h1>Count: {Count}</h1>
        <button onclick="increment">Increment</button>
        <button onclick="decrement">Decrement</button>
    </div>
    """, Socket).

handle_event(~"increment", _Params, Socket) ->
    Count = arizona_socket:get_binding(count, Socket),
    Socket1 = arizona_socket:put_binding(count, Count + 1, Socket),
    {noreply, Socket1};
handle_event(~"decrement", _Params, Socket) ->
    Count = arizona_socket:get_binding(count, Socket),
    Socket1 = arizona_socket:put_binding(count, Count - 1, Socket),
    {noreply, Socket1}.
```

**What's happening here:**

- The `-arizona_parse_transform([render/1])` attribute enables enhanced template processing
- Variables like `Count` can now be extracted before the template
- The parse transform automatically generates optimized `vars_indexes` for efficient updates
- When the `count` binding changes, only the specific template element containing `{Count}`
  re-renders

### 4. Start the server

```bash
# Start the Erlang shell with your application
$ rebar3 shell
```

Then in the Erlang shell:

```erlang
% Define routes for your application
1> Routes = [
    {live, ~"/counter", arizona_example_counter},
    {live_websocket, ~"/live/websocket"},
    {static, ~"/assets", {priv_dir, arizona_example, ~"assets"}}
].

% Start the Arizona server
2> {ok, _} = arizona_server:start(#{port => 8080, routes => Routes}).
```

### 5. Add client-side JavaScript

Create the client-side JavaScript file `priv/assets/js/app.js`:

```javascript
// Import the Arizona client library
import ArizonaClient from '/assets/js/arizona.min.js';

// Create and connect the client for real-time updates
const client = new ArizonaClient();
client.connect();
```

### 6. Create HTML page

Create the main HTML file `priv/assets/index.html`:

```html
<!DOCTYPE html>
<html>
<head>
    <title>Arizona Example</title>
    <script type="module" src="/assets/js/app.js"></script>
</head>
<body>
    <div id="arizona-app">
        <!-- LiveView will mount here -->
    </div>
</body>
</html>
```

Visit `http://localhost:8080/counter` to see your LiveView in action! The counter will update in
real-time as you click the buttons.

## Architecture

### Component Types

- **Stateful Components**: Maintain state across renders with lifecycle callbacks
- **Stateless Components**: Lightweight function-based components
- **List Components**: Optimized rendering for collections

### Parse Transform Enhancement

Arizona's enhanced parse transform provides advanced compile-time optimizations:

- **Variable Dependency Tracking**: Analyzes `arizona_socket:get_binding/2,3` calls to map
  variables to template elements
- **Automatic vars_indexes Generation**: Creates precise change detection maps for surgical DOM updates
- **Multi-Function Support**: Each function marked with `-arizona_parse_transform([function/arity])`
  gets its own variable context
- **Complex Dependency Handling**: Supports nested calls, conditionals, and multiple binding dependencies

```erlang
% Enhanced mode automatically generates:
vars_indexes => #{
    user_name => [1],    % UserName variable affects element 1
    count => [3]         % Count variable affects element 3
}
```

### Rendering Modes

- **render**: Standard HTML output
- **diff**: Differential updates for efficiency
- **hierarchical**: Structured JSON for WebSocket transmission

### Real-time Updates

Arizona sends minimal JSON diffs over WebSocket containing only changed elements, ensuring optimal
performance for real-time applications. With enhanced templates, updates are even more precise,
targeting only the specific DOM elements that depend on changed bindings.

## API Documentation

### Core Modules

- `arizona_live`: LiveView behavior and process management
- `arizona_socket`: State management and binding system
- `arizona_html`: Template rendering and component invocation
- `arizona_server`: HTTP server and routing configuration
- `arizona_hierarchical`: Differential rendering engine

### Request Handling

- `arizona_request`: HTTP request abstraction
- `arizona_handler`: Request lifecycle management
- `arizona_websocket`: WebSocket connection handling

For detailed API documentation, see the module documentation in the source code.

## Requirements

- Erlang/OTP 27+

## Performance

- **Compile-time optimization**: Templates processed at build time
- **Minimal WebSocket payloads**: Only changed elements transmitted
- **Efficient diffing**: Hierarchical change detection
- **Lazy loading**: Request data loaded on demand

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

# Arizona

![arizona_256x256](https://github.com/arizona-framework/arizona/assets/35941533/88b76a0c-0dfc-4f99-8608-b0ebd9c9fbd9)

Arizona is a modern web framework for Erlang, delivering real-time web applications with optimized
performance and compile-time efficiency.

## ⚠️ Warning

Work in progress.

Use it at your own risk, as the API may change at any time.

## Key Features

- Real-time WebSocket updates
- Hierarchical component rendering
- Compile-time template optimization
- Type-safe stateful and stateless components
- Efficient differential DOM updates
- Simple template syntax: plain HTML with Erlang expressions in `{}`

## Quick Start

### 1. Add Dependency

In your `rebar.config`:

```erlang
{deps, [
    {arizona, {git, "https://github.com/arizona-framework/arizona", {branch, "main"}}}
]}.
```

### 2. Start Server

```erlang
{ok, _Pid} = arizona_server:start(#{
    port => 8080,
    routes => [
        {live, ~"/my-view", my_view_module},
        {live_websocket, ~"/live/websocket"},
        {static, ~"/assets", {priv_dir, arizona, ~"static/assets"}}
    ]
}).
```

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

### Explore the Code

Each demo corresponds to complete source code in [`test/support/e2e/`](https://github.com/arizona-framework/arizona/tree/main/test/support/e2e/):

- **[Counter App](https://github.com/arizona-framework/arizona/tree/main/test/support/e2e/counter/)**
  - Layout + View with event handling
- **[Todo App](https://github.com/arizona-framework/arizona/tree/main/test/support/e2e/todo/)**
  - Complex state management patterns
- **[Modal System](https://github.com/arizona-framework/arizona/tree/main/test/support/e2e/modal/)**
  - Component composition and slots
- **[Data Grid](https://github.com/arizona-framework/arizona/tree/main/test/support/e2e/datagrid/)**
  - Advanced data presentation

## Template Syntax

Arizona templates use plain HTML with Erlang expressions in `{}`:

```erlang
arizona_template:from_string(~"""
<div class="counter">
    <h1>Count: {arizona_template:get_binding(count, Bindings)}</h1>
    <button onclick="arizona.sendEvent('increment')">+</button>
</div>
""")
```

## Component Architecture

- **Layouts**: HTML document wrappers with slots for dynamic content insertion
- **Views**: Top-level components managing application state
- **Stateful Components**: Components with lifecycle and persistent state
- **Stateless Components**: Pure rendering functions
- **List Components**: Optimized rendering for collections

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

# Arizona

![arizona_256x256](https://github.com/arizona-framework/arizona/assets/35941533/88b76a0c-0dfc-4f99-8608-b0ebd9c9fbd9)

Arizona is a real-time web framework for Erlang that delivers high-performance web applications
through compile-time optimization and differential rendering.

## ⚠️ Warning

Work in progress.

Use it at your own risk, as the API may change at any time.

## Why Arizona?

**Compile-time template optimization.** Arizona's parse transforms optimize templates at compile
time, not runtime. Templates are analyzed, diffed, and reduced to efficient rendering functions
before your application ever starts.

**Differential rendering.** Only the parts of the page that actually changed are sent over the
wire. Combine this with compile-time optimization and you get minimal network traffic and minimal
browser work.

**Three template syntaxes.** Write templates as HTML strings, pure Erlang terms (with full editor
support and type safety), or GitHub Flavored Markdown with embedded expressions — whichever fits
the use case.

**Built on the BEAM.** Lightweight live processes for every connected client. PubSub backed by
Erlang's built-in `pg` — no Redis needed. Fault tolerance and hot code upgrades come free from OTP.

**Real-time and static in one framework.** Build interactive real-time apps with WebSocket-driven
live updates, then generate static HTML from the same views for SEO and deployment flexibility.

## Key Features

- Compile-time template optimization via parse transforms
- Differential DOM updates minimizing network traffic
- Real-time WebSocket updates with automatic synchronization
- Multiple template syntaxes: HTML, Erlang terms, Markdown
- Type-safe stateful and stateless component hierarchy
- Static site generation for SEO and deployment flexibility
- Request-reply events (callEvent/callEventFrom) alongside fire-and-forget
- PubSub system using Erlang's built-in pg
- Unified middleware system for all route types
- Plugin system for configuration transformation
- Smart CSS-only reload preserving application state
- Editor support with Neovim plugin and Tree-sitter grammar

## Quick Start

Add Arizona as a dependency and create your first view.

### 1. Add Dependency

In your `rebar.config`:

```erlang
{deps, [
    {arizona, {git, "https://github.com/arizona-framework/arizona", {branch, "main"}}}
]}.
```

### 2. Create a View

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

> For configuration, layouts, and next steps, see the
> [Getting Started guide](docs/getting-started/quick-start.md).

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

Each demo corresponds to complete source code in
[`test/support/e2e/`](https://github.com/arizona-framework/arizona/tree/main/test/support/e2e/):

- **[Counter
  App](https://github.com/arizona-framework/arizona/tree/main/test/support/e2e/counter/)**
  - Layout + View with event handling and PubSub integration
- **[Todo App](https://github.com/arizona-framework/arizona/tree/main/test/support/e2e/todo/)**
  - Complex state management and CRUD operations
- **[Modal System](https://github.com/arizona-framework/arizona/tree/main/test/support/e2e/modal/)**
  - Component composition, slots, and dynamic overlays
- **[Data Grid](https://github.com/arizona-framework/arizona/tree/main/test/support/e2e/datagrid/)**
  - Advanced data presentation and sorting functionality
- **[Real-time
  App](https://github.com/arizona-framework/arizona/tree/main/test/support/e2e/realtime/)**
  - Live data updates and WebSocket communication
- **[Blog App](https://github.com/arizona-framework/arizona/tree/main/test/support/e2e/blog/)**
  - Markdown template processing with Arizona syntax integration
- **[Static Blog](https://github.com/arizona-framework/arizona/tree/main/test/support/e2e/static/)**
  - Static site generation with layouts and dynamic routing

### More Examples

For additional examples and real-world usage patterns, check out the dedicated example repository:

- **[Arizona Example](https://github.com/arizona-framework/arizona_example)**
  - Complete application examples and patterns

## Documentation

- **Getting Started** — [Installation](docs/getting-started/installation.md) · [Quick
  Start](docs/getting-started/quick-start.md) · [Project
  Structure](docs/getting-started/project-structure.md)
- **Templates** — [Overview](docs/templates/overview.md) · [HTML](docs/templates/html.md) · [Erlang
  Terms](docs/templates/erlang-terms.md) · [Markdown](docs/templates/markdown.md) ·
  [Bindings](docs/templates/bindings.md)
- **Components** — [Overview](docs/components/overview.md) · [Views](docs/components/views.md) ·
  [Stateful](docs/components/stateful.md) · [Stateless](docs/components/stateless.md) ·
  [Layouts](docs/components/layouts.md) · [Slots](docs/components/slots.md) ·
  [Collections](docs/components/collections.md)
- **Events** — [Overview](docs/events/overview.md) · [WebSocket](docs/events/websocket-events.md) ·
  [Call Events](docs/events/call-events.md) · [PubSub](docs/events/pubsub.md) · [Client
  Events](docs/events/client-events.md)
- **Actions** — [Actions](docs/actions/actions.md)
- **Server** — [Configuration](docs/server/configuration.md) · [Routing](docs/server/routing.md) ·
  [Middleware](docs/server/middleware.md) · [Plugins](docs/server/plugins.md)
- **JavaScript Client** — [Setup](docs/javascript-client/setup.md) ·
  [Logging](docs/javascript-client/logging.md)
- **Deployment** — [Production](docs/deployment/production.md) · [Static
  Site](docs/deployment/static-site.md)
- **Development** — [File Watching](docs/development/file-watching.md) · [Smart
  Reload](docs/development/smart-reload.md)
- **Ecosystem** — [Editor Support](docs/ecosystem/editor-support.md) ·
  [Examples](docs/ecosystem/examples.md)

## Requirements

- Erlang/OTP 28+

## Developer Tools

- **[arizona.nvim](https://github.com/arizona-framework/arizona.nvim)** - Neovim plugin for
  development
- **[tree-sitter-arizona](https://github.com/arizona-framework/tree-sitter-arizona)** - Tree-sitter
  grammar for Arizona templates

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

Copyright (c) 2023-2026 [William Fank Thomé](https://github.com/williamthome)

Arizona is 100% open-source and community-driven. All components are
available under the Apache 2 License on [GitHub](https://github.com/arizona-framework/arizona).

See [LICENSE.md](LICENSE.md) for more information.

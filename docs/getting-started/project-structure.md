# Project Structure

- [Architecture Overview](#architecture-overview)
- [Three Main Layers](#three-main-layers)
- [Request Lifecycle](#request-lifecycle)

## Architecture Overview

Arizona follows a layered architecture that separates concerns between the HTTP
server, the component model, and the template engine. A typical Arizona project
has the following directory layout:

```text
my_app/
+-- config/
|   +-- sys.config          # Application configuration (routes, server options)
+-- src/
|   +-- my_app.app.src      # OTP application resource file
|   +-- my_app_app.erl      # Application module
|   +-- my_counter_view.erl # View modules
|   +-- ...
+-- priv/
|   +-- static/             # Static assets served directly (CSS, images, fonts)
|   +-- templates/          # External template files (if not inline)
+-- assets/
|   +-- js/                 # JavaScript client code and entry points
|   +-- package.json        # npm dependencies (@arizona-framework/client)
+-- rebar.config            # Erlang dependencies and build configuration
+-- rebar.lock
```

At startup, the `arizona_app` module boots the top-level supervisor
`arizona_sup`, which uses a `one_for_all` strategy. Its child processes
include:

- **`arizona_live`** -- a process group (pg) that tracks all active live view
  connections.
- **`arizona_pubsub`** -- a process group (pg) that provides publish/subscribe
  messaging between views and other parts of the system.
- **`arizona_watcher_sup`** -- a supervisor for file watchers that trigger the
  reloader during development, enabling automatic recompilation when source
  files change.

This supervisor tree ensures that all live connections, messaging, and file
watching are managed under a single, fault-tolerant process hierarchy.

## Three Main Layers

Arizona is organized into three distinct layers, each with a clear
responsibility:

### 1. Server Layer

The server layer is built on top of [Cowboy](https://github.com/ninenines/cowboy),
a robust Erlang HTTP server. It handles:

- HTTP request routing via configurable route tuples (`view`, `controller`,
  `websocket`, `asset`).
- A middleware pipeline that runs before a view or controller is invoked,
  allowing cross-cutting concerns such as authentication or logging.
- A WebSocket handler that maintains persistent connections for live views,
  enabling bidirectional communication between the browser and the server.

See [Routing](../server/routing.md) for details on route configuration and
middleware.

### 2. Component Layer

The component layer provides three kinds of building blocks for constructing
user interfaces:

- **Views** implement the `arizona_view` behaviour and represent page-level
  components. Each route maps to a view, and each view manages its own state
  and event handling. Views are the entry point for rendering a page.
- **Stateful components** implement the `arizona_stateful` behaviour and
  represent interactive sub-trees within a page. They maintain their own state
  and can handle events independently, making it possible to build complex
  interfaces out of composable, self-contained pieces.
- **Stateless components** are pure rendering functions. They receive data as
  input and return markup, with no internal state and no event handling. They
  are useful for shared layout elements, formatting helpers, and any fragment
  of HTML that does not need interactivity.

See [Components Overview](../components/overview.md) for a deeper look at each
component type.

### 3. Template Layer

The template layer compiles templates at build time and optimizes them for
efficient rendering and differential updates. Arizona supports three template
formats:

- **HTML** (`arizona_template:from_html/1`) -- the most common format, used
  for writing markup with embedded Erlang expressions.
- **Erlang terms** (`arizona_template:from_erl/1`) -- a structured
  representation of templates as Erlang data, useful for programmatic
  template generation.
- **Markdown** (`arizona_template:from_markdown/1`) -- renders Markdown
  content as HTML, convenient for documentation pages or content-heavy views.

Templates use a fingerprint-based caching system for differential rendering.
When state changes, only the parts of the template whose bindings have actually
changed are recomputed and sent to the client, minimizing network traffic and
DOM operations.

See [Templates Overview](../templates/overview.md) for information on template
syntax, compilation, and rendering.

## Request Lifecycle

An Arizona request passes through several stages, depending on whether it is an
initial page load or a live interaction over WebSocket.

**Initial HTTP request (server-side rendering):**

1. The browser sends an HTTP request to the server.
2. Cowboy routes the request to the matching Arizona route.
3. The Arizona middleware chain executes in order (e.g., authentication,
   session handling).
4. The matched view module's `mount/2` callback is called to initialize the
   view state.
5. The view's `render/1` callback produces the full HTML template.
6. Arizona sends the complete HTML response to the browser, providing a fully
   rendered page on first load.

**WebSocket connection (live mode):**

1. After the initial page load, the JavaScript client establishes a WebSocket
   connection to the server.
2. The client requests the initial live render from the server.
3. The server responds with the hierarchical component structure and a
   fingerprinted diff payload.
4. The client applies the structure to the DOM using morphdom, establishing the
   live view.

**Event handling (user interaction):**

1. The user interacts with the page (e.g., clicks a button with an `az-click`
   attribute).
2. The JavaScript client captures the event and pushes it over the WebSocket
   to the server.
3. The server invokes the appropriate `handle_event/3` callback on the view or
   stateful component.
4. The callback updates the state and returns the new view.
5. Arizona computes the minimal diff between the previous render and the new
   render using fingerprint-based comparison.
6. The server sends only the changed fragments to the client.
7. The client patches the DOM with the received diff, updating only the parts
   of the page that changed.

This lifecycle ensures fast initial page loads through server-side rendering
while providing real-time interactivity through WebSocket-driven differential
updates.

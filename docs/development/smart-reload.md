# Smart Reload

- [CSS-Only Reload](#css-only-reload)
- [State Preservation](#state-preservation)
- [File Type Detection](#file-type-detection)
- [Broadcast](#broadcast)

## CSS-Only Reload

When only CSS files change, Arizona performs a lightweight CSS-only reload. Instead of refreshing
the entire page, the client replaces stylesheet links in the document, preserving the current page
state and WebSocket connection. This provides instant visual feedback during style development --
you see your CSS changes applied immediately without losing scroll position, form input, or
component state.

CSS-only reload works by appending a cache-busting query parameter to each stylesheet URL, forcing
the browser to fetch the updated version while reusing the existing DOM. The transition is seamless
and near-instantaneous, making it ideal for iterating on layout and design.

## State Preservation

During Erlang code reloads, the WebSocket connection stays alive and component state is preserved.
The server recompiles and hot-loads the changed modules using the BEAM's native code loading
capabilities, then sends a reload signal to connected clients. The client requests a fresh render,
and the server responds with a diff against the current state.

This means:

- No full page refresh occurs
- Form inputs retain their values
- Scroll position is maintained
- Component state (counters, toggles, selections) is preserved
- The WebSocket connection is never interrupted

The result is a development experience where you can edit a component's rendering logic or event
handlers, save the file, and see the changes reflected in the browser with all your application
state intact.

## File Type Detection

The reload payload includes a type indicator that tells the client what kind of reload to perform:

| Type | Trigger | Client Behavior |
| ---- | ------- | --------------- |
| `erl` | `.erl`, `.hrl` files changed | Live diff -- re-render and patch DOM |
| `js` | JavaScript files changed | Full page reload -- JS re-execution |
| `css` | CSS files changed | CSS hot swap -- replace stylesheets |

The file type is determined by the reloader handler that processes the change. Each handler knows
what type of files it manages and includes the appropriate type in its broadcast payload. This
separation allows custom handlers to define their own reload strategies while the client-side logic
remains consistent.

## Broadcast

Reload events are broadcast via PubSub on the `arizona:reload` topic. The WebSocket handler
(`arizona_websocket`) subscribes to this topic when the reloader is enabled. When a reload message
arrives, it is forwarded to the client as a `reload` WebSocket message with the appropriate type.

```erlang
%% In a reloader handler:
arizona_pubsub:broadcast(~"arizona:reload", #{type => css}).
```

All connected clients receive the reload notification simultaneously, ensuring a consistent
development experience across multiple browser windows or devices. This is particularly useful when
testing responsive layouts or multi-user interactions -- every open browser tab updates at the same
time.

The broadcast mechanism is the same PubSub system used for application-level real-time features, so
there is no additional infrastructure required. The `arizona:reload` topic is simply a reserved
topic name used by the framework's development tooling.

See also: [File Watching](file-watching.md), [PubSub](../events/pubsub.md)

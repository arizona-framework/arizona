# Client Events

- [arizona.on/2](#arizonaon2)
- [Event Types](#event-types)
  - [Custom Events](#custom-events)
  - [Framework Events](#framework-events)
- [Unsubscribing](#unsubscribing)

## arizona.on/2

The Arizona JavaScript client includes an event emitter for subscribing to
client-side events. Use `arizona.on(event, callback)` to register a listener.
It returns an **unsubscribe function** that you can call to remove the listener
later.

```javascript
const unsubscribe = arizona.on("connected", () => {
    console.log("WebSocket connected!");
});
```

Use `arizona.once(event, callback)` to listen for a **single occurrence** of
an event. The listener is automatically removed after it fires once:

```javascript
arizona.once("connected", () => {
    console.log("First connection established");
    // This callback will not fire again on reconnection
});
```

Both `on` and `once` accept the same arguments: an event name (string) and a
callback function. The callback receives the event data as its argument when
the event carries a payload.

## Event Types

Events are divided into two categories: **custom events** dispatched from the
server via actions, and **framework events** emitted automatically by the
Arizona client library.

### Custom Events

Server-side code can dispatch custom events to the client by including a
`{dispatch, EventName, Data}` action in the returned actions list. This allows
the server to push arbitrary data to client-side JavaScript handlers.

On the server:

```erlang
%% Dispatch a custom event after saving
handle_event(~"save", _Params, View) ->
    %% ... save logic ...
    {[{dispatch, ~"save_complete", #{status => ~"ok"}}], View}.
```

```erlang
%% Dispatch multiple events
handle_event(~"process", _Params, View) ->
    {[
        {dispatch, ~"step_complete", #{step => 1}},
        {dispatch, ~"step_complete", #{step => 2}}
    ], View}.
```

On the client:

```javascript
// Listen for the custom event
arizona.on("save_complete", (data) => {
    showNotification(`Save status: ${data.status}`);
});

// React to processing steps
arizona.on("step_complete", (data) => {
    updateProgressBar(data.step);
});
```

Custom events are useful for triggering JavaScript-side effects that go beyond
DOM updates -- such as showing toast notifications, starting animations,
focusing an input, scrolling to an element, or interacting with third-party
JavaScript libraries.

### Framework Events

The Arizona client library emits the following built-in events at key points
in the framework's lifecycle:

| Event | Payload | Description |
| ------- | --------- | ------------- |
| `connected` | -- | The WebSocket connection has been established. |
| `disconnected` | -- | The WebSocket connection has been lost. |
| `html_patch` | -- | The DOM has been patched with new content from the server. |
| `error` | error data | An error occurred (e.g., WebSocket error, server error). |
| `reload` | -- | The server has requested a full page reload. |
| `dispatch` | event data | A dispatch action was received from the server. |
| `reply` | reply data | A reply to a call event was received. |
| `redirect` | redirect data | The server has requested a navigation to a new URL. |

```javascript
// Track connection status
arizona.on("connected", () => {
    document.getElementById("status").textContent = "Online";
});

arizona.on("disconnected", () => {
    document.getElementById("status").textContent = "Offline - Reconnecting...";
});

// React to DOM updates
arizona.on("html_patch", () => {
    // Re-initialize third-party libraries after DOM changes
    Prism.highlightAll();
});

// Handle errors
arizona.on("error", (error) => {
    console.error("Arizona error:", error);
    reportToErrorTracker(error);
});

// Handle server-initiated redirects
arizona.on("redirect", (data) => {
    console.log("Navigating to:", data.url);
});
```

Framework events fire regardless of which view or component triggered the
underlying action. They provide global hooks into the client-side lifecycle
of the application.

## Unsubscribing

There are three ways to remove event listeners:

**1. Use the returned unsubscribe function.** This is the recommended approach,
especially for listeners that should be cleaned up when a component or page
is no longer active.

```javascript
const unsubscribe = arizona.on("connected", handler);

// Later, when the listener is no longer needed:
unsubscribe();
```

**2. Use `arizona.off(event, callback)` with the same callback reference.**
This requires keeping a reference to the original function.

```javascript
function handler() {
    console.log("Connected!");
}

arizona.on("connected", handler);

// Later:
arizona.off("connected", handler);
```

Note that `off` requires the exact same function reference that was passed to
`on`. An anonymous function cannot be removed with `off` because there is no
way to reference it again.

**3. Use `arizona.removeAllListeners()` to remove listeners in bulk.**

```javascript
// Remove all listeners for a specific event
arizona.removeAllListeners("connected");

// Remove all listeners for all events
arizona.removeAllListeners();
```

Use `removeAllListeners` with care -- removing all listeners for an event may
affect other parts of the application that rely on the same event. It is most
appropriate during teardown (e.g., when navigating away from a page) or in
testing scenarios.

See also:

- [Actions](../actions/actions.md) -- the `dispatch` action and other action types
- [JavaScript Client Setup](../javascript-client/setup.md) -- initial client configuration

# Events Overview

- [Event Flow](#event-flow)
- [Event Types Summary](#event-types-summary)

## Event Flow

Arizona's event system connects the JavaScript client with server-side Erlang
processes via WebSocket. Every user interaction follows a predictable path
through the framework:

1. The user interacts with the page (click, input, form submission, etc.).
2. The JavaScript client sends an event message over the WebSocket connection.
3. The `arizona_live` GenServer receives the event on the server.
4. The appropriate `handle_event/3` callback is invoked -- either on the
   view (page-level) or on a specific stateful component, depending on how
   the event was sent.
5. The callback returns `{Actions, UpdatedState}`, where `Actions` is a list
   of instructions for the client and `UpdatedState` is the new view or
   component state.
6. Arizona computes a diff of the changed template parts by comparing the
   previous and current bindings.
7. The diff and any actions are sent back to the client over the WebSocket.
8. The client patches the DOM via morphdom and processes the returned actions
   (dispatches, replies, redirects, etc.).

Events can also flow from server to client through PubSub. When a broadcast
message arrives, the view's `handle_event/3` callback is invoked with the topic
as the event name. State changes made there trigger the same diff-and-patch
cycle described above. This means
a single user's action can update every connected client in real time without
any additional client-side code.

```text
Client                          Server
  |                                |
  |-- pushEvent("inc", %{}) ----->|
  |                                |-- handle_event/3
  |                                |-- compute diff
  |<----- diff + actions ---------|
  |                                |
  |-- patch DOM                    |
  |-- process actions              |
```

## Event Types Summary

Arizona provides four categories of events. Each serves a different
communication pattern between the client and the server.

| Type | Direction | API | Use Case |
| ---- | --------- | --- | -------- |
| WebSocket | Client -> Server | `pushEvent` | User interactions |
| Call | Client -> Server -> Client | `callEvent` | Request-reply patterns |
| PubSub | Server -> Server -> Client | `broadcast` | Real-time updates |
| Client | Server -> Client (JS) | `arizona.on` | JS-side reactions |

**WebSocket Events** are the most common type. The client sends an event name
and optional parameters; the server handles it and returns state changes.

**Call Events** extend WebSocket events with a request-reply pattern. The
client receives a Promise that resolves when the server explicitly replies.

**PubSub** enables server-to-server-to-client communication. A broadcast
message reaches every subscribed view process, which can then update its state
and push changes to the connected client.

**Client Events** are JavaScript-side events emitted by the Arizona client
library. They allow your JS code to react to framework lifecycle events
(connection status, DOM patches) and custom events dispatched from the server.

See each dedicated guide for full details:

- [WebSocket Events](websocket-events.md)
- [Call Events](call-events.md)
- [PubSub](pubsub.md)
- [Client Events](client-events.md)

# Views

- [`arizona_view` Behaviour](#arizona_view-behaviour)
- [Callbacks](#callbacks)
  - [mount](#mount)
  - [render](#render)
  - [handle\_event](#handle_event)
  - [handle\_info](#handle_info)
- [Layout](#layout)
- [State](#state)

## `arizona_view` Behaviour

Views are page-level components that implement the `arizona_view` behaviour. Each route in your
application maps to exactly one view module. The view is responsible for managing the entire page
state, handling both client-side events and Erlang messages, and coordinating its stateful child
components.

When a user first requests a page, the view is mounted and rendered server-side (SSR) to produce the
initial HTML. When the client connects via WebSocket, the view is mounted again inside an
`arizona_live` GenServer process, which keeps the connection alive and handles all subsequent
interactions through differential updates.

## Callbacks

The `arizona_view` behaviour defines two required callbacks (`mount/2` and `render/1`) and three
optional callbacks (`handle_event/3`, `handle_info/2`, and `terminate/2`).

### mount

```erlang
mount(MountArg, Request) -> View
```

Called once when the view is created, both during server-side rendering and when a WebSocket
connection is established. `MountArg` is the value provided in the route configuration. `Request` is
an `arizona_request` record with lazy-loaded fields (query parameters, headers, etc.). The callback
must return a `View` created via `arizona_view:new/3`.

```erlang
mount(#{title := Title}, _Request) ->
    arizona_view:new(?MODULE, #{title => Title, count => 0}, none).
```

### render

```erlang
render(Bindings) -> Template
```

Called to produce the view's template. It receives the current bindings map and must return a
template (built with `arizona_template:from_html/1`, `arizona_template:from_erl/1`, or
`arizona_template:from_markdown/1`).

```erlang
render(Bindings) ->
    arizona_template:from_html(<<"
        <div>
            <h1>{get_binding(title, Bindings)}</h1>
            <span>{get_binding(count, Bindings)}</span>
            <button az-click=\"increment\">+</button>
        </div>
    ">>).
```

### handle\_event

```erlang
handle_event(EventName, Params, View) -> {Actions, View}
```

Optional. Handles events sent from the client (e.g., via `az-click` attributes or `pushEvent` calls
from JavaScript) and PubSub broadcasts (where the topic becomes the event name). `EventName` is a
binary matching the event name, and `Params` is the event payload. The callback returns a list of
actions and the updated view.

```erlang
handle_event(~"increment", _Params, View) ->
    State = arizona_view:get_state(View),
    Count = arizona_stateful:get_binding(count, State),
    State1 = arizona_stateful:put_binding(count, Count + 1, State),
    {[], arizona_view:update_state(State1, View)}.
```

### handle\_info

```erlang
handle_info(Info, View) -> {Actions, View}
```

Optional. Handles non-PubSub Erlang messages sent to the live process (e.g., `erlang:send_after`
timers, direct `pid ! Msg` sends). PubSub messages are routed to `handle_event/3` instead — see
[PubSub](../events/pubsub.md) for details.

```erlang
handle_info(update_tick, View) ->
    State = arizona_view:get_state(View),
    Count = arizona_stateful:get_binding(count, State),
    State1 = arizona_stateful:put_binding(count, Count + 1, State),
    erlang:send_after(1000, self(), update_tick),
    {[], arizona_view:update_state(State1, View)}.
```

There is also an optional `terminate/2` callback:

```erlang
terminate(Reason, View) -> term()
```

Called when the live process is shutting down. Use it for cleanup tasks such as unsubscribing from
PubSub topics or releasing resources.

## Layout

The layout is set via the third argument of `arizona_view:new/3`. It wraps the view's rendered
content with the full HTML document structure (head, body, scripts, stylesheets). The layout tuple
has the format `{LayoutModule, RenderFun, SlotName, Bindings}`, or pass `none` for no layout.

```erlang
mount(_MountArg, _Req) ->
    Layout = {my_layout, render, inner_block, #{title => ~"My App"}},
    arizona_view:new(?MODULE, #{count => 0}, Layout).
```

- `LayoutModule` — the module containing the layout render function.
- `RenderFun` — the function name to call (typically `render`).
- `SlotName` — the atom used inside the layout to insert the view content via `render_slot/1`.
- `Bindings` — a map of bindings available to the layout template.

See [Layouts](layouts.md) for a complete layout example.

## State

View state is accessed and updated through the `arizona_view` API:

- `arizona_view:get_state(View)` — returns the view's state (an `arizona_stateful:state()` record).
- `arizona_view:update_state(State, View)` — returns the view with the updated state.

The state record uses the same binding API as stateful components:

- `arizona_stateful:get_binding(Key, State)` — get a binding value.
- `arizona_stateful:get_binding(Key, State, Default)` — get a binding value with a default.
- `arizona_stateful:put_binding(Key, Value, State)` — set a binding value. This marks the binding as
  changed, triggering a diff on the next render.
- `arizona_stateful:merge_bindings(NewBindings, State)` — merge multiple bindings at once.
- `arizona_stateful:get_changed_bindings(State)` — retrieve only the bindings that have changed
  since the last render.

Views can also access the state of their stateful children:

- `arizona_view:get_stateful_state(Id, View)` — get a child component's state by its `id`.
- `arizona_view:put_stateful_state(Id, State, View)` — update a child component's state by its `id`.

See also: [Stateful Components](stateful.md), [Layouts](layouts.md), [WebSocket
Events](../events/websocket-events.md)

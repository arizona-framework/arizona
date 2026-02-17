# WebSocket Events

- [Client API](#client-api)
  - [pushEvent/pushEventTo](#pusheventpusheventto)
- [Server Callbacks](#server-callbacks)
  - [handle\_event/3](#handle_event3)
- [Targeting Components](#targeting-components)

## Client API

The JavaScript client provides methods to send events to the server over the
WebSocket connection. Events carry a name (a string) and an optional params
object that is JSON-encoded and decoded on the server side.

### pushEvent/pushEventTo

`pushEvent(event, params?)` sends an event to the **view's** `handle_event/3`
callback. Use this for page-level events that are not scoped to a particular
component.

`pushEventTo(statefulId, event, params?)` sends an event to a **specific
stateful component** identified by its `statefulId`. Use this when an action
should be handled by a component rather than the view.

```javascript
// Send an event to the view
arizona.pushEvent("submit_form", { name: "Alice", email: "alice@example.com" });

// Send an event to a specific stateful component
arizona.pushEventTo("counter-1", "increment", { amount: 5 });
```

Events can also be triggered declaratively via HTML attributes. The `az-click`
attribute sends a click event to the enclosing component (or view) without
writing any JavaScript:

```html
<button az-click="increment">+</button>
<button az-click="decrement">-</button>
```

When `params` is omitted, the server receives an empty map (`#{}`) as the
params argument.

## Server Callbacks

Events sent from the client are received by the `handle_event/3` callback
on the targeted module (view or stateful component). The callback processes the
event and returns a tuple of actions and the updated state.

### handle\_event/3

The callback signature depends on whether the target is a view or a stateful
component:

- **View**: `handle_event(EventName, Params, View) -> {Actions, View}`
- **Stateful component**: `handle_event(EventName, Params, State) -> {Actions, State}`

`EventName` is a binary string (e.g., `~"increment"`, `~"submit_form"`).
`Params` is the decoded JSON payload -- typically a map with binary keys.
`Actions` is a list of action tuples to send back to the client (or an empty
list `[]` if no client-side actions are needed).

```erlang
%% View handler -- handle a form submission
handle_event(~"submit_form", #{~"name" := Name}, View) ->
    State = arizona_view:get_state(View),
    State1 = arizona_stateful:put_binding(name, Name, State),
    {[], arizona_view:update_state(State1, View)}.
```

```erlang
%% Stateful component handler -- handle an increment event
handle_event(~"increment", #{~"amount" := Amount}, State) ->
    Count = arizona_stateful:get_binding(count, State),
    {[], arizona_stateful:put_binding(count, Count + Amount, State)}.
```

In both cases, Arizona computes a diff of the template bindings after
`handle_event/3` returns. Only the parts of the template that actually changed
are sent to the client.

## Targeting Components

Arizona routes events to the correct handler based on how they were sent:

- **`pushEvent`** always routes to the **view** module's `handle_event/3`.
  Use this for page-level concerns such as navigation, global state, or
  events that span multiple components.

- **`pushEventTo`** routes to the **stateful component** whose `id` matches
  the provided `statefulId`. The event is handled entirely within that
  component's `handle_event/3` callback.

- **`az-click`** (and other `az-` event attributes) on elements within a
  stateful component's template automatically target that component. There is
  no need to specify a target -- the framework resolves it based on the
  component hierarchy.

This separation keeps event handling close to the state it modifies. A counter
component handles its own increment/decrement events, while the view handles
page-level concerns like form submissions or route changes.

```erlang
%% In a view module -- delegates page-level events
handle_event(~"change_tab", #{~"tab" := Tab}, View) ->
    State = arizona_view:get_state(View),
    State1 = arizona_stateful:put_binding(active_tab, Tab, State),
    {[], arizona_view:update_state(State1, View)}.
```

```erlang
%% In a stateful component module -- handles its own events
handle_event(~"toggle", _Params, State) ->
    Open = arizona_stateful:get_binding(open, State),
    {[], arizona_stateful:put_binding(open, not Open, State)}.
```

See also:

- [Call Events](call-events.md) -- for request-reply patterns
- [Views](../components/views.md) -- view lifecycle and callbacks
- [Stateful Components](../components/stateful.md) -- component lifecycle
- [Actions](../actions/actions.md) -- available action types

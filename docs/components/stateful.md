# Stateful Components

- [`arizona_stateful` Behaviour](#arizona_stateful-behaviour)
- [Lifecycle](#lifecycle)
  - [mount](#mount)
  - [render](#render)
  - [handle\_event](#handle_event)
  - [unmount](#unmount)
- [Diffs](#diffs)

## `arizona_stateful` Behaviour

Stateful components are interactive sub-trees within a view. They implement the `arizona_stateful`
behaviour and maintain their own state, handle events independently, and produce minimal diffs when
their state changes. Each stateful component must have a unique binary `id` binding for event
routing and state management.

Render a stateful component inside a template with `render_stateful/2` or `render_stateful/3`:

```erlang
%% In a parent template:
render(Bindings) ->
    arizona_template:from_html(<<"
        <div>
            {render_stateful(my_counter, #{id => ~"counter-1", initial => 0})}
        </div>
    ">>).
```

You can also pass render options as a third argument with `render_stateful(Module, Bindings,
Options)`.

## Lifecycle

Stateful components follow a mount, render, handle_event, unmount lifecycle. The mount callback
initializes state, render produces the template, handle_event responds to client interactions, and
unmount performs cleanup when the component is removed from the tree.

### mount

```erlang
mount(Bindings) -> State
```

Called once when the component first appears in the rendered tree. It receives the bindings map
passed from the parent. The callback must return a state created via `arizona_stateful:new/2`.

```erlang
mount(Bindings) ->
    Initial = maps:get(initial, Bindings, 0),
    arizona_stateful:new(?MODULE, #{
        id => maps:get(id, Bindings),
        count => Initial
    }).
```

The bindings passed to `mount/1` are the ones provided by the parent in the `render_stateful/2`
call. These are plain maps, so use `maps:get/2,3` to read them. After calling
`arizona_stateful:new/2`, use `arizona_stateful:get_binding/2,3` on the returned state.

### render

```erlang
render(Bindings) -> Template
```

Called on every render cycle when the component's bindings have changed. It receives the current
bindings map and must return a template.

```erlang
render(Bindings) ->
    arizona_template:from_html(<<"
        <div id={get_binding(id, Bindings)}>
            <span>{get_binding(count, Bindings)}</span>
            <button az-click-to={get_binding(id, Bindings)} az-click=\"increment\">+</button>
            <button az-click-to={get_binding(id, Bindings)} az-click=\"decrement\">-</button>
        </div>
    ">>).
```

### handle\_event

```erlang
handle_event(EventName, Params, State) -> {Actions, State}
```

Optional. Handles events targeted at this specific component. Events are routed to the component by
its `id` when the client uses `pushEventTo` or `az-click-to` attributes. `EventName` is a binary,
`Params` is the event payload. Returns a list of actions and the updated state.

```erlang
handle_event(~"increment", _Params, State) ->
    Count = arizona_stateful:get_binding(count, State),
    {[], arizona_stateful:put_binding(count, Count + 1, State)};

handle_event(~"decrement", _Params, State) ->
    Count = arizona_stateful:get_binding(count, State),
    {[], arizona_stateful:put_binding(count, Count - 1, State)}.
```

### unmount

```erlang
unmount(State) -> ok
```

Optional. Called when the component is removed from the tree, for example when conditional rendering
causes it to disappear or when the parent view navigates away. Use this callback for cleanup tasks
such as cancelling timers or releasing resources.

```erlang
unmount(State) ->
    TimerRef = arizona_stateful:get_binding(timer_ref, State),
    erlang:cancel_timer(TimerRef),
    ok.
```

## Diffs

Arizona's differential rendering engine minimizes the data sent over the WebSocket by tracking
exactly which bindings changed and which template elements depend on those bindings.

When you call `arizona_stateful:put_binding/3`, the binding is marked as changed. On the next render
cycle, only the template expressions that reference changed bindings are re-evaluated. The diff
engine compares the new output against the previous output using **fingerprints** — structural
hashes of template fragments. If a fingerprint matches, the entire sub-tree is skipped.

Each stateful component produces its own diff independently. The resulting patch is a minimal set of
DOM operations that the client applies using morphdom. This per-component diffing means that a state
change in one component does not cause unrelated components to re-render.

The `id` binding plays a key role here: it scopes the fingerprints so that each component's template
structures are compared against the correct previous version, even when multiple instances of the
same component module exist on the page.

See also: [Views](views.md), [Bindings](../templates/bindings.md), [WebSocket
Events](../events/websocket-events.md)

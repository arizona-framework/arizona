# Bindings

- [Functions](#functions)
  - [`get_binding/2`](#get_binding2)
  - [`put_binding/3`](#put_binding3)
- [Binding Lifecycle](#binding-lifecycle)

## Functions

Bindings are the key-value data that templates render. They are represented as
maps where keys are atoms and values can be any Erlang term.

In templates, bindings are accessed through the `Bindings` variable that is
passed to the `render/1` callback. On stateful component state, bindings are
managed through the `arizona_stateful` API.

### `get_binding/2`

Retrieves a binding value by key. This function crashes if the key is not
present in the bindings.

Inside a template expression:

```erlang
render(Bindings) ->
    arizona_template:from_html(~"<span>{get_binding(name, Bindings)}</span>").
```

On stateful component state:

```erlang
Name = arizona_stateful:get_binding(name, State).
```

Arizona provides several variants for different retrieval strategies:

- **`get_binding/3`** -- accepts a default value, returned when the key is
  missing:

    ```erlang
    Name = arizona_stateful:get_binding(name, State, ~"Anonymous").
    ```

- **`get_binding_lazy/3`** -- accepts a zero-arity fun that is only called
  when the key is missing, useful for expensive default computations:

    ```erlang
    Config = arizona_stateful:get_binding_lazy(config, State, fun() ->
        load_default_config()
    end).
    ```

- **`get_binding/3` with a sentinel default** -- use a sentinel value such as
  `undefined` to check whether a key exists without crashing:

    ```erlang
    case arizona_stateful:get_binding(email, State, undefined) of
        undefined -> skip;
        Email -> send_notification(Email)
    end.
    ```

### `put_binding/3`

Updates a binding on a stateful component's state and marks the change for the
diff engine. Returns the updated state.

```erlang
arizona_stateful:put_binding(Key, Value, State) -> State.
```

When a binding is updated via `put_binding/3`, Arizona records that the key has
changed. On the next render cycle, the diff engine inspects the set of changed
bindings and re-evaluates only the template expressions that depend on those
keys. Unchanged parts of the template are skipped entirely.

A typical usage in an event handler:

```erlang
handle_event(~"update_name", #{~"name" := Name}, View) ->
    State = arizona_view:get_state(View),
    State1 = arizona_stateful:put_binding(name, Name, State),
    View1 = arizona_view:update_state(State1, View),
    {[], View1}.
```

To update multiple bindings at once, use `merge_bindings/2`:

```erlang
handle_event(~"submit_form", Params, View) ->
    State = arizona_view:get_state(View),
    State1 = arizona_stateful:merge_bindings(#{
        name => maps:get(~"name", Params),
        email => maps:get(~"email", Params),
        submitted => true
    }, State),
    View1 = arizona_view:update_state(State1, View),
    {[], View1}.
```

## Binding Lifecycle

Bindings flow through the component lifecycle in a predictable sequence:

1. **mount** -- Initial bindings are set when creating state via
   `arizona_view:new/3` or `arizona_stateful:new/2`. These bindings establish
   the initial values that the component will render.

    ```erlang
    mount(_MountArg, _Request) ->
        arizona_view:new(?MODULE, #{
            name => ~"World",
            count => 0
        }, none).
    ```

2. **render** -- The `render/1` callback receives the current bindings map.
   Template expressions read values from this map using `get_binding/2` and
   related functions. The render function produces the template structure that
   Arizona will convert to HTML.

    ```erlang
    render(Bindings) ->
        arizona_template:from_html(<<"
            <div>
                <h1>Hello, {get_binding(name, Bindings)}!</h1>
                <p>Count: {get_binding(count, Bindings)}</p>
            </div>
        ">>).
    ```

3. **handle_event / handle_info** -- When user interactions or system messages
   arrive, these callbacks update bindings via `put_binding/3` or
   `merge_bindings/2`. Each call to `put_binding/3` marks the binding as
   changed, building up a set of modifications.

    ```erlang
    handle_event(~"increment", _Payload, View) ->
        State = arizona_view:get_state(View),
        Count = arizona_stateful:get_binding(count, State),
        State1 = arizona_stateful:put_binding(count, Count + 1, State),
        View1 = arizona_view:update_state(State1, View),
        {[], View1}.
    ```

4. **diff** -- After event handling, the diff engine calls
   `get_changed_bindings/1` to retrieve the set of keys that were modified. It
   then re-evaluates only the template expressions that reference those keys,
   produces a minimal set of changes, and sends them to the client for
   patching.

This lifecycle ensures that updates are efficient: only the bindings that
actually changed trigger re-evaluation, and only the affected parts of the
HTML are sent to the browser.

---

See also: [Views](../components/views.md) |
[Stateful Components](../components/stateful.md)

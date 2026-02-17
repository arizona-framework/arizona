# Slots

- [`render_slot/1`](#render_slot1)
- [Passing Content](#passing-content)
- [Composition Patterns](#composition-patterns)

## `render_slot/1`

The `render_slot/1` function inserts slot content into a template. It accepts a slot value, which
can be one of three types:

- **`view`** — used in layouts to insert the view's rendered output.
- **`template()`** — a template returned by `arizona_template:from_html/1` or similar.
- **`arizona_html:value()`** — a raw value such as a binary, iodata, atom, or number.

Use `render_slot/1` by retrieving the slot value from bindings and passing it in:

```erlang
render(Bindings) ->
    arizona_template:from_html(<<"
        <div class=\"card\">
            <div class=\"card-body\">
                {render_slot(get_binding(inner_block, Bindings))}
            </div>
        </div>
    ">>).
```

The slot value is resolved at render time. If the value is a template, it is rendered and its output
is inserted. If it is a raw value, it is inserted directly (with appropriate HTML escaping).

## Passing Content

Slot content is passed via bindings when rendering components.

**In layouts**, the view content is automatically injected under the slot name specified in the
layout tuple. You do not need to pass it manually — Arizona handles this when it renders the view
inside its layout:

```erlang
%% Layout tuple: the view output will be available as inner_block
Layout = {my_layout, render, inner_block, #{title => ~"Home"}}
```

**In stateless components**, pass slot content as a regular binding:

```erlang
%% Parent template passes content via bindings:
render(Bindings) ->
    arizona_template:from_html(<<"
        {render_stateless(my_components, modal, #{
            header => ~"Confirm Action",
            inner_block => ~"Are you sure you want to proceed?",
            confirm => ~"Yes, continue",
            cancel => ~"Cancel"
        })}
    ">>).
```

The stateless component then uses `render_slot/1` to place each piece of content:

```erlang
modal(Bindings) ->
    arizona_template:from_html(<<"
        <div class=\"modal\">
            <div class=\"modal-header\">
                {render_slot(get_binding(header, Bindings))}
            </div>
            <div class=\"modal-body\">
                {render_slot(get_binding(inner_block, Bindings))}
            </div>
            <div class=\"modal-footer\">
                <button class=\"btn-cancel\">{render_slot(get_binding(cancel, Bindings))}</button>
                <button class=\"btn-confirm\">{render_slot(get_binding(confirm, Bindings))}</button>
            </div>
        </div>
    ">>).
```

**In stateful components**, slot content can similarly be passed via the bindings in
`render_stateful/2`:

```erlang
{render_stateful(my_accordion, #{
    id => ~"faq-1",
    title => ~"What is Arizona?",
    inner_block => ~"Arizona is an Erlang web framework for building interactive applications."
})}
```

## Composition Patterns

Slots enable flexible component composition. Here are common patterns:

**Named slots** — Use multiple binding keys to define distinct content areas within a single
component. The modal example above demonstrates this with `header`, `inner_block`, `confirm`, and
`cancel` slots.

**Default content** — Use `arizona_stateful:get_binding/3` (or `get_binding/3`) with a default value
for optional slots. This lets consumers of your component omit slots that have sensible defaults:

```erlang
card(Bindings) ->
    arizona_template:from_html(<<"
        <div class=\"card\">
            <div class=\"card-header\">
                {render_slot(get_binding(header, Bindings, ~"Untitled"))}
            </div>
            <div class=\"card-body\">
                {render_slot(get_binding(inner_block, Bindings))}
            </div>
            <div class=\"card-footer\">
                {render_slot(get_binding(footer, Bindings, <<>>))}
            </div>
        </div>
    ">>).
```

**Nested composition** — Slots can contain the output of other components. Because a slot value can
be any template, you can compose deeply nested structures while keeping each component focused on a
single responsibility.

See also: [Layouts](layouts.md), [Stateless Components](stateless.md)

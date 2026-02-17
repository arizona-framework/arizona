# Stateless Components

- [Pure Rendering Functions](#pure-rendering-functions)
- [`render_stateless/3`](#render_stateless3)
- [When to Use](#when-to-use)

## Pure Rendering Functions

Stateless components are plain Erlang functions that accept a bindings map and return a template.
They do not implement any behaviour, hold no state, and cannot handle events. They re-render
whenever their parent re-renders with changed bindings that affect the stateless component's inputs.

Define a stateless component as a regular exported function:

```erlang
-module(my_components).
-export([badge/1, icon/1]).

badge(Bindings) ->
    arizona_template:from_html(<<"
        <span class=\"badge {get_binding(color, Bindings)}\">
            {get_binding(label, Bindings)}
        </span>
    ">>).

icon(Bindings) ->
    arizona_template:from_html(<<"
        <svg class=\"icon icon-{get_binding(name, Bindings)}\">
            <use href=\"/assets/icons.svg#{get_binding(name, Bindings)}\"></use>
        </svg>
    ">>).
```

Because stateless components are just functions, they are easy to test in isolation — call the
function with a bindings map and assert on the returned template.

## `render_stateless/3`

Call stateless components inside templates with `render_stateless(Module, Function, Bindings)`:

```erlang
render(Bindings) ->
    arizona_template:from_html(<<"
        <div class=\"user-profile\">
            {render_stateless(my_components, icon, #{name => ~"user"})}
            <h2>{get_binding(username, Bindings)}</h2>
            {render_stateless(my_components, badge, #{
                color => ~"green",
                label => ~"Active"
            })}
        </div>
    ">>).
```

Under the hood, this calls `arizona_stateless:call_render_callback(Module, Function, Bindings)`. The
returned template is inlined directly into the parent template at the call site, becoming part of
the parent's rendered output and diff tree.

## When to Use

Use stateless components for reusable UI fragments that do not need their own state or event
handling:

- **Display elements**: badges, tags, labels, status indicators
- **Navigation**: menus, breadcrumbs, links
- **Cards and panels**: content wrappers with consistent styling
- **Formatted data**: dates, currencies, numbers, addresses
- **Icons and images**: SVG icons, avatar displays

Stateless components are the simplest and most performant component type. Because they carry no
state or lifecycle overhead, prefer them whenever possible.

If you find that a component needs to handle user interactions (clicks, form submissions) or
maintain values between renders independently of its parent, promote it to a [stateful
component](stateful.md) instead.

See also: [Stateful Components](stateful.md), [Components Overview](overview.md)

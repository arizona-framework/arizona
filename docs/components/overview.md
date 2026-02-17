# Components Overview

- [Hierarchical Component Model](#hierarchical-component-model)
- [Component Types Summary](#component-types-summary)
- [`id` Requirement](#id-requirement)

## Hierarchical Component Model

Arizona uses a hierarchical component tree to structure every page. At the root of the tree sits a
**view** — there is exactly one view per route. Views contain two kinds of child components:
**stateful components**, which are interactive sub-trees with their own state and event handling,
and **stateless components**, which are pure rendering functions with no state or lifecycle.

The hierarchy looks like this:

```text
View (page root)
├── Stateful Component (interactive, own state)
│   ├── Stateless Component (pure render)
│   └── Stateful Component (nested)
├── Stateless Component (pure render)
└── Stateful Component (interactive, own state)
```

This tree structure enables efficient differential rendering. When state changes in a component,
only that sub-tree is re-evaluated and diffed against its previous output. The diff engine uses
fingerprints and variable dependency tracking to skip unchanged template structures entirely,
sending minimal patches to the client over WebSocket.

## Component Types Summary

Arizona provides three component types, each suited to a different role:

| Type | Behaviour | State | Events | Use Case |
| ---- | --------- | ----- | ------ | -------- |
| View | `arizona_view` | Yes | `handle_event`, `handle_info` | Page-level, one per route |
| Stateful | `arizona_stateful` | Yes | `handle_event` | Interactive sub-components |
| Stateless | (none) | No | No | Pure rendering helpers |

**Views** are the entry point for every page. They run inside an `arizona_live` GenServer process
when connected via WebSocket, giving them the ability to handle both client-side events and Erlang
messages (such as PubSub notifications). See [Views](views.md) for details.

**Stateful components** live within a view and manage their own slice of state. They handle events
targeted at them via `pushEventTo` and produce their own diffs independently. See [Stateful
Components](stateful.md) for details.

**Stateless components** are plain Erlang functions that accept bindings and return a template. They
have no behaviour module, no state, and no event handling. They re-render whenever their parent
re-renders with changed bindings. See [Stateless Components](stateless.md) for details.

## `id` Requirement

Every stateful component must have a unique `id` binding of type binary. The `id` serves several
purposes:

- **Event targeting**: Client-side events use the `id` to route to the correct component's
  `handle_event` callback.
- **State lookup**: The parent view accesses child state via `arizona_view:get_stateful_state(Id,
  View)` and `arizona_view:put_stateful_state(Id, State, View)`.
- **Fingerprint scoping**: The diff engine uses the `id` to scope fingerprints, ensuring that
  template structures are compared against the correct previous version.
- **Differential updates**: The `id` identifies which DOM sub-tree to patch when a component's state
  changes.

Views do not need an explicit `id` — they implicitly use the module name as their root identifier.

```erlang
%% The id must be unique among siblings and is always a binary:
render_stateful(my_counter, #{id => ~"counter-1", initial => 0})
render_stateful(my_counter, #{id => ~"counter-2", initial => 10})
```

See also: [Views](views.md), [Stateful Components](stateful.md), [Stateless
Components](stateless.md)

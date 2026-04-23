---
name: new-handler
description: Scaffold a new Arizona handler module. Ask whether it's a route-level view or an embeddable stateful component.
argument-hint: [module_name]
allowed-tools: Write, Glob
---

Scaffold a new handler module named `$ARGUMENTS` in `src/`. First ask the user which kind it is:

- **Route-level view** (included in routes, rendered at the top of a URL) → `arizona_view.hrl`,
  `mount/2` takes `(Bindings, Request)`
- **Embeddable stateful component** (instantiated from a parent template via
  `?stateful(Handler, Props)`) → `arizona_stateful.hrl`, `mount/1`

### View template

```erlang
-module($ARGUMENTS).
-include("arizona_view.hrl").
-export([mount/2, render/1, handle_event/3]).

mount(Bindings, _Req) ->
    {maps:merge(#{id => <<"$ARGUMENTS">>}, Bindings), #{}}.

render(Bindings) ->
    ?html({'div', [{id, ?get(id)}], [
        <<"$ARGUMENTS">>
    ]}).

handle_event(_Event, _Payload, Bindings) ->
    {Bindings, #{}, []}.
```

### Stateful component template

```erlang
-module($ARGUMENTS).
-include("arizona_stateful.hrl").
-export([mount/1, render/1, handle_event/3]).

mount(Bindings) ->
    {maps:merge(#{id => <<"$ARGUMENTS">>}, Bindings), #{}}.

render(Bindings) ->
    ?html({'div', [{id, ?get(id)}], [
        <<"$ARGUMENTS">>
    ]}).

handle_event(_Event, _Payload, Bindings) ->
    {Bindings, #{}, []}.
```

Rules:

- File goes in `src/$ARGUMENTS.erl`
- `mount` returns `{Bindings, Resets}` where `Resets` is typically `#{}`
- `render/1` returns a `?html(...)` template; the root element must use `{id, ?get(id)}`
- `handle_event/3` returns `{Bindings, Resets, Effects}`
- If the user provides additional details about what the handler should do, adapt the template
  accordingly

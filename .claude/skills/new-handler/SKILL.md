---
name: new-handler
description: Scaffold a new stateful Arizona handler module with mount, render, and handle_event.
argument-hint: [module_name]
allowed-tools: Write, Glob
---

Create a new stateful handler module named `$ARGUMENTS` in `src/`.

Use this template:

```erlang
-module($ARGUMENTS).
-include("arizona_stateful.hrl").
-export([mount/1, render/1, handle_event/3]).

mount(Bindings) ->
    maps:merge(#{id => <<"$ARGUMENTS">>}, Bindings).

render(Bindings) ->
    ?html({'div', [], [
        <<"$ARGUMENTS">>
    ]}).

handle_event(_Event, _Payload, Bindings) ->
    {Bindings, []}.
```

Rules:
- File goes in `src/$ARGUMENTS.erl`
- Include `arizona_stateful.hrl` (sets behaviour + parse transform + macros)
- `mount/1` must set an `id` key in bindings
- `render/1` returns a `?html(...)` template
- `handle_event/3` returns `{Bindings, Effects}`
- If the user provides additional details about what the handler should do, adapt the template accordingly

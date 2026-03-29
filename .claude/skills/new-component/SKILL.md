---
name: new-component
description: Scaffold a new stateless Arizona component module with render/1.
argument-hint: [module_name]
allowed-tools: Write, Glob
---

Create a new stateless component module named `$ARGUMENTS` in `src/`.

Use this template:

```erlang
-module($ARGUMENTS).
-include("arizona_stateless.hrl").
-export([render/1]).

render(Bindings) ->
    ?html({'div', [], [
        <<"$ARGUMENTS">>
    ]}).
```

Rules:
- File goes in `src/$ARGUMENTS.erl`
- Include `arizona_stateless.hrl` (sets parse transform + macros, no behaviour)
- `render/1` takes a `Bindings` map (props passed from parent via `?stateless`)
- Use `?get(Key)` or `?get(Key, Default)` to access bindings
- No `mount`, `handle_event`, or `handle_info` -- stateless components are pure render functions
- If the user describes what the component should render, adapt the template accordingly

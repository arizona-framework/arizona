# Templates Overview

- [Compile-Time Optimization](#compile-time-optimization)
- [Differential Rendering](#differential-rendering)
- [Template Expressions](#template-expressions)

## Compile-Time Optimization

Arizona templates are processed at compile time via a parse transform
(`arizona_parse_transform`). When you call `arizona_template:from_html/1`,
`arizona_template:from_erl/1`, or `arizona_template:from_markdown/1`, the
parse transform intercepts the call during compilation and separates the
template into static HTML fragments and dynamic expressions. Each dynamic
expression becomes a callback function, while static parts are stored as
pre-computed binaries.

The result is a template structure with pre-computed fingerprints. At runtime,
there is zero parsing overhead -- the template is already decomposed into its
constituent parts and ready for rendering.

```erlang
%% This call is rewritten at compile time by the parse transform.
%% At runtime, the template is already a compiled data structure.
render(Bindings) ->
    arizona_template:from_html(~"<h1>{get_binding(title, Bindings)}</h1>").
```

## Differential Rendering

When state changes in a stateful component, Arizona does not re-render the
entire template. Instead, the engine computes only the differences between the
previous and current output.

The diff engine tracks variable dependencies so that changing a single binding
re-evaluates only the dynamic parts that depend on it. Fingerprints let the
engine skip unchanged template structures entirely, making updates efficient
even for large pages.

Arizona supports three render modes:

- **`render`** -- produces full HTML output, used for the initial page render.
- **`diff`** -- produces only the changed fragments, used when state updates
  occur after the initial render.
- **`hierarchical`** -- produces a structured representation for client-side
  tracking and patching.

The combination of compile-time decomposition and fingerprint-based diffing
means that Arizona sends the minimum possible data over the wire on each
update.

## Template Expressions

Dynamic values are inserted into templates using `{Expression}` syntax. Any
valid Erlang expression can appear inside the braces. To include a literal
`{` character in your output, escape it as `\{`.

Inside template expressions, the following functions are available:

| Function | Purpose |
| --- | --- |
| `get_binding(Key, Bindings)` | Retrieve a binding value (crashes if missing) |
| `find_binding(Key, Bindings)` | Retrieve a binding value (returns `{ok, V} \| error`) |
| `get_binding_lazy(Key, Bindings, DefaultFun)` | Retrieve with a lazy default |
| `render_stateful(Module, Bindings)` | Render a stateful child component |
| `render_stateless(Module, Fun, Bindings)` | Render a stateless function component |
| `render_slot(SlotValue)` | Render a slot passed from a parent |
| `render_list(Fun, List)` | Render a list of items |
| `render_map(Fun, Map)` | Render a map of key-value pairs |

```erlang
render(Bindings) ->
    arizona_template:from_html(<<"
        <ul>
            {render_list(fun(Item) ->
                arizona_template:from_html(<<\"<li>{Item}</li>\">>)
            end, get_binding(items, Bindings))}
        </ul>
    ">>).
```

---

See also: [HTML Templates](html.md) | [Erlang Terms](erlang-terms.md) |
[Markdown](markdown.md) | [Bindings](bindings.md)

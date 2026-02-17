# Erlang Terms Templates

- [Element Tuples](#element-tuples)
- [Boolean Attributes](#boolean-attributes)
- [Void Elements](#void-elements)
- [Nested Components](#nested-components)

## Element Tuples

Erlang terms templates provide a programmatic way to build HTML using native
Erlang data structures. Create them with `arizona_template:from_erl/1`.

Each HTML element is represented as a tuple:

- **`{Tag, Attrs, Children}`** -- a standard element with a tag name (atom), a
  list of attribute tuples, and a list of children.
- **`{Tag, Attrs}`** -- a self-closing or void element with no children.

```erlang
render(Bindings) ->
    arizona_template:from_erl(
        {div, [{class, ~"container"}], [
            {h1, [], [get_binding(title, Bindings)]},
            {p, [], [~"Hello from Erlang terms!"]}
        ]}
    ).
```

This renders as:

```html
<div class="container">
    <h1>My Title</h1>
    <p>Hello from Erlang terms!</p>
</div>
```

Attributes are `{Name, Value}` tuples where `Name` is an atom and `Value` is
typically a binary. Children can be binaries (text nodes), other element tuples,
or the result of calling template functions like `render_stateful/2`.

## Boolean Attributes

HTML boolean attributes (such as `disabled`, `checked`, `readonly`, `hidden`)
are rendered by including bare atoms in the attributes list. A bare atom renders
as the attribute name alone (no value), following the HTML boolean attribute
convention:

```erlang
{button, [disabled], [~"Click"]}
%% Renders: <button disabled>Click</button>

{button, [], [~"Click"]}
%% Renders: <button>Click</button>
```

Note that `{Key, Value}` tuples always render as `Key="Value"`, regardless of
the value. For example, `{disabled, true}` renders as `disabled="true"`, not
as a boolean attribute:

```erlang
{button, [{disabled, true}], [~"Click"]}
%% Renders: <button disabled="true">Click</button>
```

To conditionally include a boolean attribute, use a conditional where each
branch contains a static attribute list:

```erlang
render(Bindings) ->
    arizona_template:from_erl(
        case get_binding(locked, Bindings) of
            true -> {input, [{type, ~"text"}, readonly]};
            false -> {input, [{type, ~"text"}]}
        end
    ).
```

When `locked` is `true`, the input renders with the `readonly` attribute. When
`locked` is `false`, the attribute is omitted and the input becomes editable.

## Void Elements

HTML void elements are elements that cannot have children. These include `br`,
`hr`, `img`, `input`, `meta`, `link`, `area`, `base`, `col`, `embed`,
`source`, `track`, and `wbr`. Use the 2-tuple form `{Tag, Attrs}` for these
elements:

```erlang
{hr, []}
%% Renders: <hr />

{img, [{src, ~"/photo.jpg"}, {alt, ~"A photo"}]}
%% Renders: <img src="/photo.jpg" alt="A photo" />

{input, [{type, ~"email"}, {placeholder, ~"you@example.com"}]}
%% Renders: <input type="email" placeholder="you@example.com" />

{br, []}
%% Renders: <br />
```

Void elements are rendered in XHTML-compatible style with a trailing `/>`.

## Nested Components

Erlang terms templates can render child components inline using the standard
template functions. This lets you compose complex UIs from smaller, reusable
pieces.

```erlang
render(Bindings) ->
    arizona_template:from_erl(
        {div, [], [
            {h1, [], [~"Dashboard"]},
            render_stateful(my_counter, #{id => ~"counter-1", initial => 0}),
            render_stateless(?MODULE, sidebar, #{
                items => get_binding(items, Bindings)
            })
        ]}
    ).
```

In this example, `render_stateful/2` embeds a stateful component
(`my_counter`) with its own lifecycle and state management. The
`render_stateless/3` call embeds a stateless function component defined as
`sidebar/1` in the current module.

You can also render lists of components:

```erlang
render(Bindings) ->
    arizona_template:from_erl(
        {ul, [], [
            render_list(fun(Item) ->
                {li, [], [Item]}
            end, get_binding(items, Bindings))
        ]}
    ).
```

---

See also: [Templates Overview](overview.md) |
[Components Overview](../components/overview.md)

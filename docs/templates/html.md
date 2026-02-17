# HTML Templates

- [Inline HTML Templates](#inline-html-templates)
- [File-Based Templates](#file-based-templates)
- [Expressions and Escaping](#expressions-and-escaping)

## Inline HTML Templates

HTML templates are created with `arizona_template:from_html/1` by passing an
HTML binary string. Dynamic values are embedded using `{}` expressions.

```erlang
render(Bindings) ->
    arizona_template:from_html(<<"
        <div class=\"greeting\">
            <h1>Hello, {get_binding(name, Bindings)}!</h1>
            <p>Welcome to Arizona.</p>
        </div>
    ">>).
```

At compile time, the parse transform processes this call and separates the
template into static and dynamic parts. In the example above, the static parts
are the surrounding HTML fragments:

```text
"<div class=\"greeting\"><h1>Hello, "
"!</h1><p>Welcome to Arizona.</p></div>"
```

The expression `get_binding(name, Bindings)` becomes a dynamic callback that is
evaluated at render time. Static parts are never re-evaluated, which makes
rendering fast and memory-efficient.

You can use any valid Erlang expression inside `{}`, including function calls,
case expressions, and arithmetic:

```erlang
render(Bindings) ->
    arizona_template:from_html(<<"
        <span class=\"count\">
            {integer_to_binary(get_binding(count, Bindings) * 2)}
        </span>
    ">>).
```

## File-Based Templates

For larger or more complex templates, Arizona supports loading HTML from files.
File contents are read and compiled at build time, not at runtime, so there is
no filesystem overhead during request handling.

Load from an absolute or relative file path:

```erlang
render(_Bindings) ->
    arizona_template:from_html({file, "path/to/template.html"}).
```

Load from your OTP application's `priv` directory:

```erlang
render(_Bindings) ->
    arizona_template:from_html({priv_file, my_app, "templates/page.html"}).
```

File-based templates use the same `{}` expression syntax as inline templates.
The template file is a plain HTML file with embedded expressions:

```html
<!-- priv/templates/page.html -->
<div class="page">
    <h1>{get_binding(title, Bindings)}</h1>
    <main>{get_binding(body, Bindings)}</main>
</div>
```

## Expressions and Escaping

Dynamic values produced by template expressions are automatically converted to
safe HTML output via `arizona_html:to_html/1`. The following Erlang types are
supported for automatic conversion:

- **Binaries** -- rendered as-is (assumed to be valid HTML/text).
- **Iodata** -- flattened and rendered.
- **Atoms** -- converted to their string representation.
- **Integers** -- converted to their decimal string representation.
- **Floats** -- converted to their string representation.

To include a literal `{` character in your template output, escape it with a
backslash:

```erlang
arizona_template:from_html(~"<code>Map = #\{key => value}</code>").
%% Renders: <code>Map = #{key => value}</code>
```

Template expressions can contain any valid Erlang expression, including pattern
matching, guards, and function calls. The expression must evaluate to a type
that `arizona_html:to_html/1` can convert.

---

See also: [Templates Overview](overview.md) | [Bindings](bindings.md)

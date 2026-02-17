# Markdown Templates

- [GFM Support](#gfm-support)
- [Template Syntax Inside Markdown](#template-syntax-inside-markdown)
- [File-Based](#file-based)
- [Unsafe Option](#unsafe-option)

## GFM Support

Arizona supports GitHub Flavored Markdown (GFM) via the `erlang-markdown`
library. Create markdown templates with `arizona_template:from_markdown/1`.

All standard GFM features are supported, including headings, bold and italic
text, ordered and unordered lists, code blocks with syntax highlighting, tables,
links, images, blockquotes, and task lists.

```erlang
render(Bindings) ->
    arizona_template:from_markdown(<<"
# {get_binding(title, Bindings)}

{get_binding(content, Bindings)}
    ">>).
```

The markdown is converted to HTML at compile time by the parse transform. The
resulting HTML is then treated like any other Arizona template, with static and
dynamic parts separated for efficient rendering.

```erlang
render(_Bindings) ->
    arizona_template:from_markdown(<<"
## Features

- Fast compile-time processing
- Differential rendering
- Small payloads over the wire

| Column A | Column B |
|----------|----------|
| Value 1  | Value 2  |
    ">>).
```

## Template Syntax Inside Markdown

Arizona's `{}` expression syntax works seamlessly inside markdown content. You
can embed dynamic values anywhere in your markdown -- headings, paragraphs, list
items, table cells, and more.

```erlang
render(Bindings) ->
    arizona_template:from_markdown(<<"
# Welcome, {get_binding(username, Bindings)}

You have **{get_binding(count, Bindings)}** unread messages.

- Account: {get_binding(email, Bindings)}
- Role: {get_binding(role, Bindings)}
    ">>).
```

Under the hood, Arizona protects dynamic tokens with placeholders before passing
the content to the GFM converter. After the markdown-to-HTML conversion is
complete, the placeholders are restored with the original template expressions.
This ensures that markdown processing (which may alter special characters or
restructure content) does not interfere with template expressions.

## File-Based

Like HTML templates, markdown templates support file-based loading. The file is
read and compiled at build time, so there is no filesystem access at runtime.

Load from a file path:

```erlang
render(_Bindings) ->
    arizona_template:from_markdown({file, "posts/article.md"}).
```

Load from your OTP application's `priv` directory:

```erlang
render(_Bindings) ->
    arizona_template:from_markdown({priv_file, my_app, "content/page.md"}).
```

File-based markdown templates use the same `{}` expression syntax. A typical
markdown template file might look like:

```markdown
<!-- priv/content/page.md -->
# {get_binding(title, Bindings)}

Published on {get_binding(date, Bindings)}.

{get_binding(body, Bindings)}
```

## Unsafe Option

By default, raw HTML embedded inside markdown content is escaped for safety.
This prevents untrusted markdown sources from injecting arbitrary HTML into
your pages.

If you trust the markdown source and need to embed raw HTML elements within it,
use the `unsafe` option:

```erlang
arizona_markdown:to_html(Markdown, [unsafe])
```

With the `unsafe` option enabled, HTML tags inside the markdown are passed
through to the final output without escaping:

```markdown
This paragraph has <strong>bold HTML</strong> and a custom element:

<div class="custom-widget" data-id="42">
    Widget content here.
</div>
```

Without the `unsafe` option, the `<strong>` and `<div>` tags above would be
escaped and rendered as visible text rather than interpreted as HTML.

Use the `unsafe` option only when the markdown content comes from a trusted
source, such as your own application code or a vetted content management
system.

---

See also: [Templates Overview](overview.md) |
[Blog Example](../ecosystem/examples.md#blog)

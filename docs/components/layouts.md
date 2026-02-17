# Layouts

- [Document Wrappers](#document-wrappers)
- [Head/Assets](#headassets)
- [Slot Insertion](#slot-insertion)
- [Static Nature](#static-nature)

## Document Wrappers

Layouts wrap view content with the full HTML document structure. They provide the `<html>`,
`<head>`, and `<body>` tags, along with CSS stylesheets, JavaScript includes, and any other
page-level markup that should surround every view.

Set a layout when creating a view by passing a layout tuple as the third argument to
`arizona_view:new/3`:

```erlang
mount(_MountArg, _Req) ->
    Layout = {my_layout, render, inner_block, #{title => ~"My App"}},
    arizona_view:new(?MODULE, #{count => 0}, Layout).
```

The layout tuple has the format `{Module, RenderFun, SlotName, Bindings}`:

- **Module** — the Erlang module containing the layout render function.
- **RenderFun** — the function name to call (typically `render`).
- **SlotName** — the atom used inside the layout to insert the view content. This atom becomes a
  binding key.
- **Bindings** — a map of bindings available to the layout template (e.g., page title, meta tags).

Pass `none` instead of a tuple if the view does not need a layout wrapper.

## Head/Assets

Layouts are where you include the Arizona JavaScript client, CSS stylesheets, meta tags, and any
other document-level resources. A typical layout module looks like this:

```erlang
-module(my_layout).
-export([render/1]).

render(Bindings) ->
    arizona_template:from_html(<<"
        <!DOCTYPE html>
        <html lang=\"en\">
        <head>
            <meta charset=\"UTF-8\">
            <meta name=\"viewport\" content=\"width=device-width, initial-scale=1.0\">
            <title>{get_binding(title, Bindings)}</title>
            <link rel=\"stylesheet\" href=\"/assets/app.css\">
        </head>
        <body>
            {render_slot(get_binding(inner_block, Bindings))}
            <script src=\"/assets/arizona.js\"></script>
            <script>
                const arizona = new Arizona();
                arizona.connect(\"/ws\");
            </script>
        </body>
        </html>
    ">>).
```

The `arizona.js` script and the `arizona.connect("/ws")` call establish the WebSocket connection
that enables live updates. Place these at the end of the `<body>` so the DOM is fully loaded before
the connection is initiated.

## Slot Insertion

Use `render_slot/1` inside the layout template to insert the view's rendered content:

```erlang
{render_slot(get_binding(inner_block, Bindings))}
```

The slot name (`inner_block` in this example) must match the `SlotName` atom specified in the layout
tuple. When Arizona renders the page, it evaluates the view's `render/1` callback, then passes the
result as the slot value into the layout. The `render_slot/1` call inserts that content at the
appropriate position in the document.

You can place additional markup before and after the slot to create consistent page chrome:

```erlang
render(Bindings) ->
    arizona_template:from_html(<<"
        <!DOCTYPE html>
        <html lang=\"en\">
        <head>
            <title>{get_binding(title, Bindings)}</title>
        </head>
        <body>
            <header>
                <nav>{render_stateless(my_components, navbar, #{})}</nav>
            </header>
            <main>
                {render_slot(get_binding(inner_block, Bindings))}
            </main>
            <footer>
                <p>Arizona Framework</p>
            </footer>
            <script src=\"/assets/arizona.js\"></script>
            <script>
                const arizona = new Arizona();
                arizona.connect(\"/ws\");
            </script>
        </body>
        </html>
    ">>).
```

See [Slots](slots.md) for more on the slot mechanism.

## Static Nature

Layouts are rendered **once** during the initial HTTP response (server-side rendering). After the
page loads and the WebSocket connection is established, layouts do **not** participate in
differential rendering. Only the view and its stateful components produce diffs and receive live
updates.

This means:

- Layout bindings (such as the page title) are fixed after the initial render. Changing them from
  `handle_event` or `handle_info` will have no effect on the client.
- Stateless components rendered inside the layout (like a navbar) are also static after page load.
- If you need dynamic content in the header area (e.g., a notification badge), place it inside the
  view's template rather than in the layout, or use a stateful component within the view that
  renders the dynamic portion.

This static behavior is intentional — it avoids unnecessary diffing of the document wrapper and
keeps the live update path focused on the interactive content within the view.

See also: [Views](views.md), [Slots](slots.md), [JavaScript Client
Setup](../javascript-client/setup.md)

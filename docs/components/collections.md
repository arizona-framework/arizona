# Collections

- [Functions](#functions)
  - [`render_list/2`](#render_list2)
  - [`render_map/2`](#render_map2)

## Functions

Arizona provides two collection rendering functions for use inside templates. Both accept a callback
function that produces a template for each item, and both support an optional third argument for
render options.

### `render_list/2`

Renders each element of a list. The callback receives each item and must return a template. Items
are keyed by their index for the diff engine, so the engine can track which items changed, were
added, or were removed between renders.

```erlang
render(Bindings) ->
    arizona_template:from_html(<<"
        <ul>
            {render_list(fun(Item) ->
                arizona_template:from_html(<<"
                    <li>{Item}</li>
                ">>)
            end, get_binding(items, Bindings))}
        </ul>
    ">>).
```

For lists of complex items, destructure the item in the callback:

```erlang
render(Bindings) ->
    arizona_template:from_html(<<"
        <table>
            <tbody>
                {render_list(fun(#{name := Name, email := Email}) ->
                    arizona_template:from_html(<<"
                        <tr>
                            <td>{Name}</td>
                            <td>{Email}</td>
                        </tr>
                    ">>)
                end, get_binding(users, Bindings))}
            </tbody>
        </table>
    ">>).
```

An optional third argument provides render options via `render_list(Fun, List, Options)`. The
available option is:

- `#{update => false}` — Disables differential updates for this list. Use this when the list content
  is static and will never change after the initial render. Skipping diffs for static lists improves
  performance by avoiding unnecessary comparison work.

```erlang
%% Static list: skip diffs since these items never change
{render_list(fun(Item) ->
    arizona_template:from_html(~"<li>{Item}</li>")
end, get_binding(menu_items, Bindings), #{update => false})}
```

### `render_map/2`

Renders each entry of a map. The callback receives `{Key, Value}` tuples. Entries are keyed by their
map key for the diff engine, which provides stable identity even when the map is reordered or
entries are added/removed.

```erlang
render(Bindings) ->
    arizona_template:from_html(<<"
        <dl>
            {render_map(fun({Key, Value}) ->
                arizona_template:from_html(<<"
                    <dt>{Key}</dt>
                    <dd>{Value}</dd>
                ">>)
            end, get_binding(metadata, Bindings))}
        </dl>
    ">>).
```

Like `render_list/2`, an optional third argument accepts render options via `render_map(Fun, Map,
Options)`:

- `#{update => false}` — Disables differential updates for this map. Use this for static
  configuration or reference data that does not change after the initial render.

```erlang
%% Static map: no diffs needed
{render_map(fun({Label, Url}) ->
    arizona_template:from_html(<<"
        <a href=\"{Url}\">{Label}</a>
    ">>)
end, get_binding(links, Bindings), #{update => false})}
```

See also: [Templates Overview](../templates/overview.md), [Bindings](../templates/bindings.md)

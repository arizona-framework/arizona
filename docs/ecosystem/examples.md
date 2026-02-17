# Examples

- [Basic](#basic)
  - [Counter](#counter)
  - [Todo](#todo)
  - [Modal](#modal)
- [Advanced](#advanced)
  - [DataGrid](#datagrid)
  - [Realtime](#realtime)
- [Content Sites](#content-sites)
  - [Blog](#blog)
  - [Static Blog](#static-blog)

## Basic

These examples demonstrate core Arizona concepts: views, stateful components, event handling, and
template rendering. Each example is self-contained and focuses on a specific pattern.

### Counter

A classic counter application showing views, stateful components, and PubSub. The view provides the
page layout, while a stateful component manages the counter state. PubSub integration allows
multiple browser windows to stay in sync -- incrementing the counter in one tab immediately updates
all other connected tabs.

Key concepts: `arizona_view` with layout, `arizona_stateful` for the counter widget,
`arizona_pubsub:join/broadcast` for real-time sync, `handle_event/3` for increment/decrement/reset.

```erlang
%% Stateful counter component
handle_event(~"increment", _Params, State) ->
    Count = arizona_stateful:get_binding(count, State) + 1,
    arizona_pubsub:broadcast(~"counter", #{count => Count}),
    {[], arizona_stateful:put_binding(count, Count, State)}.
```

The view subscribes to the `counter` PubSub topic during mount, so when any client broadcasts a
count change, all subscribed views receive the update and re-render with the new value. The reset
handler sets the count back to zero and broadcasts the change to all connected clients.

### Todo

A full CRUD todo application with filtering. Demonstrates the complete lifecycle of managing a list
of items: adding new todos, toggling their completion state, deleting individual items, and
filtering the displayed list by status (all, active, completed).

Key concepts: `from_html` templates, `render_list` for rendering collections, conditional rendering
with `find_binding`, and multiple event handlers for add/toggle/delete/filter operations. The todo
list is rendered using `render_list`, which efficiently handles list additions, removals, and
reordering by tracking item identity.

### Modal

A modal dialog component using dynamic slots. The modal accepts content through named slots
(`header`, `inner_block`, `confirm`, `cancel`), allowing the parent component to customize every
part of the dialog.

Key concepts: slot composition pattern with named slots, dynamic slot content based on modal type
(success, error, info). The modal component defines the structure and behavior (open/close, backdrop
click), while the parent provides the content through slot bindings. This pattern enables flexible,
reusable UI components without tight coupling between the container and its content.

## Advanced

These examples showcase more complex patterns that combine multiple Arizona features for real-world
use cases.

### DataGrid

An interactive data grid with sortable columns and row deletion. Columns are defined as a
configuration list with header labels, data accessor functions, and optional formatting callbacks.
Clicking a column header sorts the data by that column, toggling between ascending and descending
order.

Key concepts: deeply nested `from_html` templates, column configuration via callback functions,
complex event handling for sort and delete operations. The grid demonstrates how Arizona handles
efficient re-rendering of tabular data -- when the sort order changes or a row is deleted, only the
affected cells are updated in the DOM.

### Realtime

A real-time clock display powered by PubSub. A GenServer process broadcasts the current time every
second to the `clock` PubSub topic. Connected views subscribe to this topic and update their display
automatically, showing a live-updating clock without any client-side JavaScript.

```erlang
%% Clock server broadcasts every second
handle_info(tick, State) ->
    Time = calendar:local_time(),
    arizona_pubsub:broadcast(~"clock", #{time => Time}),
    erlang:send_after(1000, self(), tick),
    {noreply, State}.
```

Key concepts: `arizona_live:is_connected/1` to conditionally subscribe only when on a live WebSocket
connection (avoiding subscription during the initial static HTML render), GenServer as a background
data source, and PubSub-driven live updates. This pattern applies to any real-time data feed: stock
prices, sensor readings, notification counts, or chat messages.

## Content Sites

These examples show content-focused applications where Arizona renders structured content from
various template formats.

### Blog

A blog application using both `from_html` and `from_markdown` templates. Blog posts are authored as
markdown templates, which Arizona compiles into efficient render functions at build time. The
listing page uses HTML templates with `render_list` to display post summaries.

Key concepts: mixing template types within a single application, `from_markdown` for content
authoring, nested stateless components for post rendering (a post card component used inside the
listing template), and filtering posts by category or tag. The markdown templates support the same
`{}` binding syntax as HTML templates, allowing dynamic content within markdown-authored pages.

### Static Blog

A statically generated blog using `arizona_static:generate/1`. This example demonstrates the full
static generation workflow: define routes with path parameters, implement views that read route
bindings to determine which content to render, generate HTML files, and produce a sitemap.

Views use `arizona_request:get_bindings/1` to access route parameters (e.g., post slug) and load the
corresponding content:

```erlang
mount(#{} = _MountArg, Request) ->
    {Bindings, _Req} = arizona_request:get_bindings(Request),
    Slug = maps:get(slug, Bindings),
    Post = blog_posts:get_by_slug(Slug),
    arizona_view:new(?MODULE, #{
        title => maps:get(title, Post),
        content => maps:get(content, Post)
    }, none).
```

The generated output is a set of plain HTML files and a `sitemap.xml`, ready to deploy to any static
hosting provider without an Erlang runtime.

See also: [Views](../components/views.md), [Stateful Components](../components/stateful.md),
[PubSub](../events/pubsub.md), [Static Site Generation](../deployment/static-site.md)

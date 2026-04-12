# Arizona Architecture

## Source modules

| Module | Purpose |
|--------|---------|
| `src/arizona.hrl` | Shared header -- op codes, `?EACH` constant, `#stream{}` record |
| `src/arizona_template.erl` | Pure helpers -- binding access (`get/2,3`), dep tracking (`track/1`), descriptor constructors (`stateful/2`, `stateless/2,3`), `each/2`, `to_bin/1`, snapshot utilities |
| `src/arizona_eval.erl` | Template evaluation -- dynamics to snapshots, stateful/stateless child eval, stream/each eval |
| `src/arizona_render.erl` | HTML output -- `render/1,2`, SSR (`render_to_iolist/1,2`), `zip/2`, `fingerprint_payload/1` |
| `src/arizona_diff.erl` | Diff engine -- `diff/2,3,4`, stream/list diffing, LIS algorithm |
| `src/arizona_cowboy_router.erl` | Cowboy route compilation -- `compile_routes/1` |
| `src/arizona_js.erl` | Unified client commands and server effects -- `push_event/1,2`, `toggle/1`, `show/1`, `hide/1`, `dispatch_event/2`, `set_title/1`, `reload/0`, etc. |
| `src/arizona_stream.erl` | Pure stream data structure -- create, insert, delete, update, move, sort, reset, `clear_stream_pending/2`, `stream_keys/1` |
| `src/arizona_stateful.erl` | Behaviour -- callbacks (`mount/1`, `render/1`, `handle_event/3`, `handle_info/2`, `handle_update/2`) and types (`bindings/0`, `resets/0`, `effects/0`) |
| `src/arizona_live.erl` | Gen_server -- mount, handle_event, handle_info, views map, transport push |
| `src/arizona_parse_transform.erl` | Compile-time transform -- `?html`, `?each` (1-arg or 2-arg) DSL to `#{s, d, f}` maps, `az-view` auto-injection, directive extraction (`az-nodiff`), attribute compilation |
| `src/arizona_socket.erl` | Framework-agnostic WebSocket protocol state machine -- JSON encode/decode, event dispatch, navigation, crash recovery, op scoping |
| `src/arizona_adapter.erl` | Behaviour defining `resolve_route/2` callback for framework adapters |
| `src/arizona_cowboy_adapter.erl` | Cowboy implementation of `arizona_adapter` -- route resolution via `cowboy_router` |
| `src/arizona_cowboy_http.erl` | Cowboy HTTP handler -- calls `render_to_iolist` and serves result |
| `src/arizona_cowboy_ws.erl` | Cowboy WebSocket handler -- thin wrapper over `arizona_socket` |
| `src/arizona_cowboy_static.erl` | Cowboy static file handler -- serves files from a directory with content-type detection |
| `src/arizona_app.erl` | Application start -- Cowboy routes |
| `src/arizona_watcher.erl` | File watcher gen_server -- subscribes to `fs` events, debounces, calls callback, broadcasts via `arizona_pubsub` |
| `src/arizona_dev.erl` | Dev-mode relay -- named gen_server subscribing to `arizona_watcher` channel, broadcasts `arizona_dev_reload` to browser WS handlers |
| `src/arizona_pubsub.erl` | PubSub -- thin `pg` wrapper for cross-view communication |
| `src/arizona_sup.erl` | Supervisor -- starts `arizona_pubsub` worker and `arizona_dev` worker |

## API -- `arizona_template.erl`

Pure helpers for binding access, descriptor construction, and template composition.

- `get/2,3`, `get_lazy/3` -- binding access with dep tracking via process dictionary (`$arizona_deps`)
- `track/1` -- manually track a binding key as a dependency
- `stateful/2` -- returns `#{stateful => Handler, props => Props}` descriptor for child views
- `stateless/2` -- returns `#{callback => Callback, props => Props}` descriptor (Callback is a fun/1)
- `stateless/3` -- returns `#{callback => fun Handler:Fun/1, props => Props}` descriptor
- `each/2` -- template composition: 1-arg `fun(Item)` for lists, 2-arg `fun(Item, Key)` for streams, 2-arg `fun(Key, Value)` for maps. Returns `#{t, source, template}` descriptor
- `to_bin/1` -- convert value to binary (binary/integer/float/atom/iolist)

## API -- `arizona_render.erl`

HTML output and rendering.

- `render/1` -- evaluate dynamics, produce `{HTML, Snapshot}`
- `render/2` -- like `render/1` but threads a views map for stateful children, collects deps: `{HTML, Snapshot, Views}`
- `render_to_iolist/1` -- render a template to iolist (used for layout templates with `diff => false`)
- `render_to_iolist/2` -- SSR: `render_to_iolist(Handler, Opts)` where Opts may contain `layout => {Mod, Fun}` and `bindings => map()`
- `resolve_id/1` -- resolves a binary id (passthrough) or a `#{s, d}` template (renders to binary)
- `zip/2` -- interleave statics and evaluated dynamics into iolist
- `fingerprint_payload/1` -- convert a snapshot to fingerprint wire format

## API -- `arizona_diff.erl`

Diff engine.

- `diff/2` -- compare new template against old snapshot: `{Ops, NewSnapshot}`
- `diff/3` -- like `diff/2` with views map: `{Ops, NewSnapshot, Views}`
- `diff/4` -- like `diff/3` but skips dynamics whose deps haven't changed (takes `Changed` map): `{Ops, NewSnapshot, Views}`

## API -- `arizona_js.erl`

Unified client commands and server effects. All functions return `{arizona_js, [OpCode, ...Args]}` (nominal type `cmd()`). Used in two contexts:

**Template attributes** -- commands embedded in `az-click`, `az-submit`, etc.:
```erlang
{'button', [{az_click, arizona_js:push_event(~"inc")}], [<<"+">>]}
{'button', [{az_click, [arizona_js:push_event(~"inc"), arizona_js:toggle(~"#modal")]}], [<<"Both">>]}
```
Auto-encoded to HTML-safe JSON by `arizona_template:to_bin/1`. No explicit `encode` call needed.

**Handler effects** -- returned from `handle_event/3` and `handle_info/2`:
```erlang
handle_event(~"inc", _P, B) ->
    {B#{count => Count + 1}, #{}, [arizona_js:set_title(~"Updated")]}.
```

**Commands:**
- `push_event/1,2` -- send event to server (template-only). Without explicit payload, auto-collects from element context (see below). Explicit payload is merged on top of auto-collected (explicit wins).
- `toggle/1`, `show/1`, `hide/1` -- element visibility via `hidden` attribute
- `add_class/2`, `remove_class/2`, `toggle_class/2` -- CSS class manipulation
- `set_attr/3`, `remove_attr/2` -- attribute manipulation
- `dispatch_event/2` -- dispatch CustomEvent on document
- `navigate/1,2` -- SPA navigation (opts: `#{replace => true}`)
- `focus/1`, `blur/1` -- focus management
- `scroll_to/1,2` -- scroll element into view (opts: `#{behavior => <<"smooth">>}`)
- `set_title/1` -- set document title
- `reload/0` -- reload page
- `encode/1` -- encode single cmd or list of cmds to HTML-safe JSON binary (called automatically by `to_bin`)

**Payload auto-collection** (`push_event`):
When `push_event` fires on an element, the client auto-collects a base payload from the element and event context, then merges explicit payload on top:
- **Drop events** → `{data_transfer: "dragged-key", drop_index: N}` -- drag data from `dataTransfer` and drop position among `[az-key]` siblings
- **Forms** → `Object.fromEntries(new FormData(form))` -- all form field values keyed by `name`
- **Inputs/selects/textareas** → `{value: el.value}` -- current input value
- **Other** → `{}` -- empty

This means `az-keydown` or `az-focusout` on an input automatically includes `{value: "typed text"}`, and `az-drop` automatically includes `{data_transfer, drop_index}`. No special attributes needed.

**Op codes** defined in `include/arizona_js.hrl` -- integer constants shared with the client JS runtime. Same codes for both template commands and server effects.

**Key filtering** via `on_key/2` -- wraps a command so it only executes when the pressed key matches:
```erlang
%% Atom -- literal match
{az_keydown, arizona_js:on_key(enter, arizona_js:push_event(~"submit"))}
%% List -- match any
{az_keydown, arizona_js:on_key([enter, escape], arizona_js:push_event(~"close"))}
%% Binary -- regex pattern
{az_keydown, arizona_js:on_key(~"^[a-z0-9]$", arizona_js:push_event(~"type"))}
```

## API -- `arizona_cowboy_router.erl`

Cowboy route compilation.

- `compile_routes/1` -- build Cowboy dispatch from route specs and store in `persistent_term`

**Route types** (via `compile_routes/1`):
- `{live, Path, Handler, Opts}` -- live route (`Opts` may contain `layout => {Mod, Fun}` and `bindings => map()`)
- `{ws, Path, Opts}` -- WebSocket endpoint
- `{asset, Path, {priv_dir, App, SubDir}}` -- static asset route from priv directory
- `{asset, Path, {dir, Dir}}` -- static asset route from absolute directory
- `{controller, Path, Handler, State}` -- generic Cowboy handler route

## API -- `arizona_stream.erl`

Pure stream data structure -- create, mutate, and query streams. The `#stream{}` record is defined in `src/arizona.hrl`. All rendering/diffing of streams stays in `arizona_eval.erl`/`arizona_diff.erl`.

- `new/1,2,3` -- create a stream with key function, optional initial items, and optional opts (`#{limit => N}`)
- `insert/2,3` -- insert item (append or at position)
- `delete/2` -- delete item by key
- `update/3` -- update item by key
- `move/3` -- move item to new position (emits `{move, Key, AfterKey}` pending op)
- `sort/2` -- sort by comparator function (emits `reorder` pending op)
- `reset/1,2` -- clear stream or replace with new items (emits `reset` pending op)
- `to_list/1` -- return items in order
- `get/2` -- look up item by key (crashes if key not found)
- `get/3` -- look up item by key with default value
- `get_lazy/3` -- look up item by key, calls `Fun()` only if key is missing
- `clear_stream_pending/2` -- clear pending ops for stream keys in bindings
- `stream_keys/1` -- scan bindings for stream keys

## API -- `arizona_watcher.erl`

File watcher gen_server -- subscribes to `fs` events for a directory, debounces, calls optional callback, broadcasts changed files via `arizona_pubsub`.

- `watch(Dir, Opts)` -- spawns a gen_server that subscribes to `fs` events for `Dir`. Options: `patterns` (list of regex strings, default `[".*"]`), `callback` (fun receiving list of changed file paths), `debounce` (ms, default 100). On debounce fire: calls callback, then `broadcast/1`
- `broadcast(Files)` -- broadcasts `{arizona_watcher, Files}` via `arizona_pubsub` on channel `arizona_watcher`

## API -- `arizona_dev.erl`

Dev-mode relay -- named gen_server that subscribes to the `arizona_watcher` pubsub channel and relays file change notifications as browser reload messages.

- `start_link/0` -- starts named gen_server (`{local, arizona_dev}`), subscribes to `arizona_watcher` pubsub channel
- `watch/2` -- delegates to `arizona_watcher:watch/2`
- `join/1` -- subscribes `Pid` to the `arizona_dev` pubsub channel. No-op if `arizona_pubsub` isn't running
- `broadcast_reload/0` -- broadcasts `arizona_dev_reload` to all subscribers of the `arizona_dev` pubsub channel

When the watcher fires, `arizona_dev` receives `{arizona_watcher, Files}` via `handle_info/2` and calls `broadcast_reload/0`.

## API -- `arizona_pubsub.erl`

PubSub for cross-view communication. Thin wrapper around `pg` with scope `arizona_pubsub`. Channels are arbitrary terms. Messages are raw data (no wrapper tuple).

- `subscribe(Channel, Pid)` -- subscribe `Pid` to `Channel`. Duplicate-safe: checks membership first, no-op if already subscribed
- `unsubscribe(Channel, Pid)` -- remove `Pid` from `Channel`
- `broadcast(Channel, Data)` -- send `Data` to all subscribers via `Pid ! Data`
- `broadcast_from(From, Channel, Data)` -- same but excludes `From` pid
- `subscribers(Channel)` -- return list of subscriber pids

## API -- `arizona_live.erl`

Simplified gen_server wrapper:

- `start_link/1,2,3` -- start with handler module, optional initial bindings (default `#{}`), and optional transport PID. `/3` accepts `TransportPid` for server-push; `/1` and `/2` pass `undefined`
- `mount/1` -- calls `Handler:mount(Bindings)` -> extracts `ViewId = maps:get(id, Bindings)` -> `Handler:render(B1)` -> `arizona_render:render/2` to establish snapshot. Returns `{ok, ViewId}` (no HTML -- SSR is handled separately by `arizona_cowboy_http`)
- `handle_event/4` -- unified event dispatch: `handle_event(Pid, ViewId, Event, Payload)`. Checks views map -- if `ViewId` is a known child, dispatches to child handler; otherwise dispatches to root handler. Returns `{ok, Ops, Effects}`
- `handle_info/2` -- gen_server callback for Erlang messages (`Pid ! Msg`, `erlang:send_after`, etc.). If handler exports `handle_info/2`, calls it, diffs, and pushes `{arizona_push, Ops, Effects}` to `transport_pid`. Pre-mount messages and handlers without `handle_info/2` are silently dropped. Empty ops+effects are not pushed
- `navigate/3` -- `navigate(Pid, NewHandler, InitBindings)`. Mounts new handler, resets gen_server state (handler, bindings, snapshot, views), preserves `transport_pid`, returns `{ok, NewViewId, PageHTML}`

Internal state: `#state{handler, bindings, snapshot, views, transport_pid}` where `views :: #{ViewId => #{handler, bindings, snapshot}}`.

`compute_changed/2` builds the Changed map by comparing old and new bindings key-by-key.

`push/3` sends `{arizona_push, Ops, Effects}` to the transport PID. No-ops when PID is `undefined` or ops and effects are both empty.

## API -- `arizona_socket.erl`

Framework-agnostic WebSocket protocol state machine. Extracted from `arizona_cowboy_ws` so any framework can integrate Arizona without reimplementing the wire protocol. Cowboy is an optional dependency -- the core engine works without it.

- `init/3` -- `init(Handler, Bindings, Opts)`. Traps exits, starts `arizona_live:start_link/3` (passing `self()` as transport PID), mounts. On reconnect (`#{reconnect => true}`), also renders and returns `OP_REPLACE` frame. Opts: `reconnect` (boolean), `adapter` (module implementing `arizona_adapter`), `adapter_state` (passed to adapter callbacks)
- `handle_in/2` -- decode incoming text frame: ping/pong, `["cached_fps", FpList]`, `["navigate", {path}]`, `[target, event, payload]`
- `handle_info/2` -- handle `{arizona_push, Ops, Effects}` from `arizona_live`, `EXIT` signals (crash → remount, normal → close)

**Return type** (`result()`): `{ok, Socket}` | `{reply, iodata(), Socket}` | `{close, Code, Reason, Socket}`

Internal functions: `scope_ops/2` (prepend view ID to op targets), `encode_reply/3` (build `#{<<"o">> => Ops, <<"e">> => Effects}` JSON), `remount_or_close/1` (crash recovery), `dispatch_event/4`, `handle_navigate/2` (calls `Adapter:resolve_route/2`).

## API -- `arizona_adapter` behaviour

Defines the integration contract for framework adapters. A single callback:

```erlang
-callback resolve_route(Path :: binary(), State :: term()) ->
    {module(), map()}.
```

Called by `arizona_socket` during client-side SPA navigation to resolve a path to a handler module and initial bindings.

**Shipped implementation:** `arizona_cowboy_adapter` -- resolves routes via `cowboy_router:execute/2` and the compiled dispatch stored by `arizona_cowboy_router`. Also exports `resolve_cowboy_route/1` used by `arizona_cowboy_ws` during init.

## Cowboy handlers

- `arizona_cowboy_http.erl` -- generic Cowboy HTTP handler, calls `render_to_iolist/2` and serves the result
- `arizona_cowboy_ws.erl` -- thin Cowboy WebSocket adapter over `arizona_socket`. Extracts path/reconnect from query string, resolves route via `arizona_cowboy_adapter`, delegates to `arizona_socket:init/3`, `handle_in/2`, `handle_info/2`. Translates `arizona_socket:result()` to Cowboy return tuples

## Data flow

**SSR (HTTP):**
`arizona_cowboy_http` calls `arizona_render:render_to_iolist(Handler, Opts)` where Opts may contain `layout => {Mod, Fun}` and `bindings => map()`. When a layout is provided, it mounts the page handler, renders to page HTML, then injects the page HTML into mount bindings as `inner_content` and passes the bindings to the layout's `render/1`. The layout uses `?html` with `az_nodiff` on the root element -- a stateless HTML shell (DOCTYPE, head, body, scripts) with no markers or `az` attributes. When layout is absent, the page is rendered directly without a wrapper. Route config provides `handler`, `layout`, and `bindings`.

**WebSocket mount:**
`arizona_cowboy_ws` calls `arizona_live:start_link/3` (passing `self()` as transport PID) then `arizona_live:mount/1`. Mount establishes the server-side snapshot (matching SSR). Returns `{ok, ViewId}` where ViewId comes from `maps:get(id, MountBindings)`. Handlers detect the connected context via `?connected` macro (delegates to `arizona_live:connected()`) which reads a process dictionary flag set in `arizona_live:init/1`. For post-connection effects, handlers use `?send(arizona_connected)` in mount and handle it in `handle_info/2`.

**Server push (`handle_info`) -- per-view routing:**
Messages sent via `?send(Msg)` / `arizona_live:send(ViewId, Msg)` are tagged as `{arizona_view, ViewId, Msg}` and routed to the correct handler's `handle_info/2` -- root or child. The root view ID is matched from `#state.bindings`, children from the views map. Unknown view IDs crash with `{unknown_view, ViewId, Msg}`. Plain untagged messages (`Pid ! Msg`) route to the root handler for backward compatibility. Delayed sends use `?send_after(Time, Msg)` / `arizona_live:send_after(ViewId, Time, Msg)`. PubSub subscriptions use `?subscribe(Topic)` / `?unsubscribe(Topic)`. After `handle_info/2` returns, the template is re-rendered and diffed, and ops+effects are sent as `{arizona_push, Ops, Effects}` to the transport PID.

**Events:**
Client sends `[target, eventName, payload]` over WebSocket. `arizona_cowboy_ws` dispatches to `arizona_live:handle_event/4`. The gen_server checks the views map -- if target is a known child view, dispatches to child; otherwise dispatches to root handler. Returns `{ok, Ops, Effects}`. Ops are scoped with `"viewId:target"` format. The response is sent as a JSON envelope `{"o": scopedOps, "e": effects}`.

**SPA navigation + unmount:**
Client clicks `[az-navigate]` link -> JS intercepts, calls `history.pushState`, sends `["navigate", {path}]` over WebSocket. `arizona_cowboy_ws` calls `arizona_live:navigate/3`. Before mounting the new handler, the framework cancels pending `send_after` timers and calls the old root handler's `unmount/1` callback (if exported). Propagation to children is opt-in -- the root can broadcast via pubsub in its `unmount/1`. Then the new handler is mounted and gen_server state is reset. Returns `{ok, NewViewId, PageHTML}`. WS handler sends `[OP_REPLACE, OldViewId, PageHTML]`. Browser back/forward also triggers navigate via `popstate`.

**Stateful children:**
`arizona_template:stateful(Handler, Props)` returns a descriptor map. During `eval_val_v/2`, the engine checks if the child is already in the views map -- if so, calls `handle_update/2` (or `maps:merge` if not exported); if not, calls `Handler:mount(Props)`. Child templates are recursively rendered/diffed with their own snapshots. The views map uses a `{OldViews, NewViews}` tuple during eval -- `OldViews` is read-only for child lookup, `NewViews` accumulates only children rendered this cycle. Children removed from the template (conditional rendering) are pruned from the views map and their `unmount/1` callback is called if exported.

**Slots:**
Slots are implemented via stateless children and bindings. A layout receives the page HTML via a configurable binding key (e.g. `inner_content`) in `render/1`. Stateless components receive props with arbitrary content:
```erlang
%% Layout slot -- inner_content is the page HTML (layout uses ?html with az_nodiff)
render(Bindings) ->
    ?html({body, [az_nodiff], [maps:get(inner_content, Bindings)]}).

%% Component slot -- via stateless child props
?stateless(render_card, #{label => ~"Hello", content => SomeTemplate})
```

## Handler callbacks

Handlers include `arizona_stateful.hrl` (which sets the behaviour, parse transform, and template macros) and export these functions:

```erlang
-module(my_handler).
-include("arizona_stateful.hrl").
-export([mount/1, render/1, handle_event/3]).

mount(Bindings) ->
    maps:merge(#{id => ~"page", count => 0}, Bindings).

render(Bindings) ->
    ?html({'div', [], [?get(count, 0)]}).

handle_event(~"inc", _Payload, Bindings) ->
    {Bindings#{count => maps:get(count, Bindings, 0) + 1}, []}.
```

**Callback signatures:**

```erlang
mount(Bindings) -> NewBindings.                     %% Required
render(Bindings) -> #{s => [...], d => [...]}.      %% Required
handle_event(Event, Payload, Bindings) -> {NewBindings, Resets, Effects}.  %% Optional
handle_info(Info, Bindings) -> {NewBindings, Resets, Effects}.             %% Optional (server push)
handle_update(Props, Bindings) -> NewBindings.                     %% Optional (child views)
```

`mount/1` returns `Bindings :: map()`. Bindings must include an `id` key -- this becomes the ViewId used for event routing and navigate targeting. The parse transform auto-injects `az-view` as a boolean attribute on the root element of `render/1` in `arizona_stateful` modules. Using `az_view` manually outside this context raises a compile error.

**`id` attribute restriction:** The root element's `id` MUST use `{id, ?get(id)}` (or the equivalent `arizona_template:get(id, Bindings)` / `az:get(id, Bindings)`). Static binaries and composed values are not allowed. This is a compile-time check.

The `id` serves three roles simultaneously: DOM id (`document.getElementById`), views map key (server-side state tracking), and wire protocol target (op scoping). All three must be the same value. Using `?get(id)` ensures the template renders the same value the parent passed in Props and the server uses for tracking. `id` is a restricted key -- `mount/1` cannot override the value passed by the parent in Props.

`handle_event/3` returns `{Bindings, Resets, Effects}`. Effects is a list of `arizona_js:cmd()` tuples, built using functions in `arizona_js.erl`. Same commands used in template attributes. Supported effects:
- `arizona_js:dispatch_event(Name, Payload)` -- dispatches a CustomEvent on the client's document
- `arizona_js:set_title(Title)` -- sets the browser document title
- `arizona_js:reload()` -- reloads the page
- Any other `arizona_js` command (toggle, show, hide, etc.) also works as an effect

`handle_info/2` returns `{Bindings, Effects}`. Same return format as `handle_event/3`. `Info` is any Erlang term sent to the gen_server PID. If not exported, messages are silently dropped (checked via `erlang:function_exported/3`).

`handle_update/2` returns `NewBindings` (pure state sync, no side effects). Called on stateful children when the parent re-renders. If not exported, `maps:merge(Bindings, Props)` is used as default (`call_handle_update/3`).

## Dynamic `eval` return values

- `iodata()` -- rendered value
- `#{s => ..., d => ...}` -- nested template (recursively rendered/diffed)
- `#{stateful => Handler, props => Props}` -- stateful child descriptor
- `#{callback => Fun, props => Props}` -- stateless child descriptor
- `remove` -- sentinel that triggers `OP_REMOVE_NODE`

## Change tracking

`arizona_template:get/2,3` tracks which binding keys each dynamic reads via the process dictionary key `'$arizona_deps'`. The structure is a map of keys, e.g. `#{count => true, title => true}`, or `undefined` when inactive.

**Lifecycle per dynamic (in `eval_one_v`):**
1. `erlang:put('$arizona_deps', #{})` -- start tracking
2. The dynamic's fun is called -- each `arizona_template:get(Key, Bindings)` adds `Key => true`
3. `erlang:erase('$arizona_deps')` -- harvest the dep map
4. Deps are stored in the snapshot as `deps => [#{key() => true}]`, parallel to the `d` list

**Nesting:** Stateful children and nested templates save/restore `'$arizona_deps'` (`SavedDeps = erlang:get(...)` before, `erlang:put(..., SavedDeps)` after) to prevent child `get` calls from polluting the parent's dep list.

**Inactive paths:** `render/1`, `diff/2`, and SSR never set `'$arizona_deps'`, so `track_dep/1` sees `undefined` and is a no-op.

**Consumption:** Only `diff/4` uses deps. `deps_changed/2` checks whether any key in a dynamic's dep map appears in the `Changed` map (via `maps:intersect`). If none do, the dynamic is skipped entirely -- its fun is never called.

## Op codes

| Code | Name | Args | Description |
|------|------|------|-------------|
| 0 | `OP_TEXT` | `[target, value]` | Update text node |
| 1 | `OP_SET_ATTR` | `[target, attr, value]` | Set attribute |
| 2 | `OP_REM_ATTR` | `[target, attr]` | Remove attribute |
| 3 | `OP_UPDATE` | `[target, html]` | innerHTML replacement (diff engine) |
| 4 | `OP_REMOVE_NODE` | `[target]` | Remove element |
| 5 | `OP_INSERT` | `[target, key, pos, html]` | Stream insert (pos=-1 -> append, otherwise index) |
| 6 | `OP_REMOVE` | `[target, key]` | Stream remove |
| 7 | `OP_ITEM_PATCH` | `[target, key, innerOps]` | Stream item patch |
| 8 | `OP_REPLACE` | `[target, html]` | outerHTML replacement (navigate) |
| 9 | `OP_MOVE` | `[target, key, afterKey]` | Stream move (afterKey=null -> prepend) |

## Target scoping

Patch targets are `"viewId:relativeTarget"`. Root view `<<"page">>`: `"page:0"`, `"page:1"`. Child view `<<"counter">>`: `"counter:0"`, `"counter:1"`. `arizona_socket` prefixes ops with the view id via internal scoping. Child view ops use `[ChildViewId, ChildOps]` nesting and recurse into these. Bare targets (no `:`) resolve to the view root via `document.getElementById(target)` -- used by `OP_REPLACE` during navigate.

## Framework integration

Arizona is designed as a reusable library. Cowboy is an optional dependency -- the core engine (`arizona_template`, `arizona_render`, `arizona_diff`, `arizona_live`, `arizona_stream`, `arizona_pubsub`, `arizona_js`, parse transform) works without it.

To integrate Arizona into any Erlang web framework:

**1. Add `arizona` as a dependency.** Ensure `cowboy` is NOT required (unless your framework already uses it).

**2. Implement the `arizona_adapter` behaviour** for route resolution during SPA navigation:

```erlang
-module(my_framework_adapter).
-behaviour(arizona_adapter).
-export([resolve_route/2]).

resolve_route(Path, _State) ->
    %% Use your framework's routing to resolve Path -> {Handler, Bindings}
    case my_framework_router:match(Path) of
        {ok, Handler, Bindings} -> {Handler, Bindings};
        error -> error({no_route, Path})
    end.
```

**3. Handle HTTP requests** -- mount the handler and render HTML:

```erlang
handle_http_request(Req) ->
    Handler = my_page_handler,
    Opts = #{layout => {my_layout, render}, bindings => #{title => <<"Home">>}},
    HTML = arizona_render:render_to_iolist(Handler, Opts),
    {200, #{<<"content-type">> => <<"text/html">>}, HTML}.
```

**4. Handle WebSocket connections** -- delegate to `arizona_socket`:

```erlang
-module(my_framework_ws).

ws_init(State) ->
    {Handler, Bindings} = resolve_handler_from_framework(State),
    Opts = #{adapter => my_framework_adapter, adapter_state => State},
    handle_result(arizona_socket:init(Handler, Bindings, Opts), State).

ws_receive(Data, #{socket := Socket} = State) ->
    handle_result(arizona_socket:handle_in(Data, Socket), State).

ws_info(Msg, #{socket := Socket} = State) ->
    handle_result(arizona_socket:handle_info(Msg, Socket), State).

handle_result({ok, Socket}, State) ->
    {ok, State#{socket => Socket}};
handle_result({reply, Data, Socket}, State) ->
    {reply, Data, State#{socket => Socket}};
handle_result({close, Code, Reason, _Socket}, State) ->
    {close, Code, Reason, State}.
```

**5. Serve Arizona's client JS** from `priv/static/assets/js/arizona.min.js` through your framework's static file handler.

The shipped Cowboy integration (`arizona_cowboy_ws`, `arizona_cowboy_http`, `arizona_cowboy_adapter`, `arizona_cowboy_router`, `arizona_cowboy_server`) serves as a reference implementation.

# Arizona Architecture

## Source modules

| Module                              | Purpose                                                                                                                                                                   |
| ----------------------------------- | ------------------------------------------------------------------------------------------------------------------------------------------------------------------------- |
| `src/arizona.hrl`                   | Shared header -- op codes, `?EACH` constant, `#stream{}` record                                                                                                           |
| `src/arizona_template.erl`          | Pure helpers -- binding access (`get/2,3`), dep tracking (`track/1`), descriptor constructors (`stateful/2`, `stateless/2,3`), `each/2`, `to_bin/1`, snapshot utilities   |
| `src/arizona_eval.erl`              | Template evaluation -- dynamics to snapshots, stateful/stateless child eval, stream/each eval                                                                             |
| `src/arizona_render.erl`            | Render orchestration (target-aware) -- `render/1,2`, SSR (`render_to_iolist/1,2`, `render_view_to_iolist/3`), `zip/2`, `fingerprint_payload/1`                            |
| `src/arizona_renderer.erl`          | Render-target backend behaviour -- byte emission + `attr_command/2`; HTML and native JSON backends below                                                                  |
| `src/arizona_html.erl`              | HTML render backend -- emits HTML statics/attrs (the `?html` target)                                                                                                      |
| `src/arizona_native.erl`            | Native render backend -- emits a JSON widget tree (the `?native` target); see docs/native.md                                                                              |
| `src/arizona_diff.erl`              | Diff engine -- `diff/2,3,4`, stream/list diffing, LIS algorithm                                                                                                           |
| `src/arizona_roadrunner_router.erl` | Roadrunner route compilation -- `compile_routes/1,2`, `routes/1,2` (map-shape routes with state under `#{arizona => ...}` namespace)                                      |
| `src/arizona_effect.erl`            | Neutral effect plumbing -- the `{arizona_effect, [...]}` tuple; `encode/1` (HTML attr) + `encode_json/1` (raw); op codes in `include/arizona_effect.hrl`                  |
| `src/arizona_js.erl`                | Web/browser command + effect builders -- `push_event`, `navigate`, `toggle`/`show`/`hide`, `focus`/`blur`/`scroll_to`, `on_key`, `dispatch_event`, `set_title`, `reload`  |
| `src/arizona_android.erl`           | Native (`?native`) command builders -- the portable `push_event/1,2` and `navigate/1,2`                                                                                   |
| `src/arizona_stream.erl`            | Pure stream data structure -- create, insert, delete, update, move, sort, reset, `clear_stream_pending/2`, `stream_keys/1`                                                |
| `src/arizona_stateful.erl`          | Behaviour for embedded components -- `mount/1` callback + `call_mount/2` dispatcher; shared callbacks come from `arizona_handler`                                         |
| `src/arizona_view.erl`              | Behaviour for route-level pages -- `mount/2` callback taking `(Bindings, Request)` + `call_mount/3` dispatcher; shared callbacks come from `arizona_handler`              |
| `src/arizona_handler.erl`           | Shared behaviour hosting callbacks common to views and stateful components (`render/1`, `handle_event/3`, `handle_info/2`, `handle_update/2`, `unmount/1`) + dispatchers  |
| `src/arizona_req.erl`               | Opaque request -- eager `method`/`path`, lazy `bindings`/`params`/`cookies`/`headers`/`body`/`user_agent`, `apply_middlewares/3`, `redirect`/`halted_redirect`            |
| `src/arizona_user_agent.erl`        | User-Agent classification for dual-serve views -- `browser/1`, `os/1`, `mobile/1` (best-effort); pairs with `arizona_req:user_agent/1`                                    |
| `src/arizona_http.erl`              | Transport-agnostic HTTP render pipeline -- `render/3` runs middlewares, renders the view, returns `{halt\|redirect\|ok\|error, ...}` tuples                               |
| `src/arizona_ws.erl`                | Transport-agnostic WS upgrade bootstrap -- `prepare/3` parses framework keys, resolves the route, runs middlewares, returns state for `arizona_socket`                    |
| `src/arizona_live.erl`              | Gen_server -- mount (stateful or view), handle_event, handle_info, views map, transport push                                                                              |
| `src/arizona_parse_transform.erl`   | Compile-time transform -- `?html`/`?native`, `?each` DSL to `#{s, d, f}` maps, `az-view` auto-injection, `az-nodiff`, attribute compilation, cross-target nesting guard   |
| `src/arizona_socket.erl`            | Framework-agnostic WebSocket protocol state machine -- JSON encode/decode, event dispatch, navigation, op scoping. Crash closes cleanly; client reconnects via backoff    |
| `src/arizona_roadrunner_http.erl`   | Roadrunner HTTP handler -- thin wrapper: delegates to `arizona_http:render/3` and translates results into roadrunner's `{Response, Req}` reply shape                      |
| `src/arizona_roadrunner_ws.erl`     | Roadrunner WebSocket handler -- dual behaviour (`roadrunner_handler` for upgrade + `roadrunner_ws_handler` for session); delegates to `arizona_ws:prepare/3`              |
| `src/arizona_roadrunner_static.erl` | Roadrunner static file handler -- returns `{sendfile, ...}` for zero-copy serving with `cache-control: immutable`                                                         |
| `src/arizona_roadrunner_server.erl` | Roadrunner listener boot -- compiles routes, stashes them for hot reload, validates TLS opts, starts a clear/TLS listener                                                 |
| `src/arizona_roadrunner_req.erl`    | Roadrunner `arizona_req` adapter -- parsing callbacks plus `resolve_route/3` for SPA navigate; populates `request_id` from roadrunner                                     |
| `src/arizona_roadrunner_reload.erl` | Dev-mode SSE endpoint -- streams reload events from `arizona_reloader` to the browser                                                                                     |
| `src/arizona_error_page.erl`        | Dev-mode error page renderer -- pretty-prints compile and runtime errors                                                                                                  |
| `src/arizona_app.erl`               | Application callback -- starts `arizona_sup` and, when the `server` env is set, launches the roadrunner listener                                                          |
| `src/arizona_watcher.erl`           | File watcher gen_server -- subscribes to `fs` events, debounces, calls callback, broadcasts via `arizona_pubsub`                                                          |
| `src/arizona_reloader.erl`          | Dev-mode hot reloader -- recompiles changed `.erl` files, broadcasts reload messages on the `arizona_reloader` pubsub topic                                               |
| `src/arizona_pubsub.erl`            | PubSub -- thin `pg` wrapper for cross-view communication                                                                                                                  |
| `src/arizona_sup.erl`               | Supervisor -- always starts `arizona_pubsub`; additionally starts one `arizona_watcher` per configured reloader rule                                                      |
| `src/az.erl`                        | User-facing facade -- shared type aliases and short-form helpers re-exported from internal modules                                                                        |

## API -- `arizona_template.erl`

Pure helpers for binding access, descriptor construction, and template composition.

- `get/2,3`, `get_lazy/3` -- binding access with dep tracking via process dictionary
  (`$arizona_deps`)
- `track/1` -- manually track a binding key as a dependency
- `stateful/2` -- returns `#{stateful => Handler, props => Props}` descriptor for child views
- `stateless/2` -- returns `#{callback => Callback, props => Props}` descriptor (Callback is a
  fun/1)
- `stateless/3` -- returns `#{callback => fun Handler:Fun/1, props => Props}` descriptor
- `each/2` -- template composition: 1-arg `fun(Item)` for lists, 2-arg `fun(Item, Key)` for streams,
  2-arg `fun(Key, Value)` for maps. Returns `#{t, source, template}` descriptor
- `to_bin/1` -- convert value to binary (binary/integer/float/atom/iolist)

## API -- `arizona_render.erl`

HTML output and rendering.

- `render/1` -- evaluate dynamics, produce `{HTML, Snapshot}`
- `render/2` -- like `render/1` but threads a views map for stateful children, collects deps:
  `{HTML, Snapshot, Views}`
- `render_to_iolist/1` -- render a template to iolist (used for layout templates with
  `diff => false`)
- `render_to_iolist/2` -- stateful-component SSR (test-only): `render_to_iolist(Handler, Opts)`
  where Opts may contain `layouts => [{Mod, Fun}]` and `bindings => map()`. `on_mount` is not
  honored -- it's a route-level concept
- `render_view_to_iolist/3` -- production HTTP SSR for views: `render_view_to_iolist(Handler, Req, Opts)`.
  Mounts via `arizona_view:call_mount/3`, applies `on_mount` hooks (threading `Req`), optionally
  wraps in a layout
- `resolve_id/1` -- resolves a binary id (passthrough) or a `#{s, d}` template (renders to binary)
- `zip/2` -- interleave statics and evaluated dynamics into iolist
- `fingerprint_payload/1` -- convert a snapshot to fingerprint wire format

## API -- `arizona_diff.erl`

Diff engine.

- `diff/2` -- compare new template against old snapshot: `{Ops, NewSnapshot}`
- `diff/3` -- like `diff/2` with views map: `{Ops, NewSnapshot, Views}`
- `diff/4` -- like `diff/3` but skips dynamics whose deps haven't changed (takes `Changed` map):
  `{Ops, NewSnapshot, Views}`

## Dep tracking and per-dynamic isolation

Each dynamic owns a private dep set: `#{key() => true}` of the binding keys
read while it was evaluated. The diff engine compares each dep set against
the `Changed` map and skips re-evaluation when they don't intersect.

The mechanism is a single process-dictionary slot, `'$arizona_deps'`:

- `eval_one_v` (in `arizona_eval`) brackets each dynamic with
  `erlang:put('$arizona_deps', #{})` at entry and `erlang:erase` at exit.
- `arizona_template:get/2,3`, `arizona_template:get_lazy/3`, and
  `arizona_template:track/1` write the read key into the slot.
- The dynamic's deps are read back at `erlang:erase` and stored on the
  snapshot (parallel to `d`).

**Per-dynamic isolation invariant:** every nested-template entry point
runs inside a `with_saved_deps` wrapper (in `arizona_eval`), which
snapshots the slot before entering and restores it after. Without this,
an inner template's `?get` calls would land in the outer dynamic's slot
and pollute its deps. Bracketed entry points and what each covers:

- `eval_template` -- the inner template's per-dynamic eval loop
- `eval_each` -- per-item rendering (list, stream, map sources)
- `eval_stateful` -- the whole lifecycle: `mount/1` / `handle_update/2`,
  `render/1`, and the inner per-dynamic eval loop
- the stateless inline path -- the entire `eval_val_v` recursion for
  `#{callback := _, props := _}` descriptors, including the callback
  body and any subsequent unwrapping

Inner *dynamics* re-bracket their own slot, so per-inner-dynamic deps are
independent of outer.

**Usage convention:** outer-scope `?get` reads belong in the props
expression of `?stateful`/`?stateless` -- the parse transform places them
in the outer dynamic's closure, where they track correctly. Eager `?get`
calls inside a `?stateless` callback body, or inside a stateful's
`mount/1` / `handle_update/2`, are discarded by the wrap; this is
intentional -- those reads belong to the inner scope. Idiomatic callbacks
use a named fun reference (`?stateless(fun bar/1, Props)`), which cannot
close over outer `Bindings` and therefore avoids the footgun entirely.

## API -- effect commands (`arizona_js` / `arizona_android` / `arizona_effect`)

Client effect commands, built per platform -- `arizona_js` (web), `arizona_android` (native) --
all returning the neutral tuple `{arizona_effect, [OpCode, ...Args]}` (type `arizona_effect:cmd()`,
encoded by `arizona_effect`). Used in two contexts:

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

- `push_event/1,2` -- send event to server (template-only). Without explicit payload, auto-collects
  from element context (see below). Explicit payload is merged on top of auto-collected (explicit
  wins).
- `toggle/1`, `show/1`, `hide/1` -- element visibility via `hidden` attribute
- `add_class/2`, `remove_class/2`, `toggle_class/2` -- CSS class manipulation
- `set_attr/3`, `remove_attr/2` -- attribute manipulation
- `dispatch_event/2` -- dispatch CustomEvent on document
- `navigate/1,2` -- SPA navigation (opts: `#{replace => true}`)
- `focus/1`, `blur/1` -- focus management
- `scroll_to/1,2` -- scroll element into view (opts: `#{behavior => <<"smooth">>}`)
- `set_title/1` -- set document title
- `reload/0` -- reload page
- `encode/1` -- encode single cmd or list of cmds to HTML-safe JSON binary (called automatically by
  `to_bin`)

**Payload auto-collection** (`push_event`): When `push_event` fires on an element, the client
auto-collects a base payload from the element and event context, then merges explicit payload on
top:

- **Drop events** → `{data_transfer: "dragged-key", drop_index: N}` -- drag data from `dataTransfer`
  and drop position among `[az-key]` siblings
- **Forms** → `Object.fromEntries(new FormData(form))` -- all form field values keyed by `name`
- **Inputs/selects/textareas** → `{value: el.value}` -- current input value
- **Other** → `{}` -- empty

This means `az-keydown` or `az-focusout` on an input automatically includes `{value: "typed text"}`,
and `az-drop` automatically includes `{data_transfer, drop_index}`. No special attributes needed.

**Op codes** defined in `include/arizona_effect.hrl` -- integer constants shared with the client JS
runtime. Same codes for both template commands and server effects.

**Key filtering** via `on_key/2` -- wraps a command so it only executes when the pressed key
matches:

```erlang
%% Atom -- literal match
{az_keydown, arizona_js:on_key(enter, arizona_js:push_event(~"submit"))}
%% List -- match any
{az_keydown, arizona_js:on_key([enter, escape], arizona_js:push_event(~"close"))}
%% Binary -- regex pattern
{az_keydown, arizona_js:on_key(~"^[a-z0-9]$", arizona_js:push_event(~"type"))}
```

## Client-owned slots -- `?local`

A `?local(Key, Init)` slot is **static to the server, dynamic to the client**: the server
renders `Init` once at SSR and marks the slot `diff => false` so it is never patched; the
browser owns the value (keyed by `Key`) and mutates it locally with **no WebSocket
round-trip**. For UI-only state (dialog open/close, tabs, toggles) the server doesn't need.

Authoring (`?local` = `arizona_template:local/2`, also `az:local/2`):

```erlang
{'span', [], [?local(~"title", ~"Hello")]}                 %% content
{'dialog', [{open, ?local(~"modal_open", false)}], [...]}  %% boolean attribute
{'div', [{'data-active', ?local(~"tab", ~"home")}], [...]} %% valued attribute (CSS-driven tabs)
{'p', [], [~"Name: ", ?local(~"first", ~"Ada"), ~" ", ?local(~"last", ~"Lovelace")]} %% many content slots + static text
{'a', [{href, [~"/u/", ?local(~"id", ~"1"), ~"/edit"]}], [...]} %% interpolated attribute (static + one local)
```

Compile-time constraints: `Key` must be a literal binary; a key can't bind both content and
an attribute on one element; not allowed under `az-nodiff` or in `?native` templates.
A content `?local` does **not** have to be the sole child -- an element can hold several
content slots, freely mixed with static text and other dynamic children. An attribute value
may also **interpolate** one `?local` with static text around it (the statics become a
prefix/suffix); two `?local` in one attribute, or a `?local` mixed with a server-owned
dynamic there, are compile errors (`local_attr_multiple` / `local_attr_mixed`). Interpolation
is for **string-valued** attributes (`class`, `style`, `href`, `data-*`, `value`); a boolean
attribute must use a **whole-value** `?local` (`{disabled, ?local(~"d", false)}`, which
renders `false` → absent / `true` → bare) -- an interpolated value always renders
`name="value"`, so an interpolated boolean would be `disabled="false"`, i.e. still present.

Mechanism: `arizona_template:local/2` returns `#{diff => false, az_local => Key, v => Init}`
(attributes add `target => {attr, Name}`). `eval` preserves it; `arizona_diff` skips it via
the existing per-dynamic `#{diff := false}` clause; `arizona_render` unwraps it. Each content
slot is an individually comment-marked text node (`<!--az:X-->Init<!--/az-->`), exactly like
any dynamic text child. The parse transform bakes a self-describing `az-local` descriptor
attribute (escaped JSON `{c: {slotIdx: key}, a: {attrName: key}, ap: {attrName: [prefix, suffix]}}`)
onto the element -- `c` maps each content slot's dynamic-slot index to its key, `a` maps each
attribute name to its key, and `ap` (present only for interpolated attributes) carries the
static prefix/suffix. The client scans `[az-local]` live (no persistent index): for a content
slot it reconstructs the marker `az` from the element's runtime `az` + the slot index
(mirroring `arizona_html:text_az/2`); for an interpolated attribute it recomposes
`prefix ++ value ++ suffix` on set and strips them on read. An interpolated attribute's
composed initial value is baked into the bind-map's `v` (`[Prefix, to_bin(Init), Suffix]`), so
SSR render and diff-skip reuse the whole-value attribute path unchanged.

Updating (event attributes / handler effects via `arizona_js`; never sent to the server --
op `?EFFECT_SET_LOCAL`):

| Builder | Scope |
| --- | --- |
| `arizona_js:set(Key, Value)` | closest view of the trigger (markup-only) |
| `arizona_js:set(ViewId, Key, Value)` | a named view |
| `arizona_js:set_all(Key, Value)` | every view (document-wide) |

As a **handler effect** (returned from `handle_event`/`handle_info`), use `set/3` or
`set_all/2`: the closest-view `set/2` resolves against the trigger element, which a handler
effect runs without (`applyEffects` dispatches against `<html>`), so it is a no-op there.

Client JS API: `arizona.set(viewId, key, value)`, `arizona.setAll(key, value)`,
`arizona.get(key)` / `arizona.get(viewId, key)`. Content writes are **text-only** (a `<` in
a value renders as text, never HTML -- no injection); attribute writes reuse
`setAttribute`/`removeAttribute` (with `value`/`checked` property sync): boolean `true` =>
present, `false` => absent.

Caveats (by design):

- **Wholesale re-render resets it.** A `?local` value survives normal per-slot diffs, but
  an enclosing region re-rendered as a unit (`OP_UPDATE` innerHTML, `OP_REPLACE`, an `?each`
  item swap, a conditional template switch) recreates the slot at its SSR initial.
- **Forced reconnect resets it.** A non-1000 socket close fresh-mounts the view.
- **Server never reads the value.** To use it server-side, read it client-side
  (`arizona.get`) and include it in a `push_event` payload -- auto-collection ignores `[az-local]`.
- **No JS = frozen at initial**; `get` returns strings (no type preservation).
- **`az-hook` `updated()` fires on a client `set`.** A bound element carrying an `az-hook`
  sees `updated()` for changes the server never observed -- treat that hook's state as
  client-local too.
- **Accessibility is the author's job.** Driving a presentational attribute (`open`,
  `data-active`) does not touch ARIA. A native `<dialog open>` is announced for free, but the
  CSS-tabs pattern above sets no `aria-selected` -- and since `?local` writes a literal value
  (no expressions), per-element ARIA that depends on the slot needs one boolean slot per
  element or server-tracked state.
- **Dangerous attributes are the author's responsibility.** Content writes are always text
  (never HTML), but binding `href`/`style`/an `on*` handler to author-supplied data is on you
  -- only the attribute *name* is fixed at compile time, the value is not escaped.

## API -- `arizona_roadrunner_router.erl`

Roadrunner route compilation. A `{reload, ...}` entry adds the dev-mode SSE endpoint, and a
build-opts variant supports hot-reload-safe rebuilds.

- `compile_routes/1,2` -- build the map-shape roadrunner dispatch from route specs and store in
  `persistent_term`; the `/2` form threads build-time opts (e.g. `compress => false`) for
  replay across hot reloads
- `routes/1,2` -- expand specs into roadrunner's map-shape route entries without compiling
  (used by the listener boot path)

**Route types**:

- `{live, Path, Handler, Opts}` -- live route (`Opts` may contain `layouts => [{Mod, Fun}]` and
  `bindings => map()`)
- `{ws, Path, Opts}` -- WebSocket endpoint
- `{asset, Path, {priv_dir, App, SubDir}}` -- static asset from priv (served via zero-copy
  sendfile by `arizona_roadrunner_static`)
- `{asset, Path, {dir, Dir}}` -- static asset from absolute directory
- `{controller, Path, Handler, State}` -- generic roadrunner handler route
- `{reload, Path, Opts}` -- dev SSE reload endpoint (roadrunner-only convenience)

## API -- `arizona_stream.erl`

Pure stream data structure -- create, mutate, and query streams. The `#stream{}` record is defined
in `src/arizona.hrl`. All rendering/diffing of streams stays in
`arizona_eval.erl`/`arizona_diff.erl`.

- `new/1,2,3` -- create a stream with key function, optional initial items, and optional opts
  (`#{limit => N}`)
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

File watcher gen_server -- subscribes to `fs` events for a directory, debounces, calls optional
callback, broadcasts changed files via `arizona_pubsub`.

- `start_link(Dir, Opts)` -- starts a linked gen_server that subscribes to `fs` events for `Dir`.
  Options: `patterns` (list of regex strings, default `[".*"]`), `callback` (fun receiving list of
  changed file paths), `debounce` (ms, default 100). On debounce fire: calls callback, then
  `broadcast/1`
- `broadcast(Files)` -- broadcasts `{arizona_watcher, Files}` via `arizona_pubsub` on channel
  `arizona_watcher`

## API -- `arizona_pubsub.erl`

PubSub for cross-view communication. Thin wrapper around `pg` with scope `arizona_pubsub`. Channels
are arbitrary terms. Messages are raw data (no wrapper tuple).

- `subscribe(Channel, Pid)` -- subscribe `Pid` to `Channel`. Duplicate-safe: checks membership
  first, no-op if already subscribed
- `unsubscribe(Channel, Pid)` -- remove `Pid` from `Channel`
- `broadcast(Channel, Data)` -- send `Data` to all subscribers via `Pid ! Data`
- `broadcast_from(From, Channel, Data)` -- same but excludes `From` pid
- `subscribers(Channel)` -- return list of subscriber pids

## API -- `arizona_live.erl`

Simplified gen_server wrapper:

- `start_link/1,2,3,4,5` -- start with handler module, optional initial bindings (default `#{}`),
  optional `az:request()` (required for views with `mount/2`), optional transport PID, and optional
  `on_mount` hook list. `/5` takes all five positional args explicitly; the lower arities delegate
  with `undefined` for the missing params
- `mount/1` -- dispatches by handler arity: `Handler:mount(Bindings)` for stateful handlers,
  `Handler:mount(Bindings, Request)` for views. Extracts `ViewId = maps:get(id, Bindings)`, calls
  `Handler:render(B1)` via `arizona_handler:call_render/2`, builds a snapshot via
  `arizona_render:render/2`. Returns `{ok, ViewId}` (no HTML -- SSR is handled separately by
  `arizona_roadrunner_http`)
- `handle_event/4` -- unified event dispatch: `handle_event(Pid, ViewId, Event, Payload)`. Checks
  views map -- if `ViewId` is a known child, dispatches to child handler; otherwise dispatches to
  root handler. Returns `{ok, Ops, Effects}`
- `handle_info/2` -- gen_server callback for Erlang messages (`Pid ! Msg`, `erlang:send_after`,
  etc.). If handler exports `handle_info/2`, calls it, diffs, and pushes
  `{arizona_push, Ops, Effects}` to `transport_pid`. Pre-mount messages and handlers without
  `handle_info/2` are silently dropped. Empty ops+effects are not pushed
- `navigate/4,5` -- `navigate(Pid, NewHandler, InitBindings, NewReq [, OnMount])`. Mounts new
  handler (applying any `OnMount` hooks with the new `Request`), resets gen_server state
  (handler, req, snapshot, views), preserves `transport_pid` and `sent_fps`, returns
  `{ok, NewViewId, PageContent}`. The previous root handler's final bindings are carried
  forward as the floor for the new handler's mount input -- `InitBindings` (route static
  config + middleware enrichments) overrides on key overlap. Keys the new handler omits
  from its mount return value are dropped on the next navigate. Stateful children's state
  (in `views`) is wiped: a child that was alive on the old page is gone unless the new
  page re-embeds it
- `apply_on_mount/3` -- folds the `on_mount` hook chain: `apply_on_mount(OnMount, Bindings, Req)`.
  Each hook is `fun((Bindings, az:request()) -> Bindings)` or `{Module, Function}`

Internal state: `#state{handler, bindings, req, snapshot, views, on_mount, transport_pid,
sent_fps}` where `views :: #{ViewId => #{handler, bindings, snapshot}}`, `req` is the currently
mounted `az:request()` (or `undefined` for stateful-only flows), and `sent_fps` tracks fingerprints
already shipped to the client for deduplication.

`compute_changed/2` builds the Changed map by comparing old and new bindings key-by-key.

`push/3` sends `{arizona_push, Ops, Effects}` to the transport PID. No-ops when PID is `undefined`
or ops and effects are both empty.

## API -- `arizona_socket.erl`

Framework-agnostic WebSocket protocol state machine. The transport handler
(`arizona_roadrunner_ws`) feeds frames in and ships return tuples out, so the wire protocol
lives here independent of the server.

- `init/4` -- `init(Handler, Bindings, Req, Opts)`. Traps exits, starts
  `arizona_live:start_link/5` (passing `self()` as transport PID, `Req`, and any `on_mount`
  hooks), mounts. On reconnect (`#{reconnect => true}`), also renders and returns `OP_REPLACE`
  frame. Opts: `reconnect` (boolean), `on_mount` (list of `t:arizona_live:on_mount_hook/0`).
  The route adapter for SPA navigate is recovered from `Req` via `arizona_req:adapter/1`
- `handle_in/2` -- decode incoming text frame: ping/pong, `["cached_fps", FpList]`,
  `["navigate", #{~"path" := Path, ~"qs" := Qs}]`, `[target, event, payload]`
- `handle_info/2` -- handle `{arizona_push, Ops, Effects}` from `arizona_live` and `EXIT` signals.
  On non-normal exit the socket closes with `?CLOSE_CRASH` (4500); on normal exit with 1000. The
  client reconnects via backoff in `arizona-worker.js` -- crash remount is intentionally not
  attempted server-side

**Return type** (`result()`): `{ok, Socket}` | `{reply, iodata(), Socket}` |
`{close, Code, Reason, Socket}`

The `#socket{}` record carries only `pid, view_id, req` -- the post-mount state needed to
dispatch events and navigate. The route adapter is recovered from `req` on demand.

Internal functions: `scope_ops/2` (prepend view ID to op targets), `encode_reply/3` (build
`#{<<"o">> => Ops, <<"e">> => Effects}` JSON), `close_crash/1` (crash close tuple),
`dispatch_event/4`, `handle_navigate/3` (drives SPA navigate by invoking the adapter's
`resolve_route/3`).

## SPA navigate route resolution

When the client sends a navigate frame, `arizona_socket` invokes the optional
`resolve_route/3` callback declared on the `arizona_req` behaviour:

```erlang
-callback resolve_route(Path :: binary(), Qs :: binary(), Raw :: term()) ->
    {module(), arizona_live:route_opts(), arizona_req:request()}.

-optional_callbacks([resolve_route/3]).
```

The callback returns a handler module, the route's static options
(`t:arizona_live:route_opts/0` -- `bindings`, `on_mount`, `layouts`, `middlewares`), and a
navigate-scoped `arizona_req` carrying the new URL. Cookies/headers on the returned Request are
inherited from the original upgrade Req; `path`, `bindings` (path bindings from the router), and
`params` reflect the new path/qs.

**Shipped implementation:** `arizona_roadrunner_req` exports the optional `resolve_route/3` and
runs `roadrunner_router:match/2` against the compiled dispatch stored by
`arizona_roadrunner_router`.

## API -- `arizona_req.erl`

Opaque request abstraction backed by the transport adapter. Constructed via
`new/3(Adapter, Raw, #{method := ..., path := ...})`; other fields are lazy-loaded via the
adapter's behaviour callbacks on first access and cached in the returned request.

**Accessors** (eager):

- `method/1` -- HTTP method (`~"GET"`, `~"POST"`, ...)
- `path/1` -- request path, no query string
- `raw/1` -- the native transport value the adapter wraps

**Accessors** (lazy, return `{Value, Req1}`):

- `bindings/1` -- route-pattern path bindings (map of atoms)
- `params/1` -- parsed query string as a proplist
- `cookies/1` -- parsed cookies as a proplist
- `headers/1` -- request headers as a map
- `body/1` -- the request body

**Other:**

- `set_raw/2` -- swap the native raw value, clear all lazy caches
- `apply_middlewares/3(Middlewares, Req, Bindings)` -- threads a request and bindings map through
  a list of `middleware()`, returning `{cont, Req1, Bindings1}` or `{halt, HaltReq}`
- `redirect/2(Req, Location)` / `redirect/3(Req, Status, Location)` -- stash a 3xx redirect
  intent in the request. Transports pick it up on halt and translate uniformly (HTTP 3xx
  reply, or `arizona_js:navigate` effect on WS navigate)
- `halted_redirect/1` -- returns `{Status, Location}` if `redirect/2,3` was called, else
  `undefined`

The behaviour expects adapters to implement `parse_bindings/1`, `parse_params/1`,
`parse_cookies/1`, `parse_headers/1`, `read_body/1` against their native request type.

## API -- `arizona_http.erl`

Shared HTTP render pipeline that serves an Arizona view as the initial page response. Wraps the
native request in an `arizona_req:request()`, runs any route middlewares, and calls
`arizona_render:render_view_to_iolist/3`. The transport handler translates the returned result
into its native reply shape.

- `render/3(Handler, Req, Opts)` -- returns one of:
  - `{halt, Raw}` -- middleware halted; the native raw req already has a response written
  - `{redirect, redirect_status(), Location}` -- middleware halted via `arizona_req:redirect/2,3`
  - `{ok, 200, iolist()}` -- rendered page body
  - `{error, 500, iolist()}` -- rendered error page body (crash or stashed hot-reload error)

`arizona_roadrunner_http` consumes this helper and maps the four result shapes into roadrunner's
reply tuple.

## API -- `arizona_ws.erl`

Transport-agnostic upgrade bootstrap for WebSocket handlers.

- `prepare/3(QS, Adapter, AdapterState)` -- accepts the pre-parsed upgrade query string
  (`[{binary(), binary() | true}]`), reads `_az_path` and `_az_reconnect` framework keys,
  strips them to compute the user-visible query string, resolves the target route via the
  adapter's `resolve_route/3` callback, runs middlewares, and returns:
  - `{halt, az:request()}` -- middleware blocked the upgrade; caller extracts the native raw
    via `arizona_req:raw/1` to emit its transport response
  - `{cont, State}` -- `State` is a map carrying `handler`, `bindings`, `on_mount`, `req`,
    `reconnect` that the caller threads into `arizona_socket:init/4`

`arizona_roadrunner_ws` collapses to a few lines that call `parse_qs`, invoke
`arizona_ws:prepare/3`, and wire the result into roadrunner's callback contract.

## Roadrunner handlers

- `arizona_roadrunner_http.erl` -- thin HTTP handler. Delegates the pipeline to
  `arizona_http:render/3` and translates its result into roadrunner's `{Response, Req}` reply shape
- `arizona_roadrunner_ws.erl` -- thin WebSocket handler. Delegates the upgrade to
  `arizona_ws:prepare/3` and forwards frames to `arizona_socket`. Translates
  `arizona_socket:result()` to roadrunner return tuples

## Data flow

**SSR (HTTP):** `arizona_roadrunner_http:handle/1` delegates to `arizona_http:render/3`, which wraps
the native req in an `arizona_req:request()`, runs any route middlewares
(`arizona_req:apply_middlewares/3`), then calls
`arizona_render:render_view_to_iolist(Handler, Req, Opts)` where Opts may contain
`layouts => [{Mod, Fun}]`, `bindings => map()`, and `on_mount => [...]`. `render_view_to_iolist/3`
mounts the page via `arizona_view:call_mount/3` (passing the Request), applies `on_mount` hooks,
renders to page HTML, then optionally injects the page HTML into mount bindings as `inner_content`
and passes the bindings to the layout's `render/1`. The layout uses `?html` with `az_nodiff` on
the root element -- a stateless HTML shell (DOCTYPE, head, body, scripts) with no markers or `az`
attributes. When layout is absent, the page is rendered directly without a wrapper. Route config
provides `handler`, `layout`, `bindings`, `on_mount`, and `middlewares`. URL data (path bindings,
query params) does NOT flat-merge into Bindings -- handlers reach it via `arizona_req:bindings/1`
/ `arizona_req:params/1` or a middleware that projects what it wants. Middleware halts that call
`arizona_req:redirect/2,3` surface as a `{redirect, Status, Location}` result that the transport
emits as a 3xx reply.

**WebSocket mount:** `arizona_roadrunner_ws:handle/1` delegates to `arizona_ws:prepare/3`, which
reads `_az_path`/`_az_reconnect`, resolves the route, runs middlewares, and returns the state
for the transport to hand to `arizona_socket:init/4`. The socket calls
`arizona_live:start_link/5` (passing `self()` as transport PID, the Req, and route `on_mount`
hooks) then `arizona_live:mount/1`. Mount establishes the
server-side snapshot (matching SSR). Returns `{ok, ViewId}` where ViewId comes from
`maps:get(id, MountBindings)`. Handlers detect the connected context via `?connected` macro
(delegates to `arizona_live:connected()`) which reads a process dictionary flag set in
`arizona_live:init/1`. For post-connection effects, handlers use `?send(arizona_connected)` in mount
and handle it in `handle_info/2`.

**Server push (`handle_info`) -- per-view routing:** Messages sent via `?send(Msg)` /
`arizona_live:send(ViewId, Msg)` are tagged as `{arizona_view, ViewId, Msg}` and routed to the
correct handler's `handle_info/2` -- root or child. The root view ID is matched from
`#state.bindings`, children from the views map. Unknown view IDs crash with
`{unknown_view, ViewId, Msg}`. Plain untagged messages (`Pid ! Msg`) route to the root handler for
backward compatibility. Delayed sends use `?send_after(Time, Msg)` /
`arizona_live:send_after(ViewId, Time, Msg)`. PubSub subscriptions use `?subscribe(Topic)` /
`?unsubscribe(Topic)`. After `handle_info/2` returns, the template is re-rendered and diffed, and
ops+effects are sent as `{arizona_push, Ops, Effects}` to the transport PID.

**Events:** Client sends `[target, eventName, payload]` over WebSocket. `arizona_roadrunner_ws`
dispatches to `arizona_live:handle_event/4`. The gen_server checks the views map -- if target is a
known child view, dispatches to child; otherwise dispatches to root handler. Returns
`{ok, Ops, Effects}`. Ops are scoped with `"viewId:target"` format. The response is sent as a JSON
envelope `{"o": scopedOps, "e": effects}`.

**SPA navigation + unmount:** Client clicks `[az-navigate]` link -> JS intercepts, calls
`history.pushState`, sends `["navigate", {path, qs}]` over WebSocket. `arizona_socket`'s navigate
handler resolves the new route via the adapter, then runs the new route's middlewares (lifecycle
parity with HTTP init and WS upgrade). If middleware halts with `arizona_req:redirect/2,3`, the socket
emits `[arizona_js:navigate(Location)]` as a client effect -- no `arizona_live:navigate` call
happens; the browser pushes the new URL and the fresh HTTP handshake runs middleware again on
the redirect target. On `cont`, `arizona_live:navigate/5` is called. Before mounting the new
handler, the framework cancels pending `send_after` timers and calls the old root handler's
`unmount/1` callback (if exported). Propagation to children is opt-in -- the root can broadcast
via pubsub in its `unmount/1`. Then the new handler is mounted and gen_server state is reset.
Returns `{ok, NewViewId, PageHTML}`. WS handler sends `[OP_REPLACE, OldViewId, PageHTML]`.
Browser back/forward also triggers navigate via `popstate`.

**Stateful children:** `arizona_template:stateful(Handler, Props)` returns a descriptor map. During
`eval_val_v/2`, the engine checks if the child is already in the views map -- if so, calls
`handle_update/2` (or `maps:merge` if not exported); if not, calls `Handler:mount(Props)`. Child
templates are recursively rendered/diffed with their own snapshots. The views map uses a
`{OldViews, NewViews}` tuple during eval -- `OldViews` is read-only for child lookup, `NewViews`
accumulates only children rendered this cycle. Children removed from the template (conditional
rendering) are pruned from the views map and their `unmount/1` callback is called if exported.

**Slots:** Slots are implemented via stateless children and bindings. A layout receives the page
HTML via a configurable binding key (e.g. `inner_content`) in `render/1`. Stateless components
receive props with arbitrary content:

```erlang
%% Layout slot -- inner_content is the page HTML (layout uses ?html with az_nodiff)
render(Bindings) ->
    ?html({body, [az_nodiff], [maps:get(inner_content, Bindings)]}).

%% Component slot -- via stateless child props
?stateless(render_card, #{label => ~"Hello", content => SomeTemplate})
```

## Handler callbacks

Three roles, each with its own header:

- **Route-level pages** include `arizona_view.hrl` (sets `-behaviour(arizona_view)` +
  `-behaviour(arizona_handler)`, parse transform, template macros). Mount is `mount/2` and takes
  `(Bindings, az:request())`.
- **Embeddable stateful components** include `arizona_stateful.hrl` (sets
  `-behaviour(arizona_stateful)` + `-behaviour(arizona_handler)`). Mount is `mount/1`;
  instantiated from a parent template via `?stateful(Handler, Props)`.
- **Pure stateless templates** include `arizona_stateless.hrl` (parse transform only, no
  behaviour). Invoked via `?stateless(Callback, Props)` or `?stateless(Mod, Fun, Props)`.

```erlang
%% Route-level page
-module(my_page).
-include("arizona_view.hrl").
-export([mount/2, render/1, handle_event/3]).

mount(Bindings, _Req) ->
    {maps:merge(#{id => ~"page", count => 0}, Bindings), #{}}.

render(Bindings) ->
    ?html({'div', [], [?get(count, 0)]}).

handle_event(~"inc", _Payload, Bindings) ->
    {Bindings#{count => maps:get(count, Bindings, 0) + 1}, #{}, []}.
```

**Callback signatures** (see `src/arizona_view.erl`, `src/arizona_stateful.erl`, and
`src/arizona_handler.erl` for authoritative types):

```erlang
mount(Bindings, Request) -> {NewBindings, Resets}.                             %% Required -- view
mount(Bindings) -> {NewBindings, Resets}.                                      %% Required -- stateful
render(Bindings) -> #{s => [...], d => [...]}.                                 %% Required (arizona_handler)
handle_event(Event, Payload, Bindings) -> {NewBindings, Resets, Effects}.      %% Optional (arizona_handler)
handle_info(Info, Bindings) -> {NewBindings, Resets, Effects}.                 %% Optional (arizona_handler)
handle_update(Props, Bindings) -> {NewBindings, Resets}.                       %% Optional (arizona_handler; stateful only in practice)
unmount(Bindings) -> term().                                                   %% Optional (arizona_handler)
```

`Resets` is a map of binding values reapplied on top of `NewBindings` after the callback returns --
typically `#{}`, or a subset of keys you want cleared (form fields, transient flags). `Effects` is a
list of `arizona_js:cmd()` values to run client-side after the diff is applied.

Mount must produce `Bindings` containing an `id` key -- this becomes the ViewId used for event
routing and navigate targeting. The parse transform auto-injects `az-view` as a boolean attribute
on the root element of `render/1` when the module declares either `arizona_view` or
`arizona_stateful` behaviour. Using `az_view` manually outside this context raises a compile error.

**`id` attribute restriction:** The root element's `id` MUST use `{id, ?get(id)}` (or the equivalent
`arizona_template:get(id, Bindings)` / `az:get(id, Bindings)`). Static binaries and composed values
are not allowed. This is a compile-time check.

The `id` serves three roles simultaneously: DOM id (`document.getElementById`), views map key
(server-side state tracking), and wire protocol target (op scoping). All three must be the same
value. Using `?get(id)` ensures the template renders the same value the parent passed in Props and
the server uses for tracking. `id` is a restricted key -- the handler's mount callback cannot
override the value passed by the parent in Props.

**Dispatcher error tagging.** Each `arizona_handler:call_*` wrapper catches `error:undef` and
`error:function_clause` only when the failing stack frame is the user's exact callback, and
re-raises with a structured reason carrying an `error_info` annotation: `{missing_callback, Mod,
Name, Arity}`, `{unhandled_event, Mod, Event, Bindings}`, `{unhandled_info, Mod, Info, Bindings}`,
`{unhandled_update, Mod, Props, Bindings}`, `{unhandled_unmount, Mod, Bindings}`, or
`{render_no_clause, Mod, Bindings}`. `arizona_handler:format_error/2` turns each into a sentence
that names the offending module/event and (when known) the view id. Errors raised from inside a
callback's body propagate untagged.

`handle_event/3` effects: a list of `arizona_js:cmd()` tuples, built using functions in
`arizona_js.erl`. Same commands used in template attributes. Common effects:

- `arizona_js:dispatch_event(Name, Payload)` -- dispatches a CustomEvent on the client's document
- `arizona_js:set_title(Title)` -- sets the browser document title
- `arizona_js:reload()` -- reloads the page
- Any other `arizona_js` command (toggle, show, hide, etc.) also works as an effect

`handle_info/2` is invoked for any Erlang term landing in the gen_server mailbox (`?send`,
`?send_after`, pubsub, raw `Pid ! Msg`). If not exported, messages are silently dropped (checked via
`erlang:function_exported/3`).

`handle_update/2` intercepts parent prop updates before they reach the bindings. Called on stateful
children when the parent re-renders with new `Props`. If not exported, the framework merges `Props`
into `Bindings` directly (`call_handle_update/3`).

`unmount/1` runs when an instance is removed -- either the parent stopped rendering it (conditional
or navigate) or the live process is shutting down. Use it to release resources (timers, external
subscriptions). Return value is discarded.

## Dynamic `eval` return values

- `iodata()` -- rendered value
- `#{s => ..., d => ...}` -- nested template (recursively rendered/diffed)
- `#{stateful => Handler, props => Props}` -- stateful child descriptor
- `#{callback => Fun, props => Props}` -- stateless child descriptor
- `remove` -- sentinel that triggers `OP_REMOVE_NODE`

## Change tracking

`arizona_template:get/2,3` tracks which binding keys each dynamic reads via the process dictionary
key `'$arizona_deps'`. The structure is a map of keys, e.g. `#{count => true, title => true}`, or
`undefined` when inactive.

**Lifecycle per dynamic (in `eval_one_v`):**

1. `erlang:put('$arizona_deps', #{})` -- start tracking
2. The dynamic's fun is called -- each `arizona_template:get(Key, Bindings)` adds `Key => true`
3. `erlang:erase('$arizona_deps')` -- harvest the dep map
4. Deps are stored in the snapshot as `deps => [#{key() => true}]`, parallel to the `d` list

**Nesting:** Stateful children and nested templates save/restore `'$arizona_deps'`
(`SavedDeps = erlang:get(...)` before, `erlang:put(..., SavedDeps)` after) to prevent child `get`
calls from polluting the parent's dep list.

**Inactive paths:** `render/1`, `diff/2`, and SSR never set `'$arizona_deps'`, so
`arizona_template:track/1` sees `undefined` and is a no-op.

**Consumption:** Only `diff/4` uses deps. `deps_changed/2` checks whether any key in a dynamic's dep
map appears in the `Changed` map (via `maps:intersect`). If none do, the dynamic is skipped entirely
-- its fun is never called.

## Op codes

| Code | Name             | Args                       | Description                                       |
| ---- | ---------------- | -------------------------- | ------------------------------------------------- |
| 0    | `OP_TEXT`        | `[target, value]`          | Update text node                                  |
| 1    | `OP_SET_ATTR`    | `[target, attr, value]`    | Set attribute                                     |
| 2    | `OP_REM_ATTR`    | `[target, attr]`           | Remove attribute                                  |
| 3    | `OP_UPDATE`      | `[target, html]`           | innerHTML replacement (diff engine)               |
| 4    | `OP_REMOVE_NODE` | `[target]`                 | Remove element                                    |
| 5    | `OP_INSERT`      | `[target, key, pos, html]` | Stream insert (pos=-1 -> append, otherwise index) |
| 6    | `OP_REMOVE`      | `[target, key]`            | Stream remove                                     |
| 7    | `OP_ITEM_PATCH`  | `[target, key, innerOps]`  | Stream item patch                                 |
| 8    | `OP_REPLACE`     | `[target, html]`           | outerHTML replacement (navigate)                  |
| 9    | `OP_MOVE`        | `[target, key, afterKey]`  | Stream move (afterKey=null -> prepend)            |

## Target scoping

Patch targets are `"viewId:relativeTarget"`. Root view `<<"page">>`: `"page:0"`, `"page:1"`. Child
view `<<"counter">>`: `"counter:0"`, `"counter:1"`. `arizona_socket` prefixes ops with the view id
via internal scoping. Child view ops use `[ChildViewId, ChildOps]` nesting and recurse into these.
Bare targets (no `:`) resolve to the view root via `document.getElementById(target)` -- used by
`OP_REPLACE` during navigate.

## Server integration

The core engine (`arizona_template`, `arizona_render`, `arizona_diff`, `arizona_live`,
`arizona_stream`, `arizona_pubsub`, `arizona_js`, parse transform) is transport-agnostic. The
roadrunner server is wired in through a thin layer of `arizona_*` modules that translate between
roadrunner's callbacks and Arizona's shared pipeline:

- `arizona_roadrunner_server` -- boots the listener and compiles routes
- `arizona_roadrunner_router` -- compiles route specs into roadrunner's dispatch
- `arizona_roadrunner_http` -- HTTP handler; delegates to `arizona_http:render/3`
- `arizona_roadrunner_ws` -- WebSocket handler; delegates the upgrade to `arizona_ws:prepare/3`
  and forwards frames to `arizona_socket`
- `arizona_roadrunner_static` -- static file serving
- `arizona_roadrunner_reload` -- dev-mode SSE reload endpoint
- `arizona_roadrunner_req` -- implements the `arizona_req` behaviour (the request abstraction
  consumed by handlers), including the optional `resolve_route/3` for SPA navigate

The `arizona_req` behaviour is the boundary between the server and the engine: handlers and the
shared pipeline only ever see an `arizona_req:request()`, never roadrunner's native request type.

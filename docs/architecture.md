# Arizona Architecture

## Source modules

| Module                                    | Purpose                                                                                                                                                                                   |
| ----------------------------------------- | ----------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------- |
| `src/arizona.hrl`                         | Shared header -- op codes, `?EACH` constant, `#stream{}` record                                                                                                                           |
| `src/arizona_template.erl`                | Pure helpers -- binding access (`get/2,3`), dep tracking (`track/1`), descriptor constructors (`stateful/2`, `stateless/2,3`), `each/2`, `to_bin/1`, snapshot utilities                   |
| `src/arizona_eval.erl`                    | Template evaluation -- dynamics to snapshots, stateful/stateless child eval, stream/each eval                                                                                             |
| `src/arizona_render.erl`                  | Render orchestration (target-aware) -- `render/1,2`, SSR (`render_to_iolist/1,2`, `render_view_to_iolist/2`), `zip/2`, `fingerprint_payload/1`                                            |
| `src/arizona_renderer.erl`                | Render-target backend behaviour -- byte emission + `attr_command/2`; HTML and native JSON backends below                                                                                  |
| `src/arizona_html.erl`                    | HTML render backend -- emits HTML statics/attrs (the `?html` target)                                                                                                                      |
| `src/arizona_native.erl`                  | Native render backend -- emits a JSON widget tree (the `?native` target); see docs/native.md                                                                                              |
| `src/arizona_terminal.erl`                | Terminal render backend -- emits text + ANSI escapes (the `?terminal` target); vocabulary in "Terminal render target" below                                                               |
| `src/arizona_terminal_session.erl`        | Transport-agnostic orchestrator for `?terminal` views -- mounts, dispatches key/push events, repaints; parameterized by a driver module + an output fun                                   |
| `src/arizona_terminal_driver.erl`         | Terminal UX-policy behaviour -- `init/1`, `keys/2`, `paint/3`, `setup/1`, `teardown/1` (all optional; defaults in `arizona_terminal_default_driver`)                                      |
| `src/arizona_terminal_default_driver.erl` | Default `arizona_terminal_driver` -- idiomatic key events, Ctrl-D quits, cursor hide/show, non-flickering repaint, quit/set_title/bell effects; the session falls back to it per callback |
| `src/arizona_terminal_io.erl`             | Low-level terminal byte helpers -- `keys/1` decodes a raw input read into idiomatic keys (the input complement to OTP's output-only `io_ansi`)                                            |
| `src/arizona_terminal_effect.erl`         | Framework terminal effect builders -- `quit/0`, `set_title/1`, `bell/0` (the `?terminal` analog of `arizona_js` / `arizona_android`)                                                      |
| `src/arizona_terminal_tty.erl`            | Local TTY transport -- raw-mode `shell:start_interactive`, a blocking input reader, `io:put_chars` as the output                                                                          |
| `src/arizona_terminal_ssh.erl`            | SSH transport -- serves a `?terminal` view over an `ssh_server_channel` (pty-req, window-change), one live view per connection                                                            |
| `src/arizona_diff.erl`                    | Diff engine -- `diff/2,3,4`, stream/list diffing, LIS algorithm                                                                                                                           |
| `src/arizona_roadrunner_router.erl`       | Roadrunner route compilation -- `compile_routes/1,2`, `routes/1,2` (map-shape routes with state under `#{arizona => ...}` namespace)                                                      |
| `src/arizona_effect.erl`                  | Neutral effect plumbing -- the `{arizona_effect, [...]}` tuple; `encode/1` (HTML attr) + `encode_json/1` (raw); op codes in `include/arizona_effect.hrl`                                  |
| `src/arizona_js.erl`                      | Web/browser command/effect builders -- `push_event`, `navigate`, `patch`, `fetch`, `toggle`/`show`/`hide`, `focus`/`blur`/`scroll_to`, `on_key`, `dispatch_event`, `set_title`, `reload`  |
| `src/arizona_android.erl`                 | Native (`?native`) command builders -- the portable `push_event/1,2` and `navigate/1,2`                                                                                                   |
| `src/arizona_stream.erl`                  | Pure stream data structure -- create, insert, delete, update, move, sort, reset, `clear_stream_pending/2`, `stream_keys/1`                                                                |
| `src/arizona_stateful.erl`                | Behaviour for all live handlers (route-page roots + embedded `?stateful`) -- `mount`/`render`/`handle_*`/`unmount` callbacks, the `call_*` dispatchers, and `format_error/2`              |
| `src/arizona_req.erl`                     | Opaque request -- eager `method`/`path`, lazy `bindings`/`params`/`cookies`/`headers`/`body`/`user_agent`, `redirect`/`halted_redirect`                                                   |
| `src/arizona_middleware.erl`              | Request-to-bindings middleware pipeline -- `apply_middlewares/3` runner + built-in `extract/1`/`put_request/2`/`fetch_flash/2`/`fetch_session/2`/`check_origin/2` steps                   |
| `src/arizona_origin.erl`                  | CSRF Origin check -- `check/2` (same-origin or `csrf_origins` allowlist; missing Origin allowed); behind the default-on `check_origin/2` middleware                                       |
| `src/arizona_crypto.erl`                  | Signed/encrypted value primitives -- `sign/1,2`+`verify/1` (HMAC-SHA256) and `encrypt/1,2`+`decrypt/1` (AES-256-GCM), with optional `#{ttl}` expiry; keyed off `secret_key`               |
| `src/arizona_flash.erl`                   | Signed, one-time flash cookie codec -- `encode/1`/`decode/1` (signed JSON + baked-in TTL), `resp_cookie/2`; HTTP full-page-redirect path (a live navigate carries flash in-process)       |
| `src/arizona_session.erl`                 | Encrypted, durable session cookie codec -- `encode/1`/`decode/1` (AES-256-GCM via `arizona_crypto`, TTL = max-age), durable `resp_cookie/2`; a read does not consume                      |
| `src/arizona_session_store.erl`           | Server-side session store behaviour -- `get/put/delete` (+ optional `child_spec/0`); opt-in via `session_store`, opaque signed-id cookie; mirrors `Plug.Session.Store`                    |
| `src/arizona_session_store_ets.erl`       | Default in-memory `arizona_session_store` -- a supervised public-ETS owner + periodic sweep + lazy expiry; `get/put/delete` run off the request process                                   |
| `src/arizona_user_agent.erl`              | User-Agent classification for dual-serve views -- `browser/1`, `os/1`, `mobile/1` (best-effort); pairs with `arizona_req:user_agent/1`                                                    |
| `src/arizona_http.erl`                    | Transport-agnostic HTTP render pipeline -- `render/3` runs middlewares, renders the view, returns `{halt\|redirect\|ok\|error, ...}` tuples                                               |
| `src/arizona_controller.erl`              | Reply helpers for controller routes consumed by `arizona_js:fetch/2` -- `reply_effects/1` (the `{"e": [...]}` wire body), `reply_redirect/1` (a `navigate` effect)                        |
| `src/arizona_static.erl`                  | Offline static-site generation -- `generate/2,3` renders route handlers to HTML files under an out-dir; returns `{Written, Failed}`                                                       |
| `src/arizona_ws.erl`                      | Transport-agnostic WS upgrade bootstrap -- `prepare/3` parses framework keys, resolves the route, runs middlewares, returns state for `arizona_socket`                                    |
| `src/arizona_live.erl`                    | Gen_server -- mount (stateful or view), handle_event, handle_info, views map, transport push                                                                                              |
| `src/arizona_parse_transform.erl`         | Compile-time transform -- `?html`/`?native`, `?each` DSL to `#{s, d, f}` maps, `az-view` auto-injection, `az-nodiff`, attribute compilation, cross-target nesting guard                   |
| `src/arizona_socket.erl`                  | Framework-agnostic WebSocket protocol state machine -- JSON encode/decode, event dispatch, navigation, op scoping. Crash closes cleanly; client reconnects via backoff                    |
| `src/arizona_roadrunner_http.erl`         | Roadrunner HTTP handler -- thin wrapper: delegates to `arizona_http:render/3` and translates results into roadrunner's `{Response, Req}` reply shape                                      |
| `src/arizona_roadrunner_ws.erl`           | Roadrunner WebSocket handler -- dual behaviour (`roadrunner_handler` for upgrade + `roadrunner_ws_handler` for session); delegates to `arizona_ws:prepare/3`                              |
| `src/arizona_roadrunner_controller.erl`   | Roadrunner handler for controller routes -- middleware pipeline (CSRF default-on), restores `state`, then dispatches `Handler:Action/1` (`action` opt, default `handle`)                  |
| `src/arizona_roadrunner_server.erl`       | Roadrunner listener boot -- compiles routes, stashes them for hot reload, validates TLS opts, starts a clear/TLS listener                                                                 |
| `src/arizona_roadrunner_req.erl`          | Roadrunner `arizona_req` adapter -- parsing callbacks plus `resolve_route/3` for SPA navigate; populates `request_id` from roadrunner                                                     |
| `src/arizona_roadrunner_reload.erl`       | Dev-mode SSE endpoint -- streams reload events from `arizona_reloader` to the browser                                                                                                     |
| `src/arizona_jsonrpc.erl`                 | JSON-RPC 2.0 codec -- `decode/1`, `result/2`, `error/3`, `notification/2`                                                                                                                 |
| `src/arizona_mcp.erl`                     | MCP server behaviour an app implements (`init/1`, `tools/1`, `handle_tool/4`, optional `resources`/`prompts`/`channels`/`terminate`) plus the server-push API (`broadcast/3`, `notify/3`) |
| `src/arizona_mcp_handler.erl`             | Roadrunner handler -- MCP Streamable HTTP transport (POST/GET/DELETE), the Origin/auth gate, and the pure `dispatch/3` / `handle_method/4` method dispatch                                |
| `src/arizona_mcp_session.erl`             | Per-session `gen_server` -- holds the handler state, serializes a session's requests, owns the SSE channel and the resumability buffer                                                    |
| `src/arizona_mcp_session_registry.erl`    | ETS registry mapping `Mcp-Session-Id` to a session pid; sweeps dead pids on lookup                                                                                                        |
| `src/arizona_mcp_sup.erl`                 | `simple_one_for_one` supervisor owning the session processes (so a session outlives its connection) and the registry table                                                                |
| `src/arizona_error_page.erl`              | Dev-mode error page renderer -- pretty-prints compile and runtime errors                                                                                                                  |
| `src/arizona_config.erl`                  | App-env reader with env-var resolution -- `get_env/1,2` (drop-in for `application:get_env`) + `resolve/1`; expands `{env, "VAR"}` / `{env, "VAR", Default}` refs, coerced by default type |
| `src/arizona_app.erl`                     | Application callback -- starts `arizona_sup` and, when the `server` env is set, launches the roadrunner listener                                                                          |
| `src/arizona_watcher.erl`                 | File watcher gen_server -- subscribes to `fs` events, debounces, calls callback, broadcasts via `arizona_pubsub`                                                                          |
| `src/arizona_reloader.erl`                | Dev-mode hot reloader -- recompiles changed `.erl` files, broadcasts reload messages on the `arizona_reloader` pubsub topic                                                               |
| `src/arizona_pubsub.erl`                  | PubSub -- thin `pg` wrapper for cross-view communication                                                                                                                                  |
| `src/arizona_sup.erl`                     | Supervisor -- always starts `arizona_pubsub`; additionally starts one `arizona_watcher` per configured reloader rule                                                                      |
| `src/az.erl`                              | User-facing facade -- shared type aliases and short-form helpers re-exported from internal modules                                                                                        |

## API -- `arizona_template.erl`

Pure helpers for binding access, descriptor construction, and template composition.

- `get/2,3`, `get_lazy/3` -- binding access with dep tracking via process dictionary
  (`$arizona_deps`)
- `track/1` -- manually track a binding key as a dependency
- `with/2` -- tracked projection: `track/1`s each key then `maps:with(Keys, Bindings)`;
  hands a bindings subset to a sub-context while declaring its deps
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
- `render_view_to_iolist/2` -- production HTTP SSR for views: `render_view_to_iolist(Handler, Opts)`.
  Mounts via `arizona_stateful:call_mount/2`, applies `on_mount` hooks, optionally
  wraps in a layout
- `resolve_id/1` -- resolves a binary id (passthrough) or a `#{s, d}` template (renders to binary)
- `zip/2` -- interleave statics and evaluated dynamics into iolist
- `fingerprint_payload/1` -- convert a snapshot to fingerprint wire format

## Terminal render target -- `?terminal`

`?terminal(Elems)` renders a view to ANSI text instead of HTML or JSON. The backend
is `arizona_terminal` (an `arizona_renderer`), the sibling of `arizona_html` and
`arizona_native`. Its transport **repaints the whole frame** on every update
(`arizona_live:render_current/1`) rather than applying diff ops, so the backend
writes **no `az`/slot markers** -- the diff engine's ops are computed but ignored.

Escaping is per-backend via the `c:arizona_renderer:escape/1` callback, selected by the
render target: HTML entity-escapes, terminal **control-char sanitizes** (strips C0
bytes except `\n`/`\t`, and DEL; preserves UTF-8), native is a no-op (JSON handles it).
So a dynamic value in a `?terminal` view cannot inject ANSI/OSC escape sequences into
another user's terminal (the terminal analog of XSS). The `?raw` opt-out bypasses it --
never wrap untrusted data in `?raw` in a `?terminal` template.

### Elements

| Tag             | Emits                                                          |
| --------------- | -------------------------------------------------------------- |
| `line`          | its content, a style reset, then a newline -- one terminal row |
| `col` / `row`   | its content, then a style reset -- a transparent container     |
| `text` / `span` | its content, then a style reset -- inline, no newline          |
| `br`            | a void element emitting a newline                              |

Only `line` (newline) and `br` (void newline) affect layout; `col`, `row`, `text`,
and `span` are behaviorally identical (open emits nothing, close emits a style
reset) -- the names are documentary. So **vertical layout is a `col` of `line`s**,
and horizontal is several `text`/`span` inside one `line`.

### Styling

Set styles with **bare-atom boolean attributes** mapped to `io_ansi` escapes:
`bold`, `dim`, `italic`, `underline`, `inverse`, and the colours
`black`/`red`/`green`/`yellow`/`blue`/`magenta`/`cyan`/`white` plus their `light_*`
variants. For anything the atoms don't cover, use a valued attribute: `{sgr, Escape}`
(a raw escape, verbatim), `{fg, Index}` / `{bg, Index}` (256-colour palette by
index). Styles **do not inherit** across containers (every element resets on close),
so put the style on the `line`/`text` that needs it. An unknown style atom, an
unknown valued attribute, a dynamic attribute value, or an event-command attribute
is **rejected at compile time**, not silently dropped.

```erlang
render(Bindings) ->
    ?terminal(
        {col, [], [
            {line, [bold, green], [~"== Title =="]},
            {line, [], [~"plain ", {span, [yellow], [~"highlighted"]}, ~" tail"]},
            {line, [{fg, ~"208"}], [~"256-colour orange"]}
        ]}
    ).
```

**Conditional rows:** a control-flow expression (`case`/`if`/`begin`/`receive`/`try`/
`maybe`) returning a bare element tuple from a tail in a content slot is compiled into a
nested template (inheriting the enclosing target), just as a literal `?html`/`?terminal`
there would be -- no wrap needed:

```erlang
{col, [], [
    case ?get(mode) of
        warn -> {line, [yellow], [~"check your input"]};
        _ -> <<>>
    end
]}
```

For 0-or-N rows, `?each` over a list still applies:

```erlang
?each(fun(Msg) -> {line, [yellow], [Msg]} end, status_rows(?get(mode)))
```

### Running a `?terminal` view

`arizona_terminal_session` is the transport-agnostic core: it mounts the view, turns
input into `arizona_live:handle_event/4` calls, and repaints. It is parameterized by
a **driver** (`arizona_terminal_driver` -- the key map, paint model, and screen
setup/teardown, with defaults in `arizona_terminal_default_driver` that the session
falls back to per callback) and an output fun. Two transports drive it:
`arizona_terminal_tty` (a local raw-mode TTY) and `arizona_terminal_ssh` (a view served over
SSH). `arizona_terminal_io:keys/1` decodes a raw input read into idiomatic keys
(char codes, atoms like `up`/`enter`, `{ctrl, _}` / `{alt, _}`) for drivers to match
on.

A view drives the terminal back with **effects** from `arizona_terminal_effect` (the
`?terminal` analog of `arizona_js` / `arizona_android`): `quit/0` stops the session,
`set_title/1` sets the terminal title, `bell/0` rings it. The default driver's
`paint/3` interprets these and repaints in place (no full-screen clear, so no
flicker). Effects tied to a specific paint model (e.g. a scrolling log above a pinned
block) stay the concern of a custom driver, like the demo's.

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
- `eval_stateful` -- the whole lifecycle: `mount/1` / `handle_update/3`,
  `render/1`, and the inner per-dynamic eval loop
- the stateless inline path -- the entire `eval_val_v` recursion for
  `#{callback := _, props := _}` descriptors, including the callback
  body and any subsequent unwrapping

Inner *dynamics* re-bracket their own slot, so per-inner-dynamic deps are
independent of outer.

**Content-slot conditional branches:** a `case`/`if`/`maybe` in a content slot
compiles each *element* branch into a nested template, so by this invariant the
branch's `?get` reads land in the nested template's slot, not the enclosing
conditional dynamic's -- which freezes the branch when only a branch-read binding
changes (the conditional's deps would otherwise hold just the scrutinee). The parse
transform (`branch_track_touches/1` in `arizona_parse_transform`) compensates by
injecting `arizona_template:track/1` for each literal key read in an element branch
tail, recording them as deps of the conditional dynamic itself. `track/1` records the
key without reading the binding, so an absent non-taken-branch key is safe; value
(scalar) branches are left alone (their reads already track when taken). See the
"Bare element tuples in conditional tails" rule in `.claude/rules/erlang.md`.

On re-render the diff then fine-grains: `make_ops/4` diffs a
same-statics nested template's inner dynamics (each globally `Az`-addressed and
marker-anchored) and patches only the changed inner slot(s) -- the same
per-inner-dynamic diff the `view_id` child-view path uses (`diff_child_dynamics/2`),
minus the `[VId, ChildOps]` wrapper, since a plain nested template is inline in the
parent view. It recurses through nested-nested templates to the deepest slot; an
inner attribute change is a precise `?OP_SET_ATTR`. A wholesale `?OP_TEXT` re-render
is the fallback only when the statics differ (a different branch / structure change).

**`?OP_TEXT` escaping -- text vs HTML.** SSR escapes a dynamic value
(`render_dyn/1` -> `escape_value/1`); the live diff sends it **raw** (`make_op/3` ->
`to_bin/1`), with escaping completed at the *client's* output boundary -- the wire
carries text as a bare string and HTML as an object, so the client knows which to do.
A scalar `?get` value is a bare binary; the JS client renders it with a **text node**,
so a `<` is literal text (never parsed -- can't inject, matches SSR). An HTML fragment
(a nested-template / plain-list-`?each` zip-map, whose inner values `zip_or_fp` already
escaped) is a map; the client `innerHTML`s it. The escape opt-out `?raw` is the one
scalar that must `innerHTML`, so `make_op/3` tags it `#{~"raw" => Html}` (a map, not a
bare string) -- the only thing distinguishing trusted markup from a `?get` value that
merely contains `<`. The worker sets `op[3] = true` for the HTML cases. Attribute values
are escaped by the client's `setAttribute`. (`?raw` is an HTML-target feature; the
`?native`/`?terminal` backends don't HTML-escape, so `?raw` there is unsupported -- a
target-aware tag would be the follow-up if it ever needs to behave like a plain value.)

**Usage convention:** outer-scope `?get` reads belong in the props
expression of `?stateful`/`?stateless` -- the parse transform places them
in the outer dynamic's closure, where they track correctly. Eager `?get`
calls inside a `?stateless` callback body, or inside a stateful's
`mount/1` / `handle_update/3`, are discarded by the wrap; this is
intentional -- those reads belong to the inner scope. Idiomatic callbacks
use a named fun reference (`?stateless(fun bar/1, Props)`), which cannot
close over outer `Bindings` and therefore avoids the footgun entirely.

## Binding-read inlining

Dep tracking requires a `?get` to run *inside* its slot's `'$arizona_deps'`
bracket. A read hoisted into the render function body --

```erlang
render(Bindings) ->
    Name = arizona_template:get(name, Bindings),
    ?html({p, [], [Name]}).
```

-- would otherwise compile the slot to `fun() -> Name end`: a captured value
with no `?get`, so it records empty deps and the slot freezes after the first
render. The parse transform (`arizona_parse_transform`) closes the gap by
rewriting each interpolated variable back into its defining expression, so the
`?get` re-executes inside the per-slot bracket -- exactly as if it had been
written inline. This is purely compile-time; the runtime/diff path is unchanged
and idiomatic templates (reads written inside `?html`) are unaffected.

- **`collect_inline/1`** builds a per-clause map of top-level `Var = RHS`
  matches whose RHS transitively reaches a `get`/`get_lazy`/`track`/`with` call
  (`with` tracks too, so a hoisted `Sub = with(Keys, Bindings)` must inline back
  into the slot bracket or its tracking runs outside any bracket and freezes).
  Rebound vars are dropped; a derivation with no read (e.g. `Id = make_uuid()`)
  is left captured, so a pure side effect is never re-run per slot.
- **`inline_vars/2`** substitutes those variables wherever they appear in a
  `?html`/`?each` argument -- including inside a `case` scrutinee or an opaque
  call -- recursively and scope-aware (fun parameters and comprehension
  generators shadow; patterns and guards are never substituted, so a
  substitution can never form an illegal pattern or a guard `?get`).
- **`normalize_tail_binds/1`** lifts a statement-form `case` that binds one
  variable as the whole body of every branch
  (`case ?get(m) of a -> X = ?get(p); _ -> X = ?get(q) end`) into value form
  (`X = case ?get(m) of a -> ?get(p); _ -> ?get(q) end`) so the same machinery
  inlines it.
- A now-template-only match is underscore-prefixed so `warnings_as_errors`
  (unused variable) stays satisfied.

**Reads that do not inline** (left captured, slot stays static): a binding
destructured in the function head (`render(#{foo := Foo})`) or through any
non-bare-var pattern (`{ok, V} = ?get(...)`) -- a pattern match is an untracked
read; read the whole value with `?get(foo)` and destructure with plain Erlang.
Also: a variable bound inside a `case` branch whose clause head itself binds a
variable, and a rebound variable. Use `?get` for the top-level binding and plain
`maps:get/2` for sub-structures (only the `?get` records a dep, which is the
correct grain). In these cases the read is left captured and the slot keeps its
SSR value.

**A tracked read used in a guard is auto-tracked, not frozen:** a binding read in a
**guard** (`case Status of active when Confirming -> ...`, an `if`) can't run inside the
slot's dependency bracket on its own -- a guard cannot hold the `get/2` call (Erlang
rejects that as an illegal guard expression), so `inline_vars` leaves guards as bound
variables. Left alone the read would run once in the function body, outside the bracket,
and the slot would silently freeze on that binding. Instead, the guard-bearing `iv` node
(`{'case',...}` / `{'if',...}` / `{'receive',...}` / `{'try',...}` / `{'maybe',...}`, and
a nested `fun`) wraps itself in a block that first reads each tracked binding its clause
guards reference -- `begin _ = arizona_template:get(K, Bindings), ..., <expr> end` -- for
the `track/1` side effect, recording those bindings as slot dependencies
(`wrap_guard_touches/4`, collecting via `guard_tracked_vars/2` with per-clause shadow
handling). The guard keeps using the captured value, which each diff cycle rebuilds from
current bindings, so a change to a guard binding re-renders the slot. Reusing the inline
expansion as the touch (`iv({var,L,V}, Inline)`) handles `get`/`get_lazy`/`with` and
transitively-derived vars uniformly. A non-tracked local or a pattern-bound variable in a
guard needs no touch (no binding dependency). The one fun NOT wrapped is a top-level
`?each` callback (handled in `inline_vars/2`): `compile_each` needs a single-clause fun,
and its guards are over the item param, not outer tracked vars.

## Tracked accessors, the sub-map diagnostic, and `with/2`

`get/2,3`, `get_lazy/3`, and `with/2` (and their `az:` aliases) are the
dependency-tracking accessors: each calls `track/1` regardless of which map it
reads. That makes reading a *nested* map a footgun -- `Foo = ?get(foo), ?get(bar,
Foo)` records `bar` (a key of `Foo`) as a top-level slot dep, which both
over-tracks and frantically misfires. The parse transform rejects it at compile
time (`tracked_get_on_non_bindings_map`): a tracked read whose map argument is a
local that is not the bindings parameter -- nor an alias of it (`B = Bindings`),
nor a `with/2` projection of it -- is an error. Read sub-structures with plain
`maps:get/2`. Non-variable map arguments (a literal map, a `maps:get` result) are
never flagged.

**Nested-template / passed-bindings freeze.** A child template embedded via a raw
function call (or an explicit nested `?html(...)` returning a template) renders at
SSR but never updates: the outer slot is `fun() -> child(Bindings) end`, and
building `child` fires no `?get` at the outer level (the reads live in `child`'s own
closures, isolated by `with_saved_deps`), so the outer slot captures empty deps and
the diff engine skips it forever. Idiomatic composition is `?stateful`/`?stateless`,
whose props expressions read on the parent slot; an inline nested *element* is also
fine (it flattens into the parent template). When you must hand a bindings subset to
a sub-context, `arizona_template:with([keys], Bindings)` is the explicit fix: it
`track/1`s each key (so the outer slot re-renders when any listed key changes) and
returns `maps:with(Keys, Bindings)` (so the sub-context cannot silently depend on an
untracked key -- an omitted key raises `missing_binding` rather than freezing). No
`with_all`: tracking every key makes the slot depend on everything, defeating
fine-grained diffing.

**Eager `get/3` defaults over-track.** `?get(a, ?get(b))` records `b` even when `a`
is present, because Erlang evaluates the default argument eagerly and `?get(b)`
tracks unconditionally. `?get_lazy(a, fun() -> ?get(b) end)` tracks the fallback key
only when the default is actually taken.

**Stream items re-evaluate empty deps on purpose.** The main per-dynamic diff path
skips a slot whose deps are empty (the documented frozen residue above). The `?each`
item path does the opposite: its reuse predicate in `arizona_eval` (`can_reuse/2`) is
`map_size(OldDeps) > 0 andalso not deps_changed`, so an empty-deps item is always
re-evaluated. This is a
deliberate safety net for item callbacks that head-destructure fields
(`fun(#{text := T}, _) -> ...`): those reads are untracked, so the item would have
empty deps and freeze, except the item path re-evaluates it. The asymmetry is the
boundary: head-destructured *render* reads freeze in top-level slots but stay current
inside `?each` items.

**`?each` item bodies must be elements.** Because each item compiles into a per-item
template (`#{s, d, f}`), the callback body must be an element (`{Tag, Attrs, Children}`),
a list of elements, or a static/mixed fragment. `classify_body/1` in
`arizona_parse_transform` buckets the body; the only non-template bucket, `text_dynamic`
(a bare value, a runtime binary, a `?stateful`/`?stateless` descriptor, or a `case`/`if`),
compiles to one opaque value slot. A scalar value renders at
SSR and diffs (the per-item key is `arizona_template:to_bin/1` of its value), but gets no
per-item template diffing -- pointless where a comprehension is the right tool; a template or
descriptor value goes further and crashes on the first diff, when `to_bin/1` hits the stored
template/descriptor and raises `bad_template_value`. So `compile_each` rejects a
`text_dynamic` body (`each_body_not_element` for a 1-arg/list callback,
`each_stream_body_not_element` for a 2-arg/stream-or-map callback -- the two differ only in fix
advice, since a list has a comprehension fallback and a stream/map, keyed per item, does not)
at compile time. It also rejects a bare list whose item is a
template or descriptor (`[?html(...)]`, `[?stateless(...)]`) -- the item lands in the same
fragile value slot -- while keeping a list of elements or a static/dynamic-text fragment.
Map plain values with a list comprehension / `lists:map` (no per-item diffing); put a
conditional inside an element child (`{li, [], [case ... end]}`).

The callback may be an inline fun **or a local single-clause function reference** (`fun row/1`,
or `fun row/2` for a stream/map). `compile_each` looks the function up in the module's forms
(`collect_fun_defs/1`) and inlines its clause through the same path, so the identical body rules
apply (a non-element body still raises `each_body_not_element`/`each_stream_body_not_element`).
Inlining orphans the named function, so `parse_transform/2` injects
`-compile({nowarn_unused_function, ...})` and `-ignore_xref(...)` for the consumed pairs --
the function needn't be exported or otherwise used. A **same-module** explicit ref
(`fun ?MODULE:row/1`, module literal = the current module) is rewritten to the bare local form
and resolved identically. Rejected: a **genuinely remote** reference (`each_remote_fun_ref` --
another module's, or a variable module, body isn't visible to inline; an `-import`ed function
referenced as a bare `fun row/1` falls through to `each_named_fun_undefined` since it isn't in
`FunDefs`) and a **multi-clause** function (`each_named_fun_multi_clause` -- clauses would select
different per-item structures, but there is only one shared per-item template); collapse
multiple clauses into a `case` inside the returned element.

A whole-body backend wrapper -- `?html(...)` (or `?native`/`?terminal` matching the each's
target) -- is **accepted**: `compile_each` unwraps it to the element it wraps and builds the
same per-item template as the bare element (both go through `compile_body_parts/4` +
`scope_az/4` with the same fingerprint, so the result is byte-identical). The two callback
forms reach `compile_each` as different AST: a **named** ref resolves to its untransformed
clause, so its body is a raw `arizona_template:html(...)` call, unwrapped to its argument; an
**inline** fun's `?html` was already compiled bottom-up into a template-map literal, taken
directly (re-wrapping its `d`-list into the per-item `fun`). A wrapper as a **list item**
(`[?html(...)]`) is **not** unwrapped -- only a whole-body wrapper -- so it stays rejected like
a wrapped descriptor.

A per-item **component** (`{li, [], [?stateless(...)]}`) compiles and renders but currently
**crashes on the first diff**: the list-each diff in `arizona_diff` (`diff_list_zip/4`) keys a
list item by `to_bin` of its first dynamic, which fails on a nested template/descriptor. So a
component as an `?each`
item child is not usable yet (the exception is a `?stateful` child in a **stream** `?each` --
it is its own self-diffing view process). Fixing it means keying list items by position
instead of by their first dynamic's value (server plus the client's `az-key` lookup).

## API -- effect commands (`arizona_js` / `arizona_android` / `arizona_os` / `arizona_effect`)

Client effect commands, built per platform -- `arizona_js` (web), `arizona_android` (native),
`arizona_os` (native-shell OS capabilities) -- all returning the neutral tuple
`{arizona_effect, [OpCode, ...Args]}` (type `arizona_effect:cmd()`, encoded by `arizona_effect`).
Used in two contexts:

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
- `set_attr/3`, `remove_attr/2`, `toggle_attr/2,4` -- attribute manipulation (`toggle_attr/2`
  presence toggle: add if absent, remove if present; `toggle_attr/4` flips between two values)
- `dispatch_event/2` -- dispatch CustomEvent on document
- `navigate/1,2` -- SPA navigation, replaces the root view (opts: `#{replace => true}`)
- `patch/1,2` -- in-place SPA navigation, keeps the root view and re-renders it via `handle_update/3`
- `fetch/2` -- HTTP request via the browser `fetch()`, **no page reload** (opts: `method`,
  `body`, `headers`, `credentials`, `on_error`, `keep_alive`). The only mode that can set a real `Set-Cookie`
  (HttpOnly honored) without navigating, so it suits flows that rotate a session cookie while
  keeping form fields and showing inline validation. Hits a controller route (e.g. `{post, ...}`)
  that returns the `{"e": [...]}` effects payload via `arizona_controller:reply_effects/1`; those
  effects are applied (against the trigger element) **whenever the body parses, even on a
  `4xx`** (so the controller can use a real status and still drive inline validation). To
  re-render the **submitting** view, return an `arizona_js:push_event` in the response -- the
  client relays it over the existing WebSocket and the view re-renders via `handle_event` (no
  pubsub); broadcast over `arizona_pubsub` only to reach **other** views. `on_error` (plus an
  `arizona:fetch-error` DOM event) runs only when there is no usable effects body -- a non-JSON
  page, an empty non-2xx, or a network failure. A redirect is an `arizona_js:navigate` effect
  (`arizona_controller:reply_redirect/1`), not an HTTP 3xx. `keep_alive` (default `false`) maps to
  the browser's `fetch(url, { keepalive: true })` so a request fired just before a navigation
  completes instead of being cancelled (browser inflight-body cap ~64KB).
- `transition/1,2` -- wrap a command (or list) so its DOM change animates in a view
  transition (opts: `#{types => [binary()]}`); see "View transitions" below
- `focus/1`, `blur/1` -- focus management
- `reset_form/1` -- reset (clear typed fields on) every form matching the selector, imperatively
  (unlike `az-form-reset`, which fires only on a successful submit/fetch); a non-form match is a
  safe no-op
- `select/1` -- select (highlight) the first matching `<input>`/`<textarea>`'s text
  (`HTMLInputElement.select`); a non-input match is a safe no-op
- `copy_to_clipboard/1` -- copy the first match's `value` (form control) or `textContent` to the clipboard
  (`navigator.clipboard.writeText`); needs a secure context + user gesture, so it is an **event
  command only** (not a handler effect); a missing/blocked clipboard is a safe no-op
- `show_modal/1`, `close_modal/1` -- open the first matching `<dialog>` as a true modal
  (`showModal`, top layer + `::backdrop` + ESC-to-close, unlike the `open` attribute) / close it
  (`close`); a non-dialog match is a safe no-op
- `scroll_to/1,2` -- scroll element into view (opts: `#{behavior => <<"smooth">>}`)
- `set_title/1` -- set document title
- `reload/0` -- reload page
- `encode/1` -- encode single cmd or list of cmds to HTML-safe JSON binary (called automatically by
  `to_bin`)

**Native-shell (OS) commands (`arizona_os`).** When the app runs inside a native shell
(Tauri/Electron/...), `arizona_os` builds OS-capability commands -- all through one generic op
`?EFFECT_OS` carrying a capability **name** plus args (the shell owns the vocabulary; new
capabilities are new names, not new op codes): `set_title/1`, `focus/0`, `minimize/0`,
`maximize/0`, `fullscreen/1`, `notify/1,2`, `capture_protection/1`, `command/1,2`. Negotiated via
the `_az_caps` connect param and read server-side with `?capability(Name)`; a safe no-op in a
plain browser. See [os.md](os.md).

**Selector targeting:** the broadcast commands (`toggle`/`show`/`hide`, the `*_class` and
`*_attr` ops, `reset_form`) act on **all** elements matching the selector;
`focus`/`blur`/`scroll_to`/`select`/`copy_to_clipboard`/`show_modal`/`close_modal`
act on the **first** match.

**Payload auto-collection** (`push_event`): When `push_event` fires on an element, the client
auto-collects a base payload from the element and event context, then merges explicit payload on
top:

- **Drop events** → `{data_transfer: "dragged-key", drop_index: N}` -- drag data from `dataTransfer`
  and drop position among `[az-key]` siblings
- **Forms** → `Object.fromEntries(new FormData(form, submitter))` -- all form field values keyed by
  `name`; when the form was submitted by a named submit button, that button's `name`/`value` is
  included too (the native `FormData` submitter arg), so a form with two submit buttons reports
  which one fired. A non-submit trigger (a plain `az-click` button) has no submitter, so it never
  drags the enclosing form's fields along -- gathering is a property of submitting the form (the
  **Other** case below stays `{}`)
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

## View transitions

A view transition wraps **any** DOM change in `document.startViewTransition`; it is not tied to
navigation. The API is mostly CSS -- the framework only *starts* the transition. Requested
per-trigger (no global switch) by `arizona_js:transition(Cmd[, Opts])` (wraps the command whose
DOM change should animate, like `on_key/2`; `Cmd` is one command or a list) or the `az_transition`
attribute on any triggering element (link or `az_click`/...; bare = cross-fade,
`{az_transition, ~"slide back"}` = space-separated `types`).

The client picks sync vs async from the wrapped command:

- **Sync effect** (`toggle`/`add_class`/...): wrapped in place immediately.
- **`navigate`**: the page swap is a future `OP_REPLACE` (op 8); the intent is held in
  `_pendingTransition` (`kind: 'replace'`) and the worker message handler wraps that batch (a stray
  text/attr tick is ignored).
- **`push_event`**: the resulting diff is a future message; `_pendingTransition` (`kind: 'any'`)
  wraps the first response batch and is then consumed either way (a no-diff event can't dangle onto
  a later one). A concurrent server push (e.g. a timer) could race; navigation and sync effects are
  race-free. (Same-message server diffs still can't be wrapped -- the client applies ops before
  effects -- so a handler-returned `transition` pairs with `navigate`/`push_event`.)

Wrapping a mix of sync and async commands animates the async result; sync siblings apply
immediately. The wrap is applied at the **worker message handler**, so a message's ops and effects
animate together, in order. By default the whole root cross-fades; a `view-transition-name` on an
element scopes/morphs just it (and must be unique among rendered elements or the browser skips it).

Guards: no-op (instant swap) when `startViewTransition` is absent or
`prefers-reduced-motion`; `types` use the object-form call only when `:active-view-transition-type()`
is supported. Back/forward is symmetric: a transitioned nav stamps `_azTransition` onto both history
entries and popstate replays it. Cross-document (real `<a href>`) navigations animate via the user's
`@view-transition { navigation: auto; }` CSS with no framework code. All styling
(`view-transition-name`, `::view-transition-*`, `:active-view-transition-type`) is user CSS. See
`.claude/rules/js.md` for the client surface.

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

Compile-time constraints: `Key` must be a literal binary or atom (an atom normalizes to a
binary); a key can't bind both content and an attribute on one element; not allowed under
`az-nodiff` or in `?native` templates.
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

| Builder                              | Scope                                     |
| ------------------------------------ | ----------------------------------------- |
| `arizona_js:set(Key, Value)`         | closest view of the trigger (markup-only) |
| `arizona_js:set(ViewId, Key, Value)` | a named view                              |
| `arizona_js:set_all(Key, Value)`     | every view (document-wide)                |

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
- **Inside `?each`, items share the slot key.** `?local` keys are compile-time literals (you
  can't build `?local(<<"open_", Id/binary>>, ...)` -- that errors `local_key_not_literal`), so
  every item rendered from the same template carries the **same** key and `set`/`set_all`
  updates all of them at once. `?local` therefore can't hold per-item independent client state
  in a list/stream -- use server state for that. (An item reorder/remove is also a wholesale
  re-render that resets that item's slot.)
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

- `{live, Path, Handler, Opts}` -- live route, GET/HEAD-only (`Opts` may contain
  `layouts => [{Mod, Fun}]` and `bindings => map()`)
- `{ws, Path, Opts}` -- WebSocket endpoint
- `{asset, Path, {priv_dir, App, SubDir}}` -- static asset from priv (served via zero-copy
  sendfile by roadrunner's built-in `roadrunner_static`)
- `{asset, Path, {dir, Dir}}` -- static asset from absolute directory
- `{Verb, Path, Handler, Opts}` (`Verb` = `get`/`post`/`put`/`patch`/`delete`/`head`/`options`) --
  single-verb controller; `{match, Spec, Path, Handler, Opts}` covers multi/custom/any-method
  (`Spec` = a verb, a list of verbs, a custom uppercase binary like `~"PROPFIND"`, or `'*'`). Behind
  the Arizona middleware pipeline (CSRF `check_origin` on by default); `Opts` is `controller_opts()`
  carrying `state`/`action`/`middlewares`/`check_origin`. Dispatches through
  `arizona_roadrunner_controller`, which calls `Handler:Action/1` (the `action` option, default
  `handle`). A wrong-verb request gets `405` + `Allow` from `roadrunner_router:match/3` before any
  middleware, and two routes may share a path with disjoint verbs for REST-style dispatch. (Declare
  mutations non-GET: a state-changing GET is CSRF-able regardless -- a cross-site GET nav carries a
  `SameSite=Lax` cookie but no `Origin`.)
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
  `broadcast/1`. Relevant file events are `created`, `modified`, `deleted`, and `renamed`
  (MOVED_TO) -- the last covers atomic-rename saves (vim, `sed -i`, agent file-writers); directory
  renames (`isdir`) and files moved out of the dir (`removed`) are ignored
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

- `start_link/4` -- start with handler module, initial bindings, transport PID, and `on_mount`
  hook list. The transport PID receives `{arizona_push, ...}` updates; pass `undefined` for a
  standalone (non-pushing) process. The live process is transport-agnostic and takes no request
- `mount/1` -- calls `Handler:mount(Bindings)`. Extracts `ViewId = maps:get(id, Bindings)`, calls
  `Handler:render(B1)` via `arizona_stateful:call_render/2`, builds a snapshot via
  `arizona_render:render/2`. Returns `{ok, ViewId}` (no HTML -- SSR is handled separately by
  `arizona_roadrunner_http`)
- `handle_event/4` -- unified event dispatch: `handle_event(Pid, ViewId, Event, Payload)`. Checks
  views map -- if `ViewId` is a known child, dispatches to child handler; otherwise dispatches to
  root handler. Returns `{ok, Ops, Effects}`
- `handle_info/2` -- gen_server callback for Erlang messages (`Pid ! Msg`, `erlang:send_after`,
  etc.). If handler exports `handle_info/2`, calls it, diffs, and pushes
  `{arizona_push, Ops, Effects}` to `transport_pid`. Pre-mount messages and handlers without
  `handle_info/2` are silently dropped. Empty ops+effects are not pushed
- `navigate/3,4` -- `navigate(Pid, NewHandler, InitBindings [, OnMount])`. Mounts new
  handler (applying any `OnMount` hooks), resets gen_server state
  (handler, snapshot, views), preserves `transport_pid` and `sent_fps`, returns
  `{ok, NewViewId, PageContent}`. The previous root handler's final bindings are carried
  forward as the floor for the new handler's mount input -- `InitBindings` (route static
  config + middleware enrichments) overrides on key overlap. Keys the new handler omits
  from its mount return value are dropped on the next navigate. Stateful children's state
  (in `views`) is wiped: a child that was alive on the old page is gone unless the new
  page re-embeds it
- `patch/2` -- `patch(Pid, Params)`. In-place navigation: keeps the current root mounted, runs its
  `handle_update/3` with `Params`, and re-renders through the diff (no unmount, no remount, no timer
  cancel, no `OP_REPLACE`). `handler`, view id, and child views all survive. Returns `{ok, Ops,
  Effects}` -- the diff ops plus the reaction's effects (`handle_update`'s, folded with any child's).
  Mirrors `handle_root_event`, but the reaction is `handle_update` instead of `handle_event`
- `apply_on_mount/2` -- folds the `on_mount` hook chain: `apply_on_mount(OnMount, Bindings)`.
  Each hook is `fun((Bindings) -> Bindings)` or `{Module, Function}`

Internal state: `#state{handler, bindings, snapshot, views, on_mount, transport_pid,
sent_fps}` where `views :: #{ViewId => #{handler, bindings, snapshot}}` and `sent_fps` tracks
fingerprints already shipped to the client for deduplication.

`compute_changed/2` builds the Changed map by comparing old and new bindings key-by-key.

`push/3` sends `{arizona_push, Ops, Effects}` to the transport PID. No-ops when PID is `undefined`
or ops and effects are both empty.

## API -- `arizona_socket.erl`

Framework-agnostic WebSocket protocol state machine. The transport handler
(`arizona_roadrunner_ws`) feeds frames in and ships return tuples out, so the wire protocol
lives here independent of the server.

- `init/4` -- `init(Handler, Bindings, Req, Opts)`. Traps exits, starts
  `arizona_live:start_link/4` (passing `self()` as transport PID and any `on_mount`
  hooks), mounts. On reconnect (`#{reconnect => true}`), also renders and returns `OP_REPLACE`
  frame. Opts: `reconnect` (boolean), `on_mount` (list of `t:arizona_live:on_mount_hook/0`).
  The route adapter for SPA navigate is recovered from `Req` via `arizona_req:adapter/1`
- `handle_in/2` -- decode incoming text frame: ping/pong, `["cached_fps", FpList]`,
  `["navigate", #{~"path" := Path, ~"qs" := Qs}]` (replace), `["patch", #{~"path", ~"qs"}]`
  (in-place, same-handler -> `patch/2`, else falls back to navigate), `[target, event, payload]`
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

## SPA navigate / patch route resolution

There are two SPA navigation modes, picked per-link by the client:

- **`navigate`** (`az-navigate` / `arizona_js:navigate`) -- *replaces* the root view: the live
  process unmounts the old root, mounts the new handler, and ships one `OP_REPLACE` of the whole
  view element.
- **`patch`** (`az-patch` / `arizona_js:patch`) -- *keeps* the root view: the live process delivers
  the new route's params to the current root's `handle_update/3` and re-renders through the diff,
  shipping only the changed slots. The process, view id, child views, and DOM (chrome, scroll, open
  menus) all survive. `arizona_socket` decides per frame: it tracks the current root `handler` on
  the `#socket{}`, and a patch whose path resolves to the **same** handler is applied in place
  (`arizona_live:patch/2`); a patch to a **different** handler can't keep the view, so it degrades
  to a full navigate/replace. Persistent live chrome is therefore an ordinary app pattern (a view
  with a swappable `?stateful(?get(page), ...)` child), not a framework feature -- demo fixture
  `arizona_patch_demo` (`/patch-demo/:section`).

**A patch runs no mount step.** `mount/1` and `on_mount` run whenever the view *(re)mounts* --
initial GET, reconnect, `navigate` -- and **not** on a patch, which by design keeps the view alive.
This is deliberate: `on_mount` is a mount input (its output feeds `mount/1`), so re-running it on a
patch -- where there is no `mount/1` -- would dump a mount-time transform straight onto the live
bindings and clobber the state the patch preserves. Per-arrival request-shaped derivation (session,
path params) belongs in a **middleware**, which *does* run on a patch (`do_patch`) and whose output
arrives as `Params`; handler-specific per-navigation logic goes in `handle_update/3`. So: mount-time
setup -> `mount/1`/`on_mount`; every-arrival setup -> middleware; per-navigation reaction ->
`handle_update/3`.

Both modes resolve the target route the same way. `arizona_socket` invokes the optional
`resolve_route/3` callback declared on the `arizona_req` behaviour:

```erlang
-callback resolve_route(Path :: binary(), Qs :: binary(), Raw :: term()) ->
    {ok, module(), arizona_live:route_opts(), arizona_req:request()} | error.

-optional_callbacks([resolve_route/3]).
```

On a live route the callback returns `{ok, Handler, RouteOpts, NavReq}` -- a handler module, the
route's static options (`t:arizona_live:route_opts/0` -- `bindings`, `on_mount`, `layouts`,
`middlewares`), and a navigate-scoped `arizona_req` carrying the new URL. Cookies/headers on the
returned Request are inherited from the original upgrade Req; `path`, `bindings` (path bindings
from the router), and `params` reflect the new path/qs. In particular `path` is the **resolved
route path** (the `_az_path` value), not the `/ws` transport path the WebSocket rode in on, so a
middleware reading `arizona_req:path/1` on a navigate/upgrade sees the same logical route as a
plain HTTP GET.

The `Path` is client-supplied (the `_az_path` upgrade param, or a navigate/patch frame), so the
callback returns `error` -- never crashes -- when it does not resolve to a live route: no matching
route, a non-live route (controller/asset/ws), or a method mismatch. A WS upgrade to such a path
is rejected with `404`; an in-session navigate/patch degrades to a full-page navigation
(`arizona_js:navigate(Path, #{full => true})`) rather than tearing down the live session.

**Shipped implementation:** `arizona_roadrunner_req` exports the optional `resolve_route/3` and
runs `roadrunner_router:match/3` against the compiled dispatch stored by
`arizona_roadrunner_router`.

## API -- `arizona_req.erl`

Opaque request abstraction backed by the transport adapter. Constructed via
`new/3(Adapter, Raw, #{method := ..., path := ...})`; other fields are lazy-loaded via the
adapter's behaviour callbacks on first access and cached in the returned request.

**Accessors** (eager):

- `method/1` -- HTTP method (`~"GET"`, `~"POST"`, ...)
- `path/1` -- the logical route path, no query string. On a WebSocket SPA-navigate or upgrade
  this is the resolved route path (from `_az_path`), **not** the `/ws` transport path -- so it
  matches a plain HTTP GET to the same route on both transports
- `raw/1` -- the native transport value the adapter wraps

**Accessors** (lazy, return `{Value, Req1}`):

- `bindings/1` -- route-pattern path bindings (map of atoms)
- `params/1` -- parsed query string as a proplist
- `cookies/1` -- parsed cookies as a proplist
- `headers/1` -- request headers as a map
- `body/1` -- the request body

**Other:**

- `set_raw/2` -- swap the native raw value, clear all lazy caches
- `redirect/2(Req, Location)` / `redirect/3(Req, Status, Location)` -- stash a 3xx redirect
  intent in the request. Transports pick it up on halt and translate uniformly (HTTP 3xx
  reply, or `arizona_js:navigate` effect on WS navigate)
- `halted_redirect/1` -- returns `{Status, Location}` if `redirect/2,3` was called, else
  `undefined`
- flash (one-request): `put_flash/3` (set), `flash/1` (read) -- signed messages cleared on read;
  survive a full-page HTTP redirect (signed `az_flash` cookie) and a WebSocket SPA navigate
  (`arizona_socket` in-process carry, exactly-once, no cookie)
- session (durable): `put_session/3`, `delete_session/2`, `clear_session/1`, `session/1`,
  `get_session/2,3`, `read_session/1` -- encrypted state; a read does not consume, the response
  re-emits the cookie only on a write

The behaviour expects adapters to implement `parse_bindings/1`, `parse_params/1`,
`parse_cookies/1`, `parse_headers/1`, `read_body/1` against their native request type.

## API -- `arizona_middleware.erl`

The request-to-bindings middleware pipeline. A middleware reduces a request into the plain
bindings a `mount/1` handler consumes, keeping the live process and handlers free of any
request/transport coupling.

- `apply_middlewares/3(Middlewares, Req, Bindings)` -- threads a request and bindings map through
  a list of `middleware()` left-to-right, returning `{cont, Req1, Bindings1}` or `{halt, HaltReq}`;
  stops on the first halt. Run by the HTTP and WS transports before mounting a view.
- `extract/1(Keys)` -- builds a middleware copying selected request data into bindings so a
  handler reads it with `?get(Key)`. Keys: `path_bindings`, `params`, `headers`, `cookies`,
  `body`, `method`, `user_agent`.
- `put_request/2` -- escape-hatch middleware exposing the whole request under the `request`
  binding for lazy access; prefer `extract/1` for specific data.
- `fetch_flash/2` / `fetch_session/2` -- read the flash (consuming) / session (non-consuming)
  cookie into the `flash` / `session` binding. Opt-in; both run on the GET render and the WS
  upgrade, so a live view is seeded at mount.
- `check_origin/2` -- CSRF Origin check (see `arizona_origin`); the router prepends it to
  `live`/controller routes by default.

## API -- `arizona_http.erl`

Shared HTTP render pipeline that serves an Arizona view as the initial page response. Wraps the
native request in an `arizona_req:request()`, runs any route middlewares, and calls
`arizona_render:render_view_to_iolist/2`. The transport handler translates the returned result
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
  - `not_found` -- the client-supplied `_az_path` did not resolve to a live route; the caller
    rejects the upgrade with `404` (never crashes on the attacker-controllable path)
  - `{cont, State}` -- `State` is a map carrying `handler`, `bindings`, `on_mount`, `req`,
    `reconnect` that the caller threads into `arizona_socket:init/4`

`arizona_roadrunner_ws` collapses to a few lines that call `parse_qs`, invoke
`arizona_ws:prepare/3`, and wire the result into roadrunner's callback contract.

## API -- `arizona_static.erl`

Offline static-site generation -- renders route handlers to HTML files with no server, live
process, or WebSocket (built on `arizona_render:render_view_to_iolist/2`).

- `generate/2,3(OutDir, Specs[, DefaultOpts])` -- renders each spec under `OutDir`, writes the
  HTML files, and returns `{Written, Failed}` (the paths plus `{Spec, Reason}` for any spec that
  failed -- a `mount`/`render` crash or a write error; one failure does not stop the batch). A
  spec is `{Handler, Outfile}` or `{Handler, Outfile, Opts}`; `Outfile` is joined onto `OutDir`.
  `Opts` (and the optional `DefaultOpts`, which each spec's `Opts` override) is the offline subset
  of `t:arizona_live:route_opts/0` (`bindings`/`on_mount`/`layouts`; no `middlewares`). The client
  `<script>` / `connect('/ws')` lives in the layout, so a pure-static page omits the WS connect.

## Roadrunner handlers

- `arizona_roadrunner_http.erl` -- thin HTTP handler. Delegates the pipeline to
  `arizona_http:render/3` and translates its result into roadrunner's `{Response, Req}` reply shape
- `arizona_roadrunner_ws.erl` -- thin WebSocket handler. Delegates the upgrade to
  `arizona_ws:prepare/3` and forwards frames to `arizona_socket`. Translates
  `arizona_socket:result()` to roadrunner return tuples

## Data flow

**SSR (HTTP):** `arizona_roadrunner_http:handle/1` delegates to `arizona_http:render/3`, which wraps
the native req in an `arizona_req:request()`, runs any route middlewares
(`arizona_middleware:apply_middlewares/3`), then calls
`arizona_render:render_view_to_iolist(Handler, Opts)` where Opts may contain
`layouts => [{Mod, Fun}]`, `bindings => map()`, and `on_mount => [...]`. `render_view_to_iolist/2`
mounts the page via `arizona_stateful:call_mount/2`, applies `on_mount` hooks,
renders to page HTML, then optionally injects the page HTML into mount bindings as `inner_content`
and passes the bindings to the layout's `render/1`. The layout uses `?html` with `az_nodiff` on
the root element -- a stateless HTML shell (DOCTYPE, head, body, scripts) with no markers or `az`
attributes. When layout is absent, the page is rendered directly without a wrapper. Route config
provides `handler`, `layout`, `bindings`, `on_mount`, and `middlewares`. URL data (path bindings,
query params) does NOT flat-merge into Bindings -- a route opts into
`arizona_middleware:extract/1` middlewares (or `arizona_middleware:put_request/2`) to project
what its handler needs. Middleware halts that call
`arizona_req:redirect/2,3` surface as a `{redirect, Status, Location}` result that the transport
emits as a 3xx reply.

**WebSocket mount:** `arizona_roadrunner_ws:handle/1` delegates to `arizona_ws:prepare/3`, which
reads `_az_path`/`_az_reconnect`, resolves the route, runs middlewares, and returns the state
for the transport to hand to `arizona_socket:init/4`. The socket calls
`arizona_live:start_link/4` (passing `self()` as transport PID and route `on_mount`
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
the redirect target. On `cont`, `arizona_live:navigate/4` is called. Before mounting the new
handler, the framework cancels pending `send_after` timers and calls the old root handler's
`unmount/1` callback (if exported). Propagation to children is opt-in -- the root can broadcast
via pubsub in its `unmount/1`. Then the new handler is mounted and gen_server state is reset.
Returns `{ok, NewViewId, PageHTML}`. WS handler sends `[OP_REPLACE, OldViewId, PageHTML]`.
Browser back/forward also triggers navigate via `popstate`.

**SPA patch (in-place):** Client clicks an `[az-patch]` link (or runs `arizona_js:patch`) -> JS sends
`["patch", {path, qs}]`. `arizona_socket` resolves the route + runs middlewares as for navigate, then
compares the resolved handler against the tracked root `handler`: same handler -> `arizona_live:patch/2`
(the root's `handle_update/3` runs, then a diff-in-place, returning `{ok, Ops, Effects}` -- no
unmount/remount/`OP_REPLACE`); different handler -> falls back to the navigate/replace path above. The
client tags the history entry (`_azNav`) and stamps the outgoing entry so back/forward replays the
patch, keeping live chrome across the edge. See "SPA navigate / patch route resolution".

**Stateful children:** `arizona_template:stateful(Handler, Props)` returns a descriptor map. During
`eval_val_v/2`, the engine checks if the child is already in the views map -- if so, calls
`handle_update/3` (or `maps:merge` if not exported); if not, calls `Handler:mount(Props)`. Child
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

Two handler kinds, each with its own header:

- **Live handlers** (`arizona_stateful`) include `arizona_stateful.hrl` (sets
  `-behaviour(arizona_stateful)`, parse transform, template macros). Mount is `mount/1`. The same
  behaviour serves **route-level pages** -- spawned as a live-process root, with request data
  arriving as bindings via `arizona_middleware:extract/1` middlewares on the route -- and
  **embedded components**, instantiated from a parent template via `?stateful(Handler, Props)`.
- **Pure stateless templates** include `arizona_stateless.hrl` (parse transform only, no
  behaviour). Invoked via `?stateless(Callback, Props)` or `?stateless(Mod, Fun, Props)`.

```erlang
%% Route-level page
-module(my_page).
-include("arizona_stateful.hrl").
-export([mount/1, render/1, handle_event/3]).

mount(Bindings) ->
    {maps:merge(#{id => ~"page", count => 0}, Bindings), #{}}.

render(Bindings) ->
    ?html({'div', [], [?get(count, 0)]}).

handle_event(~"inc", _Payload, Bindings) ->
    {Bindings#{count => maps:get(count, Bindings, 0) + 1}, #{}, []}.
```

**Callback signatures** (see `src/arizona_stateful.erl` for authoritative types):

```erlang
mount(Bindings) -> {NewBindings, Resets}.                                      %% Required
render(Bindings) -> #{s => [...], d => [...]}.                                 %% Required
handle_event(Event, Payload, Bindings) -> {NewBindings, Resets, Effects}.      %% Optional
handle_info(Info, Bindings) -> {NewBindings, Resets, Effects}.                 %% Optional
handle_update(Props, Bindings, Effects) -> {NewBindings, Resets, Effects}.     %% Optional (embedded child OR patched root)
handle_drain(Deadline, Bindings) -> ok | {stop, Bindings, Effects}.           %% Optional
unmount(Bindings) -> term().                                                   %% Optional
```

`Resets` is a map of binding values reapplied on top of `NewBindings` after the callback returns --
typically `#{}`, or a subset of keys you want cleared (form fields, transient flags). `Effects` is a
list of `arizona_js:cmd()` values to run client-side after the diff is applied.

Mount must produce `Bindings` containing an `id` key -- this becomes the ViewId used for event
routing and navigate targeting. The parse transform auto-injects `az-view` as a boolean attribute
on the root element of `render/1` when the module declares the `arizona_stateful` behaviour. Using
`az_view` manually outside this context raises a compile error.

**`id` attribute restriction:** The root element's `id` MUST use `{id, ?get(id)}` (or the equivalent
`arizona_template:get(id, Bindings)` / `az:get(id, Bindings)`). Static binaries and composed values
are not allowed. This is a compile-time check.

The `id` serves three roles simultaneously: DOM id (`document.getElementById`), views map key
(server-side state tracking), and wire protocol target (op scoping). All three must be the same
value. Using `?get(id)` ensures the template renders the same value the parent passed in Props and
the server uses for tracking. `id` is a restricted key -- the handler's mount callback cannot
override the value passed by the parent in Props.

**Dispatcher error tagging.** Each `arizona_stateful:call_*` wrapper catches `error:undef` and
`error:function_clause` only when the failing stack frame is the user's exact callback, and
re-raises with a structured reason carrying an `error_info` annotation: `{missing_callback, Mod,
Name, Arity}`, `{unhandled_event, Mod, Event, Bindings}`, `{unhandled_info, Mod, Info, Bindings}`,
`{unhandled_update, Mod, Props, Bindings}`, `{unhandled_unmount, Mod, Bindings}`, or
`{render_no_clause, Mod, Bindings}`. `arizona_stateful:format_error/2` turns each into a sentence
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

`handle_update/3` reacts to new props arriving from an enclosing context: a parent re-rendering an
embedded child with new `Props`, **or** a `patch` navigation delivering the new route's params to a
route root (navigation as the root's prop source). If not exported, the framework merges `Props` into
`Bindings` directly (`call_handle_update/4`). `Effects` is the accumulator of effects already queued
for this update cycle; return it extended with this callback's own effects (the default returns it
verbatim) -- so a patch reaction can set the document title or redirect, threaded without any
caller-side concatenation. See "SPA navigate / patch route resolution".

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

| Code | Name             | Args                       | Description                                            |
| ---- | ---------------- | -------------------------- | ------------------------------------------------------ |
| 0    | `OP_TEXT`        | `[target, value]`          | Replace marker content (text, nested tmpl, plain each) |
| 1    | `OP_SET_ATTR`    | `[target, attr, value]`    | Set attribute                                          |
| 2    | `OP_REM_ATTR`    | `[target, attr]`           | Remove attribute                                       |
| 3    | `OP_UPDATE`      | `[target, html]`           | innerHTML replacement (stream container full render)   |
| 4    | `OP_REMOVE_NODE` | `[target]`                 | Remove element                                         |
| 5    | `OP_INSERT`      | `[target, key, pos, html]` | Stream insert (pos=-1 -> append, otherwise index)      |
| 6    | `OP_REMOVE`      | `[target, key]`            | Stream remove                                          |
| 7    | `OP_ITEM_PATCH`  | `[target, key, innerOps]`  | Stream item patch                                      |
| 8    | `OP_REPLACE`     | `[target, html]`           | Element swap (navigate); `target` is the OLD view id   |
| 9    | `OP_MOVE`        | `[target, key, afterKey]`  | Stream move (afterKey=null -> prepend)                 |
| 10   | `OP_LIST_PATCH`  | `[target, subOps]`         | Single-root plain-list `?each` positional item patch   |

A content-slot dynamic -- a value, a nested template, *or a plain-list `?each`* -- is anchored
by `<!--az:X-->...<!--/az-->` comment markers in SSR (no wrapper element carries the slot `az`),
so its diff patch replaces only the span between the markers, leaving the slot's static siblings
and the enclosing element intact. A single-root plain-list `?each` patches in place with
`OP_LIST_PATCH`: its `subOps` address items by DOM-order position between the markers
(`[OP_ITEM_PATCH, idx, innerOps]` patches item `idx`, `[OP_REMOVE, idx]` removes it, and
`[OP_INSERT, idx, html]` inserts), so an item change never touches the container's `childList`.
The wholesale marker-aware `OP_TEXT` re-render is the fallback -- used when positional patching is
unsound (the old slot was not a list, the item is not a single root element, or the list rendered a
per-item child view). A scalar value or a nested template still patches via that `OP_TEXT`.

**Exception -- raw-text elements (`script`/`style`/`textarea`/`title`).** The browser does not
parse HTML comments inside these, so a comment marker becomes literal content and corrupts it (an
inline module script's `<!--` is even a `SyntaxError`). A dynamic content slot inside a raw-text
element is therefore emitted **markerless and render-once**: the value renders at SSR with `Az =
undefined`, and the diff engine skips any `undefined`-`Az` dynamic (`diff_dynamics/3`
and `diff_dynamics_v/5`), so no `OP_TEXT` is ever produced (there would be no marker to target).
The backend classifies the tag via the `arizona_renderer` `raw_text_kind/1` callback: `raw` (`script`/`style`)
renders the value **verbatim** (the browser decodes no character references there, so HTML-escaping
would corrupt it -- this is what makes a `?raw` JSON-LD blob or a computed inline boot-script URL
correct); `escapable` (`textarea`/`title`) HTML-escapes a scalar (references *are* decoded there)
but is still markerless. A dynamic *attribute* on a raw-text element stays fully diffable -- only
the content slot is markerless. Limitation: the slot will not update after the initial render, and
`?local` is unsupported inside a raw-text element (no marker to address); a live `?get` there
silently freezes at its first value. `OP_UPDATE` (innerHTML on
the resolved element) is reserved for the **stream** `?each` container full render, where the
container is the addressable element and items carry `az-key` for incremental ops. Emitting
`OP_UPDATE` for a marker-anchored slot is a bug: the client's `resolveEl` finds no element for
the slot `az`, falls back to the enclosing element, and innerHTML clobbers its siblings. See
`arizona_diff`'s `make_op/3` (the `?EACH` list clause vs. the `order`-keyed stream clause) and
`full_update/5`.

The **stream -> list** type switch is consistent with this: a binding that was an
`arizona_stream` and becomes a plain list routes through `diff_list/4 -> full_update/5 -> OP_TEXT`,
which replaces only the surviving marker span -- so the list renders as real HTML and the slot's
siblings survive. The stream's keyed `az-key` items are element children, and stream ops
(insert/remove/move/patch query `:scope > [az-key]`) never delete the comment markers, so the
marker is still present at switch time.

**Known limitation -- the stream `?each` container is not yet marker-anchored for incremental
ops.** Two consequences, both independent of the `OP_TEXT` content-slot fix:

- **list -> stream among static siblings:** the reverse switch stays on `OP_UPDATE`, which is
  correct only when the stream each is the addressable element (its slot `az` is the enclosing
  element, i.e. the each is the sole child of its container). When a stream each shares a content
  slot with static siblings, `OP_UPDATE` falls back to the enclosing element and clobbers them --
  the same class of bug the plain-list `OP_TEXT` change fixed for lists.
- **runtime-inserted item orphaned on stream -> list:** `insertItemEl` appends keyed children to
  the container element, which places a runtime-inserted item *after* the closing `<!--/az-->`
  marker. A subsequent stream -> list `OP_TEXT` correctly re-renders the marker span but leaves
  that orphan behind.

The fix for both is to anchor stream items between the slot markers (and make stream
insert/move marker-relative), tracked as a follow-up.

## Target scoping

Patch targets are `"viewId:relativeTarget"`. Root view `<<"page">>`: `"page:0"`, `"page:1"`. Child
view `<<"counter">>`: `"counter:0"`, `"counter:1"`. `arizona_socket` prefixes ops with the view id
via internal scoping. Child view ops use `[ChildViewId, ChildOps]` nesting and recurse into these.
Bare targets (no `:`) resolve to the view root via `document.getElementById(target)` -- used by
`OP_REPLACE` during navigate.

### Repeated stateless components -- per-instance az namespacing

A stateless component's inner `az` ids derive only from its own template fingerprint (`<Fp>-<n>`,
baked at compile time by `scope_az/4`), and it has no `view_id` boundary of its own. So the *same*
render function rendered more than once in a view (`?stateless(card, #{...})` twice) would emit
byte-identical `az` targets; on the client every diff op resolves with `querySelector` (first
match) and lands on the first DOM instance. To keep each instance an independent diff target,
`arizona_template:scope_slot/2` namespaces a slot value's snapshot -- its fingerprint, its statics'
baked `az`/marker ids, and every inner dynamic `az` (recursing through inline nested templates) --
by the enclosing slot `az` at runtime, where the slot is in scope. A stateless child evaluates to
a plain snapshot map (the `#{callback, props}` clause of `arizona_eval:eval_val`/`eval_val_v` and
`arizona_render:render_ssr_val` returns it directly, with no wrapper tag), and the per-dynamic
caller (`eval_one`/`eval_one_v`/`render_ssr_one`, which knows the slot `az`) applies
`scope_slot/2`. So instance A at slot
`<<"p-0">>` becomes `p-0-<Fp>-<n>` and instance B at slot `<<"p-0:1">>` becomes `p-0-1-<Fp>-<n>`;
nested stateless children compose the full slot path. The prefix is made colon-free (`:` -> `-`)
because the client treats `:` as the content-slot-index separator (a base element `az` never
contains one) and recovers a base element by stripping at the *first* colon -- an internal colon
in the prefix would misroute multi-slot and `?each`-among-siblings ops, so no client change is
needed.

`scope_slot/2` discriminates purely by structure (it delegates to the same `scope_val/2` used when
recursing inside a scoped snapshot): a **stateless child** and an **inline nested template** (a
conditional-tail branch, or an explicit nested `?html`) are byte-identical `#{s, d}` snapshot maps,
so both are namespaced by the slot `az` -- there is no tag telling them apart, and none is needed.
Uniform scoping is also strictly *more* correct than scoping only stateless children: two
structurally identical conditional branches in different slots share a fingerprint, so without the
slot prefix their inner ids would collide on the client exactly as repeated stateless children do.
A **stateful** child (its own `view_id` boundary), an `?each` container (items addressed relative
to the container, not by global `az`), and a client-owned `?local` slot are left untouched -- only
the container/element `az` in the enclosing statics is prefixed. A stateful child is skipped by its
snapshot's `view_id`: the live path sets it via `make_child_snap`, and the SSR path
(`render_ssr_val`) sets it too, so the SSR HTML (which the client reuses on connect) and the live
snapshot produce identical ids -- without it, a stateful child nested in a stateless parent would
be scoped only at SSR and its diff ops would miss the DOM.

Prefixing a statics list is a `binary:replace` per element -- pure and deterministic in
`(prefix, statics)` -- so `scope_snapshot` memoizes it per `(prefix, fingerprint)` in the process
dictionary: a re-render or diff of the same slot (and each level of a deep nest, whose inner
statics would otherwise be re-walked once per ancestor) reuses the cached scoped statics instead of
re-scoping. Only the first render of a given slot pays the walk; the fingerprint keys the statics
the same way the client's statics cache does.

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
- `arizona_roadrunner_reload` -- dev-mode SSE reload endpoint
- `arizona_roadrunner_req` -- implements the `arizona_req` behaviour (the request abstraction
  consumed by handlers), including the optional `resolve_route/3` for SPA navigate

The `arizona_req` behaviour is the boundary between the server and the engine: handlers and the
shared pipeline only ever see an `arizona_req:request()`, never roadrunner's native request type.

## Configuration -- `arizona_config`

Every `arizona` app-env value may be a literal or an environment-variable reference resolved at
startup. `arizona_config` centralizes both the read and the resolution:

- `get_env/1` / `get_env/2` -- drop-in replacements for `application:get_env(arizona, Key[,
  Default])` that pass the stored value through `resolve/1`. The scattered scalar reads
  (`arizona_origin`, `arizona_session`, `arizona_flash`, `arizona_crypto`, `arizona_sup`,
  `arizona_req`, `arizona_session_store_ets`) call these.
- `resolve/1` -- expands references in an arbitrary term. Called once at the top of
  `arizona_roadrunner_server:start/2` to resolve the whole `server` opts map, so nested `port` /
  `scheme` / `tls` / `proto_opts` references work and every direct caller (the test server, CT
  suites) benefits without change.

Reference forms:

- `{env, "VAR"}` -- **required**. Returns `$VAR` as a binary; raises `env_not_set` (with a
  `format_error/2` message) when unset. For a secret with no sensible default.
- `{env, "VAR", Default}` -- **optional**. Coerces `$VAR` to the type of `Default`, or returns
  `Default` when unset.

Coercion is by the default's Erlang type (env vars are always strings): integer, float, boolean
(`"true"`/`"false"`, case-insensitive), binary, atom (`list_to_existing_atom/1`), or list (a
comma-split into trimmed binaries -- the `csrf_origins` / `secret_key_previous` shape). The
required form yields a binary.

`resolve/1` recurses into **maps, lists, and 2-tuples only**:

```erlang
resolve({env, Var}) -> ...;                 %% required
resolve({env, Var, Default}) -> ...;        %% optional, coerced
resolve(Map) when is_map(Map) -> #{K => resolve(V) || K := V <- Map};
resolve(List) when is_list(List) -> [resolve(E) || E <- List];
resolve({A, B}) -> {resolve(A), resolve(B)};
resolve(Other) -> Other.
```

Limiting tuple recursion to 2-tuples (proplist pairs like `{port, ...}`, `{certfile, ...}`) is
the safety property: route tuples (`{live, Path, Handler, Opts}` and friends -- all >=3-tuples)
fall to the `Other` clause and pass through untouched, so an operator-supplied literal `{env, _,
_}` sitting inside a route's `bindings`/`state` is never rewritten. Resolution is idempotent, so
resolving in `start/2` is safe even if a caller already resolved.

## MCP server

An Arizona app exposes a Model Context Protocol (MCP) server to AI agents (Claude Code, Cursor,
Claude Desktop) by implementing the `arizona_mcp` behaviour and mounting it on a route:

```erlang
{mcp, ~"/mcp", my_mcp, #{sessions => true, origins => [~"http://localhost:3000"]}}
```

MCP rides the same roadrunner transport as everything else -- it is pure application-layer code,
with no MCP-aware feature in roadrunner. The boundary: roadrunner supplies the mechanism (routes,
immutable request/response values, the `{loop, ...}` streaming shape, `roadrunner_sse` framing, the
`{roadrunner_disconnect, _}` signal); Arizona supplies the JSON-RPC 2.0 envelope, the MCP lifecycle,
the tool/resource/prompt registry, the session lifecycle, and the Origin/auth policy.

### Transport

MCP's Streamable HTTP maps onto roadrunner response shapes with no new transport feature:

- **POST** -- the client sends one JSON-RPC message. The handler decodes and dispatches; it returns
  a buffered `application/json` reply, or -- for a `tools/call` carrying a `_meta.progressToken` from
  a client that accepts `text/event-stream` -- answers as an SSE stream (the tool's
  `notifications/progress`, then the result, then close).
- **GET** (session mode) -- opens the long-lived server-to-client SSE channel as
  `{loop, 200, [event-stream headers], State}`; `handle_info/3` forwards server-initiated
  notifications and stops the loop on `{roadrunner_disconnect, _}`.
- **DELETE** (session mode) -- tears the session down.

The `{loop, ...}` SSE shape is wired on h1, h2, and h3, so the channel works on all three (h2/h3
multiplex many sessions over one connection with no head-of-line blocking). Every `Push` flushes one
frame to the wire with no coalescing, so a `notifications/*` message reaches the agent at emit time.

### Progress streaming

When a token-bearing `tools/call` streams (the client accepted SSE), the tool runs in a worker
process so the connection loop is free to push its progress as it arrives. In **stateless** mode the
loop spawns the worker; in **session** mode the session spawns it (so the session stays responsive to
a cancel) and the worker reports its threaded-back state on completion. Either way the tool emits via
the `Ctx` handed to `handle_tool/4` -- `arizona_mcp:progress/2,3` relays each `notifications/progress`
to the POST's loop, which pushes it; the final result is the last frame, then the loop
stops. The context is inert (a no-op) for a non-streaming call, so a tool can always call
`progress/2,3`.

**A JSON-RPC object is encoded by whoever produces it, never by the process that writes it to the
wire.** `json:encode/1` rejects a binary that is not valid UTF-8, and a callback module can return
one; encoding at the producer keeps that failure inside the producer's crash guard, which answers
`-32603` like any other callback crash. So the tool worker frames its own progress notifications and
its final result (`arizona_mcp_handler:message_frame/1`), and the connection loop only pushes bytes;
the buffered reply paths encode inside the same guard. The one object that cannot be answered is a
server-initiated **notification** -- it carries no id -- so an unencodable one is logged and dropped
rather than taken out on the session process, which would kill every request in flight on it.

### Dispatch, cancellation, and timeouts

Requests run in a worker process (the only exceptions are `resources/subscribe`/`unsubscribe`, which
own a session-keyed pubsub subscription and run no app code), so the session process never blocks on
app code -- it stays responsive to cancels, idle-reap, and server pushes even while a tool runs.
**Buffered** (non-streaming) requests are served through a one-at-a-time queue (so their state
threading stays serialized) and replied to asynchronously; a request that outlives
`request_timeout_ms` (default 60s) frees the client with a -32603 while the session keeps running it.
A **streaming** `tools/call` runs concurrently in its own worker, reading a state snapshot and
threading nothing back -- so the serialized buffered queue is the only path that mutates session
state, and nothing races on it.

Any in-flight request is cancellable: a `notifications/cancelled {requestId}` (routed to the
session) or a client disconnect kills the worker and frees the caller -- a buffered caller with a
-32603, a streaming POST loop told to stop. The session monitors each worker, so an unexpected
worker death frees the caller with a -32603 rather than hanging it.

An in-flight streaming request holds its session alive the way an attached channel does -- the idle
TTL is suspended while a stream runs, so a long stream is never reaped mid-run, and re-arms once it
finishes or is cancelled. A buffered worker deliberately does *not* hold the session: the idle TTL
stays the safety net that reaps, and so kills, a stuck buffered tool. Tearing the session down
(DELETE, idle TTL, or shutdown) kills any still-running worker, so a blocking tool cannot orphan and
run forever.

### Stateless vs session mode

Without `sessions => true` every request is self-contained: the handler runs `init/1` per request,
dispatches, and discards the state. With `sessions => true`, `initialize` mints an `Mcp-Session-Id`
and starts an `arizona_mcp_session` process (owned by `arizona_mcp_sup`, not linked to the
connection, so it outlives any single request); later requests carrying that id route to the
process via `arizona_mcp_session_registry`, which runs them one at a time (each in a worker, so a
slow tool never blocks the session) against its held state. A stateful callback's returned state
threads back into the session, so the session is genuinely stateful across requests; a crash answers
`-32603` and leaves the prior state intact. An idle session is reaped after `session_ttl_ms` (default
5 minutes).

### Resumability and server push

The session assigns a monotonic `id` to each emitted SSE event and buffers the last
`session_buffer_max` of them; a client that reconnects with `Last-Event-ID` gets the newer events
replayed. Two ways to push a `notifications/*` message:

- `broadcast/3` -- publishes to an `arizona_pubsub` channel; every session subscribed to it (via its
  optional `channels/1` callback) forwards the notification. Decoupled -- the caller holds no
  session reference.
- `notify/3` -- pushes to one session by its `Mcp-Session-Id`.

Both are no-ops for a session with no attached SSE channel.

### Resource subscriptions and templates

`resources/subscribe` joins the session to a per-uri `arizona_pubsub` channel
(`{mcp_resource, Uri}`); `arizona_mcp:resource_updated/1` broadcasts to it, so every subscribed
session forwards `notifications/resources/updated`. `resources/unsubscribe` leaves the channel, and
`pg` auto-removes a session on exit, so there is no per-session subscription bookkeeping. It is the
same fan-out path as `broadcast/3` + `channels/1`, just keyed per uri and joined at runtime. The
optional `resource_templates/1` callback backs `resources/templates/list` (URI templates like
`mem://user/{id}`), paginated through the same `list_reply`.

### Logging and completion

`logging/setLevel` stores a minimum severity in the session map (`log_min_severity`, default `info`),
threaded back like any other state; `arizona_mcp:log/3` resolves the level severity caller-side and
casts to the session, which emits a `notifications/message` only when the message is at or above the
stored minimum. `completion/complete` decodes the ref (`{prompt, Name}` | `{resource, Uri}`) and the
partial argument, calls the optional `complete/3` callback, and caps the returned values at 100,
setting `hasMore`. Both are capability-gated (`logging` / `completions`).

### Pagination

The `*/list` methods paginate with opaque cursors, framework-side: the `tools/1` / `resources/1` /
`prompts/1` callbacks keep returning their full list, and the transport slices it by an offset cursor
(base64 of the offset), emitting `nextCursor` when more remain. The page size is the route's
`page_size` opt (default 50), so a list that fits returns in one page with no cursor. A malformed
cursor is a `-32602`; an offset past the end returns an empty final page. The callbacks never see a
cursor. Because the cursor is an offset, it assumes a stable list across pages -- a list mutated
mid-pagination can skip or repeat an item (acceptable for the near-static MCP lists, and a
`list_changed` notification has the client re-list anyway).

### Resource limits and keep-alive

Three opt-in knobs bound a session-mode route. `max_sessions` caps the live sessions **per route**
(the registry tags each session with the request path it opened on and counts by it, so two routes
sharing a handler module still cap independently); an `initialize` past the cap is answered with a
`503` plus a JSON-RPC error body. The count is read just before starting, so a burst of concurrent
initializes can admit a few over the cap -- fine for a soft limit. `session_max_pending` (default
100) bounds the per-session buffered-dispatch queue; a dispatch arriving with the queue full is
rejected with a -32603 rather than enqueued. `session_keepalive_ms` (default 30s, `infinity`
disables) makes the session emit a periodic `roadrunner_sse:comment/1` over an attached channel so
idle proxies don't close the stream; the comment carries no event id, so it is never buffered for
resumption, and the timer runs only while a channel is attached.

### Capability gating and security

`init/1` returns an atom-keyed capability map; the transport serves `resources/*` and `prompts/*`
only when the matching capability was advertised, answering `method not found` otherwise. Tools are
always available. Before any dispatch a request passes the security gate: the `Origin` allowlist (a
DNS-rebinding defense) and the route's optional `auth` hook.

### Known caveat -- loop backpressure

The SSE channel pushes notifications into the loop process's mailbox. If an app pushed faster than
the client drains, an h1 loop mailbox is unbounded (h2/h3 have implicit stream-window backpressure).
A dev-tool MCP server pushes trivial volume, so this is not on the critical path; it matters only
for a deployment streaming high-volume notifications.

---
description: Erlang template format, parse transform conventions, and descriptor types
---

# Erlang Template Conventions

## Template format

Templates are plain maps: `#{s => [binary()], d => [dynamic()], f => binary()}`.

```erlang
%% dynamic() = {Az, fun(() -> term()), Loc}         -- text with location
%%           | {Az, {attr, Name, fun(() -> term())}, Loc} -- attribute with location
%%           | {Az, #{s => ..., d => ...}, Loc}      -- nested template with location
%%           | {Az, term(), Loc}                     -- static value with location
%% Az = binary() (diff target) | undefined (az-nodiff -- never used as op target)
%% Loc = {Module, Line} -- source location for error reporting
%% f = fingerprint -- base-36 phash2 of statics
%% Optional: diff => false -- skip diff after initial render (from az-nodiff directive)

%% Snapshot: #{s => [binary()], d => [{Az, Value}], deps => [#{key() => true}]}
%% Stateful descriptor: #{stateful => module(), props => map()}
%% Stateless descriptor: #{callback => fun/1, props => map()}
```

## Header files

```
include/arizona_common.hrl      -- utility macros (?get, ?html, ?each, ?stateful, ?stateless, ?inner_content, ?connected)
include/arizona_stateful.hrl    -- -behaviour(arizona_stateful); parse_transform, send/subscribe macros, arizona_common.hrl
include/arizona_stateless.hrl   -- parse_transform, includes arizona_common.hrl
```

Rule of thumb:

- Live handlers → `arizona_stateful.hrl` (`mount/1`). Route pages get request data as bindings via `arizona_middleware:extract/1` middlewares; embeddable components are instantiated via `?stateful(Handler, Props)`
- Pure template modules → `arizona_stateless.hrl`

## Mount bindings -- construct, don't merge

Every handler's `mount/1` must build a fresh map
literal rather than `maps:merge`-ing or `Bindings#{...}`-updating the input.
With `arizona_live:navigate/3` the input may carry arbitrary keys from
previously visited pages; merging passes them through and lets foreign keys
collide with handler-owned defaults (e.g. one page's `next_id` overriding
another's, dup-inserting on a stream). Pull each accepted override out
explicitly via `maps:get/3`.

```erlang
%% Bad -- foreign keys carry through:
mount(Bindings) ->
    {maps:merge(#{id => ~"page", count => 0}, Bindings), #{}}.

%% Good -- handler owns its keys, accepts a typed override:
mount(Bindings) ->
    {#{
         id => ~"page",
         count => maps:get(count, Bindings, 0)
     }, #{}}.
```

## Parse transform element forms

| Form | Example | Description |
|------|---------|-------------|
| `{Tag, Attrs, Children}` | `{'div', [], [?get(x)]}` | Standard element with children list |
| `{Tag, Attrs, Expr}` | `{'span', [], ?get(x)}` | Single expression as children (wrapped in list) |
| `{Tag, Attrs}` | `{'br', []}` | Void element shorthand (no children) |

Void elements (`br`, `img`, `input`, `hr`, `meta`, `link`, `base`, `col`, `embed`, `param`, `source`, `track`, `wbr`, `area`) self-close as `<tag />`.

## Parse transform attribute forms

| Form | Example | Output |
|------|---------|--------|
| `{name, <<"value">>}` | `{class, <<"box">>}` | `class="box"` |
| `{name, Expr}` | `{class, Theme}` | Dynamic attr |
| `name` (atom) | `disabled` | `disabled` |
| `<<"name">>` (binary) | `<<"hidden">>` | `hidden` |
| `{name, true}` | `{hidden, true}` | `hidden` |
| `{name, false}` | `{hidden, false}` | Stripped |
| `'az-nodiff'` / `<<"az-nodiff">>` | Directive | Stripped, emits `diff => false` |

## Route options

A route's static config is the single canonical type `arizona_live:route_opts/0`:

```erlang
-nominal route_opts() :: #{
    bindings => arizona_template:bindings(),
    on_mount => on_mount(),
    layouts => [arizona_render:layout()],
    middlewares => [arizona_middleware:middleware()],
    _ => term()
}.
```

Used in route declarations, `arizona_render:render_view_to_iolist/2`, `arizona_http:render/3`, and the optional `arizona_req:resolve_route/3` callback's return tuple. All keys are optional; consumers default at use-site (`maps:get(K, M, Default)`).

`layouts` is always a list, applied outermost-first: `[Root, Section]` produces `Root(Section(Page))`. Empty list = no wrap.

### CSRF Origin check

`check_origin` is a built-in middleware step (`{arizona_middleware, check_origin}`) the router **prepends by default** to `{live, ...}` and controller (verb-tag / `match`) routes -- it rejects a cross-origin request/upgrade with `403` (`arizona_origin:check/2`: same-origin or `csrf_origins` allowlist; a missing `Origin` is allowed). Off by exception: `check_origin => false` in a route's `Opts`, or the global `check_origin` app env (which logs a warning once when disabled). It covers the WS upgrade too (`arizona_ws:prepare/3` runs the route's middlewares).

**It protects state-changing POST/PUT/DELETE + the WS upgrade only -- never a state-changing GET.** A cross-site top-level GET navigation sends no `Origin` (so the check allows it) but still carries a `SameSite=Lax` cookie, so a cookie-only `GET /logout`-style mutation is CSRF-able regardless -- the inherent limit of Origin-checking, not framework-closable. **Method routing enforces "don't mutate on GET" by routing**: declare a mutating endpoint with a non-GET verb (`{post, ...}`/`{delete, ...}`) and the router `405`s a cross-site `GET` to it before the action runs; only a mutation you deliberately route as `{get, ...}` stays exposed, so never put state changes behind GET. Apps should also set `SameSite=Lax`/`Strict` on auth cookies. A signed double-submit token was evaluated and **deliberately not added** -- redundant with this Origin check, and the missing-Origin gap it would close is non-browser clients, which carry no ambient cookie authority and so are not a CSRF vector.

### Controller routes

A controller route is reached over HTTP (e.g. by `arizona_js:fetch/2`) and gated by HTTP method:

- `{Verb, Path, Handler, Opts}` where `Verb` is `get`/`post`/`put`/`patch`/`delete`/`head`/`options` -- single-verb sugar.
- `{match, Spec, Path, Handler, Opts}` -- multi/custom/any-method, where `Spec` is a verb, a list of verbs, a custom uppercase method binary (`~"PROPFIND"`), or `'*'` (any). A `get` allowlist implicitly answers `HEAD`.

A request whose path matches but whose method does not gets `405 Method Not Allowed` with an `Allow` header, decided by `roadrunner_router:match/3` **before** any middleware; two routes may share a path with disjoint verbs for REST-style dispatch (`{live, ...}` routes are themselves GET/HEAD-only). `Opts` is `controller_opts()`: `#{state => term(), action => atom(), middlewares => [...], check_origin => boolean()}`. The route dispatches through `arizona_roadrunner_controller`, which runs the middleware pipeline (CSRF default-on) then calls `Handler:Action/1` -- the `action` option, default `handle` -- with `state` restored into the request (read via `roadrunner_req:state/1`); the controller is a module exporting those action functions, and a route naming an action it does not export raises a clear `missing_action` error. Build the response with `arizona_controller:reply_effects/1,2` / `reply_redirect/1`.

## Comprehension generators

Use the strict generator (`<:-`, `K := V <:-`) only when the LHS is a pattern that
can fail to match -- a tuple, a map key/value pattern, an equality bind, etc. A
strict generator with a pure variable LHS (`X <:- Source`) adds noise without
adding any guarantee, since a bare variable always matches. In that case use the
lazy generator (`<-`).

```erlang
%% Pattern LHS -- strict makes mismatches loud:
[V || {_Az, V} <:- Snapshot]
[K || K := _ <:- Map]

%% Bare-variable LHS -- lazy:
[F || F <- Files]
[Pid || Pid <- Subscribers, Pid =/= Self]
```

Map comprehension generators are always strict (`<-` is not supported for maps).

## az-nodiff

Adding `'az-nodiff'` to an element's attribute list marks it as a compile-time directive. The parse transform strips it from HTML and emits `diff => false`. All dynamics in that compile unit get `undefined` Az (pre-scanned via `prescan_directives/1`). Children in separate `?html` calls are not affected at compile time, but safe because the parent's `diff => false` short-circuits before their dynamics are reached by the diff engine.

## Macros

| Macro | Expands to |
|-------|-----------|
| `?get(Key)` | `arizona_template:get(Key, Bindings)` |
| `?get(Key, Default)` | `arizona_template:get(Key, Bindings, Default)` |
| `?get_lazy(Key, Fun)` | `arizona_template:get_lazy(Key, Bindings, Fun)` |
| `?html(Elems)` | `arizona_template:html(Elems)` |
| `?terminal(Elems)` | `arizona_template:terminal(Elems)` -- ANSI render target; tags `line`/`col`/`row`/`text`/`span`/`br` + bare-atom style attrs (see docs/architecture.md "Terminal render target") |
| `?each(Fun, Source)` | `arizona_template:each(Fun, Source)` -- 1-arg for lists, 2-arg for streams/maps. The callback must return an element (see "?each body must return an element") |
| `?stateful(Handler, Props)` | `arizona_template:stateful(Handler, Props)` |
| `?stateless(Fun, Props)` | `arizona_template:stateless(fun Fun/1, Props)` |
| `?stateless(Mod, Fun, Props)` | `arizona_template:stateless(Mod, Fun, Props)` |
| `?local(Key, Init)` | `arizona_template:local(Key, Init)` -- client-owned slot: server renders `Init` once and never diffs it; the browser owns/updates the value via `Key` (a binary or atom literal; content -- one or many per element, mixed with static text -- or an attribute value, whole or interpolated with one local + static prefix/suffix) |
| `?raw(Value)` | `arizona_template:raw(Value)` -- escape opt-out: splices a trusted, already-safe HTML fragment verbatim into a content slot or attribute value instead of HTML-escaping it. The parse transform only recognizes the opt-out when the `raw` call is **literal at the template site**, so wrap values here, never inside a helper. Never for user-controlled data |
| `?connected` | `arizona_live:connected()` -- true inside a connected live process, false during SSR |
| `?send(Msg)` | `arizona_live:send(?get(id), Msg)` -- send to current view (stateful only) |
| `?send(ViewId, Msg)` | `arizona_live:send(ViewId, Msg)` -- send to specific view (stateful only) |
| `?send_after(Time, Msg)` | `arizona_live:send_after(?get(id), Time, Msg)` -- delayed send to current view (stateful only) |
| `?send_after(ViewId, Time, Msg)` | `arizona_live:send_after(ViewId, Time, Msg)` -- delayed send to specific view (stateful only) |

## `?each` body must return an element

`?each` compiles each item into a per-item template (`#{s, d, f}`) for fine-grained diffing
(insert/move/update). So the callback's body must be an **element** (`{Tag, Attrs, Children}`),
a list of elements, or a static/mixed fragment. A bare value, a runtime binary, a
`?stateful`/`?stateless` descriptor, or a `case`/`if` compiles to one opaque value
slot. A scalar value renders and diffs (keyed by content) but gets no per-item diffing -- a
comprehension is the right tool; a template or descriptor value goes further and **crashes on
the first diff** (`bad_template_value`, when `to_bin/1` hits the stored template/descriptor). A
template or descriptor wrapped in a bare list (`[?stateless(...)]`) is the same trap. The parse
transform rejects all of these at compile time (`each_body_not_element`). A 2-arg (stream/map)
callback is rejected the same way but with `each_stream_body_not_element`: a stream/map keys
each item for per-item diffing and has **no comprehension fallback**, so the body must be an
element (wrap the value: `fun(Item, Key) -> {li, [], [Item]} end`).

A whole-body `?html(...)` (or `?native`/`?terminal`) **is** accepted: it's unwrapped to the
element it wraps and compiled identically to returning that element bare -- so a helper that
returns `?html(...)` can be reused as an `?each` callback. Only a **whole-body** wrapper
unwraps; a wrapper as a **list item** (`[?html(...)]`) stays rejected (it lands in the same
fragile per-item value slot as a wrapped descriptor).

The callback may be an inline fun **or a local single-clause function reference** (`fun row/1`,
or `fun row/2` for a stream/map): the parse transform resolves the reference to the function's
body and inlines it exactly like an anonymous fun, so the **same** element-body rules apply (a
non-element body still raises `each_body_not_element`/`each_stream_body_not_element`). The named
function's now-orphaned definition is covered by auto-injected `nowarn_unused_function` /
`-ignore_xref`, so it needn't be exported or otherwise used. A **same-module** explicit ref
(`fun ?MODULE:row/1`) resolves to the local body just like `fun row/1`. Rejected: a
**genuinely remote** reference (`fun other_mod:row/1`, or a variable module `fun M:row/1` --
body not visible to inline, `each_remote_fun_ref`), an **imported** function used as a bare
`fun row/1` (its body lives in another module, so it isn't found -- `each_named_fun_undefined`),
and a **multi-clause** function (can't map to one shared per-item template,
`each_named_fun_multi_clause`; collapse the clauses into a `case` inside the returned element).

- Plain values: use a list comprehension or `lists:map/2` (no per-item diffing, fine for
  small or static lists).
- A conditional: put it **inside** an element as a text/value child. Only the `?each`
  **body** must be a direct element -- a conditional sitting in a content slot may itself
  return a bare element tuple (see "Bare element tuples in conditional tails" below).

```erlang
%% rejected (would crash on diff): a bare case body -- branches select different structures
?each(fun(U) -> case U of #{name := N} -> ?html({li,[],N}); _ -> ~"-" end end, ?get(users))
%% ok: the conditional is a child of a stable element
?each(fun(U) -> {li, [], [case U of #{name := N} -> N; _ -> ~"-" end]} end, ?get(users))
%% ok: a local single-clause named fun, resolved and inlined (same element-body rules)
row(U) -> {li, [], [case U of #{name := N} -> N; _ -> ~"-" end]}.
?each(fun row/1, ?get(users))
%% ok: a whole-body ?html(...) is unwrapped to its element (inline or named single-clause)
row(U) -> ?html({li, [], [case U of #{name := N} -> N; _ -> ~"-" end]}).
?each(fun row/1, ?get(users))
%% plain values: a comprehension, not ?each
{ul, [], [[<<"#", Tag/binary>> || Tag <- ?get(tags)]]}
```

## Bare element tuples in conditional tails

A control-flow expression in a **content slot** -- `case`, `if`, `begin`, `receive`,
`try`, or `maybe` -- may return a bare element tuple, an element list, or a mixed fragment
(static text/values interleaved with elements) directly from a tail position. The parse
transform compiles each tail into a nested template, exactly as a literal
`?html`/`?native`/`?terminal` there would, inheriting the enclosing render target. No
`?html` wrap is needed:

```erlang
%% both branches accepted: <<>> renders empty, the tuple becomes a nested template
{main, [], [
    case ?get(error) of
        undefined -> <<>>;
        Message -> {p, [{class, ~"login-error"}], Message}
    end
]}
```

The walked tail positions are the value-producing ones: clause bodies (`case`/`if`/`try`
`of`+`catch`/`maybe` `else`/`receive`), block last expressions, a `receive` `after` body,
and a `try` body. A `try`/`receive` timeout and a `try` `after` body are **not** tails
(their values are discarded). A branch returning a plain value (binary, integer, variable,
or a pure value list) still renders as a scalar, unchanged; nested control flow is walked
recursively. The set of forms and tail positions is defined once in `map_tail_exprs/3`,
shared with the live-render-root transform. (This mirrors `?each`, whose callback body
already accepts bare elements; the difference is that `?each` keys items for per-item
diffing while a conditional is a single slot.)

A conditional may also return a `?stateful`/`?stateless` descriptor from a branch -- the
idiomatic `case ?get(flag) of true -> ?stateful(child, #{id => ~"c"}); false -> ~"" end`.
A content slot is anchored by its `<!--az:X-->...<!--/az-->` comment markers in SSR, so
any branch value (the empty string, a binary, a nested template, or a child descriptor)
patches **in place** via `?OP_TEXT`, preserving the slot's siblings and the enclosing
element. (`arizona_diff:make_op/3` always emits `?OP_TEXT`, never `?OP_UPDATE`, for a
nested-template value -- an `?OP_UPDATE` would `innerHTML`-overwrite the enclosing element,
which is catastrophic when the slot's `az` is that element's own `az`, e.g. a conditional
child rendered directly under the view root.)

The same rule applies to a **plain-list `?each` in a content slot**: it is marker-anchored
exactly like any other dynamic-text child (no wrapper element carries the slot `az`), so its
container patch is the marker-aware `?OP_TEXT` -- `make_op/3` (the `?EACH` list clause) and
`arizona_diff:full_update/5` emit `?OP_TEXT`, never `?OP_UPDATE`. This is what lets a
plain-list `?each` sit **among static sibling content** in one slot: re-rendering the list
replaces only the each's marker span, leaving the siblings intact. An `?OP_UPDATE` here would
`innerHTML`-wipe the enclosing element's static siblings (the client's `resolveEl` finds no
element for the slot `az` and falls back to that enclosing element); a sole-child `?each` only
appeared to work with `?OP_UPDATE` by coincidence. (Stream `?each` -- the `order`-keyed clause
-- still uses `?OP_UPDATE` for its container full-render and `az-key`-addressed incremental
ops; the unkeyed plain-list marker rule does not apply to it.)

**Known limitation:** embedding a component (a `?stateless`/`?stateful` descriptor) as an
`?each` item child compiles and renders at SSR but **crashes on the first diff** -- the
per-item diff keys a list item by `to_bin` of its first dynamic, which fails on a nested
template/descriptor. So a per-item component is not usable yet. The exception is a
`?stateful` child in a **stream** `?each` (it is its own self-diffing view process).

## Where to read bindings

`?get` (and friends) record a dependency for diff tracking. The dependency is
attributed to whichever dynamic's closure is currently being evaluated. Two
consequences:

- Read outer bindings in **props expressions**, not in callback or lifecycle
  bodies. `?stateless(fun bar/1, #{x => ?get(x)})` and
  `?stateful(handler, #{x => ?get(x)})` record `x` on the outer dynamic
  correctly. Eager `?get` calls inside a `?stateless` callback body, or
  inside a stateful handler's `mount/1` / `handle_update/2`, are isolated
  by the eval wraps and will not record at the outer level.
- Prefer named fun references (`?stateless(fun bar/1, Props)`,
  `?stateless(Mod, Fn, Props)`). They cannot close over outer `Bindings`,
  removing the footgun entirely.

Reads **hoisted into the render function body** do track: the parse transform
inlines each interpolated variable back into its slot closure, so the `?get`
re-runs inside the dependency bracket.

```erlang
%% all of these track per-slot (compile-time inlining):
Name = ?get(name),                 ?html({p, [], [Name]}).
User = ?get(user), N = maps:get(name, User), ?html({p, [], [N]}).  %% tracks `user`
Label = case ?get(mode) of dark -> ?get(a); _ -> ?get(b) end, ?html({p, [], [Label]}).
case ?get(mode) of dark -> X = ?get(a); _ -> X = ?get(b) end, ?html({p, [], [X]}).
```

Exceptions that stay un-tracked (slot frozen after SSR): a binding destructured
in the head (`render(#{foo := Foo})`) or through any non-bare-var pattern
(`{ok, V} = ?get(...)` -- use `?get(foo)` then plain destructuring), a variable
bound inside a `case` branch whose clause head binds a variable, and a rebound
variable.

A tracked read used in a **guard** inside a template (`case Status of active when
Confirming -> ...`, an `if`, a fun-clause guard) is a compile error
(`tracked_read_in_guard`), not a silent freeze. A guard cannot hold the `get/2`
call (Erlang rejects it as an illegal guard expression), so the read can never
re-run inside the slot's dependency bracket -- the dependency would be dropped and
the slot would stop updating when that binding changes. The transform rejects it:
move the test into the `case` scrutinee or clause body so the read is tracked
(`case {?get(status), ?get(confirming)} of {active, true} -> ...`), or replace the
guard with a pattern match. Only a tracked read reaches this -- a non-tracked local
or a pattern-bound variable in a guard is fine.

`?get`/`get_lazy`/`with` are for the **view bindings**, not sub-maps: they call
`track/1` regardless of which map they read, so reading a nested map records the
inner key as a spurious top-level dep. The parse transform rejects a tracked read
whose map argument is a local that is not the bindings (or an alias/`with`
projection of it) with `tracked_get_on_non_bindings_map`. Read sub-structures with
plain `maps:get/2`: `User = ?get(user), Name = maps:get(name, User)`.

### Handing a bindings subset to a sub-context -- `az:with`

A child template embedded via a **raw function call** (or an explicit nested
`?html(...)` call returning a template) freezes: the outer slot is
`fun() -> child(Bindings) end`, building `child` fires no `?get` at the outer level
(its reads sit in `child`'s own closures), so the outer slot captures empty deps and
the diff engine skips it forever. Idiomatic composition is `?stateful`/`?stateless`
(props reads track on the parent slot); an inline nested *element* is also fine (it
flattens into the parent template).

When you must hand a bindings subset to a sub-context (a helper, a passed-through
map), declare the dependency with `az:with([keys], Bindings)` -- it tracks each key
on the enclosing slot (fixing the freeze) and projects to only those keys via
`maps:with/2`, so the sub-context can't silently read an untracked key (an omitted
key fails loudly with `missing_binding` instead of freezing).

```erlang
%% Frozen: outer slot has empty deps, never re-renders.
?html({p, [], [row(Bindings)]}).
%% Tracked: `id`/`name` recorded on the outer slot; projection hides the rest.
?html({p, [], [row(az:with([id, name], Bindings))]}).
```

There is deliberately no `with_all` -- tracking every key makes the slot depend on
everything, defeating fine-grained diffing.

### Eager `get/3` defaults over-track

`?get(a, ?get(b))` (i.e. `get(a, B, get(b, B))`) records `b` even when `a` is
present, because Erlang evaluates the default argument eagerly. Use
`?get_lazy(a, fun() -> ?get(b) end)` when the default itself reads a binding, so the
fallback key is tracked only when actually taken.

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
| `?each(Fun, Source)` | `arizona_template:each(Fun, Source)` -- 1-arg for lists, 2-arg for streams/maps |
| `?stateful(Handler, Props)` | `arizona_template:stateful(Handler, Props)` |
| `?stateless(Fun, Props)` | `arizona_template:stateless(fun Fun/1, Props)` |
| `?stateless(Mod, Fun, Props)` | `arizona_template:stateless(Mod, Fun, Props)` |
| `?local(Key, Init)` | `arizona_template:local(Key, Init)` -- client-owned slot: server renders `Init` once and never diffs it; the browser owns/updates the value via `Key` (a binary or atom literal; content -- one or many per element, mixed with static text -- or an attribute value, whole or interpolated with one local + static prefix/suffix) |
| `?connected` | `arizona_live:connected()` -- true inside a connected live process, false during SSR |
| `?send(Msg)` | `arizona_live:send(?get(id), Msg)` -- send to current view (stateful only) |
| `?send(ViewId, Msg)` | `arizona_live:send(ViewId, Msg)` -- send to specific view (stateful only) |
| `?send_after(Time, Msg)` | `arizona_live:send_after(?get(id), Time, Msg)` -- delayed send to current view (stateful only) |
| `?send_after(ViewId, Time, Msg)` | `arizona_live:send_after(ViewId, Time, Msg)` -- delayed send to specific view (stateful only) |

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
(`{ok, V} = ?get(...)` -- use `?get(foo)` then plain destructuring), a read
reachable only through a guard, a variable bound inside an `if` or a `case`
branch whose clause head binds a variable, and a rebound variable.

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

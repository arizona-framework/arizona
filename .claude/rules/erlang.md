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
include/arizona_handler.hrl     -- shared base: parse_transform, send/subscribe macros, arizona_common.hrl
include/arizona_view.hrl        -- -behaviour(arizona_view) + -behaviour(arizona_handler); pulls arizona_handler.hrl
include/arizona_stateful.hrl    -- -behaviour(arizona_stateful) + -behaviour(arizona_handler); pulls arizona_handler.hrl
include/arizona_stateless.hrl   -- parse_transform, includes arizona_common.hrl
```

Rule of thumb:

- Route-level pages → `arizona_view.hrl` (`mount/2` takes Bindings + `az:request()`)
- Embeddable components → `arizona_stateful.hrl` (`mount/1`, instantiated via `?stateful(Handler, Props)`)
- Pure template modules → `arizona_stateless.hrl`

## Mount bindings -- construct, don't merge

Every handler's `mount/1` (stateful) or `mount/2` (view) must build a fresh map
literal rather than `maps:merge`-ing or `Bindings#{...}`-updating the input.
With `arizona_live:navigate/4` the input may carry arbitrary keys from
previously visited pages; merging passes them through and lets foreign keys
collide with handler-owned defaults (e.g. one page's `next_id` overriding
another's, dup-inserting on a stream). Pull each accepted override out
explicitly via `maps:get/3`.

```erlang
%% Bad -- foreign keys carry through:
mount(Bindings, _Req) ->
    {maps:merge(#{id => ~"page", count => 0}, Bindings), #{}}.

%% Good -- handler owns its keys, accepts a typed override:
mount(Bindings, _Req) ->
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
    middlewares => [arizona_req:middleware()],
    _ => term()
}.
```

Used in route declarations, `arizona_render:render_view_to_iolist/3`, `arizona_http:render/3`, and the optional `arizona_req:resolve_route/3` callback's return tuple. All keys are optional; consumers default at use-site (`maps:get(K, M, Default)`).

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
| `?each(Fun, Source)` | `arizona_template:each(Fun, Source)` -- 1-arg for lists, 2-arg for streams/maps |
| `?stateful(Handler, Props)` | `arizona_template:stateful(Handler, Props)` |
| `?stateless(Fun, Props)` | `arizona_template:stateless(fun Fun/1, Props)` |
| `?stateless(Mod, Fun, Props)` | `arizona_template:stateless(Mod, Fun, Props)` |
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

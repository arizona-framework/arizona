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

Tag **classification** in the `?html` target (void, raw-text) is ASCII case-insensitive, as HTML itself is -- `{'BR', [], []}` self-closes and `{'SCRIPT', ...}` is raw text. The tag is still *emitted* exactly as written, so a camelCase SVG element or a `viewBox` attribute is never rewritten. `?native` and `?terminal` tags are case-**sensitive**: their vocabularies are Arizona's own (native tags map to Compose/SwiftUI component names). `?terminal` goes further and **rejects an unknown tag at compile time**, the way it already rejects an unknown style/attribute -- its six tags are the whole vocabulary, so `{'Line', ...}` is a typo, not an extension point.

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

Two names are reserved because the transform emits them itself and a duplicate would
misroute a patch: `az` (the element's diff address) and `az-local` (the `?local`
descriptor). Either one in a template is a compile error (`reserved_attr`), in any form
(`{az, V}`, bare `az`, `<<"az">>`, `az_local`). `az-view` has its own rule -- injected on a
live root, rejected elsewhere. Everything else `az-*` is the template author's: `az_key`
keys stream items, `az_click`/`az_submit`/... carry effects, and an app may invent its own.

## Route options

A route's static config is the single canonical type `arizona_live:route_opts/0`:

```erlang
-nominal route_opts() :: #{
    bindings => arizona_template:bindings(),
    on_mount => on_mount(),
    layouts => [arizona_render:layout()],
    middlewares => [arizona_middleware:middleware()],
    %% CSRF Origin check is on by default; set false to opt this route out.
    check_origin => boolean(),
    _ => term()
}.
```

Used in route declarations, `arizona_render:render_view_to_iolist/2`, `arizona_http:render/3`, and the optional `arizona_req:resolve_route/3` callback's return tuple. All keys are optional; consumers default at use-site (`maps:get(K, M, Default)`).

`layouts` is always a list, applied outermost-first: `[Root, Section]` produces `Root(Section(Page))`. Empty list = no wrap.

A layout is stateless chrome. A `?stateful` inside one is a render-time error
(`stateful_in_layout`), directly or through a `?stateless` helper: layouts render on the
request-free SSR path, which keeps no `views` map, so the child is mounted and thrown away
while its `az-view` marker still lands in the DOM naming a view the server never registered
-- and the client picks event targets off those markers. Use `?stateless`; for chrome that
must stay live across navigation, put it in a view the routes share and link with `az_patch`.
(A `?stateful` under a user `az-nodiff` is fine -- that renders on the live path and is
registered. The disqualifying property is the render path, not the nodiff flag, which is why
this is not a compile-time check.)

Layouts render **once, at SSR** -- a live navigate/patch replaces only the view *inside* them, so no frame can re-render one. `arizona_socket` therefore compares a navigate/patch target's `layouts` against the ones already on screen and degrades to a full page load (`arizona_js:navigate(Url, #{full => true})`) when they differ, instead of dropping the new page into the old page's shell. Whole-list term equality: a difference at any depth disqualifies an in-place swap, since an inner layer wraps the replaced view exactly as the outer one does. Crossing layout families therefore needs no hand-written `<a href>` -- `az_navigate`/`az_patch` degrade themselves, including on a server-issued redirect.

### CSRF Origin check

`check_origin` is a built-in middleware step (`{arizona_middleware, check_origin}`) the router **prepends by default** to `{live, ...}` and controller (verb-tag / `match`) routes -- it rejects a cross-origin request/upgrade with `403` (`arizona_origin:check/3`: same-origin -- authority **and** scheme, so an HTTPS request refuses a plain-`http` Origin -- or `csrf_origins` allowlist; a missing `Origin` is allowed). Off by exception: `check_origin => false` in a route's `Opts`, or the global `check_origin` app env (which logs a warning once when disabled). It covers the WS upgrade too (`arizona_ws:prepare/3` runs the route's middlewares).

**It protects state-changing POST/PUT/DELETE + the WS upgrade only -- never a state-changing GET.** A cross-site top-level GET navigation sends no `Origin` (so the check allows it) but still carries a `SameSite=Lax` cookie, so a cookie-only `GET /logout`-style mutation is CSRF-able regardless -- the inherent limit of Origin-checking, not framework-closable. **Method routing enforces "don't mutate on GET" by routing**: declare a mutating endpoint with a non-GET verb (`{post, ...}`/`{delete, ...}`) and the router `405`s a cross-site `GET` to it before the action runs; only a mutation you deliberately route as `{get, ...}` stays exposed, so never put state changes behind GET. Apps should also set `SameSite=Lax`/`Strict` on auth cookies. A signed double-submit token was evaluated and **deliberately not added** -- redundant with this Origin check, and the missing-Origin gap it would close is non-browser clients, which carry no ambient cookie authority and so are not a CSRF vector.

### Controller routes

A controller route is reached over HTTP (e.g. by `arizona_js:fetch/2`) and gated by HTTP method:

- `{Verb, Path, Handler, Opts}` where `Verb` is `get`/`post`/`put`/`patch`/`delete`/`head`/`options` -- single-verb sugar.
- `{match, Spec, Path, Handler, Opts}` -- multi/custom/any-method, where `Spec` is a verb, a list of verbs, a custom uppercase method binary (`~"PROPFIND"`), or `'*'` (any). A `get` allowlist implicitly answers `HEAD`.

A request whose path matches but whose method does not gets `405 Method Not Allowed` with an `Allow` header, decided by `roadrunner_router:match/3` **before** any middleware; two routes may share a path with disjoint verbs for REST-style dispatch (`{live, ...}` routes are themselves GET/HEAD-only). `Opts` is `controller_opts()`: `#{state => term(), action => atom(), middlewares => [...], check_origin => boolean()}`. The route dispatches through `arizona_roadrunner_controller`, which runs the middleware pipeline (CSRF default-on) then calls `Handler:Action/1` -- the `action` option, default `handle` -- with `state` restored into the request (read via `roadrunner_req:state/1`); the controller is a module exporting those action functions, and a route naming an action it does not export raises a clear `missing_action` error. The middleware pipeline's product is threaded to the action: `arizona_controller:req/1` returns the post-middleware `arizona_req:request()` (e.g. `arizona_req:get_session/2,3` after a `fetch_session` step) and `arizona_controller:bindings/1` the middleware-produced bindings. **Writing that request is a round trip** -- read, mutate, `arizona_controller:put_req/2` (roadrunner request first, pairing with `req/1` and with every other subject-first arizona writer), return the roadrunner request it gives back: the dispatcher flushes the request the action *returned*, so a mutated `arizona_req` kept in a local is dropped in favour of the pre-action copy. That round trip is what makes `arizona_req:put_session/3` / `clear_session/1` / `put_flash/3` / `put_resp_cookie/4` in an action reach the response (and, in store mode, the server-side store). Build the response with `arizona_controller:reply_effects/1,2` / `reply_redirect/1`.

```erlang
handle(Req) ->
    ArzReq = arizona_req:clear_session(arizona_controller:req(Req)),
    {arizona_controller:reply_effects([]), arizona_controller:put_req(Req, ArzReq)}.
```

## Comprehension generators

Use the strict generator (`<:-`, `K := V <:-`) only when the LHS is a pattern that
can fail to match -- a tuple, a map key/value pattern, an equality bind, etc. A
strict generator with a pure variable LHS (`X <:- Source`) adds noise without
adding any guarantee, since a bare variable always matches. In that case use the
lazy generator (`<-`).

```erlang
%% Pattern LHS -- strict makes mismatches loud:
[V || {_Az, V} <:- Snapshot]
[K || K := {ok, _} <:- Map]

%% Bare-variable LHS -- lazy:
[F || F <- Files]
[Pid || Pid <- Subscribers, Pid =/= Self]
```

The same rule applies to **map** generators (`KeyPattern := ValuePattern` LHS). Both forms
exist for maps -- `<-` and `<:-` -- so pick by whether the LHS can fail to match. Bare
variables and `_` cannot, so they take lazy `<-`; that is the dominant form in this codebase
(`arizona_config:resolve/1`, `arizona_diff`, `arizona_stream:compute_item_changed/2`). Reserve
`<:-` for a failable **value** pattern, where lazy would silently skip the entries you meant to
catch (`K := {ok, _} <:- Map` raises `{badmatch, {Key, Value}}` on the first non-`{ok, _}`).
Note a *variable* key pattern does not pin a key -- `Bound := V <- Map` shadows `Bound` and
matches every entry.

```erlang
%% Bare-variable / `_` LHS -- lazy:
#{K => resolve(V) || K := V <- Map}
#{K => true || K := _ <- All, key_changed(K, OldItem, NewItem)}
```

## az-nodiff

Adding `'az-nodiff'` to an element's attribute list marks it as a compile-time directive. The parse transform strips it from HTML and emits `diff => false`. All dynamics in that compile unit get `undefined` Az (pre-scanned via `prescan_directives/1`). Children in separate `?html` calls are not affected at compile time, but safe because the parent's `diff => false` short-circuits before their dynamics are reached by the diff engine.

## Macros

| Macro | Expands to |
|-------|-----------|
| `?get(Key)` | `arizona_template:get(Key, Bindings)` |
| `?get(Key, Default)` | `arizona_template:get(Key, Bindings, Default)` |
| `?get_lazy(Key, Fun)` | `arizona_template:get_lazy(Key, Bindings, Fun)` |
| `?with(Keys)` | `arizona_template:with(Keys, Bindings)` -- tracks each key on the enclosing slot, then projects `Bindings` to just those keys (see "Handing a bindings subset to a sub-context") |
| `?html(Elems)` | `arizona_template:html(Elems)` |
| `?native(Elems)` | `arizona_template:native(Elems)` -- `?native` JSON render target (Android/iOS clients) |
| `?terminal(Elems)` | `arizona_template:terminal(Elems)` -- ANSI render target; tags `line`/`col`/`row`/`text`/`span`/`br` + bare-atom style attrs (see docs/architecture.md "Terminal render target") |
| `?each(Fun, Source)` | `arizona_template:each(Fun, Source)` -- 1-arg for lists, 2-arg for streams/maps. The callback must return an element (see "?each body must return an element") |
| `?stateful(Handler, Props)` | `arizona_template:stateful(Handler, Props)` |
| `?stateless(Fun, Props)` | `arizona_template:stateless(Fun, Props)` -- `Fun` is a fun reference (`fun bar/1`); a literal atom is sugar the transform rewrites to `fun Atom/1` |
| `?stateless(Mod, Fun, Props)` | `arizona_template:stateless(Mod, Fun, Props)` |
| `?local(Key, Init)` | `arizona_template:local(Key, Init)` -- client-owned slot: server renders `Init` once and never diffs it; the browser owns/updates the value via `Key` (a binary or atom literal; content -- one or many per element, mixed with static text -- or an attribute value, whole or interpolated with one local + static prefix/suffix) |
| `?raw(Value)` | `arizona_template:raw(Value)` -- escape opt-out: splices a trusted, already-safe HTML fragment verbatim into a content slot or attribute value instead of HTML-escaping it. The parse transform only recognizes the opt-out when the `raw` call is **literal at the template site**, so wrap values here, never inside a helper. Never for user-controlled data. A dynamic content slot inside `<script>`/`<style>` is spliced verbatim (raw text decodes no character references, so escaping cannot apply there) and therefore **must** carry it -- an unmarked value there is a compile error (`dynamic_in_raw_text`); serialize data first, e.g. `?raw(json:encode(Data))`. The breakout neutralization behind that opt-out is **per-slot**, so two adjacent `?raw` slots in one `<script>`/`<style>` can reassemble a close tag across the boundary (`~"</scr"` + `~"ipt>"`) -- build the value in one slot |
| `?inner_content` | `az:inner_content(Bindings)` -- the rendered page a layout wraps. **Opaque, and it fills a content slot whole**: an internal nested template (so the page is spliced, not copied and re-decoded per layer), not iodata. Fine in a `case` tail or as a `?stateless` prop; raises in an attribute value, under `iolist_size`, wrapped in `?raw` (redundant -- a content slot is already unescaped), or beside other values in one slot (`[~"&lt;hr&gt;", ?inner_content]` -- give it its own slot) |
| `?connected` | `arizona_live:connected()` -- true inside a connected live process, false during SSR |
| `?reconnected` | `arizona_live:reconnected()` -- true when the connected live process is a reconnection (client re-opened the WS), false on first connect/SSR. Gate one-shot OS commands with `?connected andalso not ?reconnected` |
| `?capability(Key)` | `arizona_live:capability(Key)` -- did the native shell advertise capability `Key`? `false` in a plain browser/SSR. A UI/effect hint, **never** authorization (see [docs/os.md](../../docs/os.md)) |
| `?capabilities` | `arizona_live:capabilities()` -- the negotiated native-shell capability map (`#{}` in a plain browser/SSR) |
| `?send(Msg)` | `arizona_live:send(map_get(id, Bindings), Msg)` -- send to current view (stateful only). Reads `id` with the `map_get` BIF, not `?get`, so it records no dependency |
| `?send(ViewId, Msg)` | `arizona_live:send(ViewId, Msg)` -- send to specific view (stateful only) |
| `?send_after(Time, Msg)` | `arizona_live:send_after(map_get(id, Bindings), Time, Msg)` -- delayed send to current view (stateful only). Reads `id` with the `map_get` BIF, not `?get`, so it records no dependency |
| `?send_after(ViewId, Time, Msg)` | `arizona_live:send_after(ViewId, Time, Msg)` -- delayed send to specific view (stateful only) |
| `?subscribe(Topic)` | `arizona_pubsub:subscribe(Topic, self())` -- subscribe the live process to `Topic` (stateful only) |
| `?unsubscribe(Topic)` | `arizona_pubsub:unsubscribe(Topic, self())` -- unsubscribe the live process from `Topic` (stateful only) |

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

**Branch reads are tracked.** A binding read inside such a branch element (`?get(val)`
in `{p, [], [?get(val)]}`) compiles into the branch's nested template, whose reads are
otherwise isolated from the conditional slot's own dependency bracket -- so a change to
a binding read *only* in a branch would skip the slot and freeze the branch. The parse
transform closes this by unioning each **element** branch's reads into the conditional
slot's deps (an injected `arizona_template:track/1` per literal key, mirroring the guard
auto-tracking below), so the element form behaves like the value form
(`X = case ?get(flag) of true -> ?get(val); false -> ~"" end`). `track/1` records the
key without reading it, so a key present only in a non-taken (and possibly absent)
branch never raises `missing_binding`. A **value** branch (one returning a scalar, not
an element) is unaffected -- its read already fires when that branch is taken, and a
non-taken value branch's read is genuinely not a dependency. A change to a binding read
only in a non-taken element branch re-evaluates the slot but emits no op (the re-rendered
branch is structurally unchanged). Limitation: a branch read whose **key is computed**
(`?get(SomeVar)`, not a literal) is not auto-tracked; use the value form for that.

A conditional may also return a `?stateful`/`?stateless` descriptor from a branch -- the
idiomatic `case ?get(flag) of true -> ?stateful(child, #{id => ~"c"}); false -> ~"" end`.
A content slot is anchored by its `<!--az:X-->...<!--/az-->` comment markers in SSR, so
any branch value (the empty string, a binary, a nested template, or a child descriptor)
patches **in place** via `?OP_TEXT`, preserving the slot's siblings and the enclosing
element. (`arizona_diff:make_op/3` always emits `?OP_TEXT` for a nested-template value --
a whole-element `innerHTML` write would overwrite the enclosing element, which is
catastrophic when the slot's `az` is that element's own `az`, e.g. a conditional
child rendered directly under the view root.)

When the **same branch** re-renders (its statics are unchanged -- only an inner binding
changed), the diff does **not** re-render the whole branch: `make_ops/4` diffs the nested
template's inner dynamics and patches only the changed inner slot(s), each addressed by its
own `az` (an inner attribute change is a precise `?OP_SET_ATTR`). It recurses through
nested-nested templates to the deepest changed slot. The wholesale `?OP_TEXT` re-render is
the fallback only when the statics differ -- a different branch, an empty<->template
transition, or any structure change.

The same rule applies to a **plain-list `?each` in a content slot**: it is marker-anchored
exactly like any other dynamic-text child (no wrapper element carries the slot `az`), so its
container patch is the marker-aware `?OP_TEXT` -- `make_op/3` (the `?EACH` list clause) and
`arizona_diff:full_update/5` both emit it. This is what lets a
plain-list `?each` sit **among static sibling content** in one slot: re-rendering the list
replaces only the each's marker span, leaving the siblings intact. A whole-element write here
would `innerHTML`-wipe the enclosing element's static siblings (the client's `resolveEl` finds
no element for the slot `az` and falls back to that enclosing element); a sole-child `?each`
only appeared to work with one by coincidence. That is why op code 3 (the innerHTML op) is
**removed** rather than kept for the cases where it happened to resolve correctly -- 3 is now
unassigned in `arizona.hrl` and in every client.

**A stream `?each` container full render follows the same rule.** SSR anchors a stream each by
the identical content-slot markers, so every container-level full render is the marker-aware
`?OP_TEXT` too -- the `order`-keyed `make_op/3` clause and `diff_stream/4`'s no-`order`
(type-switch) clause, beside the plain-list clause and `full_update/5`. Among static siblings the
stream's slot az is compound (`<Root>:N`) and carried by no element, so a whole-element write
there would `innerHTML`-wipe the siblings exactly as it would for a list -- and when the
enclosing element is the view root, the whole view.

What stays stream-specific is the **incremental** ops (`?OP_INSERT`, `?OP_REMOVE`, `?OP_MOVE`,
`?OP_ITEM_PATCH`), which a plain list has no equivalent of. They carry the **container's** az as
the op target and name the item by key in a later field (`[?OP_INSERT, Az, Key, Pos, HTML]`),
mutating one keyed child rather than the container's whole content -- so the full-render op code
does not govern them. Placement is anchored to the slot span: the client
resolves the slot's markers and inserts before the closing one (or after the opening one for a
move-to-head), so an each sharing its container with static siblings keeps its items inside the
slot. The remaining case is a **marker-only** container, where no element carries the slot az at
all -- there the client refuses `?OP_INSERT`/`?OP_MOVE` (warn + skip) rather than misplace the
node, and only the key-addressed `?OP_REMOVE`/`?OP_ITEM_PATCH` still apply.

**Component as an `?each` item child.** A `?stateless` descriptor **inside** an item element
(`{li, [], [?stateless(...)]}`) renders at SSR and diffs per-item like any other item content:
the list stays on the positional path, so an inner value change ships one `?OP_LIST_PATCH`
carrying an `?OP_ITEM_PATCH` for the affected index, and an append ships an `?OP_INSERT`.
(A **bare** descriptor as the whole callback body is still a compile error -- see above.)

A `?stateful` child in a plain-list `?each` also renders and diffs without crashing, but it
costs the per-item path: `arizona_diff:diff_each_items/6` only patches positionally when the
render added **no** child view (`map_size(NewLocal1) =:= map_size(NewLocal0)`), because a
child view must be re-mounted by a full re-render. A list bearing per-item `?stateful`
children therefore falls back to the wholesale marker `?OP_TEXT` -- every item re-renders on
any change. Use a **stream** `?each` when you want self-diffing children: a stream keys items
by `az_key`, and each `?stateful` item is its own self-diffing live view. "View", not
process: a child stateful view is an entry in the root live process's `views` map, dispatched
in-process -- only a route root is ever spawned.

## Where to read bindings

`?get` (and friends) record a dependency for diff tracking. The dependency is
attributed to whichever dynamic's closure is currently being evaluated. Two
consequences:

- Read outer bindings in **props expressions**, not in callback or lifecycle
  bodies. `?stateless(fun bar/1, #{x => ?get(x)})` and
  `?stateful(handler, #{x => ?get(x)})` record `x` on the outer dynamic
  correctly. Eager `?get` calls inside a `?stateless` callback body, or
  inside a stateful handler's `mount/1` / `handle_update/3`, are isolated
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

A hoisted **template constructor** also compiles and tracks:
`Header = ?html({h1, [], [?get(title)]})`, `Items = ?each(...)`, or
`Card = ?stateless(card, #{x => ?get(x)})` bound to a local and used in a content
slot is spliced back and compiled -- a `?html`/`?native`/`?terminal` constructor
additionally gets track touches for the literal keys read inside it (a nested
template's inner reads are otherwise isolated from the slot bracket), so the slot
re-renders when they change. A hoisted `?html` used as a whole `?each` callback
body (`fun(_I) -> Row end`) keeps the per-item element path and its `single_root`
flag.

Exceptions that stay un-tracked (slot frozen after SSR): a binding destructured
in the head (`render(#{foo := Foo})`) or through any non-bare-var pattern
(`{ok, V} = ?get(...)` -- use `?get(foo)` then plain destructuring), a variable
bound inside a `case` branch whose clause head binds a variable, and a rebound
variable.

A tracked read used in a **guard** inside a template (`case Status of active when
Confirming -> ...`, an `if`, a nested fun) is **auto-tracked**, so the slot stays
reactive. A guard cannot hold the `get/2` call (Erlang rejects it as an illegal guard
expression), so the read can't run inside the slot's dependency bracket on its own; the
transform wraps the guard-bearing expression so it first reads (for the `track/1` side
effect) each binding the guards reference, recording them as slot dependencies. The guard
keeps using the captured value, which each diff cycle rebuilds from current bindings, so a
change to a guard binding re-renders the slot. A non-tracked local or a pattern-bound
variable in a guard needs nothing -- there is no binding dependency to record.

`?get`/`get_lazy`/`with` are for the **view bindings**, not sub-maps: they call
`track/1` regardless of which map they read, so reading a nested map records the
inner key as a spurious top-level dep. The parse transform rejects a tracked read
whose map argument is a local that is not the bindings (or an alias/`with`
projection of it) with `tracked_get_on_non_bindings_map`. Read sub-structures with
plain `maps:get/2`: `User = ?get(user), Name = maps:get(name, User)`.

### Local element helpers -- inlined at the call site

A **local** call in a content slot whose callee is a single-clause function
returning an element is inlined at the call site, so markup can be factored into a
named helper instead of copy-pasted:

```erlang
brand() -> {svg, [{width, ~"24"}], [...]}.
alert(Kind, Msg) -> {'p', [{class, Kind}], [Msg]}.
header(Bindings) -> {h1, [], [?get(title)]}.

?html({'div', [], [brand(), alert(~"warn", ?get(msg)), header(Bindings)]})
```

The body replaces the call (a whole-body `?html(...)` wrapper unwraps first,
exactly as `?each` callbacks do), each parameter is substituted with its argument
expression, and the spliced element compiles like a literal one -- it flattens into
the template, and `?get` reads in the body or the arguments land in the enclosing
slot's dependency bracket, so the slot stays reactive. Same-module explicit calls
(`?MODULE:brand()`) resolve like bare local calls; nested helper calls inline
recursively; a helper call as a whole `?each` callback body
(`fun(I) -> item(I) end`) inlines into the per-item element. The now-orphaned
definition is covered by auto-injected `nowarn_unused_function` / `-ignore_xref`,
mirroring `?each` named-fun refs. An element-**list** body compiles into a
nested-template child whose literal reads are tracked on the slot -- the same
tracking a literal element-list child gets.

A callee whose body is not element-shaped (a scalar helper) is untouched: the call
runs inside the slot closure, so its reads fire in-bracket as before. Shapes that
cannot be cleanly inlined are compile errors when the body is a **bare** element
(they previously compiled clean and crashed at first render with
`bad_template_value`): multi-clause (`helper_multi_clause` -- collapse the clauses
into a case inside the element), a guard (`helper_guarded`),
non-variable/repeated/`_` parameters (`helper_params_not_vars`), statements before
the element (`helper_body_not_single_expr`), and recursion (`helper_recursive`).
The same shapes with a whole-body `?html` wrapper are **not** rejected -- they
already render as a runtime nested template and keep doing so (body reads frozen;
hand a subset over with `?with` below). Genuinely remote calls (`mod:helper()`),
imported functions, and variable-bound fun calls are not resolvable at compile
time and keep today's behavior (an element-returning one still crashes at render).

### Handing a bindings subset to a sub-context -- `?with`

A child template embedded via a **raw function call** that the helper inlining
above cannot resolve (a multi-statement `?html`-wrapped helper, a remote helper)
freezes: the outer slot is `fun() -> child(Bindings) end`, building `child` fires
no `?get` at the outer level (its reads sit in `child`'s own closures), so the
outer slot captures empty deps and the diff engine skips it forever. Idiomatic
composition is `?stateful`/`?stateless` (props reads track on the parent slot); an
inline nested *element* is also fine (it flattens into the parent template).

When you must hand a bindings subset to a sub-context (a helper, a passed-through
map), declare the dependency with `?with([keys])` -- it tracks each key on the
enclosing slot (fixing the freeze) and projects to only those keys via
`maps:with/2`, so the sub-context can't silently read an untracked key (an omitted
key fails loudly with `missing_binding` instead of freezing). The macro needs
`Bindings` in scope like `?get`; the underlying `arizona_template:with/2` (or the
`az:with/2` alias) takes the map explicitly when it isn't.

```erlang
%% Frozen: outer slot has empty deps, never re-renders.
?html({p, [], [row(Bindings)]}).
%% Tracked: `id`/`name` recorded on the outer slot; projection hides the rest.
?html({p, [], [row(?with([id, name]))]}).
```

There is deliberately no `with_all` -- tracking every key makes the slot depend on
everything, defeating fine-grained diffing.

### Eager `get/3` defaults over-track

`?get(a, ?get(b))` (i.e. `get(a, B, get(b, B))`) records `b` even when `a` is
present, because Erlang evaluates the default argument eagerly. Use
`?get_lazy(a, fun() -> ?get(b) end)` when the default itself reads a binding, so the
fallback key is tracked only when actually taken.

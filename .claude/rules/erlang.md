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
%% f = fingerprint -- base-36 phash2 of statics (dynamic attribute names included)
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
After a `navigate` the input may carry arbitrary keys from
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
| `{<<"tag">>, Attrs, Children}` | `{~"my-widget_v2", [], []}` | Tag as a binary -- taken **verbatim** |
| `{Tag, Attrs, Expr}` | `{'span', [], ?get(x)}` | Single expression as children (wrapped in list) |
| `{Tag, Attrs}` | `{'br', []}` | Void element shorthand (no children) |

Void elements (`br`, `img`, `input`, `hr`, `meta`, `link`, `base`, `col`, `embed`, `param`, `source`, `track`, `wbr`, `area`) self-close as `<tag />`.

A tag may be an **atom** or a **binary**, the same pair as an attribute name and for the same reason. The atom form goes through the backend's `name/1`, which for `?html` translates `_` to `-` so `az_click` and `my_widget` need no quoting; the binary form is taken verbatim. That matters for a name that really does contain an underscore: `{'my-widget_v2', [], []}` emits `<my-widget-v2>` (a different element, silently), while `{~"my-widget_v2", [], []}` emits what it says. Both forms resolve identically through the tag-keyed callbacks (`is_void/1`, `raw_text_kind/2`, `content_context/2`), so classification does not depend on which you write. A tag that is neither -- a number, or an expression evaluated at runtime -- is a compile error.

Tag **classification** in the `?html` target (void, raw-text) is ASCII case-insensitive, as HTML itself is -- `{'BR', [], []}` self-closes and `{'SCRIPT', ...}` is raw text. The tag is still *emitted* exactly as written, so a camelCase SVG element or a `viewBox` attribute is never rewritten. `?native` and `?terminal` tags are case-**sensitive**: their vocabularies are Arizona's own (native tags map to Compose/SwiftUI component names). `?terminal` goes further and **rejects an unknown tag at compile time**, the way it already rejects an unknown style/attribute -- its six tags are the whole vocabulary, so `{'Line', ...}` is a typo, not an extension point.

## Parse transform attribute forms

| Form | Example | Output |
|------|---------|--------|
| `{name, <<"value">>}` | `{class, <<"box">>}` | `class="box"` |
| `{name, [Literal, ...]}` | `{class, [~"btn ", ~"primary"]}` | `class="btn primary"` -- a (nested) list of binary/string/char literals is static: concatenated and escaped at compile time, no dynamic, no `az` |
| `{name, Expr}` | `{class, Theme}` | Dynamic attr |
| `name` (atom) | `disabled` | `disabled` |
| `<<"name">>` (binary) | `<<"hidden">>` | `hidden` |
| `{name, true}` | `{hidden, true}` | `hidden` |
| `{name, false}` | `{hidden, false}` | Stripped (`?native` keeps a typed `false` prop) |
| `'az-nodiff'` / `<<"az-nodiff">>` | Directive | Emits `diff => false`; the HTML keeps a compiler-owned `az-nodiff` (token or bare) |

A `form` carrying `az_submit` and no `method` renders `method="post"` in the `?html` target
(appended after its attributes), so a submit made before the client script runs never puts the
fields in the URL; an explicit `method` is kept as written. See [js.md](js.md).

A list attribute value (`{class, [~"btn-", ?get(kind)]}`) is text -- chardata, so an
integer in it is a codepoint. A boolean or other atom inside it is not text and raises
`bad_template_value`; a boolean belongs in a whole-value attribute (`{disabled, ?get(off)}`).

A literal list in **content** (`{p, [], [[~"a", ~"b"], ~"c"]}`) folds the same way, but it
keeps its meaning as a value: it is escaped exactly as the runtime would escape it, unlike a
bare binary literal child, which is spliced as raw markup. Inside raw-text elements
(`script`/`style`/`textarea`/`title`) it is not folded and keeps the dynamic-slot rules.

Two names are reserved because the transform emits them itself and a duplicate would
misroute a patch: `az` (the element's diff address) and `az-local` (the `?local`
descriptor). Either one in a template is a compile error (`reserved_attr`), in any form
(`{az, V}`, bare `az`, `<<"az">>`, `az_local`). `az-nodiff` is written bare only: its
value in the HTML is the compiler's (the az-nodiff element token), so `{'az-nodiff', V}` /
`{az_nodiff, true}` is a compile error (`nodiff_value`). `az-view` has its own rule -- injected on a
live root, rejected elsewhere. Everything else `az-*` is the template author's: `az_key`
keys stream items, `az_click`/`az_submit`/... carry effects, and an app may invent its own.

An event attribute may name **any** DOM event -- the suffix becomes the
`addEventListener` type verbatim, so `az_toggle`, `az_close` and a custom element's
`az_sl_change` all bind, including events that do not bubble. The names are
collected at compile time (`compile_attr` records each one; see
[js.md](js.md)), which has two consequences worth knowing:

- **A static value is app data and is never delegated.** An effect is always a
  call, so `{az_select, ~"[1,2,3]"}` is data by construction and no `select`
  listener is bound. Only a dynamic value (a command, folded or not) or a bare
  attribute puts the name in the set.
- **Where you put it depends on whether the event bubbles.** `az_click` on a `ul`
  catches every item's click; a non-bubbling one (`load`, `toggle`, `close`,
  `play`) is dispatched at its target alone, so it must sit on the element that
  emits it. Such a command also cannot be suppressed with `stopPropagation`.

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

`middlewares` holds two kinds of entry in one list. A **step** (a 2-arity fun or `{Module, Function}`) is Arizona's request-to-bindings pipeline -- it runs before any render, and also where no HTTP response exists (the WS upgrade, navigate/patch). An **HTTP response transform** -- built with `arizona_middleware:etag/0`, `compress/0`, `cors/1`, or `security_headers/0,1` -- wraps the whole HTTP exchange instead: the router splits transforms out at compile time into the server's own response pipeline (the underlying `roadrunner_etag`/`roadrunner_cors`/... middlewares), where they can see and rewrite the response a step never could. Order has phase semantics, not list semantics: steps run in list order during the request phase; transforms wrap the exchange in list order, first outermost -- except compression, which the router keeps OUTERMOST as an invariant wherever it is written (or attaches itself), so an etag always hashes the pre-compression body, the composition its weak validator is designed for. Transforms are inert on pipeline runs with no HTTP response (skipped, never crashing). The named set is the whole vocabulary on purpose -- a transform names an HTTP semantic the server adapter implements, so a route never says which server runs beneath it; a custom server-level middleware belongs in the server-specific `proto_opts` listener config instead. A route's method allowlist is enforced before any transform runs, so the router adds `OPTIONS` to a route carrying `cors/1` that does not declare it -- the preflight reaches the transform (`204` + `Access-Control-*`), a plain `OPTIONS` gets `204` + the `Allow` set a `405` on that URL would carry without reaching the action or page, and a `405` there lists `OPTIONS`. A cors origin must also be in `csrf_origins` (or the route sets `check_origin => false`), else the preflight `204` is followed by a `403`. Accepted on `live`, controller, and `asset` routes -- but an asset route runs no Arizona pipeline, so a step there fails route compilation loudly (`asset_middleware_not_transform`). A transform is a plain tagged term (`{http_transform, etag}`), so a `sys.config` route can spell it literally where a constructor call cannot be written.

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

A layout must also share the page's render target: an `?html` layout around a `?native`
page, or the reverse, is a render-time error (`cross_target_layout`). The page reaches a
layout as a backendless nested template, so the nested-template target check cannot see it;
the layout boundary compares the two instead.

A `?native` document must have a single root node, like a `?native` view: a native document
is one JSON value, so `?native([{'Banner', [], [~"top"]}, ?inner_content])` as the outermost
layout is a render-time error (`multi_root_layout`) -- wrap the nodes in one container. Only
the final document root counts: wrapped by a single-root layout, the same layout is just that
layout's children and renders. A layer that adds no node of its own (`?inner_content` returned
bare, or `?native([?inner_content])`) passes the root it wraps through, so a multi-root layer
under it is still refused. The parse transform stamps `multi_root` on a literal list of several
nodes, also one nested inside another (`[[A, B]]`), when the backend's `single_root_document/0`
says so; a layout is just a function a route names, so the check runs at render. Roots known
only at runtime are not judged, and neither is a layout with no node at all (`?native([])`),
which drops the page and serves an empty document. An `?html` or `?terminal` layout may have
several roots (a doctype beside `<html>`).

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

Adding `'az-nodiff'` to an element's attribute list marks it as a compile-time directive and emits `diff => false`. All dynamics in that compile unit get `undefined` Az (pre-scanned via `prescan_directives/1`), and no `az` attribute or `<!--az:...-->` slot marker is emitted. The directive itself stays in the HTML as a compiler-owned attribute on the element that carries it (html target only, `supports_nodiff_keep/0`): `az-nodiff="TOKEN"` on a keepable az-nodiff element root -- the single root element of a template, or of a **keyed** `?each` item -- and a bare `az-nodiff` on one that is client-owned but not keepable -- an unkeyed `?each` item root, or a root of a multi-element fragment. A template that only inherits nodiff, a layout, a mixed text+element fragment, and the `?native`/`?terminal` targets emit nothing. The bare form is the only one an author writes; a value is a compile error (`nodiff_value`).

**It inherits downward.** Every template nested in a nodiff one renders once with it, so the directive reaches all of them at compile time: conditional-branch templates, nested `?html`/`?native`/`?terminal`, `?each` item templates at any depth, hoisted constructors and inlined local helpers all compile nodiff and emit no markers. `az_nodiff` on an `?each` item's root element, or on a bare element in a conditional branch, reaches the templates nested in it the same way, and a layout (auto-nodiff via `?inner_content`) inherits too. It stops where what is built can land in a live view's diffed slot, where a nodiff value never updates in place: a `?stateful` child's props, and any fun, anonymous or named, other than an `?each` callback (a fun prop is called later, by whichever view receives it). A `?stateless` callback is a separate compile unit that may also be used live, so it keeps its own markers. A `?local` under a nodiff template still works: only the element holding a content local gets an `az` and that slot's marker (an attribute-only local needs neither). Outside any view -- a layout -- the client reaches it with `set_all`.

**An az-nodiff element is rendered by the server once, then owned by the client** -- the seam for a client-side component mounted from an `az_hook` (see "Embedding client-side components" in [docs/architecture.md](../../docs/architecture.md)). Nothing inside it is ever patched, but the slot holding it still diffs: the server can hide it, show it, or switch it to another az-nodiff element, and the switch replaces the slot through its markers so the hooks' `destroyed()`/`mounted()` run. The same az-nodiff element chosen again keeps its DOM untouched even when the values it was rendered from changed. An az-nodiff element is identified by its fingerprint, the conditional branch that produced it, and an optional `az_key` on its root -- not by its values -- so two clauses producing the same markup or calling the same helper are two az-nodiff elements, and a new `az_key` value remounts. A child view whose own root is az-nodiff is identified by its id. The branch is the one chosen by the conditional in the az-nodiff element's **own** slot: a switch above it between non-nodiff wrappers with the same statics (`case ?get(tab) of a -> {section, [], [chart()]}; b -> {section, [], [chart()]} end`) descends into the wrapper and keeps the az-nodiff element, so put `{az_key, ?get(tab)}` on the az-nodiff element's root to remount it. A slot re-run landing on the same az-nodiff element costs O(1) (the check runs before the az-nodiff element is evaluated); a plain-list item, a wrapper the diff descends into and `diff/3` evaluate the az-nodiff element and then compare, with the same result. A child view inside an az-nodiff element takes its props at mount only (no parent-driven `handle_update/3` on any path, including when a switch replaces the az-nodiff element: a same-handler, same-id child keeps its state). Reads inside an az-nodiff element are not tracked on its slot; only the reads that choose it, and its `az_key`, re-run the slot. The az-nodiff element's root carries that identity as the value of its `az-nodiff` attribute (`arizona_template:nodiff_token/3`; see above for which elements get a token and which a bare marker), so the client keeps the az-nodiff element's DOM -- and its widget -- across a wholesale re-render that names it again at the same place: a list `?each` container `?OP_TEXT` (keyed items only -- unkeyed items cannot be matched, so they remount), a stream container render, and the reconnect resync `?OP_REPLACE` (not a navigate).

**It is also the opt-out for an executable `<script>`.** A dynamic inside a raw-text element (`script`/`style`/`textarea`/`title`) is markerless -- a comment marker there would be literal content -- but the ELEMENT carries an `az` and its whole content folds into one nested template held there, so the value does update. That is what you want for the sanctioned case, a `?raw(json:encode(Data))` data island whose consumer reads `textContent`, and for a `<textarea>`, an SVG `<title>` or a `<style>` (whose CSSOM genuinely re-parses). It is questionable for an *executable* script: a script element carries an "already started" flag, so the browser does not re-run it when its text changes, and the element's source would then disagree with the code in effect. That is a browser rule, not a framework choice. Put `az-nodiff` on such a script to keep it render-once.

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
| `?raw(Value)` | `arizona_template:raw(Value)` -- escape opt-out: splices a trusted, already-safe HTML fragment verbatim into a content slot or attribute value instead of HTML-escaping it. The parse transform only recognizes the opt-out when the `raw` call is **literal at the template site**, so wrap values here, never inside a helper. In an interpolated attribute value (`{href, [~"/a?", ?raw(Q), ?get(x)]}`) the raw part is spliced verbatim and every other part, static included, is escaped, so each means the same text it would with no raw sibling. Never for user-controlled data. A dynamic content slot inside `<script>`/`<style>` is spliced verbatim (raw text decodes no character references, so escaping cannot apply there) and therefore **must** carry it -- an unmarked value there is a compile error (`dynamic_in_raw_text`); serialize data first, e.g. `?raw(json:encode(Data))`. The breakout neutralization behind that opt-out sees the slot's value and its **static** neighbours (a static `'<` before a value `/script>` is caught), and where the known text ends -- another slot or the element edge -- it treats a trailing partial sequence (`<`, `</scr`, `<!`) as complete, so adjacent `?raw` slots cannot reassemble a breakout either |
| `?inner_content` | `az:inner_content(Bindings)` -- the rendered page a layout wraps. **Opaque, and it fills a content slot whole**: an internal nested template (so the page is spliced, not copied and re-decoded per layer), not iodata. Fine in a `case` tail or as a `?stateless` prop; raises in an attribute value, under `iolist_size`, wrapped in `?raw` (redundant -- a content slot is already unescaped), or inside a list the template cannot see (a comprehension, or a list built at runtime); a literal content-slot list such as `[~"&lt;hr&gt;", ?inner_content]` gives it its own slot (see "Content-slot lists") |
| `?connected` | `arizona_live:connected()` -- true inside a connected live process, false during SSR |
| `?reconnected` | `arizona_live:reconnected()` -- true while the current page is the one a reconnection (client re-opened the WS) mounted; false on first connect/SSR and on a page reached by `navigate` after the reconnect (a same-handler `patch` keeps it; a cross-handler patch is a navigate). Gate one-shot OS commands with `?connected andalso not ?reconnected` |
| `?capability(Key)` | `arizona_live:capability(Key)` -- did the native shell advertise capability `Key`? `false` in a plain browser/SSR. A UI/effect hint, **never** authorization (see [docs/os.md](../../docs/os.md)) |
| `?capabilities` | `arizona_live:capabilities()` -- the negotiated native-shell capability map (`#{}` in a plain browser/SSR) |
| `?send(Msg)` | `arizona_live:send(map_get(id, Bindings), Msg)` -- send to current view (stateful only). Reads `id` with the `map_get` BIF, not `?get`, so it records no dependency |
| `?send(ViewId, Msg)` | `arizona_live:send(ViewId, Msg)` -- send to specific view (stateful only) |
| `?send_after(Time, Msg)` | `arizona_live:send_after(map_get(id, Bindings), Time, Msg)` -- delayed send to current view (stateful only). Reads `id` with the `map_get` BIF, not `?get`, so it records no dependency |
| `?send_after(ViewId, Time, Msg)` | `arizona_live:send_after(ViewId, Time, Msg)` -- delayed send to specific view (stateful only) |
| `?subscribe(Topic)` | `arizona_pubsub:subscribe(Topic, self())` -- subscribe the live process to `Topic` (stateful only). Returns `ok`, or `{error, already_joined}` when the process is already subscribed -- every view in a live process shares its pid, so a second view subscribing the same topic gets the error; don't match it against `ok` |
| `?unsubscribe(Topic)` | `arizona_pubsub:unsubscribe(Topic, self())` -- unsubscribe the live process from `Topic` (stateful only). Returns `ok`, or `{error, not_joined}`. The membership is the process's, so one view's unsubscribe ends it for every view in it |

## Rendered binaries must be UTF-8

Every target writes UTF-8, but a binary renders as the bytes it holds -- nothing checks
it on the render path (measured too costly; see docs/performance.md). A list is chardata
and is converted (and validated) by `to_bin/1`. The usual non-UTF-8 source is a bare
`<<"café">>`: one byte per character, Latin-1. Write `~"café"` or `<<"café"/utf8>>`, or
convert external Latin-1 data with `unicode:characters_to_binary(Bin, latin1)`.

- A non-ASCII `<<"...">>` that is a **template static** (content or attribute value) is
  folded as UTF-8 by the parse transform and renders correctly.
- Anywhere else in a module using the Arizona headers (`mount/1`, a handler, a helper, a
  `?get` default, a conditional branch value, a hoisted variable, `?raw`, props) it gets
  the compile warning `latin1_binary_literal` -- an error under `warnings_as_errors`. There
  is no suppression option: spell intended bytes as integer segments
  (`<<16#89, "PNG">>`) or a sized string (`<<"\351":8>>`), which do not warn.
- At runtime a refused frame or `?native` value raises/logs `{non_utf8_render, Byte,
  Offset}` (the JSON encoder already refused it; only the name is new). In dev mode an
  HTTP page renders with every value checked (same mount, same bytes), so the failure
  is located at its template line.

See "Output is UTF-8" in [docs/architecture.md](../../docs/architecture.md).

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
element (wrap the value: `fun(Item, Key) -> {li, [{az_key, Key}], [Item]} end`). Over a
**stream** it must be **one** element, too: a keyed op (insert, remove, move, item patch) addresses
an item by the single element carrying its `az_key`, so a sibling root would be orphaned on a
remove and out of order on a move -- put the parts inside the item element. The same 2-arg
callback may iterate a **map**, which diffs like a list (positionally for a single-root item,
else by re-rendering the container), so a list of elements, a text+element fragment or bare text
is legal there (`fun(V, K) -> [{dt, [], [K]}, {dd, [], [V]}] end` in a `dl`). The source is a
runtime value, so this is not a compile error: the transform stamps such a body `multi_root` and
pairing it with a stream raises `stream_each_multi_root` when it renders (or diffs). An
az-nodiff item is exempt, since no keyed op ever addresses it. A one-element list
(`[{li, ...}]`) is taken as its element. Over a stream that root must also carry `az_key`: the
client finds an item by it, so without one every keyed op on the item silently no-ops. This is
not enforced either, because a map diffs positionally and needs no key. When that body is a
**conditional**, the error is `each_stream_body_control_flow` and the fix is different. Every item
shares ONE compiled per-item template, so an item's **outer tag is fixed at compile time** -- that
is what makes per-item diffing cheap (statics once, dynamics per item). The branches may be
entirely different elements; put them inside one stable item element carrying `az_key`
(`fun(Item, Key) -> {li, [{az_key, Key}], [case Item of ... end]} end`), where a conditional child
may return bare element tuples. `ul`/`ol` lose nothing (their child tag was never free), and a
`div` with `display: contents` keeps the wrapper out of layout elsewhere.

A tag chosen at RUNTIME is the other route to the same goal and hits a different
error, `computed_element_tag`: an element's tag is baked into the template's statics
at compile time, which is what lets a later change ship only the values, so there is
no single set of statics to bake. The answer is the same idiom -- put the choice
inside a stable element rather than choosing the element.

**The wrapper is not legal everywhere.** Inside `table`/`tr` the HTML parser foster-parents it out
of the table, and inside `select` Firefox drops it while Chromium keeps it, so the keyed element
lands somewhere the client cannot address and every item op no-ops. Verified in both browsers. A
varying sibling tag is legitimate in exactly those containers (`th`/`td` in a row,
`option`/`optgroup` in a select) and has no workaround today, so `ul`/`ol` is the only container
where "the child tag was never free anyway" actually holds.

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
`-ignore_xref`, so it needn't be exported or otherwise used. One way it is **not** like an
anonymous fun: a named function is top-level, so it closes over nothing, and its body's own
variables are alpha-renamed at the inline site. Hoisting a name the caller also bound (`Side =
?get(side)` in both) is therefore safe -- without the rename the callee's binding degrades into
an equality test against the caller's value, rendering correctly until the two first differ.
A **same-module** explicit ref (`fun ?MODULE:row/1`) resolves to the local body just like
`fun row/1`. Rejected: a
**genuinely remote** reference (`fun other_mod:row/1`, or a variable module `fun M:row/1` --
body not visible to inline, `each_remote_fun_ref`), an **imported** function used as a bare
`fun row/1` (its body lives in another module, so it isn't found -- `each_named_fun_undefined`),
and a **multi-clause** function (can't map to one shared per-item template,
`each_named_fun_multi_clause`; collapse the clauses into a `case` inside the returned element).

A crash inside an inlined callback is attributed to the **outermost** template function -- the
one whose `?html` the whole chain flattened into, which is neither the callee nor necessarily
the function holding the `?each`. Take `render/1`, whose `?each` calls `section/1`, whose own
`?each` calls `row/1`: a failure in `row/1` surfaces as `-render/1-fun-0-` at `row/1`'s line,
and neither `section/1` nor `row/1` is a compiled function any more. Searching the named
function for that line finds nothing, and `arizona_loc` names a third place again (the `?each`
site). Read the line, not the name -- the compiler derives a fun's name from the function it is
written in, so this is not relabelable.

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

## Content-slot lists

A literal list in a content slot that holds a **block** -- an element, a nested
`?html`/`?native`/`?terminal`, an `?each`, a `?stateful`/`?stateless` component,
`?inner_content`, or `?raw`, or a `case`/`if` whose tail can be one -- compiles as a fragment:
each item gets its own slot. So `{ul, [], [[~"Tags: ", ?each(...)]]}` renders and diffs like a
literal mixed fragment, and so does the same list as a conditional tail, a hoisted template that
reads a binding, or a local helper's body. A list of plain values only stays one value slot.

**Escaping follows the list's kind.** Once a list holds a block, its static binaries are
template statics, exactly like the binaries of a children list: they render as written, so
`[~"<hr>", ?inner_content]` emits a real `<hr>`. Its dynamic values are still escaped. A list of
plain values is one value, and every binary in it is escaped. Adding a block to a list therefore
changes how its literal text renders -- write text that must show as text with entities, as in
any children list.

The list must be **literal at the template site**: a list the template cannot see -- a
comprehension of templates, a list bound at runtime, a list nested inside the list -- is still a
single value and fails at render on a template or component in it. A **variable** item bound to
markup that reads no binding (`Hdr = ?html({h1, [], [~"T"]})`, then `[[Hdr, ?each(...)]]`) is a
compile error (`markup_var_in_content_list`) naming the variable: write the markup in the list
itself. A hoisted template that reads a binding is substituted back into the slot, so it is
markup written in the list and is fine. A `?local` in such a list has no element to carry its
descriptor and is a compile error (`local_orphaned`).

An `?each` in such a list is anchored by its own slot markers, which no element carries. A
stream still diffs item by item: its `OP_INSERT`/`OP_MOVE`/`OP_REMOVE`/`OP_ITEM_PATCH` target the
slot's az, which the web client resolves to the markers' parent element, placing and finding
items inside the marker span. A plain-list `?each` re-renders its whole slot on any change; as a
flat sibling -- `{ul, [], [~"Tags: ", ?each(...)]}` -- it stays on the positional per-item path.

In `<title>`/`<textarea>` (text only) a block in content is a compile error
(`markup_in_raw_text`): the browser would show its markup as literal text. A `?raw` there is a
value, alone or in a list.

## Bare element tuples in conditional tails

A control-flow expression in a **content slot** -- `case`, `if`, `begin`, `receive`,
`try`, or `maybe` -- may return a bare element tuple, an element list, or a mixed fragment
(static text/values interleaved with elements) directly from a tail position. The parse
transform compiles each tail into a nested template, exactly as a literal
`?html`/`?native`/`?terminal` there would, inheriting the enclosing render target and the
content context of the slot, so a branch's `<script>` or `<title>` under `<svg>` is foreign
content exactly as when written directly there. A whole-tail wrapper of the enclosing target
around markup (`true -> ?html({title, [], [?get(t)]})` in an element's child) is unwrapped to
that markup first, so the wrapped and bare forms compile identically; a wrapper anywhere else
(bound to a variable, passed as an argument, nested below the tail) still starts its own
template at the root context. No `?html` wrap is needed:

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
slot. A **marker-only** container, where no element carries the slot az at all, is placed the
same way: the client resolves it through its opening marker to the enclosing element and places
within the span.

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

An **`?each` callback is not isolated**: its items render inside the each
slot's dependency scope, so an outer binding read in the callback body
(`?each(fun(I) -> {li, [], [I, ?get(suffix)]} end, ?get(items))`) is a dep
of the each slot. A change to it re-evaluates the each and the per-item diff
patches only the items that changed; a stream reconciles every visible item in
one step instead of draining its queue. The deps are the reads that actually
ran, so a read in one branch, in a helper the callback calls, in an inner
`?each`, or under a computed key (`?get(Key)`) all count. An item-field read
written as `arizona_template:get(text, Item)` counts too, so a view binding of
the same name also re-evaluates the each (harmless: nothing differs, nothing
ships). Reads in an az-nodiff item (or az-nodiff element) never count -- it renders once.

A **stream** item changed by `arizona_stream:update/3` (or kept by a reset) skips
re-evaluating a dynamic only when the parse transform could prove its deps name
every field it read: the dynamic references nothing but the item key outside the
map argument of a tracked `get`/`get_lazy`/`with` in its own closure, and that map
is the item itself (the bare item parameter, or the variable side of a
`#{...} = Item` head) or the view `Bindings`. A plain `maps:get(n, Item)`, a
head-destructured field or sub-map (even read with a tracked `get` -- its deps name
the sub-map's keys, not the item field holding it), a value bound in the callback
body, or a read inside a nested branch template makes that dynamic re-evaluate on
every change of its item -- always correct, just not skipped. Write
`arizona_template:get(n, Item)` (or, when the parameter binds the whole item, name
it `Bindings` and use `?get(n)`) where a wide item's per-field skipping matters. A
dynamic that reads nothing and references no item (a static value, `?connected`, the
key) is kept as-is on an item update.

Reads **hoisted into the render function body** do track: the parse transform
inlines each interpolated variable back into its slot closure, so the `?get`
re-runs inside the dependency bracket.

Because the definition is re-read inside the template, a fun parameter, named fun or
comprehension generator there that rebinds a name the definition reads would make that copy
read the inner binding. That is a compile error (`hoisted_var_captured`) naming both -- `Z =
<<Y/binary, (?get(a))/binary>>` used under `fun(Y) -> ... end` -- so rename the inner binder. A
pattern's map key or binary size reads a variable rather than binding it, and a name bound
inside the definition itself is not one it reads; neither is affected.

```erlang
%% all of these track per-slot (compile-time inlining):
Name = ?get(name),                 ?html({p, [], [Name]}).
User = ?get(user), N = maps:get(name, User), ?html({p, [], [N]}).  %% tracks `user`
Label = case ?get(mode) of dark -> ?get(a); _ -> ?get(b) end, ?html({p, [], [Label]}).
case ?get(mode) of dark -> X = ?get(a); _ -> X = ?get(b) end, ?html({p, [], [X]}).
case ?get(mode) of dark -> N = ?get(n), ?html({p, [], [N]}); _ -> ?html({p, [], []}) end.
```

The last form hoists inside a branch of the render function's result -- any tail of a
`case`/`if`/`begin`/`try`/`maybe`/`receive` there, nested or not (on a live root too). The
read inlines into that branch's templates only.

A hoisted **template constructor** also compiles and tracks:
`Header = ?html({h1, [], [?get(title)]})`, `Items = ?each(...)`, or
`Card = ?stateless(card, #{x => ?get(x)})` bound to a local and used in a content
slot is spliced back and compiled -- a `?html`/`?native`/`?terminal` constructor
additionally gets track touches for the literal keys read inside it (a nested
template's inner reads are otherwise isolated from the slot bracket), so the slot
re-renders when they change. A hoisted `?html` used as a whole `?each` callback
body (`fun(_I) -> Row end`) keeps the per-item element path and its `single_root`
flag.

An `?each` compiles its items for the render target and content context of the
place it appears in the template that renders it. For a hoisted `Items = ?each(...)`
that place is where `Items` is used, not where it is bound: used inside `?native`
its items are native nodes, used directly inside `<svg>` its `<title>` items are
foreign content (markers, diffable), exactly as if the `?each` were written there.
A hoisted value is checked like written code too, so a hoisted `?html` used inside
`?native` is a `cross_target_nesting` compile error. A local element helper's body
is **not** such a place for its arguments: an `?each` passed to a helper (written
in the call, or a hoisted variable) keeps the context of the call site, so
`wrap(Items)` with `wrap(X) -> {svg, [], [X]}` called from HTML content compiles
the items as HTML (an HTML `<title>` is escapable raw text, so markerless). Write
the `?each` inside the `<svg>` element to get SVG treatment.

The invariant behind all of this: a slot tracks only the reads performed **inside
its own closure**. The transform arranges that for a `?get` reaching the slot through
a bare variable (it inlines a fresh read into the closure). It cannot when the value
is bound through a non-bare-var pattern or derived by intermediate computation --
`#{side := Side} = f(?get(k))` compiles the slot to `fun() -> Side end`, which reads
nothing. That slot never updates, and nothing says so: the view re-renders and its
sibling slots move, so it looks like a diff bug. When a slot's value is computed
rather than read, put the read inside the slot (`f(?get(k))`).

Exceptions that stay un-tracked (slot frozen after SSR): a binding destructured
in the head (`render(#{foo := Foo})`) or through any non-bare-var pattern
(`{ok, V} = ?get(...)` -- use `?get(foo)` then plain destructuring), including a
variable a clause pattern binds (`{admin, Name} ->`); a variable bound by a
statement-form `case` some of whose clause heads bind a variable (it is not lifted
to value form); and a rebound variable.

**Template code is a pure function of the bindings.** Everything a slot evaluates
is evaluated lazily: the slot expression itself, the definition of a hoisted
variable the template reads (re-inlined into every slot that uses it, and still run
once in the body), and the arguments of an inlined local element helper
(substituted at each use of the parameter, and not evaluated at all when the
parameter is unused). A slot runs once on first render, then again only when its
own dependencies change, while the render function's body runs on every diff
cycle. So one source expression may run zero, one or several times per render, in
no fixed order, and two slots that read the same variable each compute it
themselves. Keep that code deterministic and free of side effects. A value that
must be generated once and shared -- a unique id pairing a `label` with its
`input`, a timestamp, a random token -- belongs in state: create it in `mount/1` or
`handle_event/3` and read it with `?get`, or derive it from bindings
(`<<"field-", (?get(name))/binary>>`).

The transform rejects the cases it can see (`template_impure_expr`): a send,
`receive`, `make_ref`, `erlang:unique_integer`, a clock read
(`erlang:now`/`timestamp`/`system_time`/`monotonic_time`, `os:system_time`,
`calendar:local_time`/`universal_time`), `put`/`erase`, `spawn*`, or any call into
`rand`, `os`, `ets`, `io` or `logger`, written in a hoisted definition that reads a
binding and is used in a template, or in a helper argument whose parameter is not
used exactly once. A definition that reads no binding is not re-inlined (every slot
sees its one value), a fun that is only constructed is not entered, and calls into
your own functions are not checked.

A tracked read used in a **guard** inside a template (`case Status of active when
Confirming -> ...`, an `if`, a nested fun) is **auto-tracked**, so the slot stays
reactive. A guard cannot hold the `get/2` call (Erlang rejects it as an illegal guard
expression), so the read can't run inside the slot's dependency bracket on its own; the
transform wraps the guard-bearing expression so it first reads (for the `track/1` side
effect) each binding the guards reference, recording them as slot dependencies. The guard
keeps using the captured value, which each diff cycle rebuilds from current bindings, so a
change to a guard binding re-renders the slot. A non-tracked local or a pattern-bound
variable in a guard needs nothing -- there is no binding dependency to record.

The same holds for a tracked read used in a **pattern**: a hoisted variable a pattern tests
(`Sel = ?get(sel)` then `case I of Sel -> ...`, the "selected row" idiom), a map-pattern
key (`#{K := V}`), a binary segment size (`<<P:N/binary, _/binary>>`), or a `maybe`
`Want ?= ...` -- in a `case`/`receive`/`try`/`maybe` clause, a match, a fun head's keys and
sizes, or a generator's keys and sizes. A pattern cannot hold the call either, so the
transform touches each such binding ahead of the expression, and a change to it re-renders
the slot. A hoisted variable is already bound, so in a `case` pattern it is an equality
test; `{Sel, X} -> [Sel, X]` uses the hoisted `Sel` in the body too.

`?get`/`get_lazy`/`with` are for the **view bindings**, not sub-maps: they call
`track/1` regardless of which map they read, so reading a nested map records the
inner key as a spurious top-level dep. The parse transform rejects a tracked read
whose map argument is a local provably holding a value read out of a map -- bound
by `V = ?get(...)`/`get_lazy`/`maps:get`/`map_get`, bound to a value by a map
pattern (`#{user := U}` in a match, a head or a case clause), or an alias of
one -- with `tracked_get_on_non_bindings_map`. Any other local may be the bindings (a parameter,
a `Bindings = bump(Bindings0)` threaded through a helper, a `case`), so it is not
flagged. Read sub-structures with plain `maps:get/2`: `User = ?get(user), Name =
maps:get(name, User)`.

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
expression (so an argument runs once per use of its parameter -- see the purity
contract above), the helper's own body variables are alpha-renamed so a name the caller
also bound cannot collide (its parameters excepted -- those are what the arguments
replace; a `case` pattern inside the element is the shape that would otherwise bite),
and the spliced element compiles like a literal one -- it flattens into
the template, and `?get` reads in the body or the arguments land in the enclosing
slot's dependency bracket, so the slot stays reactive. Same-module explicit calls
(`?MODULE:brand()`) resolve like bare local calls; nested helper calls inline
recursively; a helper call as a whole `?each` callback body
(`fun(I) -> item(I) end`) inlines into the per-item element. The now-orphaned
definition is covered by auto-injected `nowarn_unused_function` / `-ignore_xref`,
mirroring `?each` named-fun refs; crashes are attributed to the enclosing
template function, as for `?each` named refs above. An element-**list** body compiles into a
nested-template child whose literal reads are tracked on the slot -- the same
tracking a literal element-list child gets.

A parameter the body reads in a **pattern or guard** (`mark(Sel, Id) -> {span, [], [case
Id of Sel -> ... end]}`, `if N > Max`, a map key, a binary size, a `?=`) cannot take the
argument expression -- a call is not a pattern. The inlined expression that tests it is
wrapped in a block binding a fresh variable to the argument first (`begin AzArg = <arg>,
case Id of AzArg -> ... end end`), so the pattern compares against the argument's value,
never a fresh binder or a caller variable of the same name, and the argument's reads run in
the slot's bracket and track. A fun head or generator inside the body that rebinds the
parameter's name shadows it as usual. The one place with nowhere to put that binding is the
head or guard of an `?each` callback, which must stay a fun literal: reading a parameter
there is a compile error (`helper_param_in_each_guard`); test it in a `case` inside the item
element instead.

A whole-body `?html(...)` helper is inlined only where its value **is** content -- the slot
expression, an item of a children list or of a content-slot list, or a control-flow tail of
one. Anywhere else (a function argument, a `case` scrutinee, an attribute) the call stays and
returns its template, since splicing the unwrapped element or list there would hand the
consumer a different value. A bare-bodied helper is inlined anywhere: its body is the same
value the call returned.

A callee whose body is not element-shaped (a scalar helper) is untouched: the call
runs inside the slot closure, so its reads fire in-bracket as before. Shapes that
cannot be cleanly inlined are compile errors when the body is a **bare** element
(they previously compiled clean and crashed at first render with
`bad_template_value`): multi-clause (`helper_multi_clause` -- collapse the clauses
into a case inside the element), a guard (`helper_guarded`),
non-variable/repeated/`_` parameters (`helper_params_not_vars`), statements before
the element (`helper_body_not_single_expr`), and recursion (`helper_recursive`); a
parameter read in an `?each` callback's head or guard (`helper_param_in_each_guard`, above)
is rejected whichever form the body takes.
Only a call whose value **is** content (the positions listed above, plus the body of a
list comprehension sitting in one, whose results the slot renders) is rejected: a
function argument or a `case` scrutinee consumes the value as data, and an ordinary
data function returning an atom-tagged tuple (`color(red) -> {rgb, 255, 0, 0}`)
looks the same as an element there, so the call is left to run.
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

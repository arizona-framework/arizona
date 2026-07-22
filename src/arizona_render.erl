-module(arizona_render).
-moduledoc """
Walks compiled templates and produces final rendered output.

Scalar values are escaped, and dynamic attributes rendered, through the
template's render backend (an `arizona_renderer` module carried in the `backend`
field) -- so the output format (HTML, the native JSON wire, terminal ANSI) is the
backend's concern, not this module's. `undefined` backend on a hand-constructed
map means a plain-value-only template that never invokes a backend callback;
templates from the parse transform always carry one.

Two flavors of rendering:

- **Initial render** (`render/1`, `render/2`) -- evaluates dynamics through
  `arizona_eval`, builds an output iolist by zipping statics with values, and
  returns a snapshot the differ can later use to compute updates.
- **SSR render** (`render_to_iolist/1`, `render_to_iolist/2`) -- direct output
  for the very first server response, no snapshot needed because no client is
  connected yet.

The `zip_*` and `fingerprint_payload/1` functions are called by the differ
(`arizona_diff`) and live process (`arizona_live`) to materialize wire
payloads when sending updates over WebSocket.

## Snapshot shape

`render/1,2` and the SSR helpers all build maps of the form:

```
#{s := [binary()],
  d := [{az(), term()}],
  deps => [#{key() => true}],
  diff => false,    %% propagated from az-nodiff templates
  view_id => binary(),
  backend => module()}  %% render backend, propagated from the template
```

## Az-nodiff safety

Templates compiled with `az-nodiff` carry `diff => false` and dynamics with
`Az = undefined`. The diff engine short-circuits on `diff => false` before
ever inspecting individual dynamics, so `undefined` Az values never reach
op-code targets.
""".

-compile({nowarn_redefined_builtin_type, [{dynamic, 0}]}).

-include("arizona.hrl").

-ifdef(TEST).
-include_lib("eunit/include/eunit.hrl").
-endif.

%% --------------------------------------------------------------------
%% API function exports
%% --------------------------------------------------------------------

-export([render/1]).
-export([render/2]).
-export([render_to_iolist/1]).
-export([render_to_iolist/2]).
-export([render_view_to_iolist/2]).
-export([resolve_id/1]).
-export([zip/2]).
-export([zip_item/2]).
-export([zip_or_fp/1]).
-export([zip_list_fp/2]).
-export([zip_stream_fp/3]).
-export([fingerprint_payload/1]).

%% --------------------------------------------------------------------
%% Ignore xref warnings
%% --------------------------------------------------------------------

-ignore_xref([render/1]).
-ignore_xref([render_to_iolist/2]).
-ignore_xref([resolve_id/1]).
-ignore_xref([zip/2]).

%% --------------------------------------------------------------------
%% Types exports
%% --------------------------------------------------------------------

-export_type([layout/0]).
-export_type([static/0]).
-export_type([dynamic/0]).
-export_type([each_list/0]).
-export_type([each_stream/0]).

%% --------------------------------------------------------------------
%% Types definitions
%% --------------------------------------------------------------------

-nominal layout() :: {module(), atom()}.

-nominal static() :: binary().

-nominal each_list() :: #{
    t := 0,
    items := [[{arizona_template:az(), term(), map()}]],
    template := arizona_template:template()
}.

-nominal each_stream() :: #{
    t := 0,
    items := #{term() => [{arizona_template:az(), term(), map()}]},
    order := [term()],
    template := arizona_template:template()
}.

-nominal dynamic() ::
    each_list()
    | each_stream()
    | arizona_template:snapshot()
    | term().

%% --------------------------------------------------------------------
%% API Functions
%% --------------------------------------------------------------------

-doc """
Initial render of a template into HTML and a fresh snapshot.

Evaluates every dynamic, zips the result into the static skeleton, and
returns `{HTML, Snapshot}` so the live process can later diff against the
snapshot.
""".
-spec render(Template) -> {HTML, Snapshot} when
    Template :: arizona_template:template(),
    HTML :: iolist(),
    Snapshot :: arizona_template:snapshot().
render(#{s := Statics, d := Dynamics} = Tmpl) ->
    EvalDynamics = arizona_eval:eval_dynamics(Dynamics),
    Backend = backend(Tmpl),
    HTML = zip(Backend, Statics, [
        arizona_template:unwrap_val(Backend, V)
     || {_Az, V} <:- EvalDynamics
    ]),
    Snap0 = #{s => Statics, d => EvalDynamics},
    Snap = arizona_template:maybe_propagate(Tmpl, Snap0),
    {HTML, Snap}.

-doc """
Initial render that also threads view state through nested stateful children.

Returns `{HTML, Snapshot, NewViews}` -- `NewViews` accumulates the snapshots
of any child views encountered during evaluation.
""".
-spec render(Template, Views) -> {HTML, Snapshot, Views1} when
    Template :: arizona_template:template(),
    Views :: map(),
    HTML :: iolist(),
    Snapshot :: arizona_template:snapshot(),
    Views1 :: map().
render(#{s := Statics, d := Dynamics} = Tmpl, Views0) ->
    {Triples, {_Old, NewViews}} = arizona_eval:eval_dynamics_v(Dynamics, {Views0, #{}}),
    Backend = backend(Tmpl),
    {D, Deps, Vals} = arizona_template:unzip_triples(Backend, Triples),
    HTML = zip(Backend, Statics, Vals),
    Snap0 = #{s => Statics, d => D, deps => Deps},
    Snap = arizona_template:maybe_propagate(Tmpl, Snap0),
    {HTML, Snap, NewViews}.

-doc """
SSR render: produces HTML iolist with no snapshot, used for the first
server response before any client is attached.
""".
-spec render_to_iolist(Template) -> iolist() when
    Template :: arizona_template:template().
render_to_iolist(#{s := Statics, d := Dynamics} = Tmpl) ->
    Backend = backend(Tmpl),
    zip(Backend, Statics, render_ssr_dynamics(Backend, Dynamics)).

-doc """
SSR render for a stateful handler (embedded-component style).

Mounts via `mount/1`, renders, optionally wraps in a layout. Used
by tests that SSR stateful components in isolation; production
HTTP SSR goes through `render_view_to_iolist/2`. `on_mount` is a
route-level concept and is intentionally not honored here.
""".
-spec render_to_iolist(Handler, Opts) -> iolist() when
    Handler :: module(),
    Opts :: arizona_live:route_opts().
render_to_iolist(Handler, Opts) ->
    Bindings0 = maps:get(bindings, Opts, #{}),
    {Bindings, _Resets} = arizona_stateful:call_mount(Handler, Bindings0),
    finish_ssr(Handler, Bindings, Opts).

-doc """
SSR render for a route-level page (the production HTTP path).

Applies the `on_mount` chain, mounts the handler, renders, and optionally
wraps the page output in a layout module. Request data is supplied as
bindings by the caller (via `arizona_middleware:extract/1` middlewares).
Use `render_to_iolist/2` for embedded-component SSR (no `on_mount`).
""".
-spec render_view_to_iolist(Handler, Opts) -> iolist() when
    Handler :: module(),
    Opts :: arizona_live:route_opts().
render_view_to_iolist(Handler, Opts) ->
    Bindings0 = arizona_live:apply_on_mount(
        maps:get(on_mount, Opts, []),
        maps:get(bindings, Opts, #{})
    ),
    {Bindings, _Resets} = arizona_stateful:call_mount(Handler, Bindings0),
    finish_ssr(Handler, Bindings, Opts).

-doc """
Resolves an id-or-template to a binary id.

If the input is already a binary, returns it unchanged. If it is a template,
SSR-renders it and converts the iolist to a binary.
""".
-spec resolve_id(Input) -> binary() when
    Input :: binary() | arizona_template:template().
resolve_id(Id) when is_binary(Id) -> Id;
resolve_id(Tmpl) ->
    {HTML, _Snap} = render(Tmpl),
    iolist_to_binary(HTML).

-doc """
Interleaves the static binary segments with their corresponding rendered
dynamic values to produce the final HTML iolist.

Handles plain values, nested templates, and `each` containers (lists and
streams) recursively.
""".
-spec zip(Statics, Dynamics) -> iolist() when
    Statics :: [static()],
    Dynamics :: [dynamic()].
zip(Statics, Dynamics) ->
    zip(undefined, Statics, Dynamics).

%% Backend-threaded variant: the render backend (an `arizona_renderer` module)
%% escapes interpolated scalar values in `render_dyn/2` through its `escape/1`
%% callback. The backend comes from the template being rendered; a plain value
%% never touches it, so the backend-less `zip/2` above (used only where every
%% value is pre-rendered, e.g. SSR) is harmless. `render/1,2` pass the template's
%% actual backend, so a terminal frame's dynamics are control-char sanitized.
-spec zip(Backend, Statics, Dynamics) -> iolist() when
    Backend :: module() | undefined,
    Statics :: [static()],
    Dynamics :: [dynamic()].
zip(_Backend, [S], []) ->
    [S];
zip(Backend, [S | Statics], [D | Dynamics]) ->
    [S, render_dyn(Backend, D) | zip(Backend, Statics, Dynamics)].

%% Pre-unwrapped value path -- factored out of zip/3's body so zip_stream_item/3
%% (the triple-walking variant) can share the same dispatch without duplicating
%% the case clauses.
render_dyn(Backend, {arizona_esc, V}) ->
    arizona_template:escape_value(Backend, V);
render_dyn(Backend, #{t := ?EACH, items := Items, template := Tmpl}) when is_list(Items) ->
    #{s := ItemS} = Tmpl,
    [zip_stream_item(Backend, ItemS, ItemD) || ItemD <- Items];
render_dyn(Backend, #{t := ?EACH, items := Items, order := Order, template := Tmpl}) ->
    #{s := ItemS} = Tmpl,
    [zip_stream_item(Backend, ItemS, maps:get(K, Items)) || K <- Order];
render_dyn(Backend, #{az_local := _, target := {attr, Name}, v := V}) ->
    Backend:render_attr(Name, V);
render_dyn(Backend, #{az_local := _, v := V}) ->
    %% Escape a content `?local`'s init like any content value: the client owns
    %% the slot via textContent (text, never parsed), so the SSR output must be
    %% the escaped form to match -- and to keep a binding-seeded init from being
    %% an XSS vector. `?local` is HTML-only, so the html escaper applies.
    arizona_template:escape_value(Backend, V);
render_dyn(_Backend, #{s := InnerS, d := InnerD} = Nested) ->
    %% A nested template / child snapshot carries its own backend (a cross-target
    %% child is embedded as its own ?stateful/?stateless), so render its dynamics
    %% through the nested backend, mirroring render_ssr_val/1's nested clause.
    zip_d(backend(Nested), InnerS, InnerD);
render_dyn(_Backend, V) when is_binary(V) ->
    V;
render_dyn(_Backend, V) ->
    arizona_template:to_bin(V).

-doc """
Renders a single each-item snapshot.

If the template has a fingerprint, returns a wire-format map keyed by
`~"f"`/`~"s"`/`~"d"`. Otherwise returns a plain HTML binary.
""".
-spec zip_item(Template, Dynamics) -> map() | binary() when
    Template :: map(),
    Dynamics :: [{arizona_template:az(), term(), map()}].
zip_item(#{f := F, s := S} = Tmpl, D) ->
    Backend = backend(Tmpl),
    #{
        ~"f" => F,
        ~"s" => S,
        ~"d" => [render_fp_val(Backend, V) || {_Az, V, _Deps} <:- D]
    };
zip_item(#{s := S} = Tmpl, D) ->
    flat_zip(backend(Tmpl), S, [V || {_Az, V, _Deps} <:- D]).

-doc """
Renders a snapshot to a wire-format fingerprint payload (if `f` present)
or to a plain HTML binary.
""".
-spec zip_or_fp(Snapshot) -> map() | binary() when
    Snapshot :: map().
zip_or_fp(#{f := _} = Snap) ->
    fingerprint_payload(Snap);
zip_or_fp(#{s := S, d := D} = Snap) ->
    flat_zip(backend(Snap), S, [V || {_Az, V} <:- D]).

-doc """
Renders a list of item snapshots against a template, producing either a
fingerprinted wire payload (if the template has `f`) or a flat HTML binary.
""".
-spec zip_list_fp(Template, Items) -> map() | binary() when
    Template :: map(),
    Items :: [[{arizona_template:az(), term(), map()}]].
zip_list_fp(#{f := F, s := S, t := T} = Tmpl, ItemsList) ->
    Backend = backend(Tmpl),
    #{
        ~"t" => T,
        ~"f" => F,
        ~"s" => S,
        ~"d" => [
            [render_fp_val(Backend, V) || {_Az, V, _Deps} <:- D]
         || D <- ItemsList
        ]
    };
zip_list_fp(#{s := S} = Tmpl, ItemsList) ->
    Backend = backend(Tmpl),
    iolist_to_binary([zip_stream_item(Backend, S, D) || D <- ItemsList]).

-doc """
Renders a stream's visible items (looked up from `Items` by `Order` keys)
against a template, with the same fingerprint-vs-flat-HTML branching as
`zip_list_fp/2`.
""".
-spec zip_stream_fp(Template, Items, Order) -> map() | binary() when
    Template :: map(),
    Items :: #{term() => [{arizona_template:az(), term(), map()}]},
    Order :: [term()].
zip_stream_fp(#{f := F, s := S, t := T} = Tmpl, Items, Order) ->
    Backend = backend(Tmpl),
    #{
        ~"t" => T,
        ~"f" => F,
        ~"s" => S,
        ~"d" => [
            [
                render_fp_val(Backend, V)
             || {_Az, V, _Deps} <:- maps:get(K, Items)
            ]
         || K <- Order
        ]
    };
zip_stream_fp(#{s := S} = Tmpl, Items, Order) ->
    Backend = backend(Tmpl),
    iolist_to_binary([zip_stream_item(Backend, S, maps:get(K, Items)) || K <- Order]).

-doc """
Builds the wire-format fingerprint payload for a snapshot.

Snapshots with an `f` field become a `#{~"f", ~"s", ~"d"}` map;
plain snapshots fall back to a HTML binary.
""".
-spec fingerprint_payload(Snapshot) -> map() | binary() when
    Snapshot :: map().
fingerprint_payload(#{f := F, s := S, d := D} = Snap) ->
    Backend = backend(Snap),
    #{
        ~"f" => F,
        ~"s" => S,
        ~"d" => [render_fp_val(Backend, V) || {_Az, V} <:- D]
    };
fingerprint_payload(#{s := S, d := D} = Snap) ->
    flat_zip(backend(Snap), S, [V || {_Az, V} <:- D]).

%% --------------------------------------------------------------------
%% Internal functions
%% --------------------------------------------------------------------

%% The render backend (an `arizona_renderer` module) a template/snapshot was
%% compiled for, threaded through rendering so values escape via the backend's
%% callbacks. `undefined` when a hand-constructed map carries none -- such a map
%% has only plain (non-escapable, non-attribute) values, which never invoke the
%% backend; templates from the parse transform always carry one.
backend(Map) -> maps:get(backend, Map, undefined).

%% Render a fingerprint-less snapshot's statics + dynamic values to a flat binary
%% -- the shared no-`f` fallback used by zip_item/zip_or_fp/fingerprint_payload/
%% render_fp_val. Callers extract the values from their 2- or 3-tuple dynamics.
flat_zip(Backend, Statics, Vals) ->
    iolist_to_binary(zip(Backend, Statics, [arizona_template:unwrap_val(Backend, V) || V <- Vals])).

finish_ssr(Handler, Bindings, Opts) ->
    PageTmpl = arizona_stateful:call_render(Handler, Bindings),
    Backend = backend(PageTmpl),
    PageHTML = zip(
        Backend, maps:get(s, PageTmpl), render_ssr_dynamics(Backend, maps:get(d, PageTmpl))
    ),
    Layouts = maps:get(layouts, Opts, []),
    apply_layouts(Layouts, PageHTML, Bindings).

%% Wraps `Inner` with each layer. List is outermost-first, so the page
%% HTML ends up nested inside every layer in the order given:
%% `apply_layouts([Root, Sub], Page, _)` → `Root(Sub(Page))`.
apply_layouts([], Inner, _Bindings) ->
    Inner;
apply_layouts([{Mod, Fun} | Rest], Inner, Bindings) ->
    Wrapped = apply_layouts(Rest, Inner, Bindings),
    Tmpl = Mod:Fun(Bindings#{inner_content => Wrapped}),
    render_to_iolist(Tmpl).

%% Triple walker -- like zip/3 but consumes `[{Az, V, Deps}]` directly
%% (the snapshot shape `arizona_eval:render_list_items_simple/2` and
%% friends produce). Inlines unwrap_val/2's `{attr, Name, V}` case via
%% render_v/2, eliminating the per-item LC walk that previously
%% preceded zip.
zip_stream_item(_Backend, [S], []) ->
    [S];
zip_stream_item(Backend, [S | Statics], [{_Az, V, _Deps} | DRest]) ->
    [S, render_v(Backend, V) | zip_stream_item(Backend, Statics, DRest)].

%% Pair walker -- like zip_stream_item/3 but consumes `[{Az, V}]` 2-tuples
%% (the snapshot d-list shape used by nested `?stateful`/`?stateless`
%% templates). Used by render_dyn/2's `#{s, d}` clause to skip the LC
%% walk that previously extracted values for zip.
zip_d(_Backend, [S], []) ->
    [S];
zip_d(Backend, [S | Statics], [{_Az, V} | DRest]) ->
    [S, render_v(Backend, V) | zip_d(Backend, Statics, DRest)].

render_v(Backend, {attr, Name, V}) -> Backend:render_attr(Name, V);
render_v(Backend, V) -> render_dyn(Backend, V).

%% Az is ignored during SSR (only used for diff targeting), so `undefined` Az
%% from az-nodiff templates flows through harmlessly. `Backend` is the enclosing
%% template's render backend, used to escape scalar leaves and render attributes.
render_ssr_dynamics(Backend, Dynamics) ->
    [render_ssr_one(Backend, D) || D <- Dynamics].

render_ssr_one(Backend, {_Az, {attr, Name, Fun}, _Loc}) when is_function(Fun, 0) ->
    render_ssr_attr(Backend, Name, Fun());
render_ssr_one(Backend, {Az, Spec, {Mod, Line}}) ->
    try
        arizona_template:scope_slot(Az, render_ssr_val(Backend, Spec))
    catch
        %% Already wrapped by a nested render -- preserve the deepest location,
        %% which is closest to the actual failure.
        Class:{arizona_loc, _, _} = Reason:ST ->
            erlang:raise(Class, Reason, ST);
        Class:Reason:ST ->
            erlang:raise(Class, {arizona_loc, {Mod, Line}, Reason}, ST)
    end;
render_ssr_one(Backend, {_Az, {attr, Name, Fun}}) when is_function(Fun, 0) ->
    render_ssr_attr(Backend, Name, Fun());
render_ssr_one(Backend, {Az, Spec}) ->
    arizona_template:scope_slot(Az, render_ssr_val(Backend, Spec)).

render_ssr_val(Backend, {esc, Fun}) when is_function(Fun, 0) ->
    %% A value interpolation: escape the rendered scalar bytes; nested
    %% templates/descriptors come back as snapshots (their inner dynamics carry
    %% their own marks), so pass those through untouched.
    case render_ssr_val(Backend, Fun()) of
        Bin when is_binary(Bin) -> arizona_template:escape_value(Backend, Bin);
        Other -> Other
    end;
render_ssr_val(Backend, Fun) when is_function(Fun, 0) ->
    render_ssr_val(Backend, Fun());
render_ssr_val(_Backend, #{t := ?EACH, source := Items, template := Tmpl}) when is_list(Items) ->
    ItemSnaps = arizona_eval:render_list_items_simple(Items, Tmpl),
    #{t => ?EACH, items => ItemSnaps, template => Tmpl};
render_ssr_val(_Backend, #{
    t := ?EACH,
    source := #stream{
        items = ItemsMap,
        order = Order,
        limit = Limit
    },
    template := Tmpl
}) ->
    VKeys = arizona_template:visible_keys(Order, Limit),
    ItemSnaps = arizona_eval:render_stream_items_simple(VKeys, ItemsMap, Tmpl),
    #{t => ?EACH, items => ItemSnaps, order => VKeys, template => Tmpl};
render_ssr_val(_Backend, #{t := ?EACH, source := Source, template := Tmpl}) when is_map(Source) ->
    ItemSnaps = arizona_eval:render_map_items_simple(Source, Tmpl),
    #{t => ?EACH, items => ItemSnaps, template => Tmpl};
render_ssr_val(Backend, #{stateful := H, props := Props}) ->
    {B1, _Resets} = arizona_stateful:call_mount(H, Props),
    Snap = render_ssr_val(Backend, arizona_stateful:call_render(H, B1)),
    %% Mark the child as a view boundary (mirroring the live path's
    %% make_child_snap `view_id`) so a scope_slot pass over an enclosing
    %% stateless parent skips it -- a stateful child's inner az ids are resolved
    %% via its own view id, not the parent slot, so they must stay unscoped and
    %% match the live snapshot. Its own stateless grandchildren still get scoped
    %% when this subtree is rendered live.
    Snap#{view_id => maps:get(id, Props)};
render_ssr_val(Backend, #{callback := Callback, props := Props}) ->
    %% A stateless child renders to a plain snapshot map; render_ssr_one (which
    %% knows the slot az) namespaces its inner az ids via scope_slot -- repeated
    %% same-function stateless renders otherwise share one set of ids and collide
    %% on the client (see arizona_template).
    render_ssr_val(Backend, Callback(Props));
render_ssr_val(Backend, #{az_local := _, target := {attr, Name}, v := V}) ->
    render_ssr_attr(Backend, Name, V);
render_ssr_val(Backend, #{az_local := _, v := V}) ->
    %% Escape the content `?local` init to match the client's text-node semantics
    %% (see render_dyn/2); a raw splice here is an XSS vector for a seeded init.
    arizona_template:escape_value(Backend, V);
render_ssr_val(_Backend, #{s := Statics, d := Dynamics} = Tmpl) ->
    %% A nested template carries its own backend (cross-target children are
    %% embedded as their own ?stateful/?stateless), so render its dynamics
    %% through the nested backend rather than the enclosing one.
    NestedBackend = backend(Tmpl),
    Snap0 = #{
        s => Statics,
        d => [
            {arizona_template:dyn_az(D), render_ssr_one(NestedBackend, D)}
         || D <- Dynamics
        ]
    },
    arizona_template:maybe_propagate(Tmpl, Snap0);
render_ssr_val(_Backend, Val) ->
    arizona_template:to_bin(Val).

render_ssr_attr(Backend, Name, Val) ->
    Backend:render_attr(Name, Val).

render_fp_val(Backend, {arizona_esc, V}) ->
    %% The fingerprint (initial hydration) payload is the server-rendered output,
    %% so a scalar content value is escaped through the backend here -- HTML
    %% entity-escapes, native's escape/1 is the identity (raw). This is unlike the
    %% live diff, where `make_op` sends the value raw for the client to text-node.
    arizona_template:escape_value(Backend, V);
render_fp_val(Backend, {attr, Name, V}) ->
    %% One clause for every backend: HTML emits ` Name="Escaped"`, native strips
    %% the name (baked into the static) and stringifies the value -- both via the
    %% backend's render_attr/2, so no backend is named here.
    Backend:render_attr(Name, V);
render_fp_val(Backend, #{az_local := _, target := {attr, Name}, v := V}) ->
    Backend:render_attr(Name, V);
render_fp_val(Backend, #{az_local := _, v := V}) ->
    %% Escape the content `?local` init to match the client's text-node semantics
    %% (see render_dyn/2). `?local` is HTML-only (compile-rejected for native), so
    %% the enclosing backend is always HTML here.
    arizona_template:escape_value(Backend, V);
render_fp_val(_Backend, #{f := _} = Nested) ->
    fingerprint_payload(Nested);
render_fp_val(Backend, #{s := S, d := D}) ->
    flat_zip(Backend, S, [V || {_Az, V} <:- D]);
render_fp_val(Backend, #{t := ?EACH, items := Items, template := #{f := F, t := T, s := S}}) when
    is_list(Items)
->
    #{
        ~"t" => T,
        ~"f" => F,
        ~"s" => S,
        ~"d" => [
            [render_fp_val(Backend, V) || {_Az, V, _Deps} <:- D]
         || D <- Items
        ]
    };
render_fp_val(Backend, #{t := ?EACH, items := Items, template := #{s := S}}) when
    is_list(Items)
->
    [flat_zip(Backend, S, [V || {_Az, V, _Deps} <:- D]) || D <- Items];
render_fp_val(Backend, #{
    t := ?EACH,
    items := Items,
    order := Order,
    template := #{f := F, t := T, s := S}
}) ->
    #{
        ~"t" => T,
        ~"f" => F,
        ~"s" => S,
        ~"d" => [
            [
                render_fp_val(Backend, V)
             || {_Az, V, _Deps} <:- maps:get(K, Items)
            ]
         || K <- Order
        ]
    };
render_fp_val(Backend, #{
    t := ?EACH,
    items := Items,
    order := Order,
    template := #{s := S}
}) ->
    [iolist_to_binary(zip_stream_item(Backend, S, maps:get(K, Items))) || K <- Order];
render_fp_val(_Backend, V) when is_list(V) ->
    iolist_to_binary(V);
render_fp_val(_Backend, V) ->
    arizona_template:to_bin(V).

-ifdef(TEST).

render_ssr_one_location_wrapping_test() ->
    Dyn = {~"0", fun() -> {} end, {test_mod, 99}},
    ?assertError(
        {arizona_loc, {test_mod, 99}, {bad_template_value, {}}},
        render_ssr_one(arizona_html, Dyn)
    ).

render_ssr_one_3tuple_text_test() ->
    Dyn = {~"0", fun() -> ~"hello" end, {test_mod, 10}},
    ?assertEqual(~"hello", render_ssr_one(arizona_html, Dyn)).

render_ssr_one_3tuple_attr_test() ->
    Dyn = {~"0", {attr, ~"class", fun() -> ~"box" end}, {test_mod, 20}},
    ?assertEqual(~" class=\"box\"", render_ssr_one(arizona_html, Dyn)).

render_ssr_one_2tuple_bad_value_test() ->
    Dyn = {~"0", fun() -> {} end},
    ?assertError({bad_template_value, {}}, render_ssr_one(arizona_html, Dyn)).

-endif.

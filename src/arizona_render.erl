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
`Az = undefined`. The diff engine never evaluates such a template's own
dynamics once rendered (a root short-circuits; a slot keeps its first render
while it holds the same az-nodiff element), so `undefined` Az values never reach
op-code targets.
""".

-compile({nowarn_redefined_builtin_type, [{dynamic, 0}]}).
%% render_view/2 is render_view/3 with no template fun: inlined with the literal
%% `none`, the compiler folds page_template/2 away, so production pays no call
%% for the shared sequence.
-compile({inline, [render_view/3, page_template/2]}).

-include("arizona.hrl").

%% Set while a layout layer renders (see render_layout/3).
-define(IN_LAYOUT, '$arizona_in_layout').

%% Set whenever a fingerprint wire payload is built in this process (see
%% note_fp_payload/0 / drain_fp_note/0).
-define(FP_NOTE, '$arizona_fp_payload').

-ifdef(TEST).
-include_lib("eunit/include/eunit.hrl").
-endif.

%% --------------------------------------------------------------------
%% API function exports
%% --------------------------------------------------------------------

-export([render/1]).
-export([render/2]).
-export([render_snapshot/2]).
-export([snapshot_html/1]).
-export([render_to_iolist/1]).
-export([render_to_iolist/2]).
-export([render_view_to_iolist/2]).
-export([render_view/2]).
-export([render_view/3]).
-export([current_layout/0]).
-export([resolve_id/1]).
-export([zip/2]).
-export([zip_item/2]).
-export([zip_or_fp/1]).
-export([zip_list_fp/2]).
-export([zip_stream_fp/3]).
-export([fingerprint_payload/1]).
-export([drain_fp_note/0]).
-export([format_error/2]).

%% --------------------------------------------------------------------
%% Ignore xref warnings
%% --------------------------------------------------------------------

-ignore_xref([render/1]).
-ignore_xref([render_to_iolist/2]).
-ignore_xref([resolve_id/1]).
-ignore_xref([zip/2]).
-ignore_xref([format_error/2]).

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
    %% `zip_d/3` walks the `{Az, V}` pairs directly. Unwrapping them into a list of
    %% values first and zipping that built an N-element list per template for nothing:
    %% `render_v/2` makes the same call `unwrap_val/2` would (`render_attr/2` for an
    %% attribute, whose result is a binary that `render_dyn/2` hands straight back).
    HTML = zip_d(Backend, Statics, EvalDynamics),
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
render(#{s := Statics} = Tmpl, Views0) ->
    {Triples, NewViews} = eval_triples(Tmpl, Views0),
    {HTML, D, Deps} = zip_triples(backend(Tmpl), Statics, Triples),
    {HTML, snapshot(Tmpl, D, Deps), NewViews}.

-doc """
Initial render without the document: the snapshot and child views of
`render/2`, minus the HTML.

A live mount ships the fingerprint payload (`fingerprint_payload/1`) whenever the
root carries a fingerprint, which is always for a compiled template, so the HTML
`render/2` builds alongside would be thrown away. `snapshot_html/1` builds it
from the snapshot for the rare root that has none.
""".
-spec render_snapshot(Template, Views) -> {Snapshot, Views1} when
    Template :: arizona_template:template(),
    Views :: map(),
    Snapshot :: arizona_template:snapshot(),
    Views1 :: map().
render_snapshot(Tmpl, Views0) ->
    {Triples, NewViews} = eval_triples(Tmpl, Views0),
    {D, Deps} = arizona_template:split_triples(Triples),
    {snapshot(Tmpl, D, Deps), NewViews}.

eval_triples(#{d := Dynamics}, Views0) ->
    {Triples, {_Old, NewViews}} = arizona_eval:eval_dynamics_v(Dynamics, {Views0, #{}}),
    {Triples, NewViews}.

snapshot(#{s := Statics} = Tmpl, D, Deps) ->
    arizona_template:maybe_propagate(Tmpl, #{s => Statics, d => D, deps => Deps}).

-doc """
The document `render/2` would have returned for `Snapshot`: the same walk over
the same values, so the bytes are identical.
""".
-spec snapshot_html(Snapshot) -> iolist() when
    Snapshot :: arizona_template:snapshot().
snapshot_html(#{s := Statics, d := D} = Snap) ->
    zip_d(backend(Snap), Statics, D).

%% One pass over statics x triples: emits the HTML skeleton while splitting
%% each triple into the snapshot's `{Az, V}` pair and its deps. Replaces an
%% unzip into three lists plus a second zip walk over one of them.
%% `render_v/2` is `unwrap_val/2` fused with `render_dyn/2`: the one shape
%% unwrap rewrote (`{attr, Name, V}` to its rendered binary) it renders
%% directly, and a binary passes through `render_dyn/2` verbatim.
zip_triples(_Backend, [S], []) ->
    {[S], [], []};
zip_triples(Backend, [S | Statics], [{Az, V, Deps} | TRest]) ->
    {HTML, DRest, DepsRest} = zip_triples(Backend, Statics, TRest),
    {[S, render_v(Backend, V) | HTML], [{Az, V} | DRest], [Deps | DepsRest]}.

-doc """
SSR render: produces HTML iolist with no snapshot, used for the first
server response before any client is attached.
""".
-spec render_to_iolist(Template) -> iolist() when
    Template :: arizona_template:template().
render_to_iolist(#{s := Statics, d := Dynamics} = Tmpl) ->
    Backend = backend(Tmpl),
    zip_ssr(Backend, Statics, Dynamics).

-doc """
SSR render for a stateful handler (embedded-component style).

Mounts via `mount/1`, renders, optionally wraps in a layout. Used
by tests that SSR stateful components in isolation; production
HTTP SSR goes through `render_view/2`. `on_mount` is a
route-level concept and is intentionally not honored here.
""".
-spec render_to_iolist(Handler, Opts) -> iolist() when
    Handler :: module(),
    Opts :: arizona_live:route_opts().
render_to_iolist(Handler, Opts) ->
    Bindings0 = maps:get(bindings, Opts, #{}),
    {Bindings, _Resets} = arizona_stateful:call_mount(Handler, Bindings0),
    {_Backend, Page} = finish_page(
        arizona_stateful:call_render(Handler, Bindings), Bindings, Opts
    ),
    Page.

-doc """
SSR render for a route-level page, returning the page alone (static
generation; the HTTP path uses `render_view/2`, which also names its media type).

Applies the `on_mount` chain, mounts the handler, renders, and optionally
wraps the page output in a layout module. Request data is supplied as
bindings by the caller (via `arizona_middleware:extract/1` middlewares).
Use `render_to_iolist/2` for embedded-component SSR (no `on_mount`).
""".
-spec render_view_to_iolist(Handler, Opts) -> iolist() when
    Handler :: module(),
    Opts :: arizona_live:route_opts().
render_view_to_iolist(Handler, Opts) ->
    {_ContentType, Page} = render_view(Handler, Opts),
    Page.

-doc """
Like `render_view_to_iolist/2`, paired with the document's media type: the
outermost rendered template's backend answers it (`content_type/0`), so a route
serving a `?native` view answers with JSON and an HTML one with HTML.
""".
-spec render_view(Handler, Opts) -> {ContentType, iolist()} when
    Handler :: module(),
    Opts :: arizona_live:route_opts(),
    ContentType :: binary().
render_view(Handler, Opts) ->
    render_view(Handler, Opts, none).

-doc """
`render_view/2` with the page's template passed through `TmplFun` between its
`render/1` and the walk: the one mount sequence for both. `render_view/2` passes
`none` (the template as it is); `arizona_utf8:render_view/2` (dev mode) passes a wrap that checks
every value the walk evaluates, so the page still mounts once.
""".
-spec render_view(Handler, Opts, TmplFun) -> {ContentType, iolist()} when
    Handler :: module(),
    Opts :: arizona_live:route_opts(),
    TmplFun :: none | fun((arizona_template:template()) -> arizona_template:template()),
    ContentType :: binary().
render_view(Handler, Opts, TmplFun) ->
    Bindings0 = arizona_live:apply_on_mount(
        maps:get(on_mount, Opts, []),
        maps:get(bindings, Opts, #{})
    ),
    {Bindings, _Resets} = arizona_stateful:call_mount(Handler, Bindings0),
    PageTmpl = page_template(TmplFun, arizona_stateful:call_render(Handler, Bindings)),
    {Backend, Page} = finish_page(PageTmpl, Bindings, Opts),
    {content_type(Backend), Page}.

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
render_dyn(_Backend, {ssr_rendered, Rendered}) ->
    %% Already rendered, by render_ssr_val/2's each clauses (list, stream and map).
    Rendered;
render_dyn(Backend, {arizona_esc, V}) ->
    arizona_template:escape_marked(Backend, V);
render_dyn(Backend, #{t := ?EACH, items := Items, template := Tmpl}) when is_list(Items) ->
    #{s := ItemS} = Tmpl,
    join_items(Backend, [zip_stream_item(Backend, ItemS, ItemD) || ItemD <- Items]);
render_dyn(Backend, #{t := ?EACH, items := Items, order := Order, template := Tmpl}) ->
    #{s := ItemS} = Tmpl,
    join_items(Backend, [zip_stream_item(Backend, ItemS, maps:get(K, Items)) || K <- Order]);
render_dyn(Backend, #{az_local := _, target := {attr, Name}, v := V}) ->
    Backend:render_attr(Name, V);
render_dyn(Backend, #{az_local := _, v := V}) ->
    %% Escape a content `?local`'s init like any content value: the client owns
    %% the slot via textContent (text, never parsed), so the SSR output must be
    %% the escaped form to match -- and to keep a binding-seeded init from being
    %% an XSS vector. `?local` is HTML-only, so the html escaper applies.
    arizona_template:escape_value(Backend, V);
render_dyn(Backend, #{s := InnerS, d := InnerD} = Nested) ->
    %% Mirrors render_ssr_val/2's nested clause, including the same-target check.
    NestedBackend = backend(Nested),
    ok = assert_same_target(Backend, NestedBackend),
    zip_d(NestedBackend, InnerS, InnerD);
render_dyn(_Backend, V) when is_binary(V) ->
    V;
render_dyn(Backend, V) ->
    text_leaf(Backend, arizona_template:to_bin(V)).

%% An unescaped scalar (a trusted value, or one from a slot that is not escaped)
%% is spliced as a text child: verbatim for HTML, a JSON string for native.
text_leaf(undefined, Bin) ->
    Bin;
text_leaf(Backend, Bin) ->
    Backend:text_child(Bin).

join_items(undefined, Items) ->
    Items;
join_items(Backend, Items) ->
    Backend:each_items(Items).

-doc """
Renders a single each-item snapshot.

If the template has a fingerprint, returns a wire-format map keyed by
`~"f"`/`~"s"`/`~"d"`. Otherwise returns a plain HTML binary.
""".
-spec zip_item(Template, Dynamics) -> map() | binary() when
    Template :: map(),
    Dynamics :: [{arizona_template:az(), term(), map()}].
zip_item(#{f := F, s := S} = Tmpl, D) ->
    ok = note_fp_payload(),
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
Renders a list of item snapshots against a template as a fingerprinted wire
payload -- a `~"t"`/`~"f"`/`~"s"`/`~"d"` map, `~"d"` being one dynamics list
per item.

A `t:arizona_template:each_template/0` declares `f` and `t` required and the
parse transform is its only producer, so both are always present. There is
deliberately **no** binary fallback for a template missing them: the four
callers in `arizona_diff` put the result straight into `[?OP_TEXT, Az, Payload]`
and the client decides text-vs-markup purely by payload type (`isHtml =
typeof op[2] !== 'string'`). A map is an object and gets parsed as HTML; a
binary is a *string* on the wire and would be written into a TEXT NODE, so the
whole list would show up as visibly escaped markup with nothing raising
anywhere. A malformed template fails loudly at the match instead.
""".
-spec zip_list_fp(Template, Items) -> map() when
    Template :: arizona_template:each_template(),
    Items :: [[{arizona_template:az(), term(), map()}]].
zip_list_fp(#{f := F, s := S, t := T} = Tmpl, ItemsList) ->
    ok = note_fp_payload(),
    Backend = backend(Tmpl),
    #{
        ~"t" => T,
        ~"f" => F,
        ~"s" => S,
        ~"d" => [
            [render_fp_val(Backend, V) || {_Az, V, _Deps} <:- D]
         || D <- ItemsList
        ]
    }.

-doc """
Renders a stream's visible items (looked up from `Items` by `Order` keys)
against a template, producing the same fingerprinted wire payload as
`zip_list_fp/2` -- and, for the same reason, only that shape.
""".
-spec zip_stream_fp(Template, Items, Order) -> map() when
    Template :: arizona_template:each_template(),
    Items :: #{term() => [{arizona_template:az(), term(), map()}]},
    Order :: [term()].
zip_stream_fp(#{f := F, s := S, t := T} = Tmpl, Items, Order) ->
    ok = note_fp_payload(),
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
    }.

-doc """
Builds the wire-format fingerprint payload for a snapshot.

Snapshots with an `f` field become a `#{~"f", ~"s", ~"d"}` map;
plain snapshots fall back to a HTML binary.
""".
-spec fingerprint_payload(Snapshot) -> map() | binary() when
    Snapshot :: map().
fingerprint_payload(#{f := F, s := S, d := D} = Snap) ->
    ok = note_fp_payload(),
    Backend = backend(Snap),
    #{
        ~"f" => F,
        ~"s" => S,
        ~"d" => [render_fp_val(Backend, V) || {_Az, V} <:- D]
    };
fingerprint_payload(#{s := S, d := D} = Snap) ->
    flat_zip(backend(Snap), S, [V || {_Az, V} <:- D]).

-doc """
True when a fingerprint wire payload was built in this process since the last
drain, clearing the note. `arizona_live` asks this after a diff to skip the
`dedup_fps/2` walk outright for the dominant all-scalar frame -- a frame with
no `~"f"` payload has nothing to strip and no fingerprint to record. Every
producer of such a payload (`zip_item/2`, `zip_list_fp/2`, `zip_stream_fp/3`,
`fingerprint_payload/1`; the `render_fp_val/2` constructors run only inside
them) sets the note, so a missed strip is impossible; the failure direction is
a STALE note (a payload built outside a drain bracket, e.g. at mount), which
merely walks one frame unnecessarily.
""".
-spec drain_fp_note() -> boolean().
drain_fp_note() ->
    case erase(?FP_NOTE) of
        undefined -> false;
        true -> true
    end.

note_fp_payload() ->
    put(?FP_NOTE, true),
    ok.

-doc """
Formats the render errors: a `?stateful` in a layout, a child or a layout
of another render target.

Picked up by `erl_error:format_exception/3` via the `error_info`
annotation, so the dev error page (and crash log) reads a sentence
naming the offending layout and the way out.
""".
%% The macro that spells a backend's target, asked of the backend rather than
%% mapped here -- a registry in this module would have to be edited for every new
%% backend, and would be a second place for the answer to drift from.
macro_of(Backend) ->
    [$?, atom_to_list(Backend:target())].

%% A nested template must render through the same target as its parent. Mixing
%% them produces a payload no client can read, and it produced one silently: the
%% compile-time `cross_target_nesting` guard only sees literal nesting, and its
%% own message recommends the child-module route that lands here.
assert_same_target(Backend, Backend) ->
    ok;
assert_same_target(_Parent, undefined) ->
    %% Not every nested snapshot records a backend (`backend/1` defaults to
    %% `undefined`); an unrecorded one is inherited, not a mismatch.
    ok;
assert_same_target(undefined, _Child) ->
    ok;
assert_same_target(Parent, Child) ->
    error({cross_target_child, Parent, Child}, none, [{error_info, #{module => ?MODULE}}]).

-spec format_error(Reason, Stacktrace) -> ErrorInfo when
    Reason :: term(),
    Stacktrace :: [tuple()],
    ErrorInfo :: #{general := iolist()}.
format_error({cross_target_child, Parent, Child}, _ST) ->
    #{
        general => io_lib:format(
            "a ~s child was embedded in a ~s template. The render targets emit "
            "incompatible statics -- HTML bytes spliced into a ~s payload cannot "
            "be parsed by the client -- so a template tree must use one target "
            "throughout. The compile-time check only sees targets nested in ONE "
            "template; reached through a ?stateful/?stateless child module the "
            "mismatch is invisible until render, which is here. Give the child "
            "the same target as its parent, or render it from a separate tree.",
            [macro_of(Child), macro_of(Parent), macro_of(Parent)]
        )
    };
format_error({cross_target_layout, Mod, Fun, LayoutBackend, PageBackend}, _ST) ->
    #{
        general => io_lib:format(
            "layout ~s:~s/1 is a ~s template but wraps a ~s page. The render "
            "targets emit incompatible output, so the wrapped document is one no "
            "client can parse. A route's layouts must use the page's target: give "
            "the page its own layouts, or none.",
            [Mod, Fun, macro_of(LayoutBackend), macro_of(PageBackend)]
        )
    };
format_error({stateful_in_layout, Mod, Fun, Handler}, _ST) ->
    #{
        general => io_lib:format(
            "layout ~s:~s/1 embeds ?stateful(~s, ...), but a layout renders once "
            "at SSR and never joins the live view tree, so the component would "
            "emit an az-view marker for a view the server never registers and "
            "every event under it would be dropped. Use ?stateless for layout "
            "chrome; chrome that must stay live across navigation belongs in a "
            "view the routes share, linked with az_patch.",
            [Mod, Fun, Handler]
        )
    }.

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

%% Returns the rendered document with the backend of its outermost template that
%% records one (a layout, else the page), which is what the document as a whole
%% is. A layout with a backend must share the page's (assert_layout_target/4), and
%% a backendless one (`?inner_content` returned bare) inherits the inner backend.
page_template(none, PageTmpl) -> PageTmpl;
page_template(TmplFun, PageTmpl) -> TmplFun(PageTmpl).

finish_page(PageTmpl, Bindings, Opts) ->
    Backend = backend(PageTmpl),
    PageHTML = zip_ssr(Backend, maps:get(s, PageTmpl), maps:get(d, PageTmpl)),
    Layouts = maps:get(layouts, Opts, []),
    apply_layouts(Layouts, Backend, {Backend, PageHTML}, Bindings).

%% A hand-built template carries no backend; it renders as the default target.
content_type(undefined) -> arizona_html:content_type();
content_type(Backend) -> Backend:content_type().

%% Wraps `Inner` with each layer. List is outermost-first, so the page
%% HTML ends up nested inside every layer in the order given:
%% `apply_layouts([Root, Sub], Backend, Page, _)` → `Root(Sub(Page))`.
%%
%% The inner content is handed over as a single-static, no-dynamics template --
%% which is what finished output *is* -- so the layout's `?inner_content` slot
%% splices it verbatim: `zip_d/3` walks statics straight into the output iolist,
%% no copy, no decode. As a bare list it would instead reach
%% `arizona_template:to_bin/1`, whose list clause decodes chardata with
%% `unicode:characters_to_binary/1` -- copying and full-UTF-8-validating the
%% entire page once per layer (twice for the nested `[Root, Section]` form), and
%% turning a single non-UTF-8 byte anywhere on the page into a
%% `bad_template_value` blamed on the innocent layout. That clause is right for
%% a *user* value and stays; the page is not one.
%%
%% Deliberately NOT a new tagged tuple. An escaping content slot escapes only
%% what renders to a binary, so any tuple this module unwrapped back to raw
%% output would be an escape bypass the moment a binding carried the same shape
%% -- unlike `{arizona_raw, _}`/`{arizona_effect, _}`, which are safe precisely
%% because `arizona_template:classify_trusted/1` knows them and re-escapes.
%% The `#{s, d}` shape adds no surface: a binding holding one already splices
%% raw (a nested template is exactly what it claims to be).
apply_layouts([], _PageBackend, Inner, _Bindings) ->
    Inner;
apply_layouts([{Mod, Fun} | Rest], PageBackend, Inner, Bindings) ->
    {InnerBackend, Wrapped} = apply_layouts(Rest, PageBackend, Inner, Bindings),
    Tmpl = Mod:Fun(Bindings#{inner_content => #{s => [Wrapped], d => []}}),
    LayoutBackend = backend(Tmpl),
    ok = assert_layout_target(Mod, Fun, LayoutBackend, PageBackend),
    {layer_backend(LayoutBackend, InnerBackend), render_layout(Mod, Fun, Tmpl)}.

%% A layer that records no backend adds no target of its own, so the document is
%% still what it wraps.
layer_backend(undefined, InnerBackend) -> InnerBackend;
layer_backend(LayoutBackend, _InnerBackend) -> LayoutBackend.

%% The page reaches a layout as a backendless template, so the nested-template
%% check (assert_same_target/2) waves it through and a layout of one target can
%% wrap a page of another into a document no client parses. Compare the two
%% backends at the boundary instead; every layer wraps the same page, so each
%% must match the page's target. An unrecorded backend is not a mismatch.
assert_layout_target(_Mod, _Fun, Backend, Backend) ->
    ok;
assert_layout_target(_Mod, _Fun, undefined, _PageBackend) ->
    ok;
assert_layout_target(_Mod, _Fun, _LayoutBackend, undefined) ->
    ok;
assert_layout_target(Mod, Fun, LayoutBackend, PageBackend) ->
    erlang:error(
        {cross_target_layout, Mod, Fun, LayoutBackend, PageBackend},
        none,
        [{error_info, #{module => ?MODULE}}]
    ).

%% Mark the layer so `render_ssr_val/2` can refuse a `?stateful` inside it. The
%% inner layers have already rendered into `Wrapped` by the time this runs, and
%% `inner_content` is handed over as a no-dynamics template, so the flag never
%% spans a nested layout or the page itself -- only this layer's own dynamics.
-doc """
The layout layer currently rendering in this process, or `undefined` outside
one. The flag spans exactly one layer's own dynamics (see `render_layout/3`).
""".
-spec current_layout() -> {module(), atom()} | undefined.
current_layout() ->
    erlang:get(?IN_LAYOUT).

render_layout(Mod, Fun, Tmpl) ->
    erlang:put(?IN_LAYOUT, {Mod, Fun}),
    try
        render_to_iolist(Tmpl)
    after
        erlang:erase(?IN_LAYOUT)
    end.

%% A layout renders once at SSR, outside any live process, so nothing it renders
%% is registered in the live `views` map. The `az-view` marker is baked into a
%% stateful module's compiled statics, so it cannot be stripped here -- and the
%% client reads that marker to decide which view handles an event. A layout-borne
%% marker therefore claims a view the server never knows about, and every event
%% under it is dropped. Refuse before mounting rather than emit it.
refuse_stateful_in_layout(H) ->
    case erlang:get(?IN_LAYOUT) of
        undefined ->
            ok;
        {Mod, Fun} ->
            erlang:error(
                {stateful_in_layout, Mod, Fun, H}, none, [{error_info, #{module => ?MODULE}}]
            )
    end.

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
%%
%% SSR pair walker: renders each dynamic and splices it between the statics in ONE
%% pass. Building the rendered list first (a comprehension) and zipping it after cost
%% an N-element list per template that nothing else ever reads. A template and an
%% `?each` item (of any source) walk through here alike, so an item value renders,
%% and a crash in it is located, exactly as a top-level one is.
zip_ssr(_Backend, [S], []) ->
    [S];
zip_ssr(Backend, [S | Statics], [D | Dynamics]) ->
    [S, render_ssr_out(Backend, D) | zip_ssr(Backend, Statics, Dynamics)].

%% An escaped value that renders to a binary is already the output: scoping it and
%% render_dyn/2 would both hand it back untouched, and it is most of what a page holds.
render_ssr_out(Backend, {Az, {esc, Fun}, Loc}) when is_function(Fun, 0) ->
    try render_ssr_esc_val(Backend, Fun()) of
        Bin when is_binary(Bin) -> Bin;
        Rendered -> render_dyn(Backend, arizona_template:scope_slot(Az, Rendered))
    catch
        Class:Reason:ST -> arizona_eval:relocate(Class, Reason, ST, Loc)
    end;
render_ssr_out(Backend, D) ->
    render_dyn(Backend, render_ssr_one(Backend, D)).

render_ssr_one(Backend, {_Az, {attr, Name, Fun}, Loc}) when is_function(Fun, 0) ->
    try
        Backend:render_attr(Name, Fun())
    catch
        Class:Reason:ST -> arizona_eval:relocate(Class, Reason, ST, Loc)
    end;
render_ssr_one(Backend, {Az, Spec, Loc}) ->
    try
        arizona_template:scope_slot(Az, render_ssr_val(Backend, Spec))
    catch
        Class:Reason:ST -> arizona_eval:relocate(Class, Reason, ST, Loc)
    end;
render_ssr_one(Backend, {_Az, {attr, Name, Fun}}) when is_function(Fun, 0) ->
    Backend:render_attr(Name, Fun());
render_ssr_one(Backend, {Az, Spec}) ->
    arizona_template:scope_slot(Az, render_ssr_val(Backend, Spec)).

render_ssr_val(Backend, {esc, Fun}) when is_function(Fun, 0) ->
    render_ssr_esc_val(Backend, Fun());
render_ssr_val(Backend, Fun) when is_function(Fun, 0) ->
    render_ssr_val(Backend, Fun());
render_ssr_val(Backend, #{t := ?EACH, source := Items, template := Tmpl}) when is_list(Items) ->
    %% SSR keeps no snapshot, so render the items and keep nothing else. Going through
    %% the item-snapshot shape instead materialised a `{Az, Value, Deps}` triple per
    %% dynamic, a list of those per item, and a list of those -- all of it read once,
    %% by the walk that turns it into exactly this output, and then dropped. The live
    %% paths still build it, because their snapshot is what the next diff reads.
    %% What the items render TO is the backend's concern, as everywhere else here.
    #{s := ItemStatics, d := DynamicsFun} = Tmpl,
    {ssr_rendered,
        join_items(Backend, [zip_ssr(Backend, ItemStatics, DynamicsFun(Item)) || Item <- Items])};
render_ssr_val(Backend, #{
    t := ?EACH,
    source := #stream{
        items = ItemsMap,
        order = Order,
        limit = Limit
    },
    template := Tmpl
}) ->
    #{s := ItemStatics, d := DynamicsFun} = Tmpl,
    {ssr_rendered,
        join_items(Backend, [
            zip_ssr(Backend, ItemStatics, DynamicsFun(maps:get(K, ItemsMap), K))
         || K <- arizona_template:visible_keys(Order, Limit)
        ])};
render_ssr_val(Backend, #{t := ?EACH, source := Source, template := Tmpl}) when is_map(Source) ->
    %% Same entry order as the live path (`arizona_eval:render_map_items_simple/2`),
    %% so a later diff lines up with what SSR put on the page.
    #{s := ItemStatics, d := DynamicsFun} = Tmpl,
    {ssr_rendered,
        join_items(Backend, [
            zip_ssr(Backend, ItemStatics, DynamicsFun(K, V))
         || K := V <- Source
        ])};
render_ssr_val(Backend, #{stateful := H, props := Props}) ->
    ok = refuse_stateful_in_layout(H),
    {B1, _Resets} = arizona_stateful:call_mount(H, Props),
    %% Enforce restricted keys (e.g. `id`) at SSR too -- the live child-mount path
    %% (arizona_eval:fresh_mount_stateful) already does, so a handler that rewrites
    %% a restricted key must fail here rather than SSR cleanly and crash only at
    %% WS connect.
    ok = arizona_eval:check_restricted_keys(B1, Props, H),
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
    Backend:render_attr(Name, V);
render_ssr_val(Backend, #{az_local := _, v := V}) ->
    %% Escape the content `?local` init to match the client's text-node semantics
    %% (see render_dyn/2); a raw splice here is an XSS vector for a seeded init.
    arizona_template:escape_value(Backend, V);
render_ssr_val(Backend, #{s := Statics, d := Dynamics} = Tmpl) ->
    %% A nested template carries its own backend, so render its dynamics through
    %% that rather than the enclosing one -- but only when the two agree. They
    %% cannot be mixed: the targets emit incompatible statics, so an `?html` child
    %% under a `?native` parent splices HTML bytes into the JSON envelope and the
    %% client cannot parse the frame at all.
    NestedBackend = backend(Tmpl),
    ok = assert_same_target(Backend, NestedBackend),
    Snap0 = #{
        s => Statics,
        d => [
            {arizona_template:dyn_az(D), render_ssr_one(NestedBackend, D)}
         || D <- Dynamics
        ]
    },
    arizona_template:maybe_propagate(Tmpl, Snap0);
render_ssr_val(Backend, Val) ->
    text_leaf(Backend, arizona_template:to_bin(Val)).

%% A value interpolation, classified as the live render classifies it
%% (`arizona_template:mark_esc/1`): a map -- nested template, descriptor, `?local`
%% -- renders structurally with its own escaping, an effect passes through encoded,
%% and anything else is escaped once. Escaping whatever came back as a binary
%% instead escaped an effect or a `?local` init a second time. A binary or an integer
%% is escaped directly, the answer the classification is certain to give.
render_ssr_esc_val(Backend, V) when is_binary(V) ->
    Backend:escape(V);
render_ssr_esc_val(Backend, V) when is_integer(V) ->
    Backend:escape(integer_to_binary(V));
render_ssr_esc_val(Backend, V) when is_map(V) ->
    render_ssr_val(Backend, V);
render_ssr_esc_val(Backend, V) ->
    case arizona_template:mark_esc(V) of
        {arizona_esc, X} -> arizona_template:escape_marked(Backend, X);
        Trusted -> text_leaf(Backend, arizona_template:to_bin(Trusted))
    end.

render_fp_val(Backend, {arizona_esc, V}) ->
    %% The fingerprint (initial hydration) payload carries a scalar content value
    %% the way its client zips it (`wire_escape/1`): HTML entity-escaped, native
    %% raw for the client to JSON-encode. This is unlike the live diff, where
    %% `make_op` sends the value raw for the client to text-node.
    Backend:wire_escape(arizona_template:to_bin(V));
render_fp_val(Backend, {attr, Name, V}) ->
    %% One clause for every backend: HTML emits ` Name="Escaped"`, native strips
    %% the name (baked into the static) and sends the raw value -- both via the
    %% backend's wire_attr/2, so no backend is named here.
    Backend:wire_attr(Name, V);
render_fp_val(Backend, #{az_local := _, target := {attr, Name}, v := V}) ->
    Backend:render_attr(Name, V);
render_fp_val(Backend, #{az_local := _, v := V}) ->
    %% Escape the content `?local` init to match the client's text-node semantics
    %% (see render_dyn/2). `?local` is HTML-only (compile-rejected for native), so
    %% the enclosing backend is always HTML here.
    arizona_template:escape_value(Backend, V);
render_fp_val(Backend, #{f := _} = Nested) ->
    %% The same-target check render_dyn/2 makes. Every payload goes through here
    %% (a fingerprinted mount, which renders no document, and a diff's branch
    %% swap or each insert), so a child of another target is refused on all.
    ok = assert_same_target(Backend, backend(Nested)),
    fingerprint_payload(Nested);
render_fp_val(Backend, #{s := S, d := D} = Nested) ->
    ok = assert_same_target(Backend, backend(Nested)),
    flat_zip(Backend, S, [V || {_Az, V} <:- D]);
render_fp_val(Backend, #{
    t := ?EACH, items := Items, template := #{f := F, t := T, s := S}
}) when
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
%% No-`f` fallbacks are KEPT here, unlike the ones deleted from `zip_list_fp/2` and
%% `zip_stream_fp/3`. Two differences: these return a LIST, which is an array on the
%% wire, so the client parses it as HTML (the zip ones returned a binary -- a string --
%% which the client wrote into a text node, rendering markup as visible escaped text);
%% and `zip_*_fp` is typed over `each_template()`, where `f` is required and its absence
%% is a contract violation, whereas `fingerprint_payload/1` walks arbitrary snapshot
%% values. Do not "finish the job" by deleting these to match.
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

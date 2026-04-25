-module(arizona_render).
-moduledoc """
Walks compiled templates and produces final HTML output.

Two flavors of rendering:

- **Initial render** (`render/1`, `render/2`) -- evaluates dynamics through
  `arizona_eval`, builds an HTML iolist by zipping statics with values, and
  returns a snapshot the differ can later use to compute updates.
- **SSR render** (`render_to_iolist/1`, `render_to_iolist/2`) -- pure HTML
  output for the very first server response, no snapshot needed because no
  client is connected yet.

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
  view_id => binary()}
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
-export([render_view_to_iolist/3]).
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
    items := [[{arizona_template:az(), term()}]],
    template := arizona_template:template()
}.

-nominal each_stream() :: #{
    t := 0,
    items := #{term() => [{arizona_template:az(), term()}]},
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
    HTML = zip(Statics, [arizona_template:unwrap_val(V) || {_Az, V} <:- EvalDynamics]),
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
    {D, Deps, Vals} = arizona_template:unzip_triples(Triples),
    HTML = zip(Statics, Vals),
    Snap0 = #{s => Statics, d => D, deps => Deps},
    Snap = arizona_template:maybe_propagate(Tmpl, Snap0),
    {HTML, Snap, NewViews}.

-doc """
SSR render: produces HTML iolist with no snapshot, used for the first
server response before any client is attached.
""".
-spec render_to_iolist(Template) -> iolist() when
    Template :: arizona_template:template().
render_to_iolist(#{s := Statics, d := Dynamics}) ->
    zip(Statics, render_ssr_dynamics(Dynamics)).

-doc """
SSR render for a stateful handler (embedded-component style).

Mounts via `mount/1`, renders, optionally wraps in a layout. Used
by tests that SSR stateful components in isolation; production
HTTP SSR goes through `render_view_to_iolist/3`. `on_mount` is a
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
SSR render for a route-level view.

Mounts the view with the provided `az:request()`, applies the
`on_mount` chain (threading the Request into each hook), renders,
and optionally wraps the page output in a layout module.
""".
-spec render_view_to_iolist(Handler, Req, Opts) -> iolist() when
    Handler :: module(),
    Req :: az:request(),
    Opts :: arizona_live:route_opts().
render_view_to_iolist(Handler, Req, Opts) ->
    Bindings0 = arizona_live:apply_on_mount(
        maps:get(on_mount, Opts, []),
        maps:get(bindings, Opts, #{}),
        Req
    ),
    {Bindings, _Resets} = arizona_view:call_mount(Handler, Bindings0, Req),
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
zip([S], []) ->
    [S];
zip([S | Statics], [D | Dynamics]) ->
    Val =
        case D of
            #{t := ?EACH, items := Items, template := Tmpl} when is_list(Items) ->
                #{s := ItemS} = Tmpl,
                [zip_stream_item(ItemS, ItemD) || ItemD <:- Items];
            #{t := ?EACH, items := Items, order := Order, template := Tmpl} ->
                #{s := ItemS} = Tmpl,
                [zip_stream_item(ItemS, maps:get(K, Items)) || K <:- Order];
            #{s := InnerS, d := InnerD} ->
                zip(InnerS, [arizona_template:unwrap_val(V) || {_Az, V} <:- InnerD]);
            V ->
                arizona_template:to_bin(V)
        end,
    [S, Val | zip(Statics, Dynamics)].

-doc """
Renders a single each-item snapshot.

If the template has a fingerprint, returns a wire-format map keyed by
`~"f"`/`~"s"`/`~"d"`. Otherwise returns a plain HTML binary.
""".
-spec zip_item(Template, Dynamics) -> map() | binary() when
    Template :: map(),
    Dynamics :: [{arizona_template:az(), term()}].
zip_item(#{f := F, s := S}, D) ->
    #{
        ~"f" => F,
        ~"s" => S,
        ~"d" => [render_fp_val(arizona_template:unwrap_val(V)) || {_Az, V} <:- D]
    };
zip_item(#{s := S}, D) ->
    iolist_to_binary(zip(S, [arizona_template:unwrap_val(V) || {_Az, V} <:- D])).

-doc """
Renders a snapshot to a wire-format fingerprint payload (if `f` present)
or to a plain HTML binary.
""".
-spec zip_or_fp(Snapshot) -> map() | binary() when
    Snapshot :: map().
zip_or_fp(#{f := _} = Snap) ->
    fingerprint_payload(Snap);
zip_or_fp(#{s := S, d := D}) ->
    iolist_to_binary(zip(S, [arizona_template:unwrap_val(V) || {_Az, V} <:- D])).

-doc """
Renders a list of item snapshots against a template, producing either a
fingerprinted wire payload (if the template has `f`) or a flat HTML binary.
""".
-spec zip_list_fp(Template, Items) -> map() | binary() when
    Template :: map(),
    Items :: [[{arizona_template:az(), term()}]].
zip_list_fp(#{f := F, s := S, t := T}, ItemsList) ->
    #{
        ~"t" => T,
        ~"f" => F,
        ~"s" => S,
        ~"d" => [
            [render_fp_val(arizona_template:unwrap_val(V)) || {_Az, V} <:- D]
         || D <:- ItemsList
        ]
    };
zip_list_fp(#{s := S}, ItemsList) ->
    iolist_to_binary([zip_stream_item(S, D) || D <:- ItemsList]).

-doc """
Renders a stream's visible items (looked up from `Items` by `Order` keys)
against a template, with the same fingerprint-vs-flat-HTML branching as
`zip_list_fp/2`.
""".
-spec zip_stream_fp(Template, Items, Order) -> map() | binary() when
    Template :: map(),
    Items :: #{term() => [{arizona_template:az(), term()}]},
    Order :: [term()].
zip_stream_fp(#{f := F, s := S, t := T}, Items, Order) ->
    #{
        ~"t" => T,
        ~"f" => F,
        ~"s" => S,
        ~"d" => [
            [
                render_fp_val(arizona_template:unwrap_val(V))
             || {_Az, V} <:- maps:get(K, Items)
            ]
         || K <:- Order
        ]
    };
zip_stream_fp(#{s := S}, Items, Order) ->
    iolist_to_binary([zip_stream_item(S, maps:get(K, Items)) || K <:- Order]).

-doc """
Builds the wire-format fingerprint payload for a snapshot.

Snapshots with an `f` field become a `#{~"f", ~"s", ~"d"}` map;
plain snapshots fall back to a HTML binary.
""".
-spec fingerprint_payload(Snapshot) -> map() | binary() when
    Snapshot :: map().
fingerprint_payload(#{f := F, s := S, d := D}) ->
    #{
        ~"f" => F,
        ~"s" => S,
        ~"d" => [render_fp_val(arizona_template:unwrap_val(V)) || {_Az, V} <:- D]
    };
fingerprint_payload(#{s := S, d := D}) ->
    iolist_to_binary(zip(S, [arizona_template:unwrap_val(V) || {_Az, V} <:- D])).

%% --------------------------------------------------------------------
%% Internal functions
%% --------------------------------------------------------------------

finish_ssr(Handler, Bindings, Opts) ->
    PageTmpl = arizona_handler:call_render(Handler, Bindings),
    PageHTML = zip(maps:get(s, PageTmpl), render_ssr_dynamics(maps:get(d, PageTmpl))),
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

zip_stream_item(Statics, ItemD) ->
    zip(Statics, [arizona_template:unwrap_val(V) || {_Az, V} <:- ItemD]).

%% Az is ignored during SSR (only used for diff targeting), so `undefined` Az
%% from az-nodiff templates flows through harmlessly.
render_ssr_dynamics(Dynamics) ->
    [render_ssr_one(D) || D <:- Dynamics].

render_ssr_one({_Az, {attr, Name, Fun}, _Loc}) when is_function(Fun, 0) ->
    render_ssr_attr(Name, Fun());
render_ssr_one({_Az, Spec, {Mod, Line}}) ->
    try
        render_ssr_val(Spec)
    catch
        %% Already wrapped by a nested render -- preserve the deepest location,
        %% which is closest to the actual failure.
        Class:{arizona_loc, _, _} = Reason:ST ->
            erlang:raise(Class, Reason, ST);
        Class:Reason:ST ->
            erlang:raise(Class, {arizona_loc, {Mod, Line}, Reason}, ST)
    end;
render_ssr_one({_Az, {attr, Name, Fun}}) when is_function(Fun, 0) ->
    render_ssr_attr(Name, Fun());
render_ssr_one({_Az, Spec}) ->
    render_ssr_val(Spec).

render_ssr_val(Fun) when is_function(Fun, 0) ->
    render_ssr_val(Fun());
render_ssr_val(#{t := ?EACH, source := Items, template := Tmpl}) when is_list(Items) ->
    ItemSnaps = arizona_eval:render_list_items_simple(Items, Tmpl),
    #{t => ?EACH, items => ItemSnaps, template => Tmpl};
render_ssr_val(#{
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
render_ssr_val(#{t := ?EACH, source := Source, template := Tmpl}) when is_map(Source) ->
    ItemSnaps = arizona_eval:render_map_items_simple(Source, Tmpl),
    #{t => ?EACH, items => ItemSnaps, template => Tmpl};
render_ssr_val(#{stateful := H, props := Props}) ->
    {B1, _Resets} = arizona_stateful:call_mount(H, Props),
    render_ssr_val(arizona_handler:call_render(H, B1));
render_ssr_val(#{callback := Callback, props := Props}) ->
    Tmpl = Callback(Props),
    render_ssr_val(Tmpl);
render_ssr_val(#{s := Statics, d := Dynamics} = Tmpl) ->
    Vals = render_ssr_dynamics(Dynamics),
    Snap0 = #{
        s => Statics,
        d => [
            {arizona_template:dyn_az(D), Val}
         || D <:- Dynamics && Val <:- Vals
        ]
    },
    arizona_template:maybe_propagate(Tmpl, Snap0);
render_ssr_val(Val) ->
    arizona_template:to_bin(Val).

render_ssr_attr(Name, Val) ->
    arizona_template:render_attr(Name, Val).

render_fp_val({attr, Name, V}) ->
    arizona_template:render_attr(Name, V);
render_fp_val(#{f := _} = Nested) ->
    fingerprint_payload(Nested);
render_fp_val(#{s := S, d := D}) ->
    iolist_to_binary(zip(S, [arizona_template:unwrap_val(V) || {_Az, V} <:- D]));
render_fp_val(#{t := ?EACH, items := Items, template := #{f := F, t := T, s := S}}) when
    is_list(Items)
->
    #{
        ~"t" => T,
        ~"f" => F,
        ~"s" => S,
        ~"d" => [
            [render_fp_val(arizona_template:unwrap_val(V)) || {_Az, V} <:- D]
         || D <:- Items
        ]
    };
render_fp_val(#{t := ?EACH, items := Items, template := #{s := S}}) when
    is_list(Items)
->
    [
        iolist_to_binary(zip(S, [arizona_template:unwrap_val(V) || {_Az, V} <:- D]))
     || D <:- Items
    ];
render_fp_val(#{
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
                render_fp_val(arizona_template:unwrap_val(V))
             || {_Az, V} <:- maps:get(K, Items)
            ]
         || K <:- Order
        ]
    };
render_fp_val(#{
    t := ?EACH,
    items := Items,
    order := Order,
    template := #{s := S}
}) ->
    [iolist_to_binary(zip_stream_item(S, maps:get(K, Items))) || K <:- Order];
render_fp_val(V) when is_list(V) ->
    iolist_to_binary(V);
render_fp_val(V) ->
    arizona_template:to_bin(V).

-ifdef(TEST).

render_ssr_one_location_wrapping_test() ->
    Dyn = {~"0", fun() -> {} end, {test_mod, 99}},
    ?assertError(
        {arizona_loc, {test_mod, 99}, {bad_template_value, {}}},
        render_ssr_one(Dyn)
    ).

render_ssr_one_3tuple_text_test() ->
    Dyn = {~"0", fun() -> ~"hello" end, {test_mod, 10}},
    ?assertEqual(~"hello", render_ssr_one(Dyn)).

render_ssr_one_3tuple_attr_test() ->
    Dyn = {~"0", {attr, ~"class", fun() -> ~"box" end}, {test_mod, 20}},
    ?assertEqual(~" class=\"box\"", render_ssr_one(Dyn)).

render_ssr_one_2tuple_bad_value_test() ->
    Dyn = {~"0", fun() -> {} end},
    ?assertError({bad_template_value, {}}, render_ssr_one(Dyn)).

-endif.

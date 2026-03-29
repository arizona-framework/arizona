-module(arizona_render).
-export([
    render/1, render/2,
    render_to_iolist/1, render_to_iolist/2,
    resolve_id/1,
    zip/2,
    zip_item/2,
    zip_or_fp/1,
    zip_list_fp/2,
    zip_stream_fp/3,
    fingerprint_payload/1
]).
-ignore_xref([render/1, resolve_id/1, zip/2]).
-ifdef(TEST).
-include_lib("eunit/include/eunit.hrl").
-endif.
-include("arizona.hrl").

-type layout() :: {module(), atom()}.
-type render_opts() :: #{
    layout => layout(),
    bindings => map(),
    on_mount => arizona_live:on_mount()
}.
-export_type([layout/0, render_opts/0]).

%% --- render/1 ----------------------------------------------------------------
%% Snapshots from az-nodiff templates carry `diff => false` (via
%% maybe_propagate) and dynamics with Az = `undefined`. The diff engine
%% short-circuits on `diff => false` before examining individual dynamics,
%% so `undefined` Az values are never used as op targets.

-spec render(map()) -> {iolist(), map()}.
render(#{s := Statics, d := Dynamics} = Tmpl) ->
    EvalDynamics = arizona_eval:eval_dynamics(Dynamics),
    HTML = zip(Statics, [arizona_template:unwrap_val(V) || {_Az, V} <:- EvalDynamics]),
    Snap0 = #{s => Statics, d => EvalDynamics},
    Snap = arizona_template:maybe_propagate(Tmpl, Snap0),
    {HTML, Snap}.

%% --- render/2 ----------------------------------------------------------------
%% Same `undefined` Az / `diff => false` safety as render/1 above.

-spec render(map(), map()) -> {iolist(), map(), map()}.
render(#{s := Statics, d := Dynamics} = Tmpl, Views0) ->
    {Triples, {_Old, NewViews}} = arizona_eval:eval_dynamics_v(Dynamics, {Views0, #{}}),
    {D, Deps, Vals} = arizona_template:unzip_triples(Triples),
    HTML = zip(Statics, Vals),
    Snap0 = #{s => Statics, d => D, deps => Deps},
    Snap = arizona_template:maybe_propagate(Tmpl, Snap0),
    {HTML, Snap, NewViews}.

%% --- render_to_iolist/1 ------------------------------------------------------

-spec render_to_iolist(map()) -> iolist().
render_to_iolist(#{s := Statics, d := Dynamics}) ->
    zip(Statics, render_ssr_dynamics(Dynamics)).

%% --- render_to_iolist/2 ------------------------------------------------------

-spec render_to_iolist(module(), render_opts()) -> iolist().
render_to_iolist(Handler, Opts) ->
    Bindings0 = maps:get(bindings, Opts, #{}),
    OnMount = maps:get(on_mount, Opts, []),
    Bindings = arizona_live:apply_on_mount(OnMount, Bindings0),
    {B1, _Resets} = Handler:mount(Bindings),
    PageTmpl = Handler:render(B1),
    PageHTML = zip(maps:get(s, PageTmpl), render_ssr_dynamics(maps:get(d, PageTmpl))),
    case maps:get(layout, Opts, undefined) of
        undefined ->
            PageHTML;
        {LayoutMod, LayoutFun} ->
            LayoutTmpl = LayoutMod:LayoutFun(B1#{inner_content => PageHTML}),
            render_to_iolist(LayoutTmpl)
    end.

%% --- resolve_id/1 ------------------------------------------------------------

resolve_id(Id) when is_binary(Id) -> Id;
resolve_id(#{s := _, d := _} = Tmpl) ->
    {HTML, _Snap} = render(Tmpl),
    iolist_to_binary(HTML).

%% --- zip/2 -------------------------------------------------------------------

-spec zip([binary()], [term()]) -> iolist().
zip([S], []) ->
    [S];
zip([S | Statics], [D | Dynamics]) ->
    Val =
        case D of
            #{t := ?EACH, items := Items, template := Tmpl} when is_list(Items) ->
                #{s := ItemS} = Tmpl,
                [
                    zip(ItemS, [arizona_template:unwrap_val(V) || {_Az, V} <:- ItemD])
                 || ItemD <:- Items
                ];
            #{t := ?EACH, items := Items, order := Order, template := Tmpl} ->
                #{s := ItemS} = Tmpl,
                [
                    begin
                        ItemD = maps:get(K, Items),
                        zip(ItemS, [arizona_template:unwrap_val(V) || {_Az, V} <:- ItemD])
                    end
                 || K <:- Order
                ];
            #{s := InnerS, d := InnerD} ->
                zip(InnerS, [arizona_template:unwrap_val(V) || {_Az, V} <:- InnerD]);
            V when is_list(V) ->
                V;
            V ->
                arizona_template:to_bin(V)
        end,
    [S, Val | zip(Statics, Dynamics)].

zip_item(#{f := F, s := S}, D) ->
    #{
        <<"f">> => F,
        <<"s">> => S,
        <<"d">> => [render_fp_val(arizona_template:unwrap_val(V)) || {_Az, V} <:- D]
    };
zip_item(#{s := S}, D) ->
    iolist_to_binary(zip(S, [arizona_template:unwrap_val(V) || {_Az, V} <:- D])).

%% --- render_ssr_dynamics -----------------------------------------------------
%% Az is ignored during SSR (only used for diff targeting), so `undefined` Az
%% from az-nodiff templates flows through harmlessly.

render_ssr_dynamics(Dynamics) ->
    [render_ssr_one(D) || D <:- Dynamics].

render_ssr_one({_Az, {attr, _Name, Fun}, _Loc}) when is_function(Fun, 0) ->
    render_ssr_val(Fun());
render_ssr_one({_Az, Spec, {Mod, Line}}) ->
    try
        render_ssr_val(Spec)
    catch
        Class:Reason:ST ->
            erlang:raise(Class, {arizona_loc, {Mod, Line}, Reason}, ST)
    end;
render_ssr_one({_Az, {attr, _Name, Fun}}) when is_function(Fun, 0) ->
    render_ssr_val(Fun());
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
render_ssr_val(#{stateful := H, props := Props}) ->
    {B1, _Resets} = H:mount(Props),
    make_ssr_child_snap(H:render(B1));
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

%% --- make_ssr_child_snap/1 ---------------------------------------------------

make_ssr_child_snap(#{s := S, d := Dynamics} = Tmpl) ->
    Vals = render_ssr_dynamics(Dynamics),
    Snap = #{
        s => S,
        d => [
            {arizona_template:dyn_az(D), Val}
         || D <:- Dynamics && Val <:- Vals
        ]
    },
    Snap1 =
        case Tmpl of
            #{diff := false} -> Snap#{diff => false};
            #{} -> Snap
        end,
    arizona_template:maybe_put_fingerprint(Tmpl, Snap1).

%% --- fingerprint_payload/1 ---------------------------------------------------

fingerprint_payload(#{f := F, s := S, d := D}) ->
    #{
        <<"f">> => F,
        <<"s">> => S,
        <<"d">> => [render_fp_val(arizona_template:unwrap_val(V)) || {_Az, V} <:- D]
    };
fingerprint_payload(#{s := S, d := D}) ->
    iolist_to_binary(zip(S, [arizona_template:unwrap_val(V) || {_Az, V} <:- D])).

%% --- render_fp_val/1 ---------------------------------------------------------

render_fp_val({attr, _, V}) ->
    render_fp_val(V);
render_fp_val(#{f := _} = Nested) ->
    fingerprint_payload(Nested);
render_fp_val(#{s := S, d := D}) ->
    iolist_to_binary(zip(S, [arizona_template:unwrap_val(V) || {_Az, V} <:- D]));
render_fp_val(#{t := ?EACH, items := Items, template := #{f := F, t := T, s := S}}) when
    is_list(Items)
->
    #{
        <<"t">> => T,
        <<"f">> => F,
        <<"s">> => S,
        <<"d">> => [
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
        <<"t">> => T,
        <<"f">> => F,
        <<"s">> => S,
        <<"d">> => [
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
    [
        begin
            ItemD = maps:get(K, Items),
            iolist_to_binary(zip(S, [arizona_template:unwrap_val(V) || {_Az, V} <:- ItemD]))
        end
     || K <:- Order
    ];
render_fp_val(V) when is_list(V) ->
    iolist_to_binary(V);
render_fp_val(V) ->
    arizona_template:to_bin(V).

%% --- zip_or_fp/1 -------------------------------------------------------------

zip_or_fp(#{f := _} = Snap) ->
    fingerprint_payload(Snap);
zip_or_fp(#{s := S, d := D}) ->
    iolist_to_binary(zip(S, [arizona_template:unwrap_val(V) || {_Az, V} <:- D])).

%% --- zip_list_fp/2 -----------------------------------------------------------

zip_list_fp(#{f := F, s := S, t := T}, ItemsList) ->
    #{
        <<"t">> => T,
        <<"f">> => F,
        <<"s">> => S,
        <<"d">> => [
            [render_fp_val(arizona_template:unwrap_val(V)) || {_Az, V} <:- D]
         || D <:- ItemsList
        ]
    };
zip_list_fp(#{s := S}, ItemsList) ->
    iolist_to_binary([
        zip(S, [arizona_template:unwrap_val(V) || {_Az, V} <:- D])
     || D <:- ItemsList
    ]).

%% --- zip_stream_fp/3 ---------------------------------------------------------

zip_stream_fp(#{f := F, s := S, t := T}, Items, Order) ->
    #{
        <<"t">> => T,
        <<"f">> => F,
        <<"s">> => S,
        <<"d">> => [
            [
                render_fp_val(arizona_template:unwrap_val(V))
             || {_Az, V} <:- maps:get(K, Items)
            ]
         || K <:- Order
        ]
    };
zip_stream_fp(#{s := S}, Items, Order) ->
    iolist_to_binary([
        begin
            ItemD = maps:get(K, Items),
            zip(S, [arizona_template:unwrap_val(V) || {_Az, V} <:- ItemD])
        end
     || K <:- Order
    ]).

-ifdef(TEST).

render_ssr_one_location_wrapping_test() ->
    Dyn = {<<"0">>, fun() -> {} end, {test_mod, 99}},
    ?assertError(
        {arizona_loc, {test_mod, 99}, {bad_template_value, {}}},
        render_ssr_one(Dyn)
    ).

render_ssr_one_3tuple_text_test() ->
    Dyn = {<<"0">>, fun() -> <<"hello">> end, {test_mod, 10}},
    ?assertEqual(<<"hello">>, render_ssr_one(Dyn)).

render_ssr_one_3tuple_attr_test() ->
    Dyn = {<<"0">>, {attr, <<"class">>, fun() -> <<"box">> end}, {test_mod, 20}},
    ?assertEqual(<<"box">>, render_ssr_one(Dyn)).

render_ssr_one_2tuple_bad_value_test() ->
    Dyn = {<<"0">>, fun() -> {} end},
    ?assertError({bad_template_value, {}}, render_ssr_one(Dyn)).

-endif.

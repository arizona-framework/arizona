-module(arizona_diff).
-export([diff/2, diff/3, diff/4]).
-ignore_xref([diff/3]).
-include("arizona.hrl").

%% --- Types ------------------------------------------------------------------

-type op() :: [integer() | binary() | term()].

-export_type([op/0]).

%% --- diff/2 ------------------------------------------------------------------
%%
%% Note: dynamics with Az = `undefined` (from az-nodiff templates) are never
%% reached here because their parent template always has `diff => false`,
%% which short-circuits before individual dynamics are examined.

-spec diff(map(), map()) -> {list(), map()}.
diff(_NewTmpl, #{diff := false} = OldSnap) ->
    {[], OldSnap};
diff(#{s := Statics, d := NewDynamics} = Tmpl, #{s := Statics, d := OldEvals}) ->
    EvalNew = arizona_eval:eval_dynamics(NewDynamics),
    Ops = diff_dynamics(EvalNew, OldEvals),
    Snap0 = #{s => Statics, d => EvalNew},
    {Ops, arizona_template:maybe_put_fingerprint(Tmpl, Snap0)}.

%% --- diff/3 ------------------------------------------------------------------

-spec diff(map(), map(), map()) -> {list(), map(), map()}.
diff(_NewTmpl, #{diff := false} = OldSnap, Views) ->
    {[], OldSnap, Views};
diff(#{s := Statics, d := NewDynamics} = Tmpl, #{s := Statics, d := OldEvals}, Views0) ->
    {Triples, {_Old, NewViews}} = arizona_eval:eval_dynamics_v(NewDynamics, {Views0, #{}}),
    {EvalNew, NewDeps, _Vals} = arizona_template:unzip_triples(Triples),
    Ops = diff_dynamics(EvalNew, OldEvals),
    Snap0 = #{s => Statics, d => EvalNew, deps => NewDeps},
    {Ops, arizona_template:maybe_put_fingerprint(Tmpl, Snap0), NewViews}.

%% --- diff/4 ------------------------------------------------------------------

-spec diff(map(), map(), map(), map()) -> {list(), map(), map()}.
diff(_NewTmpl, #{diff := false} = OldSnap, Views, _Changed) ->
    {[], OldSnap, Views};
diff(
    #{s := Statics, d := NewDynamics} = Tmpl,
    #{s := Statics, d := OldEvals, deps := OldDeps},
    Views0,
    Changed
) ->
    {Ops, NewD, NewDeps, {_Old, NewViews}} =
        diff_dynamics_v(NewDynamics, OldEvals, OldDeps, Changed, {Views0, #{}}),
    Snap0 = #{s => Statics, d => NewD, deps => NewDeps},
    {Ops, arizona_template:maybe_put_fingerprint(Tmpl, Snap0), NewViews}.

%% --- diff_dynamics (no views) ------------------------------------------------

diff_dynamics([], []) ->
    [];
diff_dynamics([{Az, _} | NR], [{Az, #{diff := false}} | OR]) ->
    diff_dynamics(NR, OR);
diff_dynamics([{Az, Same} | NR], [{Az, Same} | OR]) ->
    diff_dynamics(NR, OR);
diff_dynamics([{Az, New} | NR], [{Az, Old} | OR]) ->
    [make_op(Az, New, Old) | diff_dynamics(NR, OR)].

%% --- diff_dynamics_v (with views + dep skip) ---------------------------------

diff_dynamics_v([], [], [], _Changed, Views) ->
    {[], [], [], Views};
diff_dynamics_v(
    [_Def | DR],
    [{Az, #{diff := false} = Old} | OR],
    [ODeps | DepsR],
    Changed,
    Views0
) ->
    Views1 = carry_skipped_view(Old, Views0),
    {OpsRest, DRest, DepsRest, Views2} =
        diff_dynamics_v(DR, OR, DepsR, Changed, Views1),
    {OpsRest, [{Az, Old} | DRest], [ODeps | DepsRest], Views2};
diff_dynamics_v([Def | DR], [{Az, Old} | OR], [ODeps | DepsR], Changed, Views0) ->
    case deps_changed(ODeps, Changed) of
        false ->
            Views1 = carry_skipped_view(Old, Views0),
            {OpsRest, DRest, DepsRest, Views2} =
                diff_dynamics_v(DR, OR, DepsR, Changed, Views1),
            {OpsRest, [{Az, Old} | DRest], [ODeps | DepsRest], Views2};
        true ->
            case Old of
                #{t := ?EACH} ->
                    {Az2, EachDesc, Deps} = arizona_eval:eval_each_def(Def),
                    case EachDesc of
                        #{source := #stream{} = Source} ->
                            {Old0, New0} = Views0,
                            OldChildViews = maps:get(child_views, Old, []),
                            {StreamOps, NewSnap0, {_, LocalNew}} =
                                diff_stream(Az2, EachDesc, Old, {Old0, #{}}),
                            %% Incremental child_views: old - deleted + rendered
                            #{items := OldItems} = Old,
                            Deleted = deleted_item_children(Source#stream.pending, OldItems),
                            Surviving = OldChildViews -- Deleted -- maps:keys(LocalNew),
                            LocalNew1 = carry_item_children(Surviving, Old0, LocalNew),
                            ChildViews = maps:keys(LocalNew1),
                            NewSnap = NewSnap0#{child_views => ChildViews},
                            Views1 = {Old0, maps:merge(New0, LocalNew1)},
                            {OpsRest, DRest, DepsRest, Views2} =
                                diff_dynamics_v(DR, OR, DepsR, Changed, Views1),
                            {
                                StreamOps ++ OpsRest,
                                [{Az2, NewSnap} | DRest],
                                [Deps | DepsRest],
                                Views2
                            };
                        #{source := Items} when is_list(Items) ->
                            {Old0, New0} = Views0,
                            {ListOps, NewSnap0, {_, LocalNew}} =
                                diff_list(Az2, EachDesc, Old, {Old0, #{}}),
                            ChildViews = maps:keys(LocalNew),
                            NewSnap = NewSnap0#{child_views => ChildViews},
                            Views1 = {Old0, maps:merge(New0, LocalNew)},
                            {OpsRest, DRest, DepsRest, Views2} =
                                diff_dynamics_v(DR, OR, DepsR, Changed, Views1),
                            {
                                ListOps ++ OpsRest,
                                [{Az2, NewSnap} | DRest],
                                [Deps | DepsRest],
                                Views2
                            }
                    end;
                _ ->
                    {Az, New, NewDeps, Views1} =
                        arizona_eval:eval_one_v_flat(Def, Views0),
                    {OpsRest, DRest, DepsRest, Views2} =
                        diff_dynamics_v(DR, OR, DepsR, Changed, Views1),
                    Ops =
                        case New =:= Old of
                            true -> OpsRest;
                            false -> [make_op(Az, New, Old) | OpsRest]
                        end,
                    {Ops, [{Az, New} | DRest], [NewDeps | DepsRest], Views2}
            end
    end.

%% When a dynamic is skipped (deps unchanged), carry its child views over
%% from OldViews to NewViews so they aren't pruned.
carry_skipped_view(#{view_id := VId}, {Old, New}) ->
    case Old of
        #{VId := View} -> {Old, New#{VId => View}};
        #{} -> {Old, New}
    end;
carry_skipped_view(#{child_views := ChildIds}, {Old, New}) ->
    lists:foldl(
        fun(Id, {O, N}) ->
            case O of
                #{Id := View} -> {O, N#{Id => View}};
                #{} -> {O, N}
            end
        end,
        {Old, New},
        ChildIds
    );
carry_skipped_view(_Old, Views) ->
    Views.

%% Extract child view IDs from deleted stream items only.
deleted_item_children(Pending, OldItems) ->
    lists:foldl(
        fun
            ({delete, Key}, Acc) ->
                case OldItems of
                    #{Key := ItemD} -> item_child_views(ItemD, Acc);
                    #{} -> Acc
                end;
            (_, Acc) ->
                Acc
        end,
        [],
        queue:to_list(Pending)
    ).

item_child_views(ItemD, Acc) ->
    lists:foldl(
        fun
            ({_Az, #{view_id := VId}}, A) -> [VId | A];
            (_, A) -> A
        end,
        Acc,
        ItemD
    ).

%% Copy child views from OldViews to NewViews for children not already present.
carry_item_children(ChildViewIds, Old, New) ->
    lists:foldl(
        fun(Id, Acc) ->
            case Old of
                #{Id := View} -> Acc#{Id => View};
                #{} -> Acc
            end
        end,
        New,
        ChildViewIds
    ).

deps_changed(Deps, Changed) ->
    map_size(maps:intersect(Deps, Changed)) > 0.

%% --- diff_stream -------------------------------------------------------------

diff_stream(
    Az,
    #{source := #stream{} = Source, template := Tmpl},
    #{items := OldItems} = OldSnap,
    Views0
) ->
    case OldSnap of
        #{order := OldOrder} ->
            diff_stream_pending(
                Az,
                Source#stream.pending,
                Source,
                Tmpl,
                OldItems,
                OldOrder,
                Views0
            );
        #{} ->
            VKeys = arizona_template:visible_keys(Source#stream.order, Source#stream.limit),
            {ItemSnaps, Views1} = arizona_eval:eval_stream_items(
                VKeys,
                Source#stream.items,
                Tmpl,
                Views0
            ),
            NewSnap = #{
                t => ?EACH,
                items => ItemSnaps,
                order => VKeys,
                source => Source,
                template => Tmpl
            },
            HTML = arizona_render:zip_stream_fp(Tmpl, ItemSnaps, VKeys),
            {[[?OP_UPDATE, Az, HTML]], NewSnap, Views1}
    end.

diff_stream_pending(Az, Queue, Source, Tmpl, SnapAcc, OldOrder, Views0) ->
    case queue:out(Queue) of
        {empty, _} ->
            apply_limit(Az, Source, Tmpl, SnapAcc, Views0);
        {{value, Op}, Rest} ->
            diff_stream_op(Az, Op, Rest, Source, Tmpl, SnapAcc, OldOrder, Views0)
    end.

diff_stream_op(
    Az,
    {insert, Key, Item, Pos},
    Rest,
    Source,
    Tmpl,
    SnapAcc,
    OldOrder,
    Views0
) ->
    {ItemD, Views1} = arizona_eval:render_stream_item(Key, Item, Tmpl, Views0),
    HTML = arizona_render:zip_item(Tmpl, ItemD),
    InsOp = [?OP_INSERT, Az, arizona_template:to_bin(Key), Pos, HTML],
    NewSnapAcc = SnapAcc#{Key => ItemD},
    {RestOps, FinalSnap, Views2} =
        diff_stream_pending(Az, Rest, Source, Tmpl, NewSnapAcc, OldOrder, Views1),
    {[InsOp | RestOps], FinalSnap, Views2};
diff_stream_op(
    Az,
    {delete, Key},
    Rest,
    Source,
    Tmpl,
    SnapAcc,
    OldOrder,
    Views0
) ->
    DelOp = [?OP_REMOVE, Az, arizona_template:to_bin(Key)],
    NewSnapAcc = maps:remove(Key, SnapAcc),
    {RestOps, FinalSnap, Views1} =
        diff_stream_pending(Az, Rest, Source, Tmpl, NewSnapAcc, OldOrder, Views0),
    {[DelOp | RestOps], FinalSnap, Views1};
diff_stream_op(
    Az,
    {update, Key, NewItem},
    Rest,
    Source,
    Tmpl,
    SnapAcc,
    OldOrder,
    Views0
) ->
    {NewD, Views1} = arizona_eval:render_stream_item(Key, NewItem, Tmpl, Views0),
    case SnapAcc of
        #{Key := OldD} ->
            {InnerOps, Views2} = diff_item_dynamics_v(NewD, OldD, Views1),
            case InnerOps of
                [] ->
                    {RestOps, FinalSnap, Views3} =
                        diff_stream_pending(
                            Az,
                            Rest,
                            Source,
                            Tmpl,
                            SnapAcc,
                            OldOrder,
                            Views2
                        ),
                    {RestOps, FinalSnap, Views3};
                _ ->
                    PatchOp = [?OP_ITEM_PATCH, Az, arizona_template:to_bin(Key), InnerOps],
                    NewSnapAcc = SnapAcc#{Key => NewD},
                    {RestOps, FinalSnap, Views3} =
                        diff_stream_pending(
                            Az,
                            Rest,
                            Source,
                            Tmpl,
                            NewSnapAcc,
                            OldOrder,
                            Views2
                        ),
                    {[PatchOp | RestOps], FinalSnap, Views3}
            end;
        #{} ->
            HTML = arizona_render:zip_item(Tmpl, NewD),
            InsOp = [?OP_INSERT, Az, arizona_template:to_bin(Key), -1, HTML],
            NewSnapAcc = SnapAcc#{Key => NewD},
            {RestOps, FinalSnap, Views2} =
                diff_stream_pending(
                    Az,
                    Rest,
                    Source,
                    Tmpl,
                    NewSnapAcc,
                    OldOrder,
                    Views1
                ),
            {[InsOp | RestOps], FinalSnap, Views2}
    end;
diff_stream_op(
    Az,
    {move, Key, AfterKey},
    Rest,
    Source,
    Tmpl,
    SnapAcc,
    OldOrder,
    Views0
) ->
    case SnapAcc of
        #{Key := _} ->
            Ref =
                case AfterKey of
                    null -> null;
                    _ -> arizona_template:to_bin(AfterKey)
                end,
            MoveOp = [?OP_MOVE, Az, arizona_template:to_bin(Key), Ref],
            {RestOps, FinalSnap, Views1} =
                diff_stream_pending(
                    Az,
                    Rest,
                    Source,
                    Tmpl,
                    SnapAcc,
                    OldOrder,
                    Views0
                ),
            {[MoveOp | RestOps], FinalSnap, Views1};
        #{} ->
            diff_stream_pending(
                Az,
                Rest,
                Source,
                Tmpl,
                SnapAcc,
                OldOrder,
                Views0
            )
    end;
diff_stream_op(
    Az,
    reorder,
    Rest,
    Source,
    Tmpl,
    SnapAcc,
    OldOrder,
    Views0
) ->
    VKeys = arizona_template:visible_keys(Source#stream.order, Source#stream.limit),
    MoveOps = compute_reorder_ops(Az, OldOrder, VKeys, SnapAcc),
    {RestOps, FinalSnap, Views1} =
        diff_stream_pending(
            Az,
            Rest,
            Source,
            Tmpl,
            SnapAcc,
            VKeys,
            Views0
        ),
    {MoveOps ++ RestOps, FinalSnap, Views1};
diff_stream_op(Az, reset, Rest, Source, Tmpl, SnapAcc, OldOrder, Views0) ->
    VKeys = arizona_template:visible_keys(Source#stream.order, Source#stream.limit),
    VSet = maps:from_keys(VKeys, true),
    RemOps = [
        [?OP_REMOVE, Az, arizona_template:to_bin(K)]
     || K := _ <:- SnapAcc, not is_map_key(K, VSet)
    ],
    Kept = maps:with(VKeys, SnapAcc),
    {DiffOps, NewSnaps, Views1} =
        smart_reset_items(Az, VKeys, Kept, Source#stream.items, Tmpl, Views0, #{}),
    MoveOps = compute_reorder_ops(Az, OldOrder, VKeys, Kept),
    {RestOps, FinalSnap, Views2} =
        diff_stream_pending(Az, Rest, Source, Tmpl, NewSnaps, VKeys, Views1),
    {RemOps ++ DiffOps ++ MoveOps ++ RestOps, FinalSnap, Views2}.

%% --- diff_list ---------------------------------------------------------------

diff_list(Az, #{source := Items, template := Tmpl}, #{items := OldItemsList}, Views0) ->
    {NewItemsList, Views1} = arizona_eval:render_list_items(Items, Tmpl, Views0),
    NewSnap = #{t => ?EACH, items => NewItemsList, template => Tmpl},
    case is_list(OldItemsList) of
        false ->
            HTML = arizona_render:zip_list_fp(Tmpl, NewItemsList),
            {[[?OP_UPDATE, Az, HTML]], NewSnap, Views1};
        true ->
            {Ops, NewTail, OldTail, Views2} =
                diff_list_zip(Az, NewItemsList, OldItemsList, Views1),
            case {NewTail, OldTail} of
                {[], []} ->
                    {Ops, NewSnap, Views2};
                _ ->
                    HTML = arizona_render:zip_list_fp(Tmpl, NewItemsList),
                    {[[?OP_UPDATE, Az, HTML]], NewSnap, Views2}
            end
    end.

diff_list_zip(Az, [NewD | NR], [OldD | OR], Views0) ->
    {InnerOps, Views1} = diff_item_dynamics_v(NewD, OldD, Views0),
    {RestOps, NewTail, OldTail, Views2} = diff_list_zip(Az, NR, OR, Views1),
    Ops =
        case InnerOps of
            [] ->
                RestOps;
            _ ->
                [{_, OldKey} | _] = OldD,
                [[?OP_ITEM_PATCH, Az, arizona_template:to_bin(OldKey), InnerOps] | RestOps]
        end,
    {Ops, NewTail, OldTail, Views2};
diff_list_zip(_Az, NewTail, OldTail, Views) ->
    {[], NewTail, OldTail, Views}.

%% --- smart_reset_items -------------------------------------------------------

smart_reset_items(_Az, [], _Kept, _ItemsMap, _Tmpl, Views, Snaps) ->
    {[], Snaps, Views};
smart_reset_items(Az, [K | Rest], Kept, ItemsMap, Tmpl, Views0, Snaps) ->
    Item = maps:get(K, ItemsMap),
    {NewD, Views1} = arizona_eval:render_stream_item(K, Item, Tmpl, Views0),
    NewSnaps = Snaps#{K => NewD},
    case Kept of
        #{K := OldD} ->
            {InnerOps, Views2} = diff_item_dynamics_v(NewD, OldD, Views1),
            case InnerOps of
                [] ->
                    smart_reset_items(
                        Az,
                        Rest,
                        Kept,
                        ItemsMap,
                        Tmpl,
                        Views2,
                        NewSnaps
                    );
                _ ->
                    PatchOp = [?OP_ITEM_PATCH, Az, arizona_template:to_bin(K), InnerOps],
                    {RestOps, FinalSnaps, Views3} =
                        smart_reset_items(
                            Az,
                            Rest,
                            Kept,
                            ItemsMap,
                            Tmpl,
                            Views2,
                            NewSnaps
                        ),
                    {[PatchOp | RestOps], FinalSnaps, Views3}
            end;
        #{} ->
            HTML = arizona_render:zip_item(Tmpl, NewD),
            InsOp = [?OP_INSERT, Az, arizona_template:to_bin(K), -1, HTML],
            {RestOps, FinalSnaps, Views2} =
                smart_reset_items(
                    Az,
                    Rest,
                    Kept,
                    ItemsMap,
                    Tmpl,
                    Views1,
                    NewSnaps
                ),
            {[InsOp | RestOps], FinalSnaps, Views2}
    end.

%% --- Limit reconciliation ----------------------------------------------------

apply_limit(
    _Az,
    #stream{limit = infinity, order = Order},
    Tmpl,
    SnapItems,
    Views
) ->
    {[], #{t => ?EACH, items => SnapItems, order => Order, template => Tmpl}, Views};
apply_limit(
    Az,
    #stream{limit = Limit, on_limit = halt, order = Order},
    Tmpl,
    SnapItems,
    Views
) ->
    VKeys = arizona_template:visible_keys(Order, Limit),
    KeepOnly = maps:with(VKeys, SnapItems),
    DropOps = [
        [?OP_REMOVE, Az, arizona_template:to_bin(K)]
     || K := _ <:- SnapItems, not is_map_key(K, KeepOnly)
    ],
    {DropOps, #{t => ?EACH, items => KeepOnly, order => VKeys, template => Tmpl}, Views};
apply_limit(
    Az,
    #stream{
        limit = Limit,
        on_limit = drop,
        items = ItemsMap,
        order = Order
    },
    Tmpl,
    SnapItems,
    Views0
) ->
    VKeys = arizona_template:visible_keys(Order, Limit),
    VSet = maps:from_keys(VKeys, true),
    RemOps = [
        [?OP_REMOVE, Az, arizona_template:to_bin(K)]
     || K := _ <:- SnapItems, not is_map_key(K, VSet)
    ],
    Pruned = #{K => V || K := V <:- SnapItems, is_map_key(K, VSet)},
    {InsOps, Final, Views1} = snap_add_missing(
        Az,
        VKeys,
        Pruned,
        ItemsMap,
        Tmpl,
        Views0
    ),
    {RemOps ++ InsOps, #{t => ?EACH, items => Final, order => VKeys, template => Tmpl}, Views1}.

snap_add_missing(_Az, [], Snaps, _ItemsMap, _Tmpl, Views) ->
    {[], Snaps, Views};
snap_add_missing(Az, [K | Rest], Snaps, ItemsMap, Tmpl, Views0) ->
    case Snaps of
        #{K := _} ->
            snap_add_missing(Az, Rest, Snaps, ItemsMap, Tmpl, Views0);
        #{} ->
            Item = maps:get(K, ItemsMap),
            {ItemD, Views1} = arizona_eval:render_stream_item(K, Item, Tmpl, Views0),
            HTML = arizona_render:zip_item(Tmpl, ItemD),
            InsOp = [?OP_INSERT, Az, arizona_template:to_bin(K), -1, HTML],
            NewSnaps = Snaps#{K => ItemD},
            {RestOps, FinalSnaps, Views2} =
                snap_add_missing(Az, Rest, NewSnaps, ItemsMap, Tmpl, Views1),
            {[InsOp | RestOps], FinalSnaps, Views2}
    end.

%% --- LIS (Longest Increasing Subsequence) ------------------------------------

lis_indices(NewOrder, OldPosMap) ->
    {Len, Tails, Parent} = lis_scan(NewOrder, OldPosMap, 1, 0, #{}, #{}),
    case Len of
        0 ->
            #{};
        _ ->
            #{(Len - 1) := {_, LastIdx}} = Tails,
            lis_backtrack(LastIdx, Parent, #{})
    end.

lis_scan([], _PosMap, _I, Len, Tails, Parent) ->
    {Len, Tails, Parent};
lis_scan([Key | Rest], PosMap, I, Len, Tails, Parent) ->
    case PosMap of
        #{Key := V} ->
            case Len of
                0 ->
                    lis_scan(Rest, PosMap, I + 1, 1, #{0 => {V, I}}, Parent);
                _ ->
                    #{(Len - 1) := {TailVal, TailIdx}} = Tails,
                    case TailVal < V of
                        true ->
                            lis_scan(
                                Rest,
                                PosMap,
                                I + 1,
                                Len + 1,
                                Tails#{Len => {V, I}},
                                Parent#{I => TailIdx}
                            );
                        false ->
                            Pos = lis_bsearch(Tails, V, 0, Len - 1),
                            Parent2 =
                                case Pos of
                                    0 ->
                                        Parent;
                                    _ ->
                                        #{(Pos - 1) := {_, PrevIdx}} = Tails,
                                        Parent#{I => PrevIdx}
                                end,
                            lis_scan(
                                Rest,
                                PosMap,
                                I + 1,
                                Len,
                                Tails#{Pos => {V, I}},
                                Parent2
                            )
                    end
            end;
        #{} ->
            lis_scan(Rest, PosMap, I + 1, Len, Tails, Parent)
    end.

lis_bsearch(_Tails, _V, Lo, Hi) when Lo >= Hi -> Lo;
lis_bsearch(Tails, V, Lo, Hi) ->
    Mid = (Lo + Hi) bsr 1,
    #{Mid := {MidVal, _}} = Tails,
    case MidVal < V of
        true -> lis_bsearch(Tails, V, Mid + 1, Hi);
        false -> lis_bsearch(Tails, V, Lo, Mid)
    end.

lis_backtrack(undefined, _Parent, Acc) ->
    Acc;
lis_backtrack(Idx, Parent, Acc) ->
    Next =
        case Parent of
            #{Idx := V} -> V;
            #{} -> undefined
        end,
    lis_backtrack(Next, Parent, Acc#{Idx => true}).

%% --- compute_reorder_ops ----------------------------------------------------

compute_reorder_ops(_Az, OldOrder, NewOrder, _Kept) when OldOrder =:= NewOrder ->
    [];
compute_reorder_ops(Az, OldOrder, NewOrder, Kept) ->
    KeptOld = [K || K <:- OldOrder, is_map_key(K, Kept)],
    case KeptOld of
        [] ->
            [];
        _ ->
            OldPosMap = pos_map(KeptOld, 1),
            LISSet = lis_indices(NewOrder, OldPosMap),
            emit_move_ops(Az, LISSet, NewOrder, 1, null)
    end.

pos_map([], _I) -> #{};
pos_map([K | Rest], I) -> (pos_map(Rest, I + 1))#{K => I}.

emit_move_ops(_Az, _LIS, [], _I, _Prev) ->
    [];
emit_move_ops(Az, LIS, [Key | Rest], I, Prev) ->
    case LIS of
        #{I := _} ->
            emit_move_ops(Az, LIS, Rest, I + 1, Key);
        #{} ->
            Ref =
                case Prev of
                    null -> null;
                    _ -> arizona_template:to_bin(Prev)
                end,
            [
                [?OP_MOVE, Az, arizona_template:to_bin(Key), Ref]
                | emit_move_ops(Az, LIS, Rest, I + 1, Key)
            ]
    end.

%% --- make_op -----------------------------------------------------------------

make_op(Az, {attr, Attr, false}, _Old) ->
    [?OP_REM_ATTR, Az, Attr];
make_op(Az, {attr, Attr, true}, _Old) ->
    [?OP_SET_ATTR, Az, Attr, <<>>];
make_op(Az, {attr, Attr, Val}, _Old) ->
    [?OP_SET_ATTR, Az, Attr, arizona_template:to_bin(Val)];
make_op(_Az, #{view_id := VId, s := S, d := NewD}, #{view_id := _, s := S, d := OldD}) ->
    [VId, diff_child_dynamics(NewD, OldD)];
make_op(Az, #{s := _, d := _} = NewNested, #{s := _, d := _}) ->
    [?OP_TEXT, Az, arizona_render:zip_or_fp(NewNested)];
make_op(Az, #{s := _, d := _} = Tmpl, _Old) ->
    [?OP_UPDATE, Az, arizona_render:zip_or_fp(Tmpl)];
make_op(Az, #{t := ?EACH, items := Items, template := Tmpl}, _Old) when
    is_list(Items)
->
    [?OP_UPDATE, Az, arizona_render:zip_list_fp(Tmpl, Items)];
make_op(Az, #{t := ?EACH, items := Items, order := Order, template := Tmpl}, _Old) ->
    [?OP_UPDATE, Az, arizona_render:zip_stream_fp(Tmpl, Items, Order)];
make_op(Az, remove, {attr, Attr, _}) ->
    [?OP_REM_ATTR, Az, Attr];
make_op(Az, remove, _Old) ->
    [?OP_REMOVE_NODE, Az];
make_op(Az, New, _Old) ->
    [?OP_TEXT, Az, arizona_template:to_bin(New)].

%% --- diff_item_dynamics_v ----------------------------------------------------

diff_item_dynamics_v([], [], Views) ->
    {[], Views};
diff_item_dynamics_v([{Az, _} | NR], [{Az, #{diff := false}} | OR], Views0) ->
    diff_item_dynamics_v(NR, OR, Views0);
diff_item_dynamics_v([{Az, Same} | NR], [{Az, Same} | OR], Views0) ->
    diff_item_dynamics_v(NR, OR, Views0);
diff_item_dynamics_v([{Az, New} | NR], [{Az, Old} | OR], Views0) ->
    case {New, Old} of
        {#{t := ?EACH, source := Src, template := Tmpl}, #{t := ?EACH}} ->
            EachDesc = #{source => Src, template => Tmpl},
            {EachOps, _NewSnap, Views1} =
                case Src of
                    #stream{} ->
                        diff_stream(Az, EachDesc, Old, Views0);
                    _ when is_list(Src) ->
                        diff_list(Az, EachDesc, Old, Views0)
                end,
            {RestOps, Views2} = diff_item_dynamics_v(NR, OR, Views1),
            {EachOps ++ RestOps, Views2};
        _ ->
            {RestOps, Views1} = diff_item_dynamics_v(NR, OR, Views0),
            {[make_op(Az, New, Old) | RestOps], Views1}
    end.

%% --- diff_child_dynamics -----------------------------------------------------

diff_child_dynamics([], []) ->
    [];
diff_child_dynamics([{Az, _New} | NR], [{Az, #{diff := false}} | OR]) ->
    diff_child_dynamics(NR, OR);
diff_child_dynamics([{Az, Same} | NR], [{Az, Same} | OR]) ->
    diff_child_dynamics(NR, OR);
diff_child_dynamics([{Az, New} | NR], [{Az, Old} | OR]) ->
    [make_child_op(Az, New, Old) | diff_child_dynamics(NR, OR)].

make_child_op(Az, {attr, Attr, false}, _Old) ->
    [?OP_REM_ATTR, Az, Attr];
make_child_op(Az, {attr, Attr, true}, _Old) ->
    [?OP_SET_ATTR, Az, Attr, <<>>];
make_child_op(Az, {attr, Attr, Val}, _Old) ->
    [?OP_SET_ATTR, Az, Attr, arizona_template:to_bin(Val)];
make_child_op(_Az, #{view_id := VId, s := S, d := NewD}, #{view_id := _, s := S, d := OldD}) ->
    [VId, diff_child_dynamics(NewD, OldD)];
make_child_op(Az, #{s := _, d := _} = New, #{s := _, d := _}) ->
    [?OP_TEXT, Az, arizona_render:zip_or_fp(New)];
make_child_op(Az, #{s := _, d := _} = New, _Old) ->
    [?OP_UPDATE, Az, arizona_render:zip_or_fp(New)];
make_child_op(Az, #{t := ?EACH, items := Items, template := Tmpl}, _Old) when
    is_list(Items)
->
    [?OP_UPDATE, Az, arizona_render:zip_list_fp(Tmpl, Items)];
make_child_op(Az, #{t := ?EACH, items := Items, order := Order, template := Tmpl}, _Old) ->
    [?OP_UPDATE, Az, arizona_render:zip_stream_fp(Tmpl, Items, Order)];
make_child_op(Az, remove, {attr, Attr, _}) ->
    [?OP_REM_ATTR, Az, Attr];
make_child_op(Az, remove, _Old) ->
    [?OP_REMOVE_NODE, Az];
make_child_op(Az, New, _Old) ->
    [?OP_TEXT, Az, arizona_template:to_bin(New)].

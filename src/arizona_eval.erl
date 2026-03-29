-module(arizona_eval).
-export([
    eval_dynamics/1,
    eval_dynamics_v/2,
    eval_one_v_flat/2,
    eval_each_def/1,
    eval_stream_items/4,
    render_stream_item/4,
    render_stream_items_simple/3,
    render_list_items/3,
    render_list_items_simple/2,
    render_map_items/3,
    render_map_items_simple/2,
    check_restricted_keys/3
]).
-ifdef(TEST).
-include_lib("eunit/include/eunit.hrl").
-endif.
-include("arizona.hrl").

-export([format_error/2]).

-define(RESTRICTED_KEYS, [id]).

%% --- eval_dynamics (no views, no dep tracking) -------------------------------

eval_dynamics(Dynamics) ->
    [eval_one(D) || D <:- Dynamics].

eval_one({Az, {attr, Name, Fun}, _Loc}) when is_function(Fun, 0) ->
    {Az, {attr, Name, eval_val(Fun())}};
eval_one({Az, Spec, _Loc}) ->
    {Az, eval_val(Spec)};
eval_one({Az, {attr, Name, Fun}}) when is_function(Fun, 0) ->
    {Az, {attr, Name, eval_val(Fun())}};
eval_one({Az, Spec}) ->
    {Az, eval_val(Spec)}.

eval_val(Fun) when is_function(Fun, 0) ->
    eval_val(Fun());
eval_val(#{t := ?EACH, source := Items, template := Tmpl}) when is_list(Items) ->
    ItemSnaps = render_list_items_simple(Items, Tmpl),
    #{t => ?EACH, items => ItemSnaps, template => Tmpl};
eval_val(#{
    t := ?EACH,
    source := #stream{
        items = ItemsMap,
        order = Order,
        limit = Limit
    },
    template := Tmpl
}) ->
    VKeys = arizona_template:visible_keys(Order, Limit),
    ItemSnaps = render_stream_items_simple(VKeys, ItemsMap, Tmpl),
    #{t => ?EACH, items => ItemSnaps, order => VKeys, template => Tmpl};
eval_val(#{t := ?EACH, source := Source, template := Tmpl}) when is_map(Source) ->
    ItemSnaps = render_map_items_simple(Source, Tmpl),
    #{t => ?EACH, items => ItemSnaps, template => Tmpl};
eval_val(#{s := Statics, d := Dynamics} = Tmpl) ->
    Snap0 = #{s => Statics, d => eval_dynamics(Dynamics)},
    arizona_template:maybe_propagate(Tmpl, Snap0);
eval_val(Val) ->
    Val.

%% --- eval_dynamics_v (with views + dep tracking) -----------------------------

eval_dynamics_v(Dynamics, Views0) ->
    lists:mapfoldl(fun eval_one_v/2, Views0, Dynamics).

eval_one_v({Az, {attr, Name, Fun}, _Loc}, Views) when is_function(Fun, 0) ->
    erlang:put('$arizona_deps', #{}),
    Val = eval_val(Fun()),
    Deps = erlang:erase('$arizona_deps'),
    {{Az, {attr, Name, Val}, Deps}, Views};
eval_one_v({Az, Spec, _Loc}, Views0) ->
    erlang:put('$arizona_deps', #{}),
    {Val, Views1} = eval_val_v(Spec, Views0),
    Deps = erlang:erase('$arizona_deps'),
    {{Az, Val, Deps}, Views1};
eval_one_v({Az, {attr, Name, Fun}}, Views) when is_function(Fun, 0) ->
    erlang:put('$arizona_deps', #{}),
    Val = eval_val(Fun()),
    Deps = erlang:erase('$arizona_deps'),
    {{Az, {attr, Name, Val}, Deps}, Views};
eval_one_v({Az, Spec}, Views0) ->
    erlang:put('$arizona_deps', #{}),
    {Val, Views1} = eval_val_v(Spec, Views0),
    Deps = erlang:erase('$arizona_deps'),
    {{Az, Val, Deps}, Views1}.

eval_val_v(Fun, Views0) when is_function(Fun, 0) ->
    eval_val_v(Fun(), Views0);
eval_val_v(#{stateful := H, props := Props}, {Old, New}) ->
    Id = maps:get(id, Props),
    SavedDeps = erlang:get('$arizona_deps'),
    case Old of
        #{Id := #{handler := H, bindings := B}} ->
            {B2, Resets} = arizona_template:call_handle_update(H, Props, B),
            Tmpl = H:render(B2),
            {ChildTriples, {Old, New1}} = eval_dynamics_v(maps:get(d, Tmpl), {Old, New}),
            {ChildD, ChildDeps} = arizona_template:split_triples(ChildTriples),
            Snap = arizona_template:make_child_snap(Tmpl, ChildD, ChildDeps, Id),
            B3 = maps:merge(B2, Resets),
            erlang:put('$arizona_deps', SavedDeps),
            {Snap, {Old, New1#{Id => #{handler => H, bindings => B3, snapshot => Snap}}}};
        #{} ->
            {B1, Resets} = H:mount(Props),
            ok = check_restricted_keys(B1, Props, H),
            Tmpl = H:render(B1),
            {ChildTriples, {Old, New1}} = eval_dynamics_v(maps:get(d, Tmpl), {Old, New}),
            {ChildD, ChildDeps} = arizona_template:split_triples(ChildTriples),
            Snap = arizona_template:make_child_snap(Tmpl, ChildD, ChildDeps, Id),
            B2 = maps:merge(B1, Resets),
            erlang:put('$arizona_deps', SavedDeps),
            {Snap, {Old, New1#{Id => #{handler => H, bindings => B2, snapshot => Snap}}}}
    end;
eval_val_v(#{t := ?EACH, source := Items, template := Tmpl}, {Old, New0}) when
    is_list(Items)
->
    SavedDeps = erlang:get('$arizona_deps'),
    {ItemSnaps, {_, LocalNew}} = render_list_items(Items, Tmpl, {Old, #{}}),
    erlang:put('$arizona_deps', SavedDeps),
    ChildViews = maps:keys(LocalNew),
    {
        #{t => ?EACH, items => ItemSnaps, template => Tmpl, child_views => ChildViews},
        {Old, maps:merge(New0, LocalNew)}
    };
eval_val_v(
    #{
        t := ?EACH,
        source := #stream{
            items = ItemsMap,
            order = Order,
            limit = Limit
        } = Source,
        template := Tmpl
    },
    {Old, New0}
) ->
    VKeys = arizona_template:visible_keys(Order, Limit),
    SavedDeps = erlang:get('$arizona_deps'),
    {ItemSnaps, {_, LocalNew}} = eval_stream_items(VKeys, ItemsMap, Tmpl, {Old, #{}}),
    erlang:put('$arizona_deps', SavedDeps),
    ChildViews = maps:keys(LocalNew),
    {
        #{
            t => ?EACH,
            items => ItemSnaps,
            order => VKeys,
            source => Source,
            template => Tmpl,
            child_views => ChildViews
        },
        {Old, maps:merge(New0, LocalNew)}
    };
eval_val_v(#{t := ?EACH, source := Source, template := Tmpl}, {Old, New0}) when
    is_map(Source)
->
    SavedDeps = erlang:get('$arizona_deps'),
    {ItemSnaps, {_, LocalNew}} = render_map_items(Source, Tmpl, {Old, #{}}),
    erlang:put('$arizona_deps', SavedDeps),
    ChildViews = maps:keys(LocalNew),
    {
        #{t => ?EACH, items => ItemSnaps, template => Tmpl, child_views => ChildViews},
        {Old, maps:merge(New0, LocalNew)}
    };
eval_val_v(#{callback := Callback, props := Props}, Views0) ->
    Tmpl = Callback(Props),
    eval_val_v(Tmpl, Views0);
eval_val_v(#{s := Statics, d := Dynamics} = Tmpl, {Old, New0}) ->
    SavedDeps = erlang:get('$arizona_deps'),
    {Triples, {_, LocalNew}} = eval_dynamics_v(Dynamics, {Old, #{}}),
    {D, DepsList} = arizona_template:split_triples(Triples),
    erlang:put('$arizona_deps', SavedDeps),
    ChildViews = maps:keys(LocalNew),
    Snap0 = #{s => Statics, d => D, deps => DepsList},
    Snap1 = arizona_template:maybe_propagate(Tmpl, Snap0),
    {Snap1#{child_views => ChildViews}, {Old, maps:merge(New0, LocalNew)}};
eval_val_v(Val, Views) ->
    {Val, Views}.

%% --- eval_stream_items -------------------------------------------------------

eval_stream_items(Keys, ItemsMap, Tmpl, Views) ->
    eval_stream_items(Keys, ItemsMap, Tmpl, Views, #{}).

eval_stream_items([], _ItemsMap, _Tmpl, Views, Acc) ->
    {Acc, Views};
eval_stream_items([K | Rest], ItemsMap, Tmpl, Views0, Acc) ->
    Item = maps:get(K, ItemsMap),
    {D, Views1} = render_stream_item(K, Item, Tmpl, Views0),
    eval_stream_items(Rest, ItemsMap, Tmpl, Views1, Acc#{K => D}).

render_stream_item(Key, Item, #{d := DFun}, Views0) ->
    Dynamics = DFun(Item, Key),
    {Triples, Views1} = eval_dynamics_v(Dynamics, Views0),
    D = [{Az, Val} || {Az, Val, _Deps} <:- Triples],
    {D, Views1}.

render_stream_items_simple(Keys, ItemsMap, Tmpl) ->
    render_stream_items_simple(Keys, ItemsMap, Tmpl, #{}).

render_stream_items_simple([], _ItemsMap, _Tmpl, Acc) ->
    Acc;
render_stream_items_simple([K | Rest], ItemsMap, Tmpl, Acc) ->
    Item = maps:get(K, ItemsMap),
    D = render_stream_item_simple(K, Item, Tmpl),
    render_stream_items_simple(Rest, ItemsMap, Tmpl, Acc#{K => D}).

render_stream_item_simple(Key, Item, #{d := DFun}) ->
    Dynamics = DFun(Item, Key),
    eval_dynamics(Dynamics).

%% --- List rendering helpers --------------------------------------------------

render_list_items_simple(Items, #{d := DFun}) ->
    [eval_dynamics(DFun(Item)) || Item <:- Items].

render_list_items(Items, #{d := DFun}, Views) ->
    render_list_items1(Items, DFun, Views).

render_list_items1([], _DFun, Views) ->
    {[], Views};
render_list_items1([Item | Rest], DFun, Views0) ->
    Dynamics = DFun(Item),
    {Triples, Views1} = eval_dynamics_v(Dynamics, Views0),
    D = [{Az, Val} || {Az, Val, _Deps} <:- Triples],
    {RestD, Views2} = render_list_items1(Rest, DFun, Views1),
    {[D | RestD], Views2}.

%% --- Map rendering helpers ---------------------------------------------------

render_map_items_simple(Map, #{d := DFun}) ->
    maps:fold(
        fun(K, V, Acc) -> [eval_dynamics(DFun(K, V)) | Acc] end,
        [],
        Map
    ).

render_map_items(Map, #{d := DFun}, Views) ->
    maps:fold(
        fun(K, V, {Acc, V0}) ->
            Dynamics = DFun(K, V),
            {Triples, V1} = eval_dynamics_v(Dynamics, V0),
            D = [{Az, Val} || {Az, Val, _Deps} <:- Triples],
            {[D | Acc], V1}
        end,
        {[], Views},
        Map
    ).

%% --- eval_one_v_flat ---------------------------------------------------------

eval_one_v_flat(Def, Views0) ->
    {{Az, Val, Deps}, Views1} = eval_one_v(Def, Views0),
    {Az, Val, Deps, Views1}.

%% --- eval_each_def -----------------------------------------------------------

eval_each_def({Az, Fun, _Loc}) when is_function(Fun, 0) ->
    erlang:put('$arizona_deps', #{}),
    #{t := ?EACH, source := _, template := _} = Val = Fun(),
    Deps = erlang:erase('$arizona_deps'),
    {Az, Val, Deps};
eval_each_def({Az, Fun}) when is_function(Fun, 0) ->
    erlang:put('$arizona_deps', #{}),
    #{t := ?EACH, source := _, template := _} = Val = Fun(),
    Deps = erlang:erase('$arizona_deps'),
    {Az, Val, Deps}.

format_error(restricted_key_modified, [{_M, _F, [Key, Handler], _Info} | _]) ->
    #{
        general => io_lib:format(
            "~s:mount/1 modified restricted key '~s'. "
            "This key is owned by the framework and cannot be changed.",
            [Handler, Key]
        )
    }.

check_restricted_keys(Bindings, Props, Handler) ->
    lists:foreach(
        fun(Key) ->
            case Props of
                #{Key := Expected} ->
                    case Bindings of
                        #{Key := Expected} ->
                            ok;
                        #{Key := _} ->
                            error(restricted_key_modified, [Key, Handler], [
                                {error_info, #{module => ?MODULE}}
                            ])
                    end;
                #{} ->
                    ok
            end
        end,
        ?RESTRICTED_KEYS
    ).

-ifdef(TEST).

eval_one_strips_location_test() ->
    %% eval_one with 3-tuple returns 2-tuple snapshot entry
    Result = eval_one({<<"0">>, fun() -> <<"val">> end, {some_mod, 7}}),
    ?assertEqual({<<"0">>, <<"val">>}, Result).

eval_one_strips_location_attr_test() ->
    %% eval_one with 3-tuple attr returns 2-tuple snapshot entry
    Result = eval_one({<<"0">>, {attr, <<"class">>, fun() -> <<"x">> end}, {m, 1}}),
    ?assertEqual({<<"0">>, {attr, <<"class">>, <<"x">>}}, Result).

eval_one_v_3tuple_test() ->
    %% eval_one_v with 3-tuple returns {Az, Val, Deps} triple
    {Result, _Views} = eval_one_v(
        {<<"0">>, fun() -> <<"val">> end, {some_mod, 7}}, {#{}, #{}}
    ),
    ?assertMatch({<<"0">>, <<"val">>, #{}}, Result).

eval_map_simple_test() ->
    Map = #{~"a" => ~"1", ~"b" => ~"2"},
    Tmpl = #{
        t => 0,
        s => [<<"<dt>">>, <<"</dt><dd>">>, <<"</dd>">>],
        d => fun(K, V) -> [{<<"0">>, fun() -> K end}, {<<"1">>, fun() -> V end}] end,
        f => <<"test">>
    },
    Result = render_map_items_simple(Map, Tmpl),
    ?assertEqual(2, length(Result)),
    %% Each item has key and value dynamics
    lists:foreach(
        fun(Item) ->
            [{<<"0">>, Key}, {<<"1">>, Val}] = Item,
            ?assert(is_binary(Key)),
            ?assert(is_binary(Val))
        end,
        Result
    ).

eval_map_empty_test() ->
    Tmpl = #{
        t => 0,
        s => [<<"<li>">>, <<"</li>">>],
        d => fun(K, _V) -> [{<<"0">>, fun() -> K end}] end,
        f => <<"test">>
    },
    ?assertEqual([], render_map_items_simple(#{}, Tmpl)).

eval_map_single_entry_test() ->
    Tmpl = #{
        t => 0,
        s => [<<"<dt>">>, <<"</dt><dd>">>, <<"</dd>">>],
        d => fun(K, V) -> [{<<"0">>, fun() -> K end}, {<<"1">>, fun() -> V end}] end,
        f => <<"test">>
    },
    [[{<<"0">>, ~"x"}, {<<"1">>, ~"1"}]] = render_map_items_simple(#{~"x" => ~"1"}, Tmpl).

eval_map_with_views_test() ->
    Map = #{~"a" => ~"1"},
    Tmpl = #{
        t => 0,
        s => [<<"<li>">>, <<"</li>">>],
        d => fun(K, V) -> [{<<"0">>, fun() -> K end}, {<<"1">>, fun() -> V end}] end,
        f => <<"test">>
    },
    {Items, {_Old, NewViews}} = render_map_items(Map, Tmpl, {#{}, #{}}),
    ?assertEqual(1, length(Items)),
    ?assertEqual(#{}, NewViews).

eval_map_via_eval_val_test() ->
    %% Test the full eval_val path for maps
    Map = #{~"x" => ~"1"},
    Tmpl = #{
        t => 0,
        s => [<<"<li>">>, <<"</li>">>],
        d => fun(K, _V) -> [{<<"0">>, fun() -> K end}] end,
        f => <<"test">>
    },
    Result = eval_val(#{t => 0, source => Map, template => Tmpl}),
    ?assertMatch(#{t := 0, items := _, template := _}, Result),
    ?assertEqual(1, length(maps:get(items, Result))).

check_restricted_keys_ok_test() ->
    %% Same id in Props and Bindings -- ok
    ?assertEqual(ok, check_restricted_keys(#{id => <<"v">>}, #{id => <<"v">>}, my_handler)).

check_restricted_keys_no_id_in_props_test() ->
    %% No id in Props (root handler) -- ok, mount can set default
    ?assertEqual(ok, check_restricted_keys(#{id => <<"v">>}, #{}, my_handler)).

check_restricted_keys_modified_test() ->
    %% Mount changed id -- raises with handler info
    ?assertError(
        restricted_key_modified,
        check_restricted_keys(#{id => <<"changed">>}, #{id => <<"original">>}, my_handler)
    ).

format_error_restricted_key_test() ->
    %% format_error produces readable message with handler and key
    StackFrame = [{arizona_eval, check, [id, my_handler], []}],
    #{general := Msg} = format_error(restricted_key_modified, StackFrame),
    MsgBin = iolist_to_binary(Msg),
    ?assertNotEqual(nomatch, binary:match(MsgBin, <<"my_handler:mount/1">>)),
    ?assertNotEqual(nomatch, binary:match(MsgBin, <<"'id'">>)).

-endif.

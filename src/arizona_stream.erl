-module(arizona_stream).
-include("arizona.hrl").

-export([
    new/1, new/2, new/3,
    insert/2, insert/3,
    delete/2,
    update/3,
    move/3,
    reset/1, reset/2,
    sort/2,
    to_list/1,
    get/2, get/3,
    get_lazy/3,
    clear_stream_pending/2,
    stream_keys/1
]).
-ignore_xref([
    new/1,
    new/2,
    new/3,
    insert/2,
    insert/3,
    delete/2,
    update/3,
    move/3,
    reset/1,
    reset/2,
    sort/2,
    to_list/1,
    get/2,
    get/3,
    get_lazy/3
]).

%% --- Public API -------------------------------------------------------------

new(KeyFun) when is_function(KeyFun, 1) ->
    #stream{
        key = KeyFun,
        items = #{},
        order = [],
        pending = queue:new(),
        limit = infinity,
        on_limit = halt,
        size = 0
    }.

new(KeyFun, Items) when is_function(KeyFun, 1), is_list(Items) ->
    new(KeyFun, Items, #{}).

new(KeyFun, Items, Opts) when is_function(KeyFun, 1), is_list(Items), is_map(Opts) ->
    Limit = maps:get(limit, Opts, infinity),
    OnLimit = maps:get(on_limit, Opts, halt),
    Keyed = key_items(KeyFun, Items),
    ItemsMap = #{K => V || {K, V} <:- Keyed},
    Order = order_from_keyed(Keyed),
    Pending = pending_from_keyed(Keyed, queue:new()),
    #stream{
        key = KeyFun,
        items = ItemsMap,
        order = Order,
        pending = Pending,
        limit = Limit,
        on_limit = OnLimit,
        size = map_size(ItemsMap)
    }.

insert(
    #stream{
        key = KeyFun,
        items = Items,
        order = Order,
        pending = Pending,
        size = Size
    } = S,
    Item
) ->
    Key = KeyFun(Item),
    S#stream{
        items = Items#{Key => Item},
        order = Order ++ [Key],
        pending = queue:in({insert, Key, Item, -1}, Pending),
        size = Size + 1
    }.

insert(
    #stream{
        key = KeyFun,
        items = Items,
        order = Order,
        pending = Pending,
        size = Size
    } = S,
    Item,
    Pos
) ->
    Key = KeyFun(Item),
    S#stream{
        items = Items#{Key => Item},
        order = order_insert_at(Order, Key, Pos),
        pending = queue:in({insert, Key, Item, Pos}, Pending),
        size = Size + 1
    }.

delete(
    #stream{
        items = Items,
        order = Order,
        pending = Pending,
        size = Size
    } = S,
    Key
) ->
    case maps:take(Key, Items) of
        {_, NewItems} ->
            S#stream{
                items = NewItems,
                order = order_delete(Order, Key),
                pending = queue:in({delete, Key}, Pending),
                size = Size - 1
            };
        error ->
            S
    end.

update(#stream{items = Items, pending = Pending} = S, Key, NewItem) ->
    S#stream{
        items = Items#{Key => NewItem},
        pending = queue:in({update, Key, NewItem}, Pending)
    }.

move(
    #stream{
        items = Items,
        order = Order,
        pending = Pending,
        size = Size
    } = S,
    Key,
    NewPos
) ->
    case Items of
        #{Key := _} ->
            Order1 = order_delete(Order, Key),
            Order2 = order_insert_at(Order1, Key, min(NewPos, Size - 1)),
            AfterKey = key_before(Key, Order2),
            S#stream{
                order = Order2,
                pending = queue:in({move, Key, AfterKey}, Pending)
            };
        #{} ->
            S
    end.

reset(#stream{} = S) ->
    S#stream{
        items = #{},
        order = [],
        pending = queue:from_list([reset]),
        size = 0
    }.

reset(#stream{key = KeyFun} = S, NewItems) when is_list(NewItems) ->
    Keyed = key_items(KeyFun, NewItems),
    ItemsMap = #{K => V || {K, V} <:- Keyed},
    Order = order_from_keyed(Keyed),
    S#stream{
        items = ItemsMap,
        order = Order,
        pending = queue:from_list([reset]),
        size = map_size(ItemsMap)
    }.

to_list(#stream{items = Items, order = Order}) ->
    [maps:get(K, Items) || K <:- Order].

get(#stream{items = Items}, Key) ->
    maps:get(Key, Items).

get(#stream{items = Items}, Key, Default) ->
    maps:get(Key, Items, Default).

get_lazy(#stream{items = Items}, Key, Fun) when is_function(Fun, 0) ->
    case Items of
        #{Key := Val} -> Val;
        #{} -> Fun()
    end.

sort(
    #stream{items = Items, order = Order, pending = Pending} = S,
    CompareFun
) ->
    NewOrder = lists:sort(
        fun(K1, K2) ->
            CompareFun(maps:get(K1, Items), maps:get(K2, Items))
        end,
        Order
    ),
    case NewOrder =:= Order of
        true ->
            S;
        false ->
            S#stream{
                order = NewOrder,
                pending = queue:in(reorder, Pending)
            }
    end.

%% --- Binding-level stream helpers -------------------------------------------

clear_stream_pending(Bindings, []) ->
    Bindings;
clear_stream_pending(Bindings, [K | Rest]) ->
    case Bindings of
        #{K := #stream{} = S} ->
            clear_stream_pending(Bindings#{K => S#stream{pending = queue:new()}}, Rest);
        _ ->
            clear_stream_pending(Bindings, Rest)
    end.

stream_keys(Bindings) when is_map(Bindings) ->
    [K || K := #stream{} <- Bindings].

%% --- Internal helpers -------------------------------------------------------

key_items(_KeyFun, []) -> [];
key_items(KeyFun, [I | Rest]) -> [{KeyFun(I), I} | key_items(KeyFun, Rest)].

order_from_keyed([]) -> [];
order_from_keyed([{K, _} | Rest]) -> [K | order_from_keyed(Rest)].

pending_from_keyed([], Q) -> Q;
pending_from_keyed([{K, I} | Rest], Q) -> pending_from_keyed(Rest, queue:in({insert, K, I, -1}, Q)).

order_insert_at(Order, Key, 0) -> [Key | Order];
order_insert_at([], Key, _Pos) -> [Key];
order_insert_at([H | T], Key, Pos) -> [H | order_insert_at(T, Key, Pos - 1)].

order_delete([], _Key) -> [];
order_delete([Key | T], Key) -> T;
order_delete([H | T], Key) -> [H | order_delete(T, Key)].

%% Find the key immediately before Key in Order, or null if Key is first.
key_before(Key, [Key | _]) -> null;
key_before(Key, [Prev, Key | _]) -> Prev;
key_before(Key, [_ | Rest]) -> key_before(Key, Rest);
key_before(_Key, []) -> null.

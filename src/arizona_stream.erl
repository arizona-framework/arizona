-module(arizona_stream).
-moduledoc """
Ordered keyed collection with queued mutations for efficient list diffing.

A stream stores items in a key-indexed map plus an ordered list of keys,
along with a `queue` of pending operations. Mutations (`insert/2,3`,
`delete/2`, `update/3`, `move/3`, `sort/2`, `reset/1,2`) update the
collection eagerly and append a corresponding op to the queue. The
queue is later drained by `arizona_diff:diff/4` to emit minimal patch
operations to the client.

This avoids re-rendering whole lists when only a few items change --
the differ knows exactly which keys to insert, remove, update, or move.

## Limit modes

- `infinity` (default) -- no cap, all items remain visible
- `{Limit, halt}` -- once `Limit` items are present, further inserts
  are rejected (kept in `items` but not visible)
- `{Limit, drop}` -- once `Limit` is reached, oldest items are dropped
  to make room

## Pending operation shapes

```
{insert, Key, Item, Pos}
{delete, Key}
{update, Key, NewItem, Changed}  %% Changed :: #{key() => true}, captured at update/3 time
{move, Key, AfterKey}    %% AfterKey is `null` for first position
reorder                  %% from sort/2 when order changes
{reset, OldItems}        %% from reset/1,2 -- OldItems captured pre-mutation for per-item skipping
```

## Example

```erlang
1> S = arizona_stream:new(fun(#{id := Id}) -> Id end).
2> S1 = arizona_stream:insert(S, #{id => 1, name => ~"Alice"}).
3> S2 = arizona_stream:insert(S1, #{id => 2, name => ~"Bob"}).
4> arizona_stream:to_list(S2).
[#{id => 1, name => ~"Alice"}, #{id => 2, name => ~"Bob"}]
```
""".

-include("arizona.hrl").

%% --------------------------------------------------------------------
%% API function exports
%% --------------------------------------------------------------------

-export([new/1]).
-export([new/2]).
-export([new/3]).
-export([insert/2]).
-export([insert/3]).
-export([delete/2]).
-export([update/3]).
-export([move/3]).
-export([reset/1]).
-export([reset/2]).
-export([sort/2]).
-export([to_list/1]).
-export([get/2]).
-export([get/3]).
-export([get_lazy/3]).
-export([clear_stream_pending/2]).
-export([stream_keys/1]).
-export([compute_item_changed/2]).

%% --------------------------------------------------------------------
%% Ignore xref warnings
%% --------------------------------------------------------------------

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

%% --------------------------------------------------------------------
%% Types exports
%% --------------------------------------------------------------------

-export_type([stream/0]).
-export_type([key/0]).
-export_type([item/0]).
-export_type([key_fun/0]).
-export_type([opts/0]).

%% --------------------------------------------------------------------
%% Types definitions
%% --------------------------------------------------------------------

-nominal stream() :: #stream{}.
-nominal key() :: term().
-nominal item() :: term().
-nominal key_fun() :: fun((item()) -> key()).
-nominal opts() :: #{
    limit => pos_integer() | infinity,
    on_limit => halt | drop
}.

%% --------------------------------------------------------------------
%% API Functions
%% --------------------------------------------------------------------

-doc """
Creates an empty stream with the given key function.
""".
-spec new(KeyFun) -> stream() when
    KeyFun :: key_fun().
new(KeyFun) when is_function(KeyFun, 1) ->
    #stream{
        key = KeyFun,
        items = #{},
        order = {[], []},
        pending = queue:new(),
        limit = infinity,
        on_limit = halt,
        size = 0
    }.

-doc """
Creates a stream pre-populated with `Items`. Equivalent to
`new(KeyFun, Items, #{})`.
""".
-spec new(KeyFun, Items) -> stream() when
    KeyFun :: key_fun(),
    Items :: [item()].
new(KeyFun, Items) when is_function(KeyFun, 1), is_list(Items) ->
    new(KeyFun, Items, #{}).

-doc """
Creates a stream pre-populated with `Items` and the given options.
Each item is queued as a pending insert so the first render emits
the corresponding ops.
""".
-spec new(KeyFun, Items, Opts) -> stream() when
    KeyFun :: key_fun(),
    Items :: [item()],
    Opts :: opts().
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
        order = {Order, []},
        pending = Pending,
        limit = Limit,
        on_limit = OnLimit,
        size = map_size(ItemsMap)
    }.

-doc """
Appends `Item` to the end of the stream and queues an insert op.
""".
-spec insert(Stream, Item) -> Stream1 when
    Stream :: stream(),
    Item :: item(),
    Stream1 :: stream().
insert(
    #stream{
        key = KeyFun,
        items = Items,
        order = {Front, Back},
        pending = Pending,
        size = Size
    } = S,
    Item
) ->
    Key = KeyFun(Item),
    %% O(1): cons to BackRev instead of `Front ++ [Key]`. The buffer is
    %% flushed by the next non-insert/2 operation (or by visible_keys
    %% on read). For 1000 sequential inserts this turns O(N^2) into O(N).
    S#stream{
        items = Items#{Key => Item},
        order = {Front, [Key | Back]},
        pending = queue:in({insert, Key, Item, -1}, Pending),
        size = Size + 1
    }.

-doc """
Inserts `Item` at the given zero-based `Pos` and queues an insert op.
""".
-spec insert(Stream, Item, Pos) -> Stream1 when
    Stream :: stream(),
    Item :: item(),
    Pos :: non_neg_integer(),
    Stream1 :: stream().
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
    Flat = flat_order(Order),
    S#stream{
        items = Items#{Key => Item},
        order = {order_insert_at(Flat, Key, Pos), []},
        pending = queue:in({insert, Key, Item, Pos}, Pending),
        size = Size + 1
    }.

-doc """
Removes the item with `Key` and queues a delete op. No-op if `Key`
is not present.
""".
-spec delete(Stream, Key) -> Stream1 when
    Stream :: stream(),
    Key :: key(),
    Stream1 :: stream().
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
            Flat = flat_order(Order),
            S#stream{
                items = NewItems,
                order = {order_delete(Flat, Key), []},
                pending = queue:in({delete, Key}, Pending),
                size = Size - 1
            };
        error ->
            S
    end.

-doc """
Replaces the item at `Key` with `NewItem` and queues an update op.
""".
-spec update(Stream, Key, NewItem) -> Stream1 when
    Stream :: stream(),
    Key :: key(),
    NewItem :: item(),
    Stream1 :: stream().
update(#stream{items = Items, pending = Pending} = S, Key, NewItem) ->
    Changed =
        case Items of
            #{Key := OldItem} -> compute_item_changed(OldItem, NewItem);
            #{} -> #{}
        end,
    S#stream{
        items = Items#{Key => NewItem},
        pending = queue:in({update, Key, NewItem, Changed}, Pending)
    }.

-doc """
Set of keys that differ between two item maps -- added, removed, or value
changed. Used by `update/3` and the differ's reset path to drive per-item
dynamic skipping. Items in Arizona are maps; non-map inputs crash here.
""".
-spec compute_item_changed(OldItem, NewItem) -> Changed when
    OldItem :: map(),
    NewItem :: map(),
    Changed :: #{term() => true}.
compute_item_changed(OldItem, NewItem) ->
    All = maps:merge(OldItem, NewItem),
    maps:fold(
        fun(K, _, Acc) ->
            case OldItem of
                #{K := V} ->
                    case NewItem of
                        #{K := V} -> Acc;
                        #{} -> Acc#{K => true}
                    end;
                #{} ->
                    Acc#{K => true}
            end
        end,
        #{},
        All
    ).

-doc """
Moves the item with `Key` to a new zero-based position and queues a
move op. No-op if `Key` is not present. The position is clamped to the
last index.
""".
-spec move(Stream, Key, NewPos) -> Stream1 when
    Stream :: stream(),
    Key :: key(),
    NewPos :: non_neg_integer(),
    Stream1 :: stream().
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
            Flat = flat_order(Order),
            Order1 = order_delete(Flat, Key),
            Order2 = order_insert_at(Order1, Key, min(NewPos, Size - 1)),
            AfterKey = key_before(Key, Order2),
            S#stream{
                order = {Order2, []},
                pending = queue:in({move, Key, AfterKey}, Pending)
            };
        #{} ->
            S
    end.

-doc """
Empties the stream and queues a reset op.
""".
-spec reset(Stream) -> Stream1 when
    Stream :: stream(),
    Stream1 :: stream().
reset(#stream{items = OldItems} = S) ->
    S#stream{
        items = #{},
        order = {[], []},
        pending = queue:from_list([{reset, OldItems}]),
        size = 0
    }.

-doc """
Replaces all items with `NewItems` and queues a reset op. The differ
will reuse client-side keys that are still present, only emitting ops
for the actual delta.
""".
-spec reset(Stream, NewItems) -> Stream1 when
    Stream :: stream(),
    NewItems :: [item()],
    Stream1 :: stream().
reset(#stream{key = KeyFun, items = OldItems} = S, NewItems) when is_list(NewItems) ->
    Keyed = key_items(KeyFun, NewItems),
    ItemsMap = #{K => V || {K, V} <:- Keyed},
    Order = order_from_keyed(Keyed),
    S#stream{
        items = ItemsMap,
        order = {Order, []},
        pending = queue:from_list([{reset, OldItems}]),
        size = map_size(ItemsMap)
    }.

-doc """
Reorders the stream by `CompareFun`. Queues a `reorder` op only when
the resulting order actually differs.
""".
-spec sort(Stream, CompareFun) -> Stream1 when
    Stream :: stream(),
    CompareFun :: fun((item(), item()) -> boolean()),
    Stream1 :: stream().
sort(
    #stream{items = Items, order = Order, pending = Pending} = S,
    CompareFun
) ->
    Flat = flat_order(Order),
    NewOrder = lists:sort(
        fun(K1, K2) ->
            CompareFun(maps:get(K1, Items), maps:get(K2, Items))
        end,
        Flat
    ),
    case NewOrder =:= Flat of
        true ->
            S#stream{order = {Flat, []}};
        false ->
            S#stream{
                order = {NewOrder, []},
                pending = queue:in(reorder, Pending)
            }
    end.

-doc """
Returns all items in display order as a list.
""".
-spec to_list(Stream) -> [item()] when
    Stream :: stream().
to_list(#stream{items = Items, order = Order}) ->
    [maps:get(K, Items) || K <- flat_order(Order)].

-doc """
Returns the item at `Key`. Errors with `{badkey, Key}` if not present.
""".
-spec get(Stream, Key) -> item() when
    Stream :: stream(),
    Key :: key().
get(#stream{items = Items}, Key) ->
    maps:get(Key, Items).

-doc """
Returns the item at `Key`, or `Default` if not present.
""".
-spec get(Stream, Key, Default) -> item() | Default when
    Stream :: stream(),
    Key :: key(),
    Default :: term().
get(#stream{items = Items}, Key, Default) ->
    maps:get(Key, Items, Default).

-doc """
Like `get/3` but the default is computed lazily by a 0-arity fun.
""".
-spec get_lazy(Stream, Key, Fun) -> item() | term() when
    Stream :: stream(),
    Key :: key(),
    Fun :: fun(() -> term()).
get_lazy(#stream{items = Items}, Key, Fun) when is_function(Fun, 0) ->
    case Items of
        #{Key := Val} -> Val;
        #{} -> Fun()
    end.

-doc """
Clears the pending queue on every stream stored under any of the
listed `Keys` in `Bindings`. Called by the live process after a render
flushes the queued ops.
""".
-spec clear_stream_pending(Bindings, Keys) -> Bindings1 when
    Bindings :: map(),
    Keys :: [term()],
    Bindings1 :: map().
clear_stream_pending(Bindings, []) ->
    Bindings;
clear_stream_pending(Bindings, [K | Rest]) ->
    case Bindings of
        #{K := #stream{} = S} ->
            clear_stream_pending(Bindings#{K => S#stream{pending = queue:new()}}, Rest);
        _ ->
            clear_stream_pending(Bindings, Rest)
    end.

-doc """
Returns the binding keys whose values are streams.
""".
-spec stream_keys(Bindings) -> [term()] when
    Bindings :: map().
stream_keys(Bindings) when is_map(Bindings) ->
    [K || K := #stream{} <- Bindings].

%% --------------------------------------------------------------------
%% Internal functions
%% --------------------------------------------------------------------

%% Flush the {Front, BackRev} order buffer to a flat oldest-first list.
%% Called by every operation that needs the full ordered keys; only
%% `insert/2` (the bulk-append hot path) leaves the buffer non-empty.
flat_order({Front, []}) -> Front;
flat_order({Front, Back}) -> Front ++ lists:reverse(Back).

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

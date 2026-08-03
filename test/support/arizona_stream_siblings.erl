-module(arizona_stream_siblings).
-include("arizona_stateful.hrl").
-export([mount/1]).
-export([render/1]).

%% A stream `?each` sharing its parent's content slot with static siblings and
%% another dynamic. SSR gives the each a COMPOUND slot az (`<Root>:1`) carried by
%% NO element -- the client can only reach it through its `<!--az:X-->` marker,
%% and the compound base az (`<Root>`) is the view ROOT's own az, which a
%% descendant-only `querySelector` cannot return. So an `?OP_UPDATE` container
%% re-render resolves to the root and innerHTML-wipes the header, the title slot
%% and the footer; only the marker-aware `?OP_TEXT` patches the slot in place.
%% Mirrors `arizona_mixed_children`, which pins the same rule for a plain list.
%%
%% `items` accepts an `arizona_stream` OR a map, so a test can drive the
%% type-switch that makes the diff re-render the whole container. The 2-arg
%% `?each` callback is handed `(Item, Key)` for a stream and `(Key, Value)` for a
%% map, hence the shape test on the first argument.

-spec mount(az:bindings()) -> az:mount_ret().
mount(Bindings) ->
    {
        #{
            id => maps:get(id, Bindings, ~"siblings"),
            title => maps:get(title, Bindings, ~"T"),
            items => maps:get(items, Bindings, arizona_stream:new(fun key_of/1, []))
        },
        #{}
    }.

-spec render(az:bindings()) -> az:template().
render(Bindings) ->
    ?html(
        {'div', [{id, ?get(id)}], [
            {p, [{class, ~"header"}], [~"header"]},
            ?get(title),
            ?each(
                fun(A, B) ->
                    {li,
                        [
                            {az_key,
                                case A of
                                    Key when is_binary(Key) -> Key;
                                    _Item -> B
                                end}
                        ],
                        [
                            case A of
                                #{label := L} -> L;
                                _Key -> maps:get(label, B)
                            end
                        ]}
                end,
                ?get(items)
            ),
            {p, [{class, ~"footer"}], [~"footer"]}
        ]}
    ).

key_of(#{id := Id}) -> integer_to_binary(Id).

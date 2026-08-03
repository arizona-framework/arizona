-module(arizona_stream_siblings_page).
-include("arizona_stateful.hrl").
-export([mount/1]).
-export([render/1]).
-export([handle_event/3]).

%% Routable twin of `arizona_stream_siblings`, which the diff/render suites pin
%% server-side. That fixture is deliberately left untouched: its CT cases assert exact
%% `az` values (`strip:2`), so adding event buttons there would shift the numbering.
%%
%% Same shape, and it is the shape that matters: a stream `?each` sharing one content
%% slot with static siblings. The each gets a COMPOUND slot az whose base is the view
%% ROOT's own az, carried by no element of its own -- so the client can only reach it
%% through the `<!--az:X-->` marker. An `?OP_UPDATE` container re-render resolves to the
%% root and innerHTML-wipes the header, the title and the footer; only the marker-aware
%% `?OP_TEXT` patches the slot in place. The CT suites prove the server EMITS `?OP_TEXT`;
%% this exists so a real browser proves the client APPLIES it without taking the
%% siblings, which is the half a jsdom probe cannot settle.
%%
%% `arizona_datatable` does not cover this: its each is the sole child of a `tbody`, the
%% one shape that survives either op.

-spec mount(az:bindings()) -> az:mount_ret().
mount(Init) ->
    Items = [
        #{id => 1, label => ~"one"},
        #{id => 2, label => ~"two"}
    ],
    Bindings = #{
        id => ~"page",
        title => maps:get(title, Init, ~"StreamSiblings"),
        items => arizona_stream:new(fun key_of/1, Items)
    },
    {Bindings, #{}}.

-spec render(az:bindings()) -> az:template().
render(Bindings) ->
    ?html(
        {main, [{id, ?get(id)}], [
            {button, [{id, ~"add"}, {az_click, arizona_js:push_event(~"add")}], [~"Add"]},
            {button, [{id, ~"reset"}, {az_click, arizona_js:push_event(~"reset")}], [~"Reset"]},
            {button, [{id, ~"switch"}, {az_click, arizona_js:push_event(~"switch")}], [~"Switch"]},
            %% One content slot, four children: static, dynamic, the stream, static.
            {'div', [{id, ~"host"}], [
                {p, [{class, ~"header"}], [~"header"]},
                ?get(title),
                %% `items` is a stream OR a map, so the 2-arg callback is handed
                %% `(Item, Key)` for a stream and `(Key, Value)` for a map. Hence the
                %% shape test on the first argument, mirroring `arizona_stream_siblings`.
                ?each(
                    fun(A, B) ->
                        {span,
                            [
                                {az_key,
                                    case A of
                                        Key when is_binary(Key) -> Key;
                                        _Item -> B
                                    end},
                                {class, ~"item"}
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
        ]}
    ).

-spec handle_event(binary(), az:payload(), az:bindings()) -> az:handle_event_ret().
handle_event(~"add", _Payload, Bindings) ->
    %% Incremental: an `?OP_INSERT` addressed by key, not a container render.
    Items = arizona_stream:insert(?get(items), #{id => 9, label => ~"added"}),
    {Bindings#{items => Items}, #{}, []};
handle_event(~"reset", _Payload, Bindings) ->
    %% Still incremental: a keyed reset diffs against the old order and emits per-item
    %% ops, so this does NOT drive a container render. Kept because it is the ordinary
    %% path and the siblings must survive it too.
    Items = arizona_stream:reset(?get(items), [#{id => 7, label => ~"reset"}]),
    {Bindings#{items => Items}, #{}, []};
handle_event(~"switch", _Payload, Bindings) ->
    %% Flips `items` from a stream to a map, so the diff has no `order` to compare and
    %% takes the type-switch clause: a wholesale container re-render addressed to the
    %% each's compound slot az. That az is carried by no element, and its base is the
    %% enclosing `#host`, so this is the one event that exercises the marker path.
    {Bindings#{items => #{~"k" => #{label => ~"switched"}}}, #{}, []}.

key_of(#{id := Id}) -> integer_to_binary(Id).

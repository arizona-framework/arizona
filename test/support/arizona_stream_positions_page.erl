-module(arizona_stream_positions_page).
-include("arizona_stateful.hrl").
-export([mount/1]).
-export([render/1]).
-export([handle_event/3]).
-export([cases/0]).

%% An unlimited stream driven through one event per case: each case first resets
%% the stream to its starting keys (`start`), then applies its steps in ONE event
%% (`run`). A key a case inserts and deletes again never reaches the client, so a
%% positional insert or a move queued while that key was in the source was placed
%% against an order the client never held. `#server-order` renders the stream's
%% own final order beside the list, so a browser test compares the two directly.

-spec cases() -> [{[binary()], [tuple()]}].
cases() ->
    [
        %% Positional ops queued while a transient key was in the source.
        {[~"a"], [{ins, ~"n", 0}, {ins, ~"m", 1}, {del, ~"n"}]},
        {[~"a", ~"b"], [{ins, ~"n", 0}, {ins, ~"m", 2}, {del, ~"n"}]},
        {[~"a", ~"b", ~"c"], [{ins, ~"n", 1}, {ins, ~"m", 3}, {del, ~"n"}]},
        {[~"a", ~"b"], [{ins, ~"n", 0}, {move, ~"b", 1}, {del, ~"n"}]},
        {[~"a", ~"b"], [{upsert, ~"n"}, {ins, ~"m", 3}, {move, ~"a", 2}, {del, ~"n"}]},
        %% Shapes the op-by-op drain already placed right.
        {[~"a"], [{ins, ~"n", 0}, {ins, ~"m", 2}, {del, ~"n"}]},
        {[~"a"], [{ins, ~"n", last}, {ins, ~"m", 1}, {del, ~"n"}]},
        {[~"a", ~"b"], [{ins, ~"n", last}, {ins, ~"m", 2}, {del, ~"n"}]},
        {[~"a", ~"b"], [{ins, ~"n", 0}, {ins, ~"m", 1}, {del, ~"n"}, {ins, ~"n", 0}]},
        {[~"a", ~"b"], [{ins, ~"n", 0}, {del, ~"n"}, {ins, ~"m", 1}]},
        {[~"a", ~"b"], [{del, ~"a"}, {ins, ~"m", 1}]},
        {[~"a", ~"b"], [{ins, ~"m", 1}, {move, ~"a", 2}]}
    ].

-spec mount(az:bindings()) -> az:mount_ret().
mount(_Bindings) ->
    {#{id => ~"positions", items => arizona_stream:new(fun key/1, [#{id => ~"a"}])}, #{}}.

-spec render(az:bindings()) -> az:template().
render(Bindings) ->
    ?html(
        {main, [{id, ?get(id)}], [
            {'div', [], [
                ?each(
                    fun(N) ->
                        {button,
                            [
                                {id, <<"start-", (integer_to_binary(N))/binary>>},
                                {'data-start', start_keys(N)},
                                {az_click, arizona_js:push_event(~"start", #{~"case" => N})}
                            ],
                            [~"start"]}
                    end,
                    lists:seq(1, length(cases()))
                )
            ]},
            {'div', [], [
                ?each(
                    fun(N) ->
                        {button,
                            [
                                {id, <<"run-", (integer_to_binary(N))/binary>>},
                                {az_click, arizona_js:push_event(~"run", #{~"case" => N})}
                            ],
                            [~"run"]}
                    end,
                    lists:seq(1, length(cases()))
                )
            ]},
            {p, [{id, ~"server-order"}], [server_order(?get(items))]},
            {ul, [{id, ~"list"}], [
                ?each(fun(_Item, Key) -> {li, [{az_key, Key}], [Key]} end, ?get(items))
            ]}
        ]}
    ).

-spec handle_event(az:event_name(), az:event_payload(), az:bindings()) ->
    az:handle_event_ret().
handle_event(~"start", #{~"case" := N}, #{items := S} = Bindings) ->
    {Start, _Steps} = lists:nth(N, cases()),
    {Bindings#{items => arizona_stream:reset(S, [#{id => K} || K <- Start])}, #{}, []};
handle_event(~"run", #{~"case" := N}, #{items := S} = Bindings) ->
    {_Start, Steps} = lists:nth(N, cases()),
    {Bindings#{items => lists:foldl(fun step/2, S, Steps)}, #{}, []}.

step({ins, Id, last}, S) -> arizona_stream:insert(S, #{id => Id});
step({ins, Id, Pos}, S) -> arizona_stream:insert(S, #{id => Id}, Pos);
step({upsert, Id}, S) -> arizona_stream:update(S, Id, #{id => Id});
step({del, Id}, S) -> arizona_stream:delete(S, Id);
step({move, Id, Pos}, S) -> arizona_stream:move(S, Id, Pos).

start_keys(N) ->
    {Start, _Steps} = lists:nth(N, cases()),
    iolist_to_binary(lists:join(~",", Start)).

server_order(S) ->
    lists:join(~",", [Id || #{id := Id} <- arizona_stream:to_list(S)]).

key(#{id := Id}) -> Id.

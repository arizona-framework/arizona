-module(arizona_bench_multi_key).
-include("arizona_view.hrl").
-export([mount/2, render/1, handle_event/3]).

%% Bench fixture: a flat view with 10 dynamics, each tracking a different
%% binding key. The `bump_three` event mutates 3 of the 10 bindings at
%% once -- exercises `arizona_live:compute_changed/2` and the per-dynamic
%% `deps_changed` intersection over a multi-key Changed map.

-spec mount(az:bindings(), az:request()) -> az:mount_ret().
mount(Bindings, _Req) ->
    Defaults = maps:from_list([{key_for(I), 0} || I <- lists:seq(0, 9)]),
    Vals = #{K => maps:get(K, Bindings, V) || K := V <- Defaults},
    {Vals#{id => ~"multi_key"}, #{}}.

-spec render(az:bindings()) -> az:template().
render(Bindings) ->
    ?html(
        {'div', [{id, ?get(id)}], [
            {span, [], ?get(b0)},
            {span, [], ?get(b1)},
            {span, [], ?get(b2)},
            {span, [], ?get(b3)},
            {span, [], ?get(b4)},
            {span, [], ?get(b5)},
            {span, [], ?get(b6)},
            {span, [], ?get(b7)},
            {span, [], ?get(b8)},
            {span, [], ?get(b9)}
        ]}
    ).

-spec handle_event(az:event_name(), az:event_payload(), az:bindings()) ->
    az:handle_event_ret().
handle_event(~"bump_three", #{~"value" := Value}, Bindings) ->
    %% Mutate bindings b0, b3, b6 simultaneously -- 3 of 10 dynamics
    %% should re-evaluate; the diff fast path skips the other 7.
    {Bindings#{b0 => Value, b3 => Value, b6 => Value}, #{}, []}.

key_for(I) ->
    list_to_atom("b" ++ integer_to_list(I)).

-module(arizona_bench_many_dyn).
-include("arizona_view.hrl").
-export([mount/2, render/1]).

%% Bench fixture: a flat view with 50 top-level dynamics, each tracking
%% a distinct binding key (`b00`..`b49`). Measures how dep tracking and
%% the diff fast-path scale linearly with the dynamic count of a single
%% template (vs `?each` fan-out which `render_each_100` already covers).

-spec mount(az:bindings(), az:request()) -> az:mount_ret().
mount(Bindings, _Req) ->
    Defaults = maps:from_list([{key_for(I), I} || I <- lists:seq(0, 49)]),
    {maps:merge(Defaults, Bindings#{id => ~"many_dyn"}), #{}}.

-spec render(az:bindings()) -> az:template().
render(Bindings) ->
    ?html(
        {'div', [{id, ?get(id)}], [
            {span, [], ?get(b00)}, {span, [], ?get(b01)},
            {span, [], ?get(b02)}, {span, [], ?get(b03)},
            {span, [], ?get(b04)}, {span, [], ?get(b05)},
            {span, [], ?get(b06)}, {span, [], ?get(b07)},
            {span, [], ?get(b08)}, {span, [], ?get(b09)},
            {span, [], ?get(b10)}, {span, [], ?get(b11)},
            {span, [], ?get(b12)}, {span, [], ?get(b13)},
            {span, [], ?get(b14)}, {span, [], ?get(b15)},
            {span, [], ?get(b16)}, {span, [], ?get(b17)},
            {span, [], ?get(b18)}, {span, [], ?get(b19)},
            {span, [], ?get(b20)}, {span, [], ?get(b21)},
            {span, [], ?get(b22)}, {span, [], ?get(b23)},
            {span, [], ?get(b24)}, {span, [], ?get(b25)},
            {span, [], ?get(b26)}, {span, [], ?get(b27)},
            {span, [], ?get(b28)}, {span, [], ?get(b29)},
            {span, [], ?get(b30)}, {span, [], ?get(b31)},
            {span, [], ?get(b32)}, {span, [], ?get(b33)},
            {span, [], ?get(b34)}, {span, [], ?get(b35)},
            {span, [], ?get(b36)}, {span, [], ?get(b37)},
            {span, [], ?get(b38)}, {span, [], ?get(b39)},
            {span, [], ?get(b40)}, {span, [], ?get(b41)},
            {span, [], ?get(b42)}, {span, [], ?get(b43)},
            {span, [], ?get(b44)}, {span, [], ?get(b45)},
            {span, [], ?get(b46)}, {span, [], ?get(b47)},
            {span, [], ?get(b48)}, {span, [], ?get(b49)}
        ]}
    ).

key_for(I) when I < 10 ->
    list_to_atom("b0" ++ integer_to_list(I));
key_for(I) ->
    list_to_atom("b" ++ integer_to_list(I)).

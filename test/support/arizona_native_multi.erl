-module(arizona_native_multi).
-include("arizona_view.hrl").
-export([mount/2, render/1, handle_event/3]).

%% Native (JSON) multi-counter: three independent counter regions in one view.
%% Per-counter events route to the root handler and update distinct bindings, so
%% incrementing one region leaves the others untouched -- the browser
%% multi-counter analogue, proving independent az slots. (True stateful-child
%% event routing on native is a separate follow-up.)

-spec mount(az:bindings(), az:request()) -> az:mount_ret().
mount(_Bindings, _Req) ->
    {#{id => ~"native_multi", count_a => 0, count_b => 0, count_c => 0}, #{}}.

-spec render(az:bindings()) -> az:template().
render(Bindings) ->
    ?native(
        {'Column', [{id, ?get(id)}], [
            {'Row', [], [
                {'Text', [], [~"A: ", ?get(count_a)]},
                {'Button', [{on_tap, arizona_android:push_event(~"inc_a")}], [~"+"]},
                {'Button', [{on_tap, arizona_android:push_event(~"dec_a")}], [~"-"]}
            ]},
            {'Row', [], [
                {'Text', [], [~"B: ", ?get(count_b)]},
                {'Button', [{on_tap, arizona_android:push_event(~"inc_b")}], [~"+"]},
                {'Button', [{on_tap, arizona_android:push_event(~"dec_b")}], [~"-"]}
            ]},
            {'Row', [], [
                {'Text', [], [~"C: ", ?get(count_c)]},
                {'Button', [{on_tap, arizona_android:push_event(~"inc_c")}], [~"+"]},
                {'Button', [{on_tap, arizona_android:push_event(~"dec_c")}], [~"-"]}
            ]}
        ]}
    ).

-spec handle_event(az:event_name(), az:event_payload(), az:bindings()) ->
    az:handle_event_ret().
handle_event(~"inc_a", _Payload, Bindings) ->
    {Bindings#{count_a => maps:get(count_a, Bindings) + 1}, #{}, []};
handle_event(~"dec_a", _Payload, Bindings) ->
    {Bindings#{count_a => maps:get(count_a, Bindings) - 1}, #{}, []};
handle_event(~"inc_b", _Payload, Bindings) ->
    {Bindings#{count_b => maps:get(count_b, Bindings) + 1}, #{}, []};
handle_event(~"dec_b", _Payload, Bindings) ->
    {Bindings#{count_b => maps:get(count_b, Bindings) - 1}, #{}, []};
handle_event(~"inc_c", _Payload, Bindings) ->
    {Bindings#{count_c => maps:get(count_c, Bindings) + 1}, #{}, []};
handle_event(~"dec_c", _Payload, Bindings) ->
    {Bindings#{count_c => maps:get(count_c, Bindings) - 1}, #{}, []}.

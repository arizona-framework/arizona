-module(arizona_reload_fun_view).
-include("arizona_stateful.hrl").
-export([mount/1, render/1, handle_event/3]).

%% A JSON view holding a fun made by another module in its bindings, the shape
%% (a stream's key fun, a fun prop) that a dev reload of that module breaks.

-spec mount(az:bindings()) -> az:mount_ret().
mount(_Bindings) ->
    {#{id => ~"reload_fun", count => 0, step => arizona_reload_fun_source:make()}, #{}}.

-spec render(az:bindings()) -> az:template().
render(Bindings) ->
    ?json(
        {'Column', [{id, ?get(id)}], [
            {'Text', [], [~"Count: ", ?get(count)]},
            {'Button', [{on_tap, arizona_android:push_event(~"inc")}], [~"+"]}
        ]}
    ).

-spec handle_event(az:event_name(), az:event_payload(), az:bindings()) ->
    az:handle_event_ret().
handle_event(~"inc", _Payload, #{count := Count, step := Step} = Bindings) ->
    {Bindings#{count => Step(Count)}, #{}, []}.

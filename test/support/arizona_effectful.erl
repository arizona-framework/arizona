-module(arizona_effectful).
-include("arizona_view.hrl").
-export([mount/2, render/1, handle_event/3]).

-spec mount(az:bindings(), az:request()) -> az:mount_ret().
mount(_Bindings, _Req) ->
    {#{id => ~"effectful", value => ~"initial"}, #{}}.

-spec render(az:bindings()) -> az:template().
render(Bindings) ->
    ?html(
        {'div', [{id, ?get(id)}], [?get(value, ~"initial")]}
    ).

-spec handle_event(az:event_name(), az:event_payload(), az:bindings()) ->
    az:handle_event_ret().
handle_event(~"notify", #{~"message" := Msg}, Bindings) ->
    {Bindings#{value => Msg}, #{}, [
        arizona_js:dispatch_event(~"notification", #{~"message" => Msg})
    ]};
handle_event(~"multi", _Payload, Bindings) ->
    {Bindings#{value => ~"multi"}, #{}, [
        arizona_js:dispatch_event(~"event1", #{~"a" => 1}),
        arizona_js:dispatch_event(~"event2", #{~"b" => 2})
    ]};
handle_event(~"noop", _Payload, Bindings) ->
    {Bindings, #{}, []};
handle_event(~"notify_only", _Payload, Bindings) ->
    {Bindings, #{}, [arizona_js:dispatch_event(~"ping", #{})]}.

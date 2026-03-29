-module(arizona_effectful).
-include("arizona_stateful.hrl").
-export([mount/1, render/1, handle_event/3]).

mount(Bindings) ->
    {maps:merge(#{id => ~"effectful", value => ~"initial"}, Bindings), #{}}.

render(Bindings) ->
    ?html(
        {'div', [{id, ?get(id)}], [?get(value, ~"initial")]}
    ).

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

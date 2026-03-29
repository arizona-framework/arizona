-module(arizona_counter).
-include("arizona_stateful.hrl").
-export([mount/1, render/1, handle_update/2, handle_event/3, handle_info/2]).

mount(Bindings) ->
    {maps:merge(#{id => ~"counter", count => 0}, Bindings), #{}}.

%% Only counter2 doubles the parent count; others merge normally.
handle_update(Props, Bindings) ->
    case Props of
        #{id := ~"counter2"} ->
            ParentCount = maps:get(count, Props, 0),
            {maps:merge(Bindings, Props#{count => ParentCount * 2}), #{}};
        #{} ->
            {maps:merge(Bindings, Props), #{}}
    end.

render(Bindings) ->
    ?html(
        {'div', [{id, ?get(id)}], [
            {button, [{az_click, arizona_js:push_event(~"inc")}], [~"+"]},
            {button, [{az_click, arizona_js:push_event(~"dec")}], [~"-"]},
            {p, [], [~"Count: ", ?get(count, 0)]}
        ]}
    ).

handle_event(~"inc", _Payload, Bindings) ->
    {Bindings#{count => maps:get(count, Bindings, 0) + 1}, #{}, []};
handle_event(~"dec", _Payload, Bindings) ->
    {Bindings#{count => maps:get(count, Bindings, 0) - 1}, #{}, []};
handle_event(~"reset", _Payload, Bindings) ->
    Id = maps:get(id, Bindings, ~"counter"),
    {Bindings#{count => 0}, #{}, [arizona_js:dispatch_event(~"counter_reset", #{~"id" => Id})]}.

handle_info({set_count, N}, Bindings) ->
    {Bindings#{count => N}, #{}, []}.

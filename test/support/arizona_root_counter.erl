-module(arizona_root_counter).
-include("arizona_view.hrl").
-export([mount/2, render/1, handle_event/3]).

%% Minimal view counter for arizona_live machinery tests. Mirrors
%% arizona_counter's mount/event shape but as a route-level view, since
%% arizona_live only spawns views.

-spec mount(az:bindings(), az:request()) -> az:mount_ret().
mount(Bindings, _Req) ->
    {
        #{
            id => ~"counter",
            count => maps:get(count, Bindings, 0)
        },
        #{}
    }.

-spec render(az:bindings()) -> az:template().
render(Bindings) ->
    ?html(
        {'div', [{id, ?get(id)}], [
            {span, [], [?get(count)]}
        ]}
    ).

-spec handle_event(az:event_name(), az:event_payload(), az:bindings()) ->
    az:handle_event_ret().
handle_event(~"inc", _Payload, Bindings) ->
    {Bindings#{count => maps:get(count, Bindings) + 1}, #{}, []};
handle_event(~"dec", _Payload, Bindings) ->
    {Bindings#{count => maps:get(count, Bindings) - 1}, #{}, []};
handle_event(~"noop", _Payload, Bindings) ->
    {Bindings, #{}, []}.

-module(arizona_drainable).
-include("arizona_view.hrl").
-export([mount/1, render/1, handle_drain/2]).

-spec mount(az:bindings()) -> az:mount_ret().
mount(Bindings) ->
    {
        #{
            id => maps:get(id, Bindings, ~"drainable"),
            drain_mode => maps:get(drain_mode, Bindings, stop)
        },
        #{}
    }.

-spec render(az:bindings()) -> az:template().
render(Bindings) ->
    ?html({'div', [{id, ?get(id)}], [~"drainable"]}).

-spec handle_drain(integer(), az:bindings()) -> arizona_stateful:handle_drain_ret().
handle_drain(_Deadline, Bindings) ->
    case maps:get(drain_mode, Bindings) of
        stop ->
            {stop, Bindings, [arizona_js:dispatch_event(~"draining", #{})]};
        keep ->
            {Bindings, #{}, []};
        noop ->
            ok
    end.

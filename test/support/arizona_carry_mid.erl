-module(arizona_carry_mid).
-include("arizona_stateful.hrl").
-export([mount/1]).
-export([render/1]).
-export([handle_event/3]).

%% Middle stateful view that renders NO grandchild until its own event asks for
%% one. So the grandchild comes into existence without any root diff running,
%% and no enclosing container's recorded `child_views` has ever seen it.

-spec mount(az:bindings()) -> az:mount_ret().
mount(Props) ->
    {
        #{
            id => maps:get(id, Props),
            notify => maps:get(notify, Props),
            show_leaf => false
        },
        #{}
    }.

-spec render(az:bindings()) -> az:template().
render(Bindings) ->
    ?html(
        {'div', [{id, ?get(id)}], [
            case ?get(show_leaf) of
                true -> ?stateful(arizona_carry_leaf, #{id => ~"g1", notify => ?get(notify)});
                false -> ~""
            end
        ]}
    ).

-spec handle_event(az:event_name(), az:event_payload(), az:bindings()) ->
    az:handle_event_ret().
handle_event(~"add_grandchild", _Payload, Bindings) ->
    {Bindings#{show_leaf => true}, #{}, []}.

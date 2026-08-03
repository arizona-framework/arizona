-module(arizona_native_conditional).
-include("arizona_stateful.hrl").
-export([mount/1]).
-export([render/1]).
-export([handle_event/3]).

%% Native (JSON) view whose content slot conditionally renders a STATEFUL child.
%% Toggling it on ships an OP_TEXT whose payload introduces the child's own view
%% id, so every op the child then emits is addressed to a node the first frame
%% never carried -- the client has to index a subtree the diff created, not just
%% the one OP_REPLACE rendered.

-spec mount(az:bindings()) -> az:mount_ret().
mount(_Bindings) ->
    {#{id => ~"native_conditional", show => false}, #{}}.

-spec render(az:bindings()) -> az:template().
render(Bindings) ->
    ?native(
        {'Column', [{id, ?get(id)}], [
            {'Button', [{on_tap, arizona_android:push_event(~"toggle")}], [~"Toggle"]},
            case ?get(show) of
                true ->
                    ?stateful(arizona_native_child_counter, #{
                        id => ~"cond_child", label => ~"C", count => 0
                    });
                false ->
                    ~""
            end
        ]}
    ).

-spec handle_event(az:event_name(), az:event_payload(), az:bindings()) ->
    az:handle_event_ret().
handle_event(~"toggle", _Payload, Bindings) ->
    {Bindings#{show => not maps:get(show, Bindings)}, #{}, []}.

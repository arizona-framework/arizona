-module(arizona_update_effect_parent).
-include("arizona_stateful.hrl").
-export([mount/1, render/1, handle_event/3]).

-spec mount(az:bindings()) -> az:mount_ret().
mount(Bindings) ->
    {#{id => maps:get(id, Bindings, ~"uep"), value => 0}, #{}}.

%% Bumping `value` changes the child's prop, triggering its handle_update/3.
-spec handle_event(az:event_name(), az:event_payload(), az:bindings()) ->
    az:handle_event_ret().
handle_event(~"bump", _Payload, Bindings) ->
    {Bindings#{value => maps:get(value, Bindings) + 1}, #{}, []};
%% Like bump, but the event ALSO emits its own effect. That effect seeds the
%% update-effects accumulator, so the child's handle_update receives it
%% (non-empty incoming) and threads it -- both effects must reach the reply.
handle_event(~"bump_titled", _Payload, Bindings) ->
    {Bindings#{value => maps:get(value, Bindings) + 1}, #{}, [arizona_js:set_title(~"titled")]}.

-spec render(az:bindings()) -> az:template().
render(Bindings) ->
    ?html(
        {main, [{id, ?get(id)}], [
            ?stateful(arizona_update_effect_child, #{
                id => ~"uep_child",
                value => ?get(value)
            })
        ]}
    ).

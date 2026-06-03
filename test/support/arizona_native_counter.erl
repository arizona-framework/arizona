-module(arizona_native_counter).
-include("arizona_stateful.hrl").
-export([mount/1, render/1, handle_event/3]).

%% Minimal native (JSON) view for arizona_live wire/cache tests. Mirrors
%% arizona_root_counter but renders via ?native instead of ?html.

-spec mount(az:bindings()) -> az:mount_ret().
mount(Bindings) ->
    {
        #{
            id => ~"native_counter",
            count => maps:get(count, Bindings, 0)
        },
        #{}
    }.

-spec render(az:bindings()) -> az:template().
render(Bindings) ->
    ?native(
        {'Column', [{id, ?get(id)}], [
            {'Text', [], [?get(count)]}
        ]}
    ).

-spec handle_event(az:event_name(), az:event_payload(), az:bindings()) ->
    az:handle_event_ret().
handle_event(~"inc", _Payload, Bindings) ->
    {Bindings#{count => maps:get(count, Bindings) + 1}, #{}, []}.

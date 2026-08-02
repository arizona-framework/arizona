-module(arizona_toast_parent).
-include("arizona_stateful.hrl").
-export([mount/1]).
-export([render/1]).
-export([handle_event/3]).

%% Root view conditionally embedding arizona_toast, so a `hide` event removes
%% the child while its ?send_after `close` timer is still pending -- the
%% removal must cancel the child's timers instead of letting a late fire crash
%% the live process with unknown_view.

-spec mount(az:bindings()) -> az:mount_ret().
mount(_Bindings) ->
    {#{id => ~"tp", show => true}, #{}}.

-spec render(az:bindings()) -> az:template().
render(Bindings) ->
    ?html(
        {main, [{id, ?get(id)}], [
            case ?get(show) of
                true -> ?stateful(arizona_toast, #{id => ~"toast"});
                false -> ~""
            end
        ]}
    ).

-spec handle_event(az:event_name(), az:event_payload(), az:bindings()) ->
    az:handle_event_ret().
handle_event(~"hide", _Payload, Bindings) ->
    {Bindings#{show => false}, #{}, []}.

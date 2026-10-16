-module(arizona_idle_watchdog).
-include("arizona_stateful.hrl").
-export([mount/1]).
-export([render/1]).
-export([handle_event/3]).
-export([handle_info/2]).

%% Idle watchdog: every `bump` cancels the pending idle timer itself and arms a
%% fresh one, so the timer only fires after a quiet period. A cancelled timer
%% delivers no message, which is what the timer-registry pruning must cope with.

-spec mount(az:bindings()) -> az:mount_ret().
mount(_Bindings) ->
    {#{id => ~"watchdog", ref => undefined, idle => false}, #{}}.

-spec render(az:bindings()) -> az:template().
render(Bindings) ->
    ?html({main, [{id, ?get(id)}], [atom_to_binary(?get(idle))]}).

-spec handle_event(az:event_name(), az:event_payload(), az:bindings()) ->
    az:handle_event_ret().
handle_event(~"bump", _Payload, #{ref := Old} = Bindings) ->
    ok = cancel(Old),
    Ref = ?send_after(60000, idle),
    {Bindings#{ref => Ref}, #{}, []}.

-spec handle_info(term(), az:bindings()) -> az:handle_info_ret().
handle_info(idle, Bindings) ->
    {Bindings#{idle => true}, #{}, []}.

cancel(undefined) ->
    ok;
cancel(Ref) ->
    erlang:cancel_timer(Ref, [{async, false}, {info, false}]).

-module(arizona_toast).
-include("arizona_stateful.hrl").
-export([mount/1]).
-export([render/1]).
-export([handle_event/3]).
-export([handle_info/2]).

%% Toast-style view: an `arm` event schedules a delayed `close` to itself via
%% ?send_after. Usable as a route root (timer-pruning tests) or embedded as a
%% child (removed-before-the-timer-fires tests, the classic dismissed-early
%% toast that must not crash the live session).

-spec mount(az:bindings()) -> az:mount_ret().
mount(Props) ->
    {#{id => maps:get(id, Props, ~"toast"), closes => 0}, #{}}.

-spec render(az:bindings()) -> az:template().
render(Bindings) ->
    ?html({'div', [{id, ?get(id)}], [?get(closes)]}).

-spec handle_event(az:event_name(), az:event_payload(), az:bindings()) ->
    az:handle_event_ret().
handle_event(~"arm", #{~"delay" := Delay}, Bindings) ->
    ?send_after(Delay, close),
    {Bindings, #{}, []}.

-spec handle_info(term(), az:bindings()) -> az:handle_info_ret().
handle_info(close, Bindings) ->
    {Bindings#{closes => maps:get(closes, Bindings) + 1}, #{}, []}.

-module(arizona_native_ticker).
-include("arizona_view.hrl").
-export([mount/2, render/1, handle_info/2]).

%% Native (JSON) server-push example: a handle_info timer pushes count updates
%% unsolicited (no client event). Proves handle_info/pubsub-driven OP_TEXT
%% reaches the native client, which applies any incoming ops regardless of what
%% triggered them.

-spec mount(az:bindings(), az:request()) -> az:mount_ret().
mount(_Bindings, _Req) ->
    Bindings = #{id => ~"native_ticker", count => 0},
    ?connected andalso ?send(arizona_connected),
    {Bindings, #{}}.

-spec render(az:bindings()) -> az:template().
render(Bindings) ->
    ?native(
        {'Column', [{id, ?get(id)}], [
            {'Text', [], [~"Tick: ", ?get(count)]}
        ]}
    ).

-spec handle_info(term(), az:bindings()) -> az:handle_info_ret().
handle_info(arizona_connected, Bindings) ->
    ?send_after(1000, tick),
    {Bindings, #{}, []};
handle_info(tick, Bindings) ->
    ?send_after(1000, tick),
    {Bindings#{count => maps:get(count, Bindings) + 1}, #{}, []}.

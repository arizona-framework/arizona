-module(arizona_json_counter).
-include("arizona_stateful.hrl").
-export([mount/1, render/1, handle_event/3, handle_info/2]).

%% Minimal JSON view for arizona_live wire/cache tests. Mirrors
%% arizona_root_counter but renders via ?json instead of ?html.

-spec mount(az:bindings()) -> az:mount_ret().
mount(Bindings) ->
    {
        #{
            id => ~"json_counter",
            count => maps:get(count, Bindings, 0)
        },
        #{}
    }.

-spec render(az:bindings()) -> az:template().
render(Bindings) ->
    ?json(
        {'Column', [{id, ?get(id)}], [
            {'Text', [], [?get(count)]}
        ]}
    ).

-spec handle_event(az:event_name(), az:event_payload(), az:bindings()) ->
    az:handle_event_ret().
handle_event(~"inc", _Payload, Bindings) ->
    {Bindings#{count => maps:get(count, Bindings) + 1}, #{}, []}.

%% Mirrors arizona_root_counter's set_count, so a suite can drive a server push
%% on the JSON target.
-spec handle_info(term(), az:bindings()) -> az:handle_info_ret().
handle_info({set_count, N}, Bindings) ->
    {Bindings#{count => N}, #{}, []}.

-module(arizona_timer).
-include("arizona_stateful.hrl").
-export([mount/1, render/1, handle_info/2]).

-spec mount(az:bindings()) -> az:mount_ret().
mount(Bindings) ->
    {maps:merge(#{id => ~"timer", message => ~"none"}, Bindings), #{}}.

-spec render(az:bindings()) -> az:template().
render(Bindings) ->
    ?html(
        {'div', [{id, ?get(id)}], [?get(message)]}
    ).

-spec handle_info(term(), az:bindings()) -> az:handle_info_ret().
handle_info({set_message, Msg}, Bindings) ->
    {Bindings#{message => Msg}, #{}, []};
handle_info({set_message_with_effect, Msg}, Bindings) ->
    {Bindings#{message => Msg}, #{}, [
        arizona_js:dispatch_event(~"message_changed", #{~"msg" => Msg})
    ]}.

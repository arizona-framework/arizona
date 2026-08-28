-module(arizona_dyn_page_b).
-moduledoc """
The other swappable page of `arizona_dyn_page`; see `arizona_dyn_page_a`.
""".
-include("arizona_stateful.hrl").

-export([mount/1]).
-export([render/1]).
-export([handle_event/3]).

-spec mount(az:bindings()) -> az:mount_ret().
mount(Init) ->
    {#{id => maps:get(id, Init, ~"inner"), taps => 0}, #{}}.

-spec render(az:bindings()) -> az:template().
render(Bindings) ->
    ?html(
        {section, [{id, ?get(id)}], [
            {p, [{id, ~"pb"}, {az_pointerup, arizona_js:push_event(~"pu")}], [
                ~"page b:",
                integer_to_binary(?get(taps))
            ]}
        ]}
    ).

-spec handle_event(az:event_name(), az:event_payload(), az:bindings()) ->
    az:handle_event_ret().
handle_event(~"pu", _Payload, Bindings) ->
    {Bindings#{taps => maps:get(taps, Bindings) + 1}, #{}, []}.

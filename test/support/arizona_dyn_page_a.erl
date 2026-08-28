-module(arizona_dyn_page_a).
-moduledoc """
A swappable page of `arizona_dyn_page`. Its `az-pointerdown` is compile-proven
in THIS module's attribute set, but the walk only reaches here through the
runtime observation of the module -- nothing names it literally.
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
            {p, [{id, ~"pa"}, {az_pointerdown, arizona_js:push_event(~"pd")}], [
                ~"page a:",
                integer_to_binary(?get(taps))
            ]}
        ]}
    ).

-spec handle_event(az:event_name(), az:event_payload(), az:bindings()) ->
    az:handle_event_ret().
handle_event(~"pd", _Payload, Bindings) ->
    {Bindings#{taps => maps:get(taps, Bindings) + 1}, #{}, []}.

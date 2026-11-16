-module(arizona_push_repeat).
-moduledoc """
**TEST FIXTURE.** `push_event` under `on_repeat => drop`: a button and an `az_submit`
form whose events the handler answers slowly (`delay_ms`, from the route bindings),
so a repeat is raised while the first event awaits its reply, beside a button under
the default `allow`. Each event the server handles is counted on the page, and
`ping` (never dropped) lets the test wait until every event sent before it was
handled. Drives `e2e/parallel/arizona_push_repeat.spec.js`.
""".
-include("arizona_stateful.hrl").
-export([mount/1, render/1, handle_event/3]).

-spec mount(az:bindings()) -> az:mount_ret().
mount(Bindings) ->
    {
        #{
            id => ~"page",
            title => maps:get(title, Bindings, ~"Push repeat"),
            delay_ms => maps:get(delay_ms, Bindings, 300),
            saves => 0,
            submits => 0,
            allowed => 0,
            pings => 0
        },
        #{}
    }.

-spec render(az:bindings()) -> az:template().
render(Bindings) ->
    ?html(
        {main, [{id, ?get(id)}], [
            {h1, [], [?get(title)]},
            {button,
                [
                    {id, ~"drop-save"},
                    {az_click, arizona_js:push_event(~"save", #{}, #{on_repeat => drop})}
                ],
                [~"Save"]},
            {form,
                [
                    {id, ~"drop-form"},
                    {az_submit, arizona_js:push_event(~"submit", #{}, #{on_repeat => drop})}
                ],
                [
                    {input, [{id, ~"drop-field"}, {name, ~"note"}, {type, ~"text"}]},
                    {button, [{id, ~"drop-submit"}, {type, ~"submit"}], [~"Submit"]}
                ]},
            {button, [{id, ~"allow-save"}, {az_click, arizona_js:push_event(~"allow")}], [
                ~"Save (allow)"
            ]},
            {button, [{id, ~"ping"}, {az_click, arizona_js:push_event(~"ping")}], [~"Ping"]},
            {p, [], [~"saves: ", {span, [{id, ~"saves"}], [?get(saves)]}]},
            {p, [], [~"submits: ", {span, [{id, ~"submits"}], [?get(submits)]}]},
            {p, [], [~"allowed: ", {span, [{id, ~"allowed"}], [?get(allowed)]}]},
            {p, [], [~"pings: ", {span, [{id, ~"pings"}], [?get(pings)]}]}
        ]}
    ).

-spec handle_event(az:event_name(), az:event_payload(), az:bindings()) ->
    az:handle_event_ret().
handle_event(~"ping", _Payload, Bindings) ->
    {Bindings#{pings => maps:get(pings, Bindings) + 1}, #{}, []};
handle_event(Event, _Payload, Bindings) ->
    Key = counter(Event),
    receive
    after maps:get(delay_ms, Bindings) -> ok
    end,
    {Bindings#{Key => maps:get(Key, Bindings) + 1}, #{}, []}.

counter(~"save") -> saves;
counter(~"submit") -> submits;
counter(~"allow") -> allowed.

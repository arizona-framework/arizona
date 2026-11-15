-module(arizona_fetch_reset).
-moduledoc """
**TEST FIXTURE.** An `az-form-reset` form submitted by `arizona_js:fetch/2`
(`e2e/parallel/arizona_fetch_reset.spec.js`).

The reset waits for the 2xx, so the request is in flight while the user keeps
typing: a chat's next message started before the last one's reply. The reset
must leave what changed since the send and reset the rest. The `reset` event it
raises pushes `was_reset` with every field, the ones left included.
""".
-include("arizona_stateful.hrl").
-export([mount/1, render/1, handle_event/3]).

-spec mount(az:bindings()) -> az:mount_ret().
mount(Bindings) ->
    {
        #{
            id => ~"page",
            title => maps:get(title, Bindings, ~"Fetch reset"),
            reset_msg => ~""
        },
        #{}
    }.

-spec render(az:bindings()) -> az:template().
render(Bindings) ->
    ?html(
        {main, [{id, ?get(id)}], [
            {h1, [], [?get(title)]},
            {p, [{id, ~"reset-msg"}], [?get(reset_msg)]},
            {form,
                [
                    {id, ~"reset-form"},
                    {az_submit, arizona_js:fetch(~"/fetch-reset/submit", #{method => post})},
                    {az_reset, arizona_js:push_event(~"was_reset")},
                    az_form_reset
                ],
                [
                    {input, [{id, ~"to"}, {name, ~"to"}, {type, ~"email"}, required]},
                    {textarea, [{id, ~"msg"}, {name, ~"msg"}], []},
                    {input, [{id, ~"urgent"}, {name, ~"urgent"}, {type, ~"checkbox"}]},
                    {button, [{type, ~"submit"}], [~"Send"]}
                ]}
        ]}
    ).

-spec handle_event(az:event_name(), az:event_payload(), az:bindings()) ->
    az:handle_event_ret().
handle_event(~"was_reset", #{~"msg" := Msg}, Bindings) ->
    {Bindings#{reset_msg => Msg}, #{}, []}.

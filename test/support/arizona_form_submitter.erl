-module(arizona_form_submitter).
-moduledoc """
**TEST FIXTURE.** Multi-submit-button form for the submitter-identity e2e
(`e2e/parallel/arizona_form_submitter.spec.js`).

One `az_submit` push_event fires whichever submit button is clicked. The payload
carries the form's fields **plus** the `name`/`value` of the button that fired
(via `new FormData(form, submitter)`), so the handler can tell the two plans
apart in a single event -- no per-button `az_click`, no `change`-sync workaround.
""".
-include("arizona_stateful.hrl").

-export([mount/1]).
-export([render/1]).
-export([handle_event/3]).

-spec mount(az:bindings()) -> az:mount_ret().
mount(Bindings) ->
    {
        #{
            id => ~"page",
            title => maps:get(title, Bindings, ~"Form Submitter"),
            chosen => ~"",
            email => ~""
        },
        #{}
    }.

-spec render(az:bindings()) -> az:template().
render(Bindings) ->
    ?html(
        {main, [{id, ?get(id)}], [
            {h1, [], [?get(title)]},
            {p, [{id, ~"chosen"}], [?get(chosen)]},
            {p, [{id, ~"email-echo"}], [?get(email)]},
            {form, [{id, ~"plan-form"}, {az_submit, arizona_js:push_event(~"submit_plan")}], [
                {input, [{id, ~"email"}, {name, ~"email"}]},
                {button, [{name, ~"plan"}, {value, ~"monthly"}], [~"Monthly"]},
                {button, [{name, ~"plan"}, {value, ~"annual"}], [~"Annual"]}
            ]}
        ]}
    ).

-spec handle_event(az:event_name(), az:event_payload(), az:bindings()) ->
    az:handle_event_ret().
handle_event(~"submit_plan", #{~"plan" := Plan, ~"email" := Email}, Bindings) ->
    {Bindings#{chosen => Plan, email => Email}, #{}, []}.

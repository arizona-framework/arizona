-module(arizona_fetch_error).
-moduledoc """
**TEST FIXTURE.** Exercises `arizona_js:fetch/2`'s `on_error` option end to end
(`e2e/parallel/arizona_fetch_error.spec.js`).

The form submits via fetch to a controller that replies `500` with no effects body, so
the client takes the failure path: it runs the `on_error` commands (a list, to exercise
the list-unwrap clause) and dispatches an `arizona:fetch-error` event. The commands mutate
`#fetch-status` (add a class + set an attribute), proving the previously-crashing
`on_error` cmds now reach the client and run.
""".
-include("arizona_stateful.hrl").
-export([mount/1, render/1]).

-spec mount(az:bindings()) -> az:mount_ret().
mount(Bindings) ->
    {
        #{
            id => ~"page",
            title => maps:get(title, Bindings, ~"Error")
        },
        #{}
    }.

-spec render(az:bindings()) -> az:template().
render(Bindings) ->
    ?html(
        {main, [{id, ?get(id)}], [
            {h1, [], [?get(title)]},
            {p, [{id, ~"fetch-status"}], [~"idle"]},
            {form,
                [
                    {id, ~"error-form"},
                    {az_submit,
                        arizona_js:fetch(~"/fetch-error/submit", #{
                            method => post,
                            on_error => [
                                arizona_js:add_class(~"#fetch-status", ~"errored"),
                                arizona_js:set_attr(~"#fetch-status", ~"data-errored", ~"yes")
                            ]
                        })}
                ],
                [
                    {button, [{type, ~"submit"}], [~"Save"]}
                ]}
        ]}
    ).

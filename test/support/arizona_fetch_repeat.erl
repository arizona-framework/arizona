-module(arizona_fetch_repeat).
-moduledoc """
**TEST FIXTURE.** Two forms posting the same slow `arizona_js:fetch/2` endpoint
(`arizona_fetch_repeat_controller`), one under `on_repeat => drop` and one under the
default `allow`. Each request carries the `token` the test types, so the test can read
how many requests the server answered for it. Drives
`e2e/parallel/arizona_fetch_repeat.spec.js`.
""".
-include("arizona_stateful.hrl").
-export([mount/1, render/1]).

-spec mount(az:bindings()) -> az:mount_ret().
mount(Bindings) ->
    {#{id => ~"page", title => maps:get(title, Bindings, ~"Repeat")}, #{}}.

-spec render(az:bindings()) -> az:template().
render(Bindings) ->
    ?html(
        {main, [{id, ?get(id)}], [
            {h1, [], [?get(title)]},
            {form,
                [
                    {id, ~"drop-form"},
                    {az_submit,
                        arizona_js:fetch(~"/fetch-repeat/hit", #{
                            method => post, on_repeat => drop
                        })}
                ],
                [
                    {input, [{id, ~"drop-token"}, {name, ~"token"}, {type, ~"text"}]},
                    {button, [{id, ~"drop-submit"}, {type, ~"submit"}], [~"Save"]}
                ]},
            {form,
                [
                    {id, ~"allow-form"},
                    {az_submit, arizona_js:fetch(~"/fetch-repeat/hit", #{method => post})}
                ],
                [
                    {input, [{id, ~"allow-token"}, {name, ~"token"}, {type, ~"text"}]},
                    {button, [{id, ~"allow-submit"}, {type, ~"submit"}], [~"Save"]}
                ]}
        ]}
    ).

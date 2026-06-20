-module(arizona_session_view).
-moduledoc """
**TEST FIXTURE.** Drives the `arizona_session` write-loop e2e
(`e2e/parallel/arizona_session.spec.js`).

The form submits via `arizona_js:fetch/2` to `arizona_session_controller`, which
rotates the encrypted `az_session` cookie and `push_event`s the new name back, so this
view re-renders live (no reload). The route carries the `fetch_session` middleware, so
on the next page load `mount/1` reads the persisted name out of the decrypted cookie --
exercising the full encrypt -> cookie -> decrypt round-trip through a real browser.
""".
-include("arizona_stateful.hrl").
-export([mount/1, render/1, handle_event/3]).

-spec mount(az:bindings()) -> az:mount_ret().
mount(Bindings) ->
    Session = maps:get(session, Bindings, #{}),
    {
        #{
            id => ~"page",
            title => maps:get(title, Bindings, ~"Session"),
            name => maps:get(~"name", Session, ~"(none)")
        },
        #{}
    }.

-spec render(az:bindings()) -> az:template().
render(Bindings) ->
    ?html(
        {main, [{id, ?get(id)}], [
            {h1, [], [?get(title)]},
            {span, [{id, ~"status"}, {'data-saved', ~"no"}], [~"idle"]},
            {p, [{id, ~"current-name"}], [~"Name: ", ?get(name)]},
            {form,
                [
                    {id, ~"session-form"},
                    {az_submit, arizona_js:fetch(~"/session/save", #{method => post})}
                ],
                [
                    {input, [{id, ~"name-input"}, {name, ~"name"}, {type, ~"text"}], []},
                    {button, [{type, ~"submit"}], [~"Save"]}
                ]}
        ]}
    ).

-spec handle_event(az:event_name(), az:event_payload(), az:bindings()) ->
    az:handle_event_ret().
handle_event(~"saved", #{~"name" := Name}, Bindings) ->
    {Bindings#{name => Name}, #{}, []}.

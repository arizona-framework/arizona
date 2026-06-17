-module(arizona_fetch_account).
-moduledoc """
**TEST FIXTURE.** Live view that drives the `arizona_js:fetch/2` e2e
(`e2e/sequential/arizona_fetch_account.spec.js`).

The form submits via `fetch` to the `arizona_fetch_account_controller` route, which
sets an HttpOnly cookie and broadcasts over `arizona_pubsub` -- so this view re-renders
through the WebSocket (the `saved_count` slot) without a page reload, while the typed
`#note` field (a static input, never diffed) survives. `#status` is flipped by the
request-local effect the controller returns.
""".
-include("arizona_stateful.hrl").
-export([mount/1, unmount/1, render/1, handle_info/2]).

-spec mount(az:bindings()) -> az:mount_ret().
mount(Bindings) ->
    ?connected andalso ?subscribe(fetch_account),
    {
        #{
            id => ~"page",
            title => maps:get(title, Bindings, ~"Account"),
            saved_count => 0,
            message => ~""
        },
        #{}
    }.

-spec unmount(az:bindings()) -> ok.
unmount(_Bindings) ->
    _ = ?unsubscribe(fetch_account),
    ok.

-spec render(az:bindings()) -> az:template().
render(Bindings) ->
    ?html(
        {main, [{id, ?get(id)}], [
            {h1, [], [?get(title)]},
            {span, [{id, ~"status"}, {'data-saved', ~"no"}], [~"idle"]},
            %% Server-computed content (set via the pubsub broadcast below) rendered
            %% server-authoritatively -- the idiomatic way to show fetch-driven content,
            %% rather than an imperative DOM effect.
            {p, [{id, ~"message"}], [?get(message)]},
            {p, [{id, ~"saved-count"}], [~"Saved: ", ?get(saved_count)]},
            {form,
                [
                    {id, ~"account-form"},
                    {az_submit, arizona_js:fetch(~"/fetch-account/submit", #{method => post})}
                ],
                [
                    {input, [{id, ~"note"}, {name, ~"note"}, {type, ~"text"}], []},
                    {button, [{type, ~"submit"}], [~"Save"]}
                ]}
        ]}
    ).

-spec handle_info(term(), az:bindings()) -> az:handle_info_ret().
handle_info({saved, Msg}, Bindings) ->
    Count = maps:get(saved_count, Bindings) + 1,
    {Bindings#{saved_count => Count, message => Msg}, #{}, []}.

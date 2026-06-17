-module(arizona_fetch_push).
-moduledoc """
**TEST FIXTURE.** The push_event variant of the `arizona_js:fetch/2` e2e
(`e2e/parallel/arizona_fetch_push.spec.js`).

Updates the **submitting** view after a fetch without pubsub: the controller returns
an `arizona_js:push_event` in the response, the client relays it over the existing
WebSocket, and this view re-renders through its normal `handle_event/3`. No subscribe,
no topic -- the right tool when only the view that submitted needs refreshing (pubsub
is for broadcasting to other views).
""".
-include("arizona_stateful.hrl").
-export([mount/1, render/1, handle_event/3]).

-spec mount(az:bindings()) -> az:mount_ret().
mount(Bindings) ->
    {
        #{
            id => ~"page",
            title => maps:get(title, Bindings, ~"Push"),
            message => ~""
        },
        #{}
    }.

-spec render(az:bindings()) -> az:template().
render(Bindings) ->
    ?html(
        {main, [{id, ?get(id)}], [
            {h1, [], [?get(title)]},
            {p, [{id, ~"message"}], [?get(message)]},
            {form,
                [
                    {id, ~"push-form"},
                    {az_submit, arizona_js:fetch(~"/fetch-push/submit", #{method => post})}
                ],
                [
                    {button, [{type, ~"submit"}], [~"Save"]}
                ]}
        ]}
    ).

-spec handle_event(az:event_name(), az:event_payload(), az:bindings()) ->
    az:handle_event_ret().
handle_event(~"saved", _Payload, Bindings) ->
    {Bindings#{message => ~"Saved via push_event"}, #{}, []}.

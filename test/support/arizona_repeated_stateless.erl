-module(arizona_repeated_stateless).
-include("arizona_stateful.hrl").
-export([mount/1, render/1, handle_event/3]).
-export([card/1]).

%% Regression fixture for the "repeated same-function stateless components share
%% one az id" bug: the view renders the SAME stateless render function (card/1)
%% twice. Before the fix both cards emitted byte-identical az ids, so every diff
%% op resolved (querySelector first-match) onto the first card. Each instance
%% must now be an independent diff target, namespaced by its slot az.

-spec mount(az:bindings()) -> az:mount_ret().
mount(Bindings) ->
    {
        #{
            id => ~"panel",
            a => maps:get(a, Bindings, ~"AAA"),
            b => maps:get(b, Bindings, ~"BBB")
        },
        #{}
    }.

-spec render(az:bindings()) -> az:template().
render(Bindings) ->
    ?html(
        {'div', [{id, ?get(id)}], [
            ?stateless(card, #{label => ?get(a)}),
            ?stateless(card, #{label => ?get(b)})
        ]}
    ).

-spec card(az:bindings()) -> az:template().
card(Props) ->
    ?html(
        {span, [{class, ~"card"}], [maps:get(label, Props)]}
    ).

-spec handle_event(az:event_name(), az:event_payload(), az:bindings()) ->
    az:handle_event_ret().
handle_event(~"set_a", #{~"v" := V}, Bindings) ->
    {Bindings#{a => V}, #{}, []};
handle_event(~"set_b", #{~"v" := V}, Bindings) ->
    {Bindings#{b => V}, #{}, []}.

-module(arizona_carry_root).
-include("arizona_stateful.hrl").
-export([mount/1]).
-export([render/1]).
-export([wrap/1]).
-export([handle_event/3]).

%% Root that holds the middle view inside a CONTAINER (a `?stateless` child)
%% rather than in a slot of its own, so the root snapshot's slot value is a
%% nested-template snapshot whose `child_views` is what drives the carry when the
%% slot is dep-skipped. The container's props never mention `title`, so a title
%% change dep-skips the whole subtree.

-spec mount(az:bindings()) -> az:mount_ret().
mount(Bindings) ->
    {
        #{
            id => ~"cr",
            title => ~"root",
            label => ~"label",
            notify => maps:get(notify, Bindings)
        },
        #{}
    }.

-spec render(az:bindings()) -> az:template().
render(Bindings) ->
    ?html(
        {main, [{id, ?get(id)}], [
            {h1, [], [?get(title)]},
            ?stateless(fun wrap/1, #{notify => ?get(notify), label => ?get(label)})
        ]}
    ).

-spec wrap(az:bindings()) -> az:template().
wrap(Bindings) ->
    ?html(
        {section, [], [
            {h2, [], [?get(label)]},
            ?stateful(arizona_carry_mid, #{id => ~"m1", notify => ?get(notify)})
        ]}
    ).

-spec handle_event(az:event_name(), az:event_payload(), az:bindings()) ->
    az:handle_event_ret().
%% Changes only what the ROOT renders, so the container slot is dep-skipped.
handle_event(~"title_change", _Payload, Bindings) ->
    {Bindings#{title => ~"Changed"}, #{}, []};
%% Changes a prop the CONTAINER reads, so its slot is re-evaluated and compared.
handle_event(~"relabel", #{~"label" := Label}, Bindings) ->
    {Bindings#{label => Label}, #{}, []}.

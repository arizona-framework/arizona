-module(arizona_counter_local).
-include("arizona_stateful.hrl").
-export([mount/1, render/1, handle_update/2, handle_event/3]).

-spec mount(az:bindings()) -> az:mount_ret().
mount(Bindings) ->
    {
        #{
            id => maps:get(id, Bindings, ~"counter_local"),
            count => maps:get(count, Bindings, 0),
            label => maps:get(label, Bindings, ~"v1")
        },
        #{}
    }.

%% Apply parent-propagated props (e.g. a changed `label`) so the child re-renders.
-spec handle_update(az:bindings(), az:bindings()) -> az:handle_update_ret().
handle_update(Props, Bindings) ->
    {maps:merge(Bindings, Props), #{}}.

-spec render(az:bindings()) -> az:template().
render(Bindings) ->
    ?html(
        {'div', [{id, ?get(id)}, {class, ~"counter-local"}], [
            {p, [{class, ~"label"}], [~"Label: ", ?get(label, ~"v1")]},
            {p, [{class, ~"count"}], [~"Count: ", ?get(count, 0)]},
            %% Client-owned slot: server renders once, browser owns it thereafter.
            {span, [{class, ~"note"}], [?local(~"note", ~"untouched")]},
            {button, [{class, ~"inc"}, {az_click, arizona_js:push_event(~"inc")}], [~"+"]},
            {button, [{class, ~"edit"}, {az_click, arizona_js:set(~"note", ~"edited")}], [
                ~"Edit note"
            ]}
        ]}
    ).

-spec handle_event(az:event_name(), az:event_payload(), az:bindings()) ->
    az:handle_event_ret().
handle_event(~"inc", _Payload, Bindings) ->
    {Bindings#{count => maps:get(count, Bindings, 0) + 1}, #{}, []}.

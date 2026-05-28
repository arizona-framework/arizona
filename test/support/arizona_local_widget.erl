-module(arizona_local_widget).
-include("arizona_stateful.hrl").
-export([mount/1, render/1]).

%% A fully client-only stateful widget: it is a real stateful child (own
%% az-view + id, scopes its ?local slots), but it never uses push_event -- all
%% interactivity is `arizona_js:set` on client-owned slots, so it produces zero
%% server round-trips after the initial render.
-spec mount(az:bindings()) -> az:mount_ret().
mount(Bindings) ->
    {
        #{
            id => maps:get(id, Bindings, ~"widget"),
            title => maps:get(title, Bindings, ~"Widget")
        },
        #{}
    }.

-spec render(az:bindings()) -> az:template().
render(Bindings) ->
    ?html(
        {'div', [{id, ?get(id)}, {class, ~"card"}], [
            {h3, [], [?get(title)]},
            %% One key "status" drives an interpolated class (CSS color) on the <p>
            %% and the text inside its child <span> -- both client-side.
            {p, [{class, [~"status status-", ?local(~"status", ~"idle")]}], [
                ~"Status: ",
                {span, [], [?local(~"status", ~"idle")]}
            ]},
            {'div', [{class, ~"controls"}], [
                {button, [{az_click, arizona_js:set(~"status", ~"idle")}], [~"Idle"]},
                {button, [{az_click, arizona_js:set(~"status", ~"active")}], [~"Active"]},
                {button, [{az_click, arizona_js:set(~"status", ~"done")}], [~"Done"]}
            ]},
            {button, [{class, ~"open-detail"}, {az_click, arizona_js:set(~"detail_open", true)}], [
                ~"Details"
            ]},
            {dialog, [{open, ?local(~"detail_open", false)}], [
                {p, [], [~"Details for ", ?get(title), ~" -- client-only, no round-trip."]},
                {button, [{az_click, arizona_js:set(~"detail_open", false)}], [~"Close"]}
            ]}
        ]}
    ).

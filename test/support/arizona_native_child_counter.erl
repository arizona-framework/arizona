-module(arizona_native_child_counter).
-include("arizona_stateful.hrl").
-export([mount/1]).
-export([render/1]).
-export([handle_event/3]).

%% Native (JSON) embeddable stateful counter -- a child component with its OWN
%% handle_event. Tapping its button routes to this child's view id (not the
%% parent's), proving native nested event routing. The `label` distinguishes
%% sibling instances in tests.

-spec mount(az:bindings()) -> az:mount_ret().
mount(Bindings) ->
    {
        #{
            id => maps:get(id, Bindings),
            label => maps:get(label, Bindings, <<>>),
            count => maps:get(count, Bindings, 0)
        },
        #{}
    }.

-spec render(az:bindings()) -> az:template().
render(Bindings) ->
    ?native(
        {'Column', [{id, ?get(id)}], [
            {'Text', [], [?get(label), ?get(count)]},
            {'Button', [{on_tap, arizona_android:push_event(~"inc")}], [?get(label)]}
        ]}
    ).

-spec handle_event(az:event_name(), az:event_payload(), az:bindings()) ->
    az:handle_event_ret().
handle_event(~"inc", _Payload, Bindings) ->
    {Bindings#{count => maps:get(count, Bindings) + 1}, #{}, []}.

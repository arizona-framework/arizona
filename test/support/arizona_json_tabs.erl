-module(arizona_json_tabs).
-include("arizona_stateful.hrl").
-export([mount/1, render/1, handle_event/3]).
-export([content/1]).

%% JSON tabs view: a Row of Buttons whose push_event switches the
%% `selected` binding, and a content area that swaps subtrees via a stateless
%% child. Switching tab emits an OP_TEXT of the content slot (the two clauses
%% render different statics, so the slot re-renders wholesale, and a content slot
%% is marker-anchored) -- the conditional/subtree-rendering path the counter
%% (text) and list (stream) don't exercise.

-spec mount(az:bindings()) -> az:mount_ret().
mount(Bindings) ->
    {#{id => ~"json_tabs", selected => maps:get(selected, Bindings, ~"home")}, #{}}.

-spec render(az:bindings()) -> az:template().
render(Bindings) ->
    ?json(
        {'Column', [{id, ?get(id)}], [
            {'Row', [], [
                {'Button', [{on_tap, arizona_android:push_event(~"select_home")}], [~"Home"]},
                {'Button', [{on_tap, arizona_android:push_event(~"select_about")}], [~"About"]}
            ]},
            ?stateless(content, #{selected => ?get(selected)})
        ]}
    ).

%% The content area's stateless child: a different subtree per selected tab.
-spec content(az:bindings()) -> az:template().
content(#{selected := ~"about"}) ->
    ?json(
        {'Column', [], [
            {'Text', [], [~"About Arizona"]},
            {'Text', [], [~"JSON target"]}
        ]}
    );
content(#{selected := _}) ->
    ?json({'Text', [], [~"Welcome home"]}).

-spec handle_event(az:event_name(), az:event_payload(), az:bindings()) ->
    az:handle_event_ret().
handle_event(~"select_home", _Payload, Bindings) ->
    {Bindings#{selected => ~"home"}, #{}, []};
handle_event(~"select_about", _Payload, Bindings) ->
    {Bindings#{selected => ~"about"}, #{}, []}.

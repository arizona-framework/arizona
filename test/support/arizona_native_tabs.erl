-module(arizona_native_tabs).
-include("arizona_stateful.hrl").
-export([mount/1, render/1, handle_event/3]).
-export([content/1]).

%% Native (JSON) tabs view: a Row of Buttons whose push_event switches the
%% `selected` binding, and a content area that swaps subtrees via a stateless
%% child. Switching tab emits an OP_UPDATE of the content slot -- the
%% conditional/subtree-rendering path the counter (text) and list (stream) don't
%% exercise.

-spec mount(az:bindings()) -> az:mount_ret().
mount(Bindings) ->
    {#{id => ~"native_tabs", selected => maps:get(selected, Bindings, ~"home")}, #{}}.

-spec render(az:bindings()) -> az:template().
render(Bindings) ->
    ?native(
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
    ?native(
        {'Column', [], [
            {'Text', [], [~"About Arizona"]},
            {'Text', [], [~"native target"]}
        ]}
    );
content(#{selected := _}) ->
    ?native({'Text', [], [~"Welcome home"]}).

-spec handle_event(az:event_name(), az:event_payload(), az:bindings()) ->
    az:handle_event_ret().
handle_event(~"select_home", _Payload, Bindings) ->
    {Bindings#{selected => ~"home"}, #{}, []};
handle_event(~"select_about", _Payload, Bindings) ->
    {Bindings#{selected => ~"about"}, #{}, []}.

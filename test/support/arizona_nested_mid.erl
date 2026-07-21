-module(arizona_nested_mid).
-include("arizona_stateful.hrl").
-export([mount/1, render/1, handle_event/3]).

%% Middle stateful view: embeds a stateful leaf (arizona_counter) as a
%% grandchild, so root -> mid -> leaf is two levels of stateful nesting. Used to
%% exercise the child-view diff path when a depth-2 child handles its own event
%% or is skipped by a root diff.

-spec mount(az:bindings()) -> az:mount_ret().
mount(Props) ->
    {
        #{
            id => maps:get(id, Props, ~"mid"),
            label => maps:get(label, Props, ~"mid"),
            show_leaf => maps:get(show_leaf, Props, true)
        },
        #{}
    }.

-spec render(az:bindings()) -> az:template().
render(Bindings) ->
    ?html(
        {'div', [{id, ?get(id)}], [
            {span, [], [?get(label)]},
            case ?get(show_leaf) of
                %% No count prop: the leaf owns its count, so a mid re-render's
                %% handle_update does not reset it -- lets a test prove the
                %% grandchild's state survives the mid's own event.
                true -> ?stateful(arizona_counter, #{id => ~"leaf"});
                false -> ~""
            end
        ]}
    ).

-spec handle_event(az:event_name(), az:event_payload(), az:bindings()) ->
    az:handle_event_ret().
handle_event(~"relabel", _Payload, Bindings) ->
    {Bindings#{label => ~"relabelled"}, #{}, []};
handle_event(~"hide_leaf", _Payload, Bindings) ->
    {Bindings#{show_leaf => false}, #{}, []}.

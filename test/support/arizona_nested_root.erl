-module(arizona_nested_root).
-include("arizona_stateful.hrl").
-export([mount/1, render/1, handle_event/3]).

%% Root view for depth-2 stateful nesting: root -> arizona_nested_mid -> leaf.
%% The mid descriptor's props are static, so a root re-render (e.g. title_change)
%% leaves the mid slot dep-unchanged and it is skipped by the diff -- exercising
%% carry_skipped_view for a child that itself has a grandchild.

-spec mount(az:bindings()) -> az:mount_ret().
mount(Props) ->
    {
        #{
            id => maps:get(id, Props, ~"root"),
            title => maps:get(title, Props, ~"root")
        },
        #{}
    }.

-spec render(az:bindings()) -> az:template().
render(Bindings) ->
    ?html(
        {main, [{id, ?get(id)}], [
            {h1, [], [?get(title)]},
            ?stateful(arizona_nested_mid, #{id => ~"mid", label => ~"mid"})
        ]}
    ).

-spec handle_event(az:event_name(), az:event_payload(), az:bindings()) ->
    az:handle_event_ret().
handle_event(~"title_change", _Payload, Bindings) ->
    {Bindings#{title => ~"Changed"}, #{}, []}.

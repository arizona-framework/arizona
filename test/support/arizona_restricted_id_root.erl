-module(arizona_restricted_id_root).
-include("arizona_stateful.hrl").
-export([mount/1]).
-export([render/1]).
-export([handle_update/3]).
-export([handle_event/3]).

%% Deliberately buggy route-level view: the `mode` binding makes `mount/1` keep,
%% change or drop the framework-restricted `id` it was given, and a patch's
%% `mode` param makes `handle_update/3` do the same. The `child_mode` binding is
%% handed to an embedded child as a prop, so an event that sets it drives the
%% child's own `handle_update/3` the same way.
-spec mount(az:bindings()) -> az:mount_ret().
mount(Bindings) ->
    Base = #{child_mode => keep},
    case maps:get(mode, Bindings, keep) of
        keep -> {Base#{id => maps:get(id, Bindings, ~"rroot")}, #{}};
        change -> {Base#{id => ~"rewritten"}, #{}};
        drop -> {Base, #{}}
    end.

-spec handle_update(az:bindings(), az:bindings(), az:effects()) -> az:handle_update_ret().
handle_update(#{mode := keep}, Bindings, Effects) ->
    {Bindings, #{}, Effects};
handle_update(#{mode := change}, Bindings, Effects) ->
    {Bindings#{id => ~"rewritten"}, #{}, Effects};
handle_update(#{mode := drop}, Bindings, Effects) ->
    {maps:remove(id, Bindings), #{}, Effects}.

-spec handle_event(az:event_name(), az:event_payload(), az:bindings()) ->
    az:handle_event_ret().
handle_event(~"child_mode", #{~"mode" := Mode}, Bindings) ->
    {Bindings#{child_mode => binary_to_existing_atom(Mode)}, #{}, []}.

-spec render(az:bindings()) -> az:template().
render(Bindings) ->
    ?html(
        {main, [{id, ?get(id)}], [
            ?stateful(arizona_restricted_id_update_child, #{
                id => ~"ruc", mode => ?get(child_mode)
            })
        ]}
    ).

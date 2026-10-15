-module(arizona_restricted_id_root).
-include("arizona_stateful.hrl").
-export([mount/1]).
-export([render/1]).
-export([handle_update/3]).
-export([handle_event/3]).
-export([handle_info/2]).

%% Deliberately buggy route-level view: the `mode` binding makes `mount/1` keep,
%% change or drop the framework-restricted `id` it was given, and a patch's
%% `mode` param, a `root_id` event or a `{root_id, Mode}` message make the other
%% callbacks do the same. The `child_mode` binding is handed to an embedded child
%% as a prop, so a `child_mode` event drives the child's own `handle_update/3`.
-spec mount(az:bindings()) -> az:mount_ret().
mount(Bindings) ->
    Base = #{child_mode => keep},
    case maps:get(mode, Bindings, keep) of
        keep -> {Base#{id => maps:get(id, Bindings, ~"rroot")}, #{}};
        change -> {Base#{id => ~"rewritten"}, #{}};
        drop -> {Base, #{}}
    end.

-spec handle_update(az:bindings(), az:bindings(), az:effects()) -> az:handle_update_ret().
handle_update(#{mode := Mode}, Bindings, Effects) ->
    {apply_mode(Mode, Bindings), #{}, Effects}.

-spec handle_event(az:event_name(), az:event_payload(), az:bindings()) ->
    az:handle_event_ret().
handle_event(~"root_id", #{~"mode" := Mode}, Bindings) ->
    {apply_mode(binary_to_existing_atom(Mode), Bindings), #{}, []};
handle_event(~"child_mode", #{~"mode" := Mode}, Bindings) ->
    {Bindings#{child_mode => binary_to_existing_atom(Mode)}, #{}, []}.

-spec handle_info(term(), az:bindings()) -> az:handle_info_ret().
handle_info({root_id, Mode}, Bindings) ->
    {apply_mode(Mode, Bindings), #{}, []}.

-spec render(az:bindings()) -> az:template().
render(Bindings) ->
    ?html(
        {main, [{id, ?get(id)}], [
            ?stateful(arizona_restricted_id_update_child, #{
                id => ~"ruc", mode => ?get(child_mode)
            })
        ]}
    ).

apply_mode(keep, Bindings) -> Bindings;
apply_mode(change, Bindings) -> Bindings#{id => ~"rewritten"};
apply_mode(drop, Bindings) -> maps:remove(id, Bindings).

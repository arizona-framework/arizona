-module(arizona_restricted_id_update_child).
-include("arizona_stateful.hrl").
-export([mount/1]).
-export([render/1]).
-export([handle_update/3]).
-export([handle_event/3]).

%% Deliberately buggy embedded view: its `handle_update/3` keeps, changes or
%% drops the framework-restricted `id` prop as the parent's `mode` prop says, and
%% an `own_id` event does the same from its own `handle_event/3`.
-spec mount(az:bindings()) -> az:mount_ret().
mount(Bindings) ->
    {#{id => maps:get(id, Bindings), mode => maps:get(mode, Bindings)}, #{}}.

-spec handle_update(az:bindings(), az:bindings(), az:effects()) -> az:handle_update_ret().
handle_update(#{mode := Mode}, Bindings, Effects) ->
    {apply_mode(Mode, Bindings#{mode => Mode}), #{}, Effects}.

-spec handle_event(az:event_name(), az:event_payload(), az:bindings()) ->
    az:handle_event_ret().
handle_event(~"own_id", #{~"mode" := Mode}, Bindings) ->
    {apply_mode(binary_to_existing_atom(Mode), Bindings), #{}, []}.

-spec render(az:bindings()) -> az:template().
render(Bindings) ->
    ?html({'div', [{id, ?get(id)}], [atom_to_binary(?get(mode))]}).

apply_mode(keep, Bindings) -> Bindings;
apply_mode(change, Bindings) -> Bindings#{id => ~"rewritten"};
apply_mode(drop, Bindings) -> maps:remove(id, Bindings).

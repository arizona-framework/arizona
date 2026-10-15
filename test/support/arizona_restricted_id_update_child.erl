-module(arizona_restricted_id_update_child).
-include("arizona_stateful.hrl").
-export([mount/1]).
-export([render/1]).
-export([handle_update/3]).

%% Deliberately buggy embedded view: its `handle_update/3` keeps, changes or
%% drops the framework-restricted `id` prop as the parent's `mode` prop says.
-spec mount(az:bindings()) -> az:mount_ret().
mount(Bindings) ->
    {#{id => maps:get(id, Bindings), mode => maps:get(mode, Bindings)}, #{}}.

-spec handle_update(az:bindings(), az:bindings(), az:effects()) -> az:handle_update_ret().
handle_update(#{mode := keep}, Bindings, Effects) ->
    {Bindings#{mode => keep}, #{}, Effects};
handle_update(#{mode := change}, Bindings, Effects) ->
    {Bindings#{id => ~"rewritten", mode => change}, #{}, Effects};
handle_update(#{mode := drop}, Bindings, Effects) ->
    {maps:remove(id, Bindings#{mode => drop}), #{}, Effects}.

-spec render(az:bindings()) -> az:template().
render(Bindings) ->
    ?html({'div', [{id, ?get(id)}], [atom_to_binary(?get(mode))]}).

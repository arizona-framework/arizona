-module(arizona_route_id_view).
-include("arizona_stateful.hrl").
-export([mount/1]).
-export([render/1]).

%% A route-level view that takes its `id` from the route's bindings and has no
%% `handle_update/3`, so a patch merges its params into the bindings by default.
%% Two routes pointing at it with different `id`s are two different views.
-spec mount(az:bindings()) -> az:mount_ret().
mount(Bindings) ->
    {#{id => maps:get(id, Bindings, ~"route-id")}, #{}}.

-spec render(az:bindings()) -> az:template().
render(Bindings) ->
    ?html({main, [{id, ?get(id)}], [~"route"]}).

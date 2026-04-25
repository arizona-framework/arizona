-module(arizona_no_diff_counter).
-include("arizona_stateful.hrl").
-export([mount/1, render/1]).

-spec mount(az:bindings()) -> az:mount_ret().
mount(Bindings) ->
    {maps:merge(#{id => ~"nd_counter", count => 0}, Bindings), #{}}.

-spec render(az:bindings()) -> az:template().
render(Bindings) ->
    ?html(
        {'div', [{id, ?get(id)}, az_nodiff], [?get(count, 0)]}
    ).

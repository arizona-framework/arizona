-module(arizona_no_diff_counter).
-include("arizona_stateful.hrl").
-export([mount/1, render/1]).

mount(Bindings) ->
    {maps:merge(#{id => ~"nd_counter", count => 0}, Bindings), #{}}.

render(Bindings) ->
    ?html(
        {'div', [{id, ?get(id)}, az_nodiff], [?get(count, 0)]}
    ).

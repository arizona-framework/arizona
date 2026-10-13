-module(arizona_ssr_list_child).
-include("arizona_stateful.hrl").
-export([mount/1, render/1]).

%% A view with a stateful child in each item of a plain-list `?each`.

-spec mount(az:bindings()) -> az:mount_ret().
mount(_Bindings) ->
    {#{id => ~"slc", ids => [~"a", ~"b"]}, #{}}.

-spec render(az:bindings()) -> az:template().
render(Bindings) ->
    ?html(
        {ul, [{id, ?get(id)}], [
            ?each(
                fun(Id) -> {li, [], [?stateful(arizona_counter, #{id => Id, count => 0})]} end,
                ?get(ids)
            )
        ]}
    ).

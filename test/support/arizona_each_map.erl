-module(arizona_each_map).
-include("arizona_stateful.hrl").
-export([mount/1, render/1]).

-spec mount(az:bindings()) -> az:mount_ret().
mount(Bindings0) ->
    Bindings = maps:merge(
        #{id => ~"page", entries => #{~"a" => ~"1", ~"b" => ~"2"}},
        Bindings0
    ),
    {Bindings, #{}}.

-spec render(az:bindings()) -> az:template().
render(Bindings) ->
    ?html(
        {'div', [{id, ?get(id)}], [
            {ul, [], [
                ?each(
                    fun(K, V) ->
                        {li, [], [K, ~": ", V]}
                    end,
                    ?get(entries)
                )
            ]}
        ]}
    ).

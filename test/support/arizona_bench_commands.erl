-module(arizona_bench_commands).
-include("arizona_stateful.hrl").
-export([mount/1, render/1]).

%% Bench fixture: a `?each` table whose rows each carry two command attributes
%% built from the row, so every render encodes them at runtime (a command that
%% reads no row folds into the statics at compile time and costs nothing).

-spec mount(az:bindings()) -> az:mount_ret().
mount(Bindings) ->
    {#{id => ~"bench_commands", rows => maps:get(rows, Bindings, [])}, #{}}.

-spec render(az:bindings()) -> az:template().
render(Bindings) ->
    ?html(
        {table, [{id, ?get(id)}], [
            {tbody, [], [
                ?each(
                    fun(#{id := Id, name := Name}) ->
                        {tr, [], [
                            {td, [], [Name]},
                            {td, [], [
                                {button,
                                    [
                                        {az_click,
                                            arizona_js:push_event(~"select", #{
                                                ~"id" => Id, ~"name" => Name
                                            })}
                                    ],
                                    [~"Select"]},
                                {button,
                                    [
                                        {az_click, [
                                            arizona_js:push_event(~"delete_row", #{~"id" => Id}),
                                            arizona_js:hide(~"#confirm")
                                        ]}
                                    ],
                                    [~"Delete"]}
                            ]}
                        ]}
                    end,
                    ?get(rows)
                )
            ]}
        ]}
    ).

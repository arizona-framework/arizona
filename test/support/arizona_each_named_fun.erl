-module(arizona_each_named_fun).
-include("arizona_stateful.hrl").
-export([mount/1, render/1]).

%% Regression fixture: a PRIVATE (non-exported) function used only as a `?each`
%% callback. The parse transform inlines `stat_row/1`'s body into the per-item
%% template, orphaning the function. This module exercises the injected
%% `nowarn_unused_function` (compiler, test profile runs warnings_as_errors) and
%% `-ignore_xref` (xref locals_not_used) suppressions end-to-end under `make ci`.

-spec mount(az:bindings()) -> az:mount_ret().
mount(Props) ->
    Bindings = #{
        id => maps:get(id, Props, ~"stats"),
        stats => maps:get(stats, Props, [{~"Users", ~"42"}, {~"Online", ~"7"}])
    },
    {Bindings, #{}}.

-spec render(az:bindings()) -> az:template().
render(Bindings) ->
    ?html(
        {'div', [{id, ?get(id)}], [
            {ul, [], [
                ?each(fun stat_row/1, ?get(stats))
            ]}
        ]}
    ).

stat_row({Label, Value}) ->
    {li, [], [
        {span, [{class, ~"label"}], [Label]},
        {span, [{class, ~"value"}], [Value]}
    ]}.

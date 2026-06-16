-module(arizona_each_html_fun).
-include("arizona_stateful.hrl").
-export([mount/1, render/1]).

%% Regression fixture: a PRIVATE `?each` callback whose body is a whole `?html(...)` wrapper.
%% The parse transform unwraps the `?html` and inlines its element into the per-item template,
%% orphaning `stat_row/1`. Mirrors `arizona_each_named_fun` but for the `?html`-body unwrap,
%% exercising the injected `nowarn_unused_function` (compiler, test profile runs
%% warnings_as_errors) and `-ignore_xref` (xref locals_not_used) suppressions under `make ci`.

-spec mount(az:bindings()) -> az:mount_ret().
mount(Props) ->
    Bindings = #{
        id => maps:get(id, Props, ~"stats_html"),
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
    ?html(
        {li, [], [
            {span, [{class, ~"label"}], [Label]},
            {span, [{class, ~"value"}], [Value]}
        ]}
    ).

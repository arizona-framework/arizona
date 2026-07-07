-module(arizona_nested_stateless).
-include("arizona_stateless.hrl").
-export([render/1]).
-export([panel/1, badge/1]).

%% Regression fixture: a repeated stateless whose body itself contains an inline
%% conditional (nested template), a client-owned ?local slot, and a nested
%% stateless grandchild. Every scoped `az` across the two instances must be
%% unique and the SSR must match the live snapshot, so the whole subtree of each
%% instance is an independent diff target. Rendered directly via render/1 in
%% arizona_render_SUITE (no mount/events needed).

-spec render(az:bindings()) -> az:template().
render(Bindings) ->
    ?html(
        {main, [{id, ?get(id)}], [
            ?stateless(panel, #{v => ?get(x), on => ?get(flag)}),
            ?stateless(panel, #{v => ?get(y), on => ?get(flag)})
        ]}
    ).

-spec panel(az:bindings()) -> az:template().
panel(Props) ->
    ?html(
        {'div', [{class, ~"panel"}], [
            case maps:get(on, Props) of
                true -> {em, [], [maps:get(v, Props)]};
                false -> <<>>
            end,
            {button, [{class, ?local(~"open", ~"closed")}], [~"toggle"]},
            ?stateless(badge, #{text => maps:get(v, Props)})
        ]}
    ).

-spec badge(az:bindings()) -> az:template().
badge(Props) ->
    ?html(
        {span, [{class, ~"badge"}], [maps:get(text, Props)]}
    ).

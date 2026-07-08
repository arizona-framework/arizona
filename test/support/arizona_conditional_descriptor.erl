-module(arizona_conditional_descriptor).
-include("arizona_stateless.hrl").
-export([stateless_nav/1]).
-export([stateful_nav/1]).
-export([menu/1]).

%% Regression fixture (#602 regression): a `?stateless`/`?stateful` descriptor
%% returned from a conditional-tail content slot must render structurally on the
%% LIVE path, exactly as on SSR -- not be escaped as a scalar and crash with
%% `bad_template_value`. Mirrors the documented idiom (.claude/rules/erlang.md
%% "A conditional may also return a ?stateful/?stateless descriptor from a
%% branch") and the downstream crash: a nav returning ?stateless(menu, ...) from
%% a `case ?get(user)` branch when a signed-in user connects over the WebSocket.
%% Rendered both ways (SSR and live) in arizona_render_SUITE.

%% A `?stateless` descriptor returned from a conditional branch.
-spec stateless_nav(az:bindings()) -> az:template().
stateless_nav(Bindings) ->
    ?html(
        {nav, [], [
            case ?get(user) of
                undefined -> {a, [{href, ~"/login"}], [~"Entrar"]};
                _ -> ?stateless(menu, #{user => ?get(user)})
            end
        ]}
    ).

%% A `?stateful` descriptor returned from a conditional branch.
-spec stateful_nav(az:bindings()) -> az:template().
stateful_nav(Bindings) ->
    ?html(
        {nav, [], [
            case ?get(user) of
                undefined -> {a, [{href, ~"/login"}], [~"Entrar"]};
                _ -> ?stateful(arizona_counter, #{id => ~"c1"})
            end
        ]}
    ).

-spec menu(az:bindings()) -> az:template().
menu(Props) ->
    ?html(
        {span, [{class, ~"menu"}], [maps:get(user, Props)]}
    ).

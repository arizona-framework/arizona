-module(arizona_stateless_stateful_nest).
-include("arizona_stateless.hrl").
-export([page/1]).
-export([wrap/1]).

%% Regression fixture: a `?stateful` child nested inside a `?stateless` component.
%% The stateful child owns its own view-id boundary, so its inner az ids must NOT
%% be scoped by the enclosing stateless slot -- otherwise the SSR HTML (scoped)
%% and the live snapshot (unscoped) diverge and the child's diff ops, which
%% target the live id, miss the reused SSR DOM element and are silently dropped.
%% Rendered both ways (SSR and live) in arizona_render_SUITE; both must agree.

-spec page(az:bindings()) -> az:template().
page(Bindings) ->
    ?html(
        {main, [{id, ?get(id)}], [
            ?stateless(wrap, #{cid => ~"c1"})
        ]}
    ).

%% Stateless wrapper embedding a stateful child.
-spec wrap(az:bindings()) -> az:template().
wrap(Props) ->
    ?html(
        {'div', [{class, ~"wrap"}], [
            ?stateful(arizona_counter, #{id => maps:get(cid, Props)})
        ]}
    ).

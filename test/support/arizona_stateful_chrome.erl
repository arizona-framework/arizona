-module(arizona_stateful_chrome).
-include("arizona_stateless.hrl").
-export([render/1]).

%% Stateless chrome holding a ?stateful, in its own compile unit -- so the
%% layout's nodiff flag never sees it and only a render-time guard can catch it.
-spec render(az:bindings()) -> az:template().
render(_Bindings) ->
    ?html({nav, [], [?stateful(arizona_counter, #{id => ~"chrome"})]}).

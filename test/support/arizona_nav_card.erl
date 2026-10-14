-module(arizona_nav_card).
-include("arizona_stateless.hrl").
-export([render/1]).

%% A stateless card two route pages share, so both render one fingerprint.
%% Drives the navigate push-ordering test in arizona_socket_SUITE.

-spec render(az:bindings()) -> az:template().
render(Bindings) ->
    ?html({p, [{class, ~"card"}], [?get(label)]}).

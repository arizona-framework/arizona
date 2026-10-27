-module(arizona_term_value).
-include("arizona_stateless.hrl").
-include("arizona_terminal.hrl").
-export([render/1]).

%% A `?terminal` line holding one dynamic value, so a suite can render arbitrary
%% bytes through the terminal escape and inspect (or write out) the frame.

-spec render(az:bindings()) -> az:template().
render(Bindings) ->
    ?terminal({line, [], [?get(v)]}).

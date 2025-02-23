-module(arizona_component).

%% --------------------------------------------------------------------
%% API function exports
%% --------------------------------------------------------------------

-export([render/3]).

%% --------------------------------------------------------------------
%% API function definitions
%% --------------------------------------------------------------------

-spec render(Mod, Fun, View) -> Token when
    Mod :: module(),
    Fun :: atom(),
    View :: arizona:view(),
    Token :: arizona:rendered_component_template().
render(Mod, Fun, View) when is_atom(Mod), is_atom(Fun) ->
    erlang:apply(Mod, Fun, [View]).

-module(arizona_component).

%% --------------------------------------------------------------------
%% API function exports
%% --------------------------------------------------------------------

-export([render/4]).

%% --------------------------------------------------------------------
%% API function definitions
%% --------------------------------------------------------------------

-spec render(Mod, Fun, View, Socket) -> Token when
    Mod :: module(),
    Fun :: atom(),
    View :: arizona_view:view(),
    Socket :: arizona_socket:socket(),
    Token :: arizona_render:token().
render(Mod, Fun, View, Socket) when is_atom(Mod), is_atom(Fun) ->
    erlang:apply(Mod, Fun, [View, Socket]).

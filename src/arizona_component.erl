-module(arizona_component).

%% --------------------------------------------------------------------
%% API function exports
%% --------------------------------------------------------------------

-export([render/4]).

%% --------------------------------------------------------------------
%% API function definitions
%% --------------------------------------------------------------------

-spec render(Mod, Fun, View0, Socket0) -> {View1, Socket1} when
    Mod :: module(),
    Fun :: atom(),
    View0 :: arizona_view:view(),
    Socket0 :: arizona_socket:socket(),
    View1 :: arizona_view:view(),
    Socket1 :: arizona_socket:socket().
render(Mod, Fun, View, Socket) when is_atom(Mod), is_atom(Fun) ->
    erlang:apply(Mod, Fun, [View, Socket]).

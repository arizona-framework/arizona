-module(arizona_stateless).

-export([call_render_callback/3]).

%-record(stateless, {
%    module :: module(),
%    function :: atom()
%}).

-spec call_render_callback(Mod, Fun, Socket) -> Socket1 when
    Mod :: module(),
    Fun :: atom(),
    Socket :: arizona_socket:socket(),
    Socket1 :: arizona_socket:socket().
call_render_callback(Mod, Fun, Socket) ->
    apply(Mod, Fun, [Socket]).

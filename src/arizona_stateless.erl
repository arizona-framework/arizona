-module(arizona_stateless).

-export([call_render_callback/3]).

%-record(stateless, {
%    module :: module(),
%    function :: atom()
%}).

call_render_callback(Mod, Fun, Socket) ->
    apply(Mod, Fun, [Socket]).

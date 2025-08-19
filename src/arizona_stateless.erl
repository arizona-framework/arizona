-module(arizona_stateless).

%% --------------------------------------------------------------------
%% API function exports
%% --------------------------------------------------------------------

-export([call_render_callback/3]).

%% --------------------------------------------------------------------
%% API function definitions
%% --------------------------------------------------------------------

-spec call_render_callback(Mod, Fun, Bindings) -> Template when
    Mod :: module(),
    Fun :: atom(),
    Bindings :: arizona_binder:map(),
    Template :: arizona_template:template().
call_render_callback(Mod, Fun, Bindings) ->
    apply(Mod, Fun, [Bindings]).

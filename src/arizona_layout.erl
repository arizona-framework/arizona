-module(arizona_layout).

%% --------------------------------------------------------------------
%% Callback support function exports
%% --------------------------------------------------------------------

-export([mount/3]).
-export([render/1]).

%% --------------------------------------------------------------------
%% Callback definitions
%% --------------------------------------------------------------------

-callback mount(Assigns, Socket) -> View when
    Assigns :: arizona:assigns(),
    Socket :: arizona:socket(),
    View :: arizona:view().

-callback render(View) -> Rendered when
    View :: arizona:view(),
    Rendered :: arizona:rendered_layout_template().

%% --------------------------------------------------------------------
%% Callback support function definitions
%% --------------------------------------------------------------------

-spec mount(Mod, Assigns, Socket) -> View when
    Mod :: module(),
    Assigns :: arizona:assigns(),
    Socket :: arizona:socket(),
    View :: arizona:view().
mount(Mod, Assigns, Socket) when is_atom(Mod), is_map(Assigns) ->
    erlang:apply(Mod, mount, [Assigns, Socket]).

-spec render(View) -> Rendered when
    View :: arizona:view(),
    Rendered :: arizona:rendered_layout_template().
render(View) ->
    Mod = arizona_view:module(View),
    erlang:apply(Mod, render, [View]).

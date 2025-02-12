-module(arizona_view).

%% --------------------------------------------------------------------
%% API function exports
%% --------------------------------------------------------------------

-export([new/2]).
-export([new/4]).
-export([get_assign/2]).
-export([rendered/1]).
-export([set_rendered/2]).
-export([put_rendered/2]).
-export([merge_changed_assigns/1]).

%

-ignore_xref([new/2]).
-ignore_xref([new/4]).

%% --------------------------------------------------------------------
%% Callback support function exports
%% --------------------------------------------------------------------

-export([mount/3]).
-export([render/3]).

%% --------------------------------------------------------------------
%% Callback definitions
%% --------------------------------------------------------------------

-callback mount(Assigns, Socket) -> {ok, View} | ignore when
    Assigns :: assigns(),
    Socket :: arizona_socket:socket(),
    View :: view().

-callback render(View0, Socket0) -> {View1, Socket1} when
    View0 :: view(),
    Socket0 :: arizona_socket:socket(),
    View1 :: view(),
    Socket1 :: arizona_socket:socket().

%% --------------------------------------------------------------------
%% Types (and their exports)
%% --------------------------------------------------------------------

-record(view, {
    module :: undefined | module(),
    assigns :: assigns(),
    changed_assigns :: assigns(),
    rendered :: rendered()
}).
-opaque view() :: #view{}.
-export_type([view/0]).

-type assigns() :: #{atom() => dynamic()}.
-export_type([assigns/0]).

-type rendered() ::
    [rendered_value()]
    | [template | Static :: [binary()] | Dynamic :: [binary()]].
-export_type([rendered/0]).

-type rendered_value() ::
    binary()
    | rendered()
    % I could not figure out a correct type without the `dynamic/0`.
    | dynamic().
-export_type([rendered_value/0]).

-type id() :: binary().
-export_type([id/0]).

%% --------------------------------------------------------------------
%% API function definitions
%% --------------------------------------------------------------------

-spec new(Mod, Assigns) -> View when
    Mod :: undefined | module(),
    Assigns :: assigns(),
    View :: view().
new(Mod, Assigns) ->
    new(Mod, Assigns, #{}, []).

-spec new(Mod, Assigns, ChangedAssigns, Rendered) -> View when
    Mod :: undefined | module(),
    Assigns :: assigns(),
    ChangedAssigns :: assigns(),
    Rendered :: rendered(),
    View :: view().
new(Mod, Assigns, ChangedAssigns, Rendered) ->
    #view{
        module = Mod,
        assigns = Assigns,
        changed_assigns = ChangedAssigns,
        rendered = Rendered
    }.

-spec get_assign(Key, View) -> Value when
    Key :: atom(),
    View :: view(),
    Value :: dynamic().
get_assign(Key, #view{} = View) when is_atom(Key) ->
    maps:get(Key, View#view.changed_assigns, maps:get(Key, View#view.assigns)).

-spec rendered(View) -> Rendered when
    View :: view(),
    Rendered :: rendered().
rendered(#view{} = View) ->
    View#view.rendered.

-spec set_rendered(Rendered, View0) -> View1 when
    Rendered :: rendered(),
    View0 :: view(),
    View1 :: view().
set_rendered(Rendered, #view{} = View) when is_list(Rendered) ->
    View#view{rendered = Rendered}.

-spec put_rendered(Rendered, View0) -> View1 when
    Rendered :: rendered_value(),
    View0 :: view(),
    View1 :: view().
put_rendered(Rendered, #view{} = View) when is_binary(Rendered); is_list(Rendered) ->
    View#view{rendered = [Rendered | View#view.rendered]}.

-spec merge_changed_assigns(View0) -> View1 when
    View0 :: view(),
    View1 :: view().
merge_changed_assigns(View) ->
    View#view{assigns = maps:merge(View#view.assigns, View#view.changed_assigns)}.

%% --------------------------------------------------------------------
%% Callback support function definitions
%% --------------------------------------------------------------------

-spec mount(Mod, Assigns, Socket) -> {ok, View} | ignore when
    Mod :: module(),
    Assigns :: assigns(),
    Socket :: arizona_socket:socket(),
    View :: view().
mount(Mod, Assigns, Socket) when is_atom(Mod), Mod =/= undefined, is_map(Assigns) ->
    erlang:apply(Mod, mount, [Assigns, Socket]).

-spec render(Mod, View0, Socket0) -> {View1, Socket1} when
    Mod :: module(),
    View0 :: view(),
    Socket0 :: arizona_socket:socket(),
    View1 :: view(),
    Socket1 :: arizona_socket:socket().
render(Mod, View, Socket) when is_atom(Mod), Mod =/= undefined ->
    erlang:apply(Mod, render, [View, Socket]).

-module(arizona_view).

%% --------------------------------------------------------------------
%% API function exports
%% --------------------------------------------------------------------

-export([new/2]).
-export([new/4]).
-export([assigns/1]).
-export([get_assign/2]).
-export([rendered/1]).
-export([set_rendered/2]).
-export([put_rendered/2]).
-export([merge_changed_assigns/1]).
-export([rendered_to_iolist/1]).

%

-ignore_xref([new/2]).
-ignore_xref([new/4]).
-ignore_xref([rendered_to_iolist/1]).

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
%% Doctests
%% --------------------------------------------------------------------

-ifdef(TEST).
-include_lib("doctest/include/doctest.hrl").
-endif.

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

-spec assigns(View) -> Assigns when
    View :: view(),
    Assigns :: assigns().
assigns(#view{} = View) ->
    View#view.assigns.

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

-doc ~"""
Formats the rendered content to `t:iolist/0`.

## Examples

```
> Socket = arizona_socket:new(#{}).
> {ok, View0} = arizona_view:mount(arizona_example_template, #{id => ~"app", count => 0}, Socket).
> {View, _Socket} = arizona_view:render(arizona_example_template, View0, Socket).
> arizona_view:rendered_to_iolist(View).
[<<"<html>\n    <head></head>\n    <body id=\"">>,<<"app">>,<<"\">">>,
 [<<"<div id=\"">>,<<"counter">>,<<"\">">>,<<"0">>,<<>>,
  [<<"<button>">>,<<"Increment">>,<<"</button>">>],
  <<"</div>">>],
 <<"</body>\n</html>">>]
```
""".
-spec rendered_to_iolist(View) -> iolist() when
    View :: view().
rendered_to_iolist(#view{} = View) ->
    rendered_to_iolist_1(View#view.rendered).

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

%% --------------------------------------------------------------------
%% Private functions
%% --------------------------------------------------------------------

rendered_to_iolist_1([template, Static, Dynamic]) ->
    zip(Static, Dynamic);
rendered_to_iolist_1(List) when is_list(List) ->
    [rendered_to_iolist_1(Rendered) || Rendered <- List];
rendered_to_iolist_1(Rendered) ->
    Rendered.

zip([], []) ->
    [];
zip([S | Static], [D | Dynamic]) ->
    [S, rendered_to_iolist_1(D) | zip(Static, Dynamic)];
zip([S | Static], []) ->
    [S | zip(Static, [])];
zip([], [D | Dynamic]) ->
    [rendered_to_iolist_1(D) | zip([], Dynamic)].

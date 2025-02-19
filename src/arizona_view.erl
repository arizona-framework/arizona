-module(arizona_view).

%% --------------------------------------------------------------------
%% API function exports
%% --------------------------------------------------------------------

-export([new/1]).
-export([new/2]).
-export([new/4]).
-export([assigns/1]).
-export([put_assign/3]).
-export([put_assigns/2]).
-export([get_assign/2]).
-export([get_assign/3]).
-export([changed_assigns/1]).
-export([set_changed_assigns/2]).
-export([rendered/1]).
-export([set_rendered/2]).
-export([put_rendered/2]).
-export([put_diff/3]).
-export([merge_changed_assigns/1]).
-export([rendered_to_iolist/1]).
-export([diff_to_iolist/2]).

%

-ignore_xref([new/2]).
-ignore_xref([new/4]).
-ignore_xref([get_assign/3]).
-ignore_xref([put_assign/3]).
-ignore_xref([put_assigns/2]).
-ignore_xref([rendered_to_iolist/1]).
-ignore_xref([diff_to_iolist/2]).

%% --------------------------------------------------------------------
%% Callback support function exports
%% --------------------------------------------------------------------

-export([mount/3]).
-export([render/2]).

%% --------------------------------------------------------------------
%% Callback definitions
%% --------------------------------------------------------------------

-callback mount(Assigns, Socket) -> {ok, View} | ignore when
    Assigns :: assigns(),
    Socket :: arizona_socket:socket(),
    View :: view().

-callback render(View) -> Token when
    View :: view(),
    Token :: arizona_render:token().

%% --------------------------------------------------------------------
%% Types (and their exports)
%% --------------------------------------------------------------------

-record(view, {
    module :: undefined | module(),
    assigns :: assigns(),
    changed_assigns :: assigns(),
    rendered :: arizona_render:rendered()
}).
-opaque view() :: #view{}.
-export_type([view/0]).

-type assigns() :: #{atom() => dynamic()}.
-export_type([assigns/0]).

-type id() :: binary().
-export_type([id/0]).

%% --------------------------------------------------------------------
%% Doctests
%% --------------------------------------------------------------------

-ifdef(TEST).
-include_lib("eunit/include/eunit.hrl").
doctest_test() -> doctest:module(?MODULE).
-endif.

%% --------------------------------------------------------------------
%% API function definitions
%% --------------------------------------------------------------------

-spec new(Assigns) -> View when
    Assigns :: assigns(),
    View :: view().
new(Assigns) ->
    new(undefined, Assigns, #{}, []).

-spec new(Mod, Assigns) -> View when
    Mod :: module(),
    Assigns :: assigns(),
    View :: view().
new(Mod, Assigns) ->
    new(Mod, Assigns, #{}, []).

-spec new(Mod, Assigns, ChangedAssigns, Rendered) -> View when
    Mod :: undefined | module(),
    Assigns :: assigns(),
    ChangedAssigns :: assigns(),
    Rendered :: arizona_render:rendered(),
    View :: view().
new(Mod, Assigns, ChangedAssigns, Rendered) when
    is_atom(Mod), is_map(Assigns), is_map(ChangedAssigns), is_list(Rendered)
->
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

-spec put_assign(Key, Value, View0) -> View1 when
    Key :: atom(),
    Value :: dynamic(),
    View0 :: view(),
    View1 :: view().
put_assign(Key, Value, #view{} = View) when is_atom(Key) ->
    View#view{changed_assigns = maps:put(Key, Value, View#view.changed_assigns)}.

-spec put_assigns(Assigns, View0) -> View1 when
    Assigns :: assigns(),
    View0 :: view(),
    View1 :: view().
put_assigns(Assigns, #view{} = View) when is_map(Assigns) ->
    maps:fold(fun(Key, Value, ViewAcc) -> put_assign(Key, Value, ViewAcc) end, View, Assigns).

-spec get_assign(Key, View) -> Value when
    Key :: atom(),
    View :: view(),
    Value :: dynamic().
get_assign(Key, #view{} = View) when is_atom(Key) ->
    maps:get(Key, View#view.changed_assigns, maps:get(Key, View#view.assigns)).

-spec get_assign(Key, View, Default) -> Value when
    Key :: atom(),
    View :: view(),
    Value :: Default | dynamic().
get_assign(Key, #view{} = View, Default) when is_atom(Key) ->
    maps:get(Key, View#view.changed_assigns, maps:get(Key, View#view.assigns, Default)).

-spec changed_assigns(View) -> ChangedAssigns when
    View :: view(),
    ChangedAssigns :: assigns().
changed_assigns(#view{} = View) ->
    View#view.changed_assigns.

-spec set_changed_assigns(ChangedAssigns, View0) -> View1 when
    ChangedAssigns :: assigns(),
    View0 :: view(),
    View1 :: view().
set_changed_assigns(ChangedAssigns, #view{} = View) when is_map(ChangedAssigns) ->
    View#view{changed_assigns = ChangedAssigns}.

-spec rendered(View) -> Rendered when
    View :: view(),
    Rendered :: arizona_render:rendered().
rendered(#view{} = View) ->
    View#view.rendered.

-spec set_rendered(Rendered, View0) -> View1 when
    Rendered :: arizona_render:rendered(),
    View0 :: view(),
    View1 :: view().
set_rendered(Rendered, #view{} = View) when is_list(Rendered) ->
    View#view{rendered = Rendered}.

-spec put_rendered(Rendered, View0) -> View1 when
    Rendered :: arizona_render:rendered_value(),
    View0 :: view(),
    View1 :: view().
put_rendered(Rendered, #view{} = View) when is_binary(Rendered); is_list(Rendered) ->
    View#view{rendered = [Rendered | View#view.rendered]}.

% NOTE: The diff is a proplist constructed in reverse order.
-spec put_diff(Index, Rendered, View0) -> View1 when
    Index :: arizona_diff:index(),
    Rendered :: arizona_render:rendered_value(),
    View0 :: view(),
    View1 :: view().
put_diff(Index, Rendered, #view{} = View) when
    is_integer(Index), Index >= 0, (is_binary(Rendered) orelse is_list(Rendered))
->
    View#view{rendered = [{Index, Rendered} | View#view.rendered]}.

-spec merge_changed_assigns(View0) -> View1 when
    View0 :: view(),
    View1 :: view().
merge_changed_assigns(View) ->
    View#view{assigns = maps:merge(View#view.assigns, View#view.changed_assigns)}.

-doc ~"""
Formats the rendered content to `t:iolist/0`.

## Examples

```
> Mod = arizona_example_template.
> Assigns = #{id => ~"app", count => 0}.
> Socket = arizona_socket:new(render).
> {ok, View0} = arizona_view:mount(Mod, Assigns, Socket).
> Rendered = arizona_view:render(Mod, View0).
> {View, _Socket} = arizona_render:render(Rendered, View0, View0, Socket).
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

-spec diff_to_iolist(Rendered0, View) -> Rendered1 when
    Rendered0 :: arizona_render:rendered(),
    View :: view(),
    Rendered1 :: arizona_render:rendered().
diff_to_iolist(Rendered, #view{} = View) when is_list(Rendered) ->
    case View#view.rendered of
        [] ->
            Rendered;
        Changed ->
            diff_replace(Changed, Rendered)
    end.

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

-spec render(Mod, View) -> Token when
    Mod :: module(),
    View :: view(),
    Token :: arizona_render:token().
render(Mod, View) when is_atom(Mod), Mod =/= undefined ->
    erlang:apply(Mod, render, [View]).

%% --------------------------------------------------------------------
%% Private functions
%% --------------------------------------------------------------------

rendered_to_iolist_1([template, Static, Dynamic]) ->
    zip(Static, Dynamic);
rendered_to_iolist_1([list_template, Static, DynamicList]) ->
    [zip(Static, Dynamic) || Dynamic <- DynamicList];
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

diff_replace([{Index, [{_, _} | _] = NestedChanged0} | T], [template, Static, Dynamic0]) ->
    NestedChanged = lists:keysort(1, NestedChanged0),
    NestedTemplate0 = lists:nth(Index + 1, Dynamic0),
    NestedTemplate = diff_replace(NestedChanged, NestedTemplate0),
    Dynamic = diff_replace_value(Dynamic0, {Index, NestedTemplate}, 0),
    diff_replace(T, [template, Static, Dynamic]);
diff_replace([{Index, Value} | T], [template, Static, Dynamic0]) ->
    Dynamic = diff_replace_value(Dynamic0, {Index, Value}, 0),
    diff_replace(T, [template, Static, Dynamic]);
diff_replace([], [template, Static, Dynamic]) ->
    zip(Static, Dynamic).

diff_replace_value([_Value | T], {Index, NewValue}, Index) ->
    [NewValue | T];
diff_replace_value([H | T], Replacement, IndexAcc) ->
    [H | diff_replace_value(T, Replacement, IndexAcc + 1)].

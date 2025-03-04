-module(arizona_view).
-moduledoc ~"""
The `arizona_view` behaviour defines the interface for creating stateful
views in the Arizona framework.

Views are responsible for managing their state, rendering templates, and
handling client-side events.

To implement a view in Arizona, a module must define the following callbacks:

- `c:mount/2`: Initializes the view with bindings and a WebSocket connection.
- `c:render/1`: Renders the view's template based on its current state.
- `c:handle_event/3`: Handles client-side events and updates the view's state.

These callbacks work together to create dynamic, stateful views that can
respond to user interactions in real-time.
""".

%% --------------------------------------------------------------------
%% API function exports
%% --------------------------------------------------------------------

-export([new/2]).
-export([put_binding/3]).
-export([put_bindings/2]).
-export([get_binding/2]).
-export([get_binding/3]).

%% --------------------------------------------------------------------
%% Support function exports
%% --------------------------------------------------------------------

-export([init_root/5]).
-export([new/1]).
-export([new/6]).
-export([module/1]).
-export([bindings/1]).
-export([changed_bindings/1]).
-export([set_changed_bindings/2]).
-export([rendered/1]).
-export([set_rendered/2]).
-export([put_rendered/2]).
-export([tmp_rendered/1]).
-export([set_tmp_rendered/2]).
-export([put_tmp_rendered/2]).
-export([diff/1]).
-export([set_diff/2]).
-export([put_diff/3]).
-export([merge_changed_bindings/1]).
-export([rendered_to_iolist/1]).
-export([diff_to_iolist/1]).

%

-ignore_xref([diff_to_iolist/1]).

%% --------------------------------------------------------------------
%% Callback support function exports
%% --------------------------------------------------------------------

-export([mount/3]).
-export([render/1]).
-export([handle_join/4]).
-export([handle_event/3]).

%% --------------------------------------------------------------------
%% Callback definitions
%% --------------------------------------------------------------------

-optional_callbacks([handle_params/2]).
-optional_callbacks([handle_join/3]).

%

-doc ~"""
Handles path parameters and query strings in the root view.

The root view serves as the entry point for page rendering or the `inner_content`
of a layout.

This callback **is optional** for all view modules.

## Parameters

- `PathParams`: Path parameters parsed from the route URL. For example, in the route
  `"/:user_id/profile"`, `user_id` is a path parameter. To retrieve it, use
  `arizona:get_path_parameter(id, PathParams)`.
- `QueryString`: The query string from the route URL. For example, in the route
  `"/:user_id/profile?menu=notifications"`, `?menu=notifications` is the query string.
  To extract a value, first parse the query string using `arizona:parse_query_string/1`,
  then call `arizona:get_query_param(menu, QueryParams)`.

## Returns

`{true, Bindings}` where `Bindings` is of type `t:bindings/0`. These bindings will
be merged into the view's bindings, or `false` if no merge is required.
""".
-callback handle_params(PathParams, QueryString) -> Return when
    PathParams :: arizona:path_params(),
    QueryString :: arizona:query_string(),
    Return :: handle_params_ret().

-doc ~"""
Is invoked when a `t:view/0` is initialized.

It sets up the initial state of the view, including its bindings.

This callback **is required** for all view modules.

## Parameters

- `Bindings`: A map (`t:bindings/0`) containing the initial data for the view.
- `Socket`: The WebSocket connection (`t:arizona_socket:socket/0`).

## Returns

`{ok, View}` where `View` is the initialized view state (`t:view/0`), or `ignore`
if the view should not be mounted.
""".
-callback mount(Bindings, Socket) -> Return when
    Bindings :: bindings(),
    Socket :: arizona_socket:socket(),
    Return :: mount_ret().

-doc ~"""
Is responsible for rendering the view's template.

It is called whenever the view needs to be re-rendered, such as after a state
update or in response to a client event.

This callback **is required** for all view modules.

## Parameters

- `View`: The current view state (`t:view/0`), which includes the view module,
  bindings, and other metadata.

## Returns

The rendered template as `t:arizona:rendered_view_template/0`.
""".
-callback render(View) -> Rendered when
    View :: view(),
    Rendered :: arizona:rendered_view_template().

-callback handle_join(Topic, Params, View) -> Return when
    Topic :: event_name(),
    Params :: event_payload(),
    View :: view(),
    Return :: handle_join_ret().

-doc ~"""
Handles events sent from the client.

It updates the view's state based on the event and returns the updated view.

This callback **is required** for all view modules.

## Parameters

- `EventName`: The name of the event (`t:event_name/0`), typically a `t:binary/0`.
- `Payload`: The data associated with the event (`t:event_payload/0`), which can
  be any `t:dynamic/0` value.
- `View`: The current view state (`t:view/0`) before handling the event.

## Returns

The updated view state (`t:view/0`) after handling the event.
""".
-callback handle_event(EventName, Payload, View0) -> Return when
    EventName :: event_name(),
    Payload :: event_payload(),
    View0 :: view(),
    Return :: handle_event_ret().

%% --------------------------------------------------------------------
%% Types (and their exports)
%% --------------------------------------------------------------------

-record(view, {
    module :: undefined | module(),
    bindings :: bindings(),
    changed_bindings :: bindings(),
    rendered :: arizona_renderer:rendered(),
    tmp_rendered :: arizona_renderer:rendered(),
    diff :: arizona_diff:diff()
}).
-opaque view() :: #view{}.
-export_type([view/0]).

-type bindings() :: #{atom() => dynamic()}.
-export_type([bindings/0]).

-type id() :: binary().
-export_type([id/0]).

-type handle_params_ret() :: {true, Bindings :: bindings()} | false.
-export_type([handle_params_ret/0]).

-type mount_ret() :: {ok, View :: view()} | ignore.
-export_type([mount_ret/0]).

-type handle_event_ret() ::
    {reply, Events :: events(), View :: view()}
    | {noreply, View :: view()}.
-export_type([handle_event_ret/0]).

-type handle_join_ret() ::
    {ok, Payload :: event_payload(), View :: view()}
    | {error, Reason :: event_payload(), View :: view()}.
-export_type([handle_join_ret/0]).

-type events() :: [{Name :: event_name(), Payload :: event_payload()}].
-export_type([events/0]).

-type event_name() :: binary().
-export_type([event_name/0]).

-type event_payload() :: dynamic().
-export_type([event_payload/0]).

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

-spec new(Mod, Bindings) -> View when
    Mod :: module(),
    Bindings :: bindings(),
    View :: view().
new(Mod, Bindings) ->
    new(Mod, Bindings, #{}, [], [], []).

-spec put_binding(Key, Value, View0) -> View1 when
    Key :: atom(),
    Value :: dynamic(),
    View0 :: view(),
    View1 :: view().
put_binding(Key, Value, #view{} = View) when is_atom(Key) ->
    View#view{changed_bindings = maps:put(Key, Value, View#view.changed_bindings)}.

-spec put_bindings(Bindings, View0) -> View1 when
    Bindings :: bindings(),
    View0 :: view(),
    View1 :: view().
put_bindings(Bindings, #view{} = View) when is_map(Bindings) ->
    maps:fold(fun(Key, Value, ViewAcc) -> put_binding(Key, Value, ViewAcc) end, View, Bindings).

-spec get_binding(Key, View) -> Value when
    Key :: atom(),
    View :: view(),
    Value :: dynamic().
get_binding(Key, #view{} = View) when is_atom(Key) ->
    maps:get(Key, View#view.changed_bindings, maps:get(Key, View#view.bindings)).

-spec get_binding(Key, View, Default) -> Value when
    Key :: atom(),
    View :: view(),
    Value :: Default | dynamic().
get_binding(Key, #view{} = View, Default) when is_atom(Key) ->
    maps:get(Key, View#view.changed_bindings, maps:get(Key, View#view.bindings, Default)).

%% --------------------------------------------------------------------
%% Support function exports
%% --------------------------------------------------------------------

-doc ~"""
Initializes the root view by setting up bindings, mounting the view, and rendering it.

This function is the entry point for rendering a view. It performs the following steps:

1. Computes the initial bindings by calling `handle_params/3` (if implemented) and
   merging the results.
2. Mounts the view using the provided module and bindings.
3. Renders the view and updates the socket with the rendered content.

## Parameters

- `Mod`: The module implementing the view.
- `PathParams`: Path parameters parsed from the route URL. For example, in the route
  `"/:user_id/profile"`, `user_id` is a path parameter.
- `QueryString`: The query string from the route URL. For example, in the route
  `"/:user_id/profile?menu=notifications"`, `?menu=notifications` is the query string.
- `Bindings`: Initial bindings passed to the view.
- `Socket`: The current socket state.

## Returns

`{View, Socket}` where `View` is the rendered view, and `Socket` is the updated
socket state after rendering.
""".
-spec init_root(Mod, PathParams, QueryString, Bindings, Socket0) -> {View, Socket1} when
    Mod :: module(),
    PathParams :: arizona:path_params(),
    QueryString :: arizona:query_string(),
    Bindings :: bindings(),
    Socket0 :: arizona:socket(),
    View :: view(),
    Socket1 :: arizona:socket().
init_root(Mod, PathParams, QueryString, Bindings0, Socket0) ->
    Bindings = initial_bindings(Mod, PathParams, QueryString, Bindings0),
    {ok, View0} = mount(Mod, Bindings, Socket0),
    Token = render(View0),
    arizona_renderer:render(Token, View0, View0, Socket0).

-spec new(Bindings) -> View when
    Bindings :: bindings(),
    View :: view().
new(Bindings) ->
    new(undefined, Bindings, #{}, [], [], []).

-spec new(Mod, Bindings, ChangedBindings, Rendered, TmpRendered, Diff) -> View when
    Mod :: undefined | module(),
    Bindings :: bindings(),
    ChangedBindings :: bindings(),
    Rendered :: arizona_renderer:rendered(),
    TmpRendered :: arizona_renderer:rendered(),
    Diff :: arizona_diff:diff(),
    View :: view().
new(Mod, Bindings, ChangedBindings, Rendered, TmpRendered, Diff) when
    is_atom(Mod),
    is_map(Bindings),
    is_map(ChangedBindings),
    is_list(Rendered),
    is_list(TmpRendered),
    is_list(Diff)
->
    #view{
        module = Mod,
        bindings = Bindings,
        changed_bindings = ChangedBindings,
        rendered = Rendered,
        tmp_rendered = TmpRendered,
        diff = Diff
    }.

-spec module(View) -> Mod when
    View :: view(),
    Mod :: module().
module(#view{} = View) ->
    View#view.module.

-spec bindings(View) -> Bindings when
    View :: view(),
    Bindings :: bindings().
bindings(#view{} = View) ->
    View#view.bindings.

-spec changed_bindings(View) -> ChangedBindings when
    View :: view(),
    ChangedBindings :: bindings().
changed_bindings(#view{} = View) ->
    View#view.changed_bindings.

-spec set_changed_bindings(ChangedBindings, View0) -> View1 when
    ChangedBindings :: bindings(),
    View0 :: view(),
    View1 :: view().
set_changed_bindings(ChangedBindings, #view{} = View) when is_map(ChangedBindings) ->
    View#view{changed_bindings = ChangedBindings}.

-spec rendered(View) -> Rendered when
    View :: view(),
    Rendered :: arizona_renderer:rendered().
rendered(#view{} = View) ->
    View#view.rendered.

-spec set_rendered(Rendered, View0) -> View1 when
    Rendered :: arizona_renderer:rendered(),
    View0 :: view(),
    View1 :: view().
set_rendered(Rendered, #view{} = View) when is_list(Rendered) ->
    View#view{rendered = Rendered}.

-spec put_rendered(Rendered, View0) -> View1 when
    Rendered :: arizona_renderer:rendered_value(),
    View0 :: view(),
    View1 :: view().
put_rendered(Rendered, #view{} = View) when is_binary(Rendered); is_list(Rendered) ->
    View#view{rendered = [Rendered | View#view.rendered]}.

-spec tmp_rendered(View) -> Rendered when
    View :: view(),
    Rendered :: arizona_renderer:rendered().
tmp_rendered(#view{} = View) ->
    View#view.tmp_rendered.

-spec set_tmp_rendered(Rendered, View0) -> View1 when
    Rendered :: arizona_renderer:rendered(),
    View0 :: view(),
    View1 :: view().
set_tmp_rendered(Rendered, #view{} = View) when is_list(Rendered) ->
    View#view{tmp_rendered = Rendered}.

-spec put_tmp_rendered(Rendered, View0) -> View1 when
    Rendered :: arizona_renderer:rendered_value(),
    View0 :: view(),
    View1 :: view().
put_tmp_rendered(Rendered, #view{} = View) when is_binary(Rendered); is_list(Rendered) ->
    View#view{tmp_rendered = [Rendered | View#view.tmp_rendered]}.

-spec diff(View) -> Diff when
    View :: view(),
    Diff :: arizona_diff:diff().
diff(#view{} = View) ->
    View#view.diff.

-spec set_diff(Diff, View0) -> View1 when
    Diff :: arizona_diff:diff(),
    View0 :: view(),
    View1 :: view().
set_diff(Diff, #view{} = View) when is_list(Diff) ->
    View#view{diff = Diff}.

-spec put_diff(Index, Payload, View0) -> View1 when
    Index :: arizona_diff:index(),
    Payload :: arizona_diff:diff() | arizona_renderer:rendered_value(),
    View0 :: view(),
    View1 :: view().
put_diff(Index, [], #view{} = View) when is_integer(Index), Index >= 0 ->
    View;
put_diff(Index, Payload, #view{} = View) when
    is_integer(Index), Index >= 0, (is_binary(Payload) orelse is_list(Payload))
->
    View#view{diff = [{Index, Payload} | View#view.diff]}.

-spec merge_changed_bindings(View0) -> View1 when
    View0 :: view(),
    View1 :: view().
merge_changed_bindings(View) ->
    View#view{
        bindings = maps:merge(View#view.bindings, View#view.changed_bindings),
        changed_bindings = #{}
    }.

-doc ~"""
Formats the tmp_renderedcontent to `t:iolist/0`.

## Examples

```
> Mod = arizona_example_template.
> Bindings = #{id => ~"app", count => 0}.
> Socket = arizona_socket:new(render).
> {ok, View0} = arizona_view:mount(Mod, Bindings, Socket).
> Rendered = arizona_view:render(View0).
> {View, _Socket} = arizona_renderer:render(Rendered, View0, View0, Socket).
> arizona_view:rendered_to_iolist(View).
[<<"<html>\n    <head></head>\n    <body id=\"">>,<<"app">>,<<"\"> ">>,
 [<<"<div id=\"">>,<<"counter">>,<<"\"> ">>,<<"0">>,<<>>,
  [<<"<button> ">>,<<"Increment">>,<<"</button>">>],
  <<"</div>">>],
 <<"</body>\n</html>">>]
```
""".
-spec rendered_to_iolist(View) -> iolist() when
    View :: view().
rendered_to_iolist(#view{} = View) ->
    rendered_to_iolist_1(View#view.tmp_rendered).

-spec diff_to_iolist(View) -> Rendered when
    View :: view(),
    Rendered :: arizona_renderer:rendered().
diff_to_iolist(#view{} = View) ->
    case View#view.diff of
        [] ->
            View#view.rendered;
        Diff ->
            diff_replace(Diff, View#view.rendered)
    end.

%% --------------------------------------------------------------------
%% Callback support function definitions
%% --------------------------------------------------------------------

-doc ~"""
Initializes a `t:view/0` by delegating to the `c:mount/2` callback defined
in the view module (`Mod`).

It is called when a view is first rendered or when a WebSocket connection is
established. This function sets up the initial state of the view, including
its bindings.

## Parameters

- `Mod`: The module name of the view being mounted. This must be a valid atom
  and cannot be `undefined`.
- `Bindings`: A map (`t:bindings/0`) containing the initial data for the view.
- `Socket`: The WebSocket connection (`t:arizona_socket:socket/0`), used for
  real-time communication between the client and server.

## Returns

The return value is determined by the `c:mount/2` callback in the view module.
It typically returns `{ok, View}` where `View` is the initialized view state
(`t:view/0`), or `ignore` if the view should not be mounted.
""".
-spec mount(Mod, Bindings, Socket) -> Return when
    Mod :: module(),
    Bindings :: bindings(),
    Socket :: arizona:socket(),
    Return :: mount_ret().
mount(Mod, Bindings, Socket) when is_atom(Mod), Mod =/= undefined, is_map(Bindings) ->
    erlang:apply(Mod, mount, [Bindings, Socket]).

-doc ~"""
Renders the view's template by delegating to the `c:render/1` callback defined in
the view module (`Mod`).

It is called whenever the view needs to be re-rendered, such as after a state update
or in response to a client event.

## Parameters

- `View`: The current view state (`t:view/0`), which includes the view module,
  bindings, and other metadata.

## Returns

The rendered template as `t:arizona:rendered_view_template/0`. This is typically
a tuple delegated to `arizona_renderer:render/4` or `arizona_diff:diff/6`. Such
delegate depends on the `t:arizona_socket:render_context/0`.
""".
-spec render(View) -> Rendered when
    View :: view(),
    Rendered :: arizona:rendered_view_template().
render(#view{module = Mod} = View) when Mod =/= undefined ->
    erlang:apply(Mod, render, [View]).

-spec handle_join(Mod, Topic, Params, View) -> Return when
    Mod :: module(),
    Topic :: event_name(),
    Params :: event_payload(),
    View :: view(),
    Return :: handle_join_ret().
handle_join(Mod, Topic, Params, #view{} = View) when is_atom(Mod), is_binary(Topic) ->
    erlang:apply(Mod, handle_join, [Topic, Params, View]).

-doc ~"""
Handles events sent from the client (e.g., button clicks or form submissions) by
delegating to the `c:handle_event/3` callback defined in the view module (`Mod`).

It updates the view's state based on the event and returns the updated view.

## Parameters

- `EventName`: The name of the event (`t:event_name/0`), typically a `t:binary/0`
  (e.g., ~"incr").
- `Payload`: The data associated with the event (`t:event_payload/0), which can
  be any `t:dynamic/0` value.
- `View`: The current view state (`t:view/0) before handling the event.

## Returns

The updated view state (`t:view/0`) after handling the event.
""".
-spec handle_event(EventName, Payload, View) -> Return when
    EventName :: event_name(),
    Payload :: event_payload(),
    View :: view(),
    Return :: handle_event_ret().
handle_event(EventName, Payload, #view{} = View) ->
    erlang:apply(View#view.module, handle_event, [EventName, Payload, View]).

%% --------------------------------------------------------------------
%% Private functions
%% --------------------------------------------------------------------

initial_bindings(Mod, PathParams, QueryString, Bindings) ->
    case handle_params(Mod, PathParams, QueryString) of
        {true, Params} ->
            maps:merge(Bindings, Params);
        false ->
            Bindings
    end.

-doc ~"""
Handles path parameters and query strings for a given module.

This function checks if the provided module implements the `handle_params/2` callback.
If the callback is implemented, it is called with the provided `PathParams` and `QueryString`.
The result is used to compute the initial bindings for the view.

## Parameters

- `Mod`: The module implementing the `handle_params/2` callback.
- `PathParams`: Path parameters parsed from the route URL. For example, in the route
  `"/:user_id/profile"`, `user_id` is a path parameter.
- `QueryString`: The query string from the route URL. For example, in the route
  `"/:user_id/profile?menu=notifications"`, `?menu=notifications` is the query string.

## Returns

`{true, Bindings}` where `Bindings` is of type `t:bindings/0`. These bindings will
be merged into the view's initial bindings, or `false` if the module does not implement
the `handle_params/2` callback or if no merge is required.

## Notes

This function ensures the module is loaded using `code:ensure_loaded/1` before checking
for the `handle_params/2` callback.
""".
-spec handle_params(Mod, PathParams, QueryString) -> Return when
    Mod :: module(),
    PathParams :: arizona:path_params(),
    QueryString :: arizona:query_string(),
    Return :: handle_params_ret().
handle_params(Mod, PathParams, QueryParams) ->
    % Sometimes `erlang:function_exported/3` returns false without `code:ensure_loaded/1`.
    {module, Mod} = code:ensure_loaded(Mod),
    case erlang:function_exported(Mod, handle_params, 2) of
        true ->
            erlang:apply(Mod, handle_params, [PathParams, QueryParams]);
        false ->
            false
    end.

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

-module(arizona_renderer).
-moduledoc ~"""
HTML rendering engine for Arizona templates and components.

Converts templates, views, and components into HTML output by executing
dynamic callbacks and combining results with static content. Handles
the full component lifecycle and template evaluation.

## Rendering Process

1. Extract static HTML parts and dynamic callback sequences from templates
2. Execute dynamic callbacks to get content or nested templates
3. Convert results to HTML using arizona_html
4. Zip static and dynamic parts together into final HTML

## Component Types

- **Layouts** - Wrapper components with slot insertion
- **Views** - Top-level stateful components
- **Stateful Components** - Components with persistent state
- **Stateless Components** - Pure rendering functions
- **Lists** - Rendered collections with item templates

## Example

```erlang
1> Template = arizona_template:from_string(~\"<h1>{Title}</h1>\").
2> {Html, View1} = arizona_renderer:render_template(Template, parent_id, View).
{[~\"<h1>\", ~\"Hello\", ~\"</h1>\"], UpdatedView}
```
""".
-compile({nowarn_redefined_builtin_type, [dynamic/0]}).

%% --------------------------------------------------------------------
%% API function exports
%% --------------------------------------------------------------------

-export([render_layout/1]).
-export([render_view/1]).
-export([render_stateful/3]).
-export([render_stateless/5]).
-export([render_list/4]).
-export([render_map/4]).
-export([render_template/3]).
-export([render_dynamic/4]).

%% --------------------------------------------------------------------
%% Types exports
%% --------------------------------------------------------------------

-export_type([dynamic/0]).

%% --------------------------------------------------------------------
%% Types definitions
%% --------------------------------------------------------------------

-nominal dynamic() :: [arizona_html:html()].

%% --------------------------------------------------------------------
%% API Functions
%% --------------------------------------------------------------------

-doc ~"""
Renders a view within its layout wrapper.

If the view has a layout, renders the layout with the view inserted
into the specified slot. Otherwise renders the view directly.
""".
-spec render_layout(View) -> {Html, View1} when
    View :: arizona_view:view(),
    Html :: arizona_html:html(),
    View1 :: arizona_view:view().
render_layout(View) ->
    case arizona_view:get_layout(View) of
        {LayoutModule, LayoutRenderFun, SlotName, SlotBindings} ->
            Slot = view,
            LayoutBindings = SlotBindings#{SlotName => Slot},
            State = arizona_view:get_state(View),
            Id = arizona_stateful:get_binding(id, State),
            arizona_renderer:render_stateless(
                LayoutModule, LayoutRenderFun, LayoutBindings, Id, View
            );
        none ->
            arizona_renderer:render_view(View)
    end.

-doc ~"""
Renders a view component by calling its render callback.

Extracts the view's ID and calls the render callback to get a template,
then renders that template to HTML.
""".
-spec render_view(View) -> {Html, View1} when
    View :: arizona_view:view(),
    Html :: arizona_html:html(),
    View1 :: arizona_view:view().
render_view(View) ->
    State = arizona_view:get_state(View),
    Id = arizona_stateful:get_binding(id, State),
    Template = arizona_view:call_render_callback(View),
    render_template(Template, Id, View).

-doc ~"""
Renders a stateful component with full lifecycle management.

Prepares the component for rendering (mount if needed), clears tracking
state, sets current component ID, and renders the resulting template.
""".
-spec render_stateful(Module, Bindings, View) -> {Html, View1} when
    Module :: module(),
    Bindings :: arizona_binder:map(),
    View :: arizona_view:view(),
    Html :: arizona_html:html(),
    View1 :: arizona_view:view().
render_stateful(Module, Bindings, View) ->
    {Id, Template, PrepRenderView} = arizona_lifecycle:prepare_render(Module, Bindings, View),
    render_template(Template, Id, PrepRenderView).

-doc ~"""
Renders a stateless component function.

Calls the stateless component's render function with bindings to get
a template, then renders that template to HTML.
""".
-spec render_stateless(Module, Function, Bindings, ParentId, View) -> {Html, View1} when
    Module :: module(),
    Function :: atom(),
    Bindings :: arizona_binder:map(),
    ParentId :: arizona_stateful:id(),
    View :: arizona_view:view(),
    Html :: arizona_html:html(),
    View1 :: arizona_view:view().
render_stateless(Module, Fun, Bindings, ParentId, View) ->
    Template = arizona_stateless:call_render_callback(Module, Fun, Bindings),
    render_template(Template, ParentId, View).

-doc ~"""
Renders a list of items using a template with item callbacks.

Applies the template to each item in the list, executing the dynamic
callback function for each item to generate individual HTML outputs.
""".
-spec render_list(Template, List, ParentId, View) -> {Html, View1} when
    Template :: arizona_template:template(),
    List :: [dynamic()],
    ParentId :: arizona_stateful:id(),
    View :: arizona_view:view(),
    Html :: [arizona_html:html()],
    View1 :: arizona_view:view().
render_list(Template, List, ParentId, View) ->
    Static = arizona_template:get_static(Template),
    DynamicSequence = arizona_template:get_dynamic_sequence(Template),
    DynamicCallback = arizona_template:get_dynamic(Template),
    HtmlList = [
        render_callback_item(DynamicSequence, DynamicCallback, Static, CallbackArg, ParentId, View)
     || CallbackArg <- List
    ],
    {HtmlList, View}.

-doc ~"""
Renders a map using a template with item callbacks.

Applies the template to each map entry, executing the dynamic callback
function for each {Key, Value} tuple to generate individual HTML outputs.
Uses map comprehension for efficient iteration over map entries.
""".
-spec render_map(Template, Map, ParentId, View) -> {Html, View1} when
    Template :: arizona_template:template(),
    Map :: map(),
    ParentId :: arizona_stateful:id(),
    View :: arizona_view:view(),
    Html :: [arizona_html:html()],
    View1 :: arizona_view:view().
render_map(Template, Map, ParentId, View) ->
    Static = arizona_template:get_static(Template),
    DynamicSequence = arizona_template:get_dynamic_sequence(Template),
    DynamicCallback = arizona_template:get_dynamic(Template),
    HtmlList = [
        render_callback_item(DynamicSequence, DynamicCallback, Static, {Key, Value}, ParentId, View)
     || Key := Value <- Map
    ],
    {HtmlList, View}.

-doc ~"""
Renders a template by executing dynamic callbacks and combining with static parts.

Extracts static HTML and dynamic callback sequences from the template,
executes the callbacks, and zips the results together into final HTML.
""".
-spec render_template(Template, ParentId, View) -> {Html, View1} when
    Template :: arizona_template:template(),
    ParentId :: arizona_stateful:id(),
    View :: arizona_view:view(),
    Html :: arizona_html:html(),
    View1 :: arizona_view:view().
render_template(Template, ParentId, View) ->
    Static = arizona_template:get_static(Template),
    DynamicSequence = arizona_template:get_dynamic_sequence(Template),
    Dynamic = arizona_template:get_dynamic(Template),
    render_static_dynamic(Static, DynamicSequence, Dynamic, ParentId, View).

-doc ~"""
Renders dynamic template parts by executing callback sequences.

Executes each dynamic callback in sequence, handling three result types:
- 4-arity functions (render callbacks) - called with render mode
- Templates - recursively rendered
- Other values - converted to HTML
""".
-spec render_dynamic(Sequence, Dynamic, ParentId, View) -> {Render, View1} when
    Sequence :: arizona_template:dynamic_sequence(),
    Dynamic :: arizona_template:dynamic(),
    ParentId :: arizona_stateful:id(),
    View :: arizona_view:view(),
    Render :: dynamic(),
    View1 :: arizona_view:view().
render_dynamic([], _Dynamic, _ParentId, View) ->
    {[], View};
render_dynamic(
    [DynamicElementIndex | T], Dynamic, ParentId, View
) ->
    DynamicCallback = element(DynamicElementIndex, Dynamic),
    case DynamicCallback() of
        Callback when is_function(Callback, 4) ->
            {Html, CallbackView} = Callback(render, ParentId, DynamicElementIndex, View),
            {RestHtml, FinalView} = render_dynamic(T, Dynamic, ParentId, CallbackView),
            {[Html | RestHtml], FinalView};
        Result ->
            case arizona_template:is_template(Result) of
                true ->
                    {Html, TemplateView} = render_template(Result, ParentId, View),
                    {RestHtml, FinalView} = render_dynamic(T, Dynamic, ParentId, TemplateView),
                    {[Html | RestHtml], FinalView};
                false ->
                    Html = arizona_html:to_html(Result),
                    {RestHtml, FinalView} = render_dynamic(T, Dynamic, ParentId, View),
                    {[Html | RestHtml], FinalView}
            end
    end.

%% --------------------------------------------------------------------
%% Internal Functions
%% --------------------------------------------------------------------

-spec render_static_dynamic(Static, DynamicSequence, Dynamic, ParentId, View) -> {Html, View1} when
    Static :: arizona_template:static(),
    DynamicSequence :: arizona_template:dynamic_sequence(),
    Dynamic :: arizona_template:dynamic(),
    ParentId :: arizona_stateful:id(),
    View :: arizona_view:view(),
    Html :: arizona_html:html(),
    View1 :: arizona_view:view().
render_static_dynamic(Static, DynamicSequence, Dynamic, ParentId, View) ->
    {DynamicRender, FinalView} = render_dynamic(DynamicSequence, Dynamic, ParentId, View),
    Html = zip_static_dynamic(Static, DynamicRender),
    {Html, FinalView}.

%% Zip static and dynamic parts for list item
zip_static_dynamic([], []) ->
    [];
zip_static_dynamic([S | Static], [D | Dynamic]) ->
    [S, D | zip_static_dynamic(Static, Dynamic)];
zip_static_dynamic([S | Static], []) ->
    [S | zip_static_dynamic(Static, [])];
zip_static_dynamic([], [D | Dynamic]) ->
    [D | zip_static_dynamic([], Dynamic)].

%% --------------------------------------------------------------------
%% EUnit Tests
%% --------------------------------------------------------------------

-ifdef(TEST).
-include_lib("eunit/include/eunit.hrl").

zip_static_dynamic_test_() ->
    [
        {"Empty lists should return empty list", ?_assertEqual([], zip_static_dynamic([], []))},

        {"Equal length lists should interleave elements",
            ?_assertEqual(
                [~"<div>", ~"content", ~"</div>", ~"more"],
                zip_static_dynamic([~"<div>", ~"</div>"], [~"content", ~"more"])
            )},

        {"Static longer than dynamic should append static elements",
            ?_assertEqual(
                [~"<p>", ~"text", ~"</p>", ~"<footer>"],
                zip_static_dynamic([~"<p>", ~"</p>", ~"<footer>"], [~"text"])
            )},

        {"Dynamic longer than static should append dynamic elements",
            ?_assertEqual(
                [~"<span>", ~"first", ~"second", ~"third"],
                zip_static_dynamic([~"<span>"], [~"first", ~"second", ~"third"])
            )}
    ].

-endif.

%% Helper function for render_list and render_map
render_callback_item(DynamicSequence, DynamicCallback, Static, CallbackArg, ParentId, View) ->
    Dynamic = DynamicCallback(CallbackArg),
    {DynamicHtml, _UpdatedView} = render_dynamic(DynamicSequence, Dynamic, ParentId, View),
    zip_static_dynamic(Static, DynamicHtml).

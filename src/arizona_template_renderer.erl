-module(arizona_template_renderer).

%% --------------------------------------------------------------------
%% API function exports
%% --------------------------------------------------------------------

-export([render_stateful/3]).
-export([render_stateless/4]).
-export([render_template/2]).
-export([render_dynamic_content/2]).

%% --------------------------------------------------------------------
%% API Functions
%% --------------------------------------------------------------------

-spec render_stateful(Module, Bindings, View) -> {Html, View1} when
    Module :: module(),
    Bindings :: arizona_binder:bindings(),
    View :: arizona_view:view(),
    Html :: arizona_html:html(),
    View1 :: arizona_view:view().
render_stateful(Module, Bindings, View) ->
    {Id, Template, View1} = arizona_stateful:prepare_render(Module, Bindings, View),
    ok = arizona_view:live_clear_component_dependencies(Id, View1),
    ok = arizona_view:live_set_current_stateful_id(Id, View1),
    render_template(Template, View1).

-spec render_stateless(Module, Function, Bindings, View) -> {Html, View1} when
    Module :: module(),
    Function :: atom(),
    Bindings :: arizona_binder:bindings(),
    View :: arizona_view:view(),
    Html :: arizona_html:html(),
    View1 :: arizona_view:view().
render_stateless(Module, Fun, Bindings, View) ->
    Template = arizona_stateless:call_render_callback(Module, Fun, Bindings),
    render_template(Template, View).

-spec render_template(Template, View) -> {Html, View1} when
    Template :: arizona_template:template(),
    View :: arizona_view:view(),
    Html :: arizona_html:html(),
    View1 :: arizona_view:view().
render_template(Template, View) ->
    Static = arizona_template:static(Template),
    {Dynamic, FinalView} = render_dynamic_content(Template, View),
    Html = zip_static_dynamic(Static, Dynamic),
    {Html, FinalView}.

-spec render_dynamic_content(Template, View) -> {Dynamic, View1} when
    Template :: arizona_template:template(),
    View :: arizona_view:view(),
    Dynamic :: [arizona_html:html()],
    View1 :: arizona_view:view().
render_dynamic_content(Template, View) ->
    DynamicSequence = arizona_template:dynamic_sequence(Template),
    DynamicTuple = arizona_template:dynamic(Template),
    render_dynamic_callbacks(DynamicSequence, DynamicTuple, View).

%% --------------------------------------------------------------------
%% Internal Functions
%% --------------------------------------------------------------------

render_dynamic_callbacks([], _DynamicTuple, View) ->
    {[], View};
render_dynamic_callbacks([ElementIndex | T], DynamicTuple, View) ->
    ok = arizona_view:live_set_current_element_index(ElementIndex, View),
    DynamicCallback = element(ElementIndex, DynamicTuple),
    case DynamicCallback() of
        Callback when is_function(Callback, 1) ->
            {Html, CallbackView} = Callback(View),
            {RestHtml, FinalView} = render_dynamic_callbacks(T, DynamicTuple, CallbackView),
            {[Html | RestHtml], FinalView};
        Result ->
            Html = arizona_html:to_html(Result),
            {RestHtml, FinalView} = render_dynamic_callbacks(T, DynamicTuple, View),
            {[Html | RestHtml], FinalView}
    end.

%% Zip static and dynamic parts for list item
zip_static_dynamic([], []) ->
    [];
zip_static_dynamic([S | Static], [D | Dynamic]) ->
    [S, D | zip_static_dynamic(Static, Dynamic)];
zip_static_dynamic([S | Static], []) ->
    [S | zip_static_dynamic(Static, [])];
zip_static_dynamic([], [D | Dynamic]) ->
    [D | zip_static_dynamic([], Dynamic)].

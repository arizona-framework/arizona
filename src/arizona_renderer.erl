-module(arizona_renderer).
-compile({nowarn_redefined_builtin_type, [dynamic/0]}).

%% --------------------------------------------------------------------
%% API function exports
%% --------------------------------------------------------------------

-export([render_view/1]).
-export([render_stateful/3]).
-export([render_stateless/5]).
-export([render_list/4]).
-export([render_template/4]).
-export([render_dynamic/5]).

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

-spec render_view(View) -> {Html, View1} when
    View :: arizona_view:view(),
    Html :: arizona_html:html(),
    View1 :: arizona_view:view().
render_view(View) ->
    State = arizona_view:get_state(View),
    Id = arizona_stateful:get_binding(id, State),
    Template = arizona_view:call_render_callback(View),
    render_template(Template, ok, Id, View).

-spec render_stateful(Module, Bindings, View) -> {Html, View1} when
    Module :: module(),
    Bindings :: arizona_binder:bindings(),
    View :: arizona_view:view(),
    Html :: arizona_html:html(),
    View1 :: arizona_view:view().
render_stateful(Module, Bindings, View) ->
    {Id, Template, PrepRenderView} = arizona_lifecycle:prepare_render(Module, Bindings, View),
    _OldTracker = arizona_tracker_dict:clear_stateful_dependencies(Id),
    _ClearTracker = arizona_tracker_dict:set_current_stateful_id(Id),
    render_template(Template, ok, Id, PrepRenderView).

-spec render_stateless(Module, Function, Bindings, ParentId, View) -> {Html, View1} when
    Module :: module(),
    Function :: atom(),
    Bindings :: arizona_binder:bindings(),
    ParentId :: arizona_stateful:id(),
    View :: arizona_view:view(),
    Html :: arizona_html:html(),
    View1 :: arizona_view:view().
render_stateless(Module, Fun, Bindings, ParentId, View) ->
    Template = arizona_stateless:call_render_callback(Module, Fun, Bindings),
    render_template(Template, ok, ParentId, View).

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
    Dynamic = arizona_template:get_dynamic(Template),
    {
        [
            begin
                {DynamicHtml, _UpdatedView} = render_dynamic(
                    DynamicSequence, Dynamic, CallbackArg, ParentId, View
                ),
                zip_static_dynamic(Static, DynamicHtml)
            end
         || CallbackArg <- List
        ],
        View
    }.

-spec render_template(Template, CallbackArg, ParentId, View) -> {Html, View1} when
    Template :: arizona_template:template(),
    CallbackArg :: dynamic(),
    ParentId :: arizona_stateful:id(),
    View :: arizona_view:view(),
    Html :: arizona_html:html(),
    View1 :: arizona_view:view().
render_template(Template, CallbackArg, ParentId, View) ->
    Static = arizona_template:get_static(Template),
    DynamicSequence = arizona_template:get_dynamic_sequence(Template),
    Dynamic = arizona_template:get_dynamic(Template),
    {DynamicRender, FinalView} = render_dynamic(
        DynamicSequence, Dynamic, CallbackArg, ParentId, View
    ),
    Html = zip_static_dynamic(Static, DynamicRender),
    {Html, FinalView}.

-spec render_dynamic(Sequence, Dynamic, CallbackArg, ParentId, View) -> {Render, View1} when
    Sequence :: arizona_template:dynamic_sequence(),
    Dynamic :: arizona_template:dynamic(),
    CallbackArg :: dynamic(),
    ParentId :: arizona_stateful:id(),
    View :: arizona_view:view(),
    Render :: dynamic(),
    View1 :: arizona_view:view().
render_dynamic([], _Dynamic, _CallbackArg, _ParentId, View) ->
    {[], View};
render_dynamic(
    [DynamicElementIndex | T], Dynamic, CallbackArg, ParentId, View
) ->
    DynamicCallback = element(DynamicElementIndex, Dynamic),
    case DynamicCallback(CallbackArg) of
        Callback when is_function(Callback, 4) ->
            {Html, CallbackView} = Callback(render, ParentId, DynamicElementIndex, View),
            {RestHtml, FinalView} = render_dynamic(T, Dynamic, CallbackArg, ParentId, CallbackView),
            {[Html | RestHtml], FinalView};
        Result ->
            case arizona_template:is_template(Result) of
                true ->
                    {Html, TemplateView} = render_template(Result, ok, ParentId, View),
                    {RestHtml, FinalView} = render_dynamic(
                        T, Dynamic, CallbackArg, ParentId, TemplateView
                    ),
                    {[Html | RestHtml], FinalView};
                false ->
                    Html = arizona_html:to_html(Result),
                    {RestHtml, FinalView} = render_dynamic(T, Dynamic, CallbackArg, ParentId, View),
                    {[Html | RestHtml], FinalView}
            end
    end.

%% --------------------------------------------------------------------
%% Internal Functions
%% --------------------------------------------------------------------

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

-module(arizona_renderer).
-compile({nowarn_redefined_builtin_type, [dynamic/0]}).

%% --------------------------------------------------------------------
%% API function exports
%% --------------------------------------------------------------------

-export([render_view/1]).
-export([render_stateful/3]).
-export([render_stateless/4]).
-export([render_dynamic/3]).

%% --------------------------------------------------------------------
%% Types exports
%% --------------------------------------------------------------------

-export_type([dynamic/0]).
-export_type([render_mode/0]).

%% --------------------------------------------------------------------
%% Types definitions
%% --------------------------------------------------------------------

-nominal dynamic() :: [arizona_html:html()].
-nominal render_mode() :: render | diff | hierarchical.

%% --------------------------------------------------------------------
%% API Functions
%% --------------------------------------------------------------------

-spec render_view(View) -> {Html, View1} when
    View :: arizona_view:view(),
    Html :: arizona_html:html(),
    View1 :: arizona_view:view().
render_view(View) ->
    Template = arizona_view:call_render_callback(View),
    render_template(Template, render, View).

-spec render_stateful(Module, Bindings, View) -> {Html, View1} when
    Module :: module(),
    Bindings :: arizona_binder:bindings(),
    View :: arizona_view:view(),
    Html :: arizona_html:html(),
    View1 :: arizona_view:view().
render_stateful(Module, Bindings, View) ->
    {Id, Template, PrepRenderView} = arizona_lifecycle:prepare_render(Module, Bindings, View),
    ok = arizona_tracker_dict:clear_stateful_dependencies(Id),
    ok = arizona_tracker_dict:set_current_stateful_id(Id),
    render_template(Template, render, PrepRenderView).

-spec render_stateless(Module, Function, Bindings, View) -> {Html, View1} when
    Module :: module(),
    Function :: atom(),
    Bindings :: arizona_binder:bindings(),
    View :: arizona_view:view(),
    Html :: arizona_html:html(),
    View1 :: arizona_view:view().
render_stateless(Module, Fun, Bindings, View) ->
    Template = arizona_stateless:call_render_callback(Module, Fun, Bindings),
    render_template(Template, render, View).

-spec render_dynamic(Template, RenderMode, View) -> {Dynamic, View1} when
    Template :: arizona_template:template(),
    RenderMode :: render_mode(),
    View :: arizona_view:view(),
    Dynamic :: dynamic(),
    View1 :: arizona_view:view().
render_dynamic(Template, RenderMode, View) ->
    DynamicSequence = arizona_template:get_dynamic_sequence(Template),
    Dynamic = arizona_template:get_dynamic(Template),
    render_dynamic_callbacks(DynamicSequence, Dynamic, RenderMode, View).

%% --------------------------------------------------------------------
%% Internal Functions
%% --------------------------------------------------------------------

-spec render_template(Template, RenderMode, View) -> {Html, View1} when
    Template :: arizona_template:template(),
    RenderMode :: render_mode(),
    View :: arizona_view:view(),
    Html :: arizona_html:html(),
    View1 :: arizona_view:view().
render_template(Template, RenderMode, View) ->
    Static = arizona_template:get_static(Template),
    {Dynamic, FinalView} = render_dynamic(Template, RenderMode, View),
    Html = zip_static_dynamic(Static, Dynamic),
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

render_dynamic_callbacks([], _Dynamic, _RenderMode, View) ->
    {[], View};
render_dynamic_callbacks([ElementIndex | T], Dynamic, RenderMode, View) ->
    ok = arizona_tracker_dict:set_current_element_index(ElementIndex),
    DynamicCallback = element(ElementIndex, Dynamic),
    case DynamicCallback() of
        Callback when is_function(Callback, 2) ->
            {Html, CallbackView} = Callback(RenderMode, View),
            {RestHtml, FinalView} = render_dynamic_callbacks(T, Dynamic, RenderMode, CallbackView),
            {[Html | RestHtml], FinalView};
        Result ->
            Html = arizona_html:to_html(Result),
            {RestHtml, FinalView} = render_dynamic_callbacks(T, Dynamic, RenderMode, View),
            {[Html | RestHtml], FinalView}
    end.

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

render_dynamic_callbacks_test_() ->
    MockView = create_test_view(),
    MockDynamic = {fun() -> ~"callback1" end, fun() -> ~"callback2" end},
    [
        {"Empty sequence should return empty dynamic and unchanged view",
            ?_assertEqual(
                {[], MockView}, render_dynamic_callbacks([], MockDynamic, render, MockView)
            )},

        {"Single callback should render one element", fun() ->
            {Dynamic, _View} = render_dynamic_callbacks([1], MockDynamic, render, MockView),
            ?assertEqual([~"callback1"], Dynamic)
        end},

        {"Multiple callbacks should render in sequence", fun() ->
            {Dynamic, _View} = render_dynamic_callbacks([1, 2], MockDynamic, render, MockView),
            ?assertEqual([~"callback1", ~"callback2"], Dynamic)
        end},

        {"Callback returning function should be invoked with view", fun() ->
            MockDynamicWithFunction = {fun() -> fun(View) -> {~"from_function", View} end end},
            {Dynamic, _View} = render_dynamic_callbacks(
                [1], MockDynamicWithFunction, render, MockView
            ),
            ?assertEqual([~"from_function"], Dynamic)
        end}
    ].

create_test_view() ->
    % Create a minimal test view for internal function testing
    arizona_view:new(test_module, #{id => ~"test"}, none).

-endif.

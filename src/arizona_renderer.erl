-module(arizona_renderer).
-compile({nowarn_redefined_builtin_type, [dynamic/0]}).

%% --------------------------------------------------------------------
%% API function exports
%% --------------------------------------------------------------------

-export([render_stateful/3]).
-export([render_stateless/4]).
-export([render_template/2]).
-export([render_dynamic/2]).

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

-spec render_stateful(Module, Bindings, View) -> {Html, View1} when
    Module :: module(),
    Bindings :: arizona_binder:bindings(),
    View :: arizona_view:view(),
    Html :: arizona_html:html(),
    View1 :: arizona_view:view().
render_stateful(Module, Bindings, View) ->
    {Id, Template, View1} = arizona_lifecycle:prepare_render(Module, Bindings, View),
    ok = arizona_view:live_clear_stateful_dependencies(Id, View1),
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
    Static = arizona_template:get_static(Template),
    {Dynamic, FinalView} = render_dynamic(Template, View),
    Html = zip_static_dynamic(Static, Dynamic),
    {Html, FinalView}.

-spec render_dynamic(Template, View) -> {Dynamic, View1} when
    Template :: arizona_template:template(),
    View :: arizona_view:view(),
    Dynamic :: dynamic(),
    View1 :: arizona_view:view().
render_dynamic(Template, View) ->
    DynamicSequence = arizona_template:get_dynamic_sequence(Template),
    Dynamic = arizona_template:get_dynamic(Template),
    render_dynamic_callbacks(DynamicSequence, Dynamic, View).

%% --------------------------------------------------------------------
%% Internal Functions
%% --------------------------------------------------------------------

render_dynamic_callbacks([], _Dynamic, View) ->
    {[], View};
render_dynamic_callbacks([ElementIndex | T], Dynamic, View) ->
    ok = arizona_view:live_set_current_element_index(ElementIndex, View),
    DynamicCallback = element(ElementIndex, Dynamic),
    case DynamicCallback() of
        Callback when is_function(Callback, 1) ->
            {Html, CallbackView} = Callback(View),
            {RestHtml, FinalView} = render_dynamic_callbacks(T, Dynamic, CallbackView),
            {[Html | RestHtml], FinalView};
        Result ->
            Html = arizona_html:to_html(Result),
            {RestHtml, FinalView} = render_dynamic_callbacks(T, Dynamic, View),
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
            ?_assertEqual({[], MockView}, render_dynamic_callbacks([], MockDynamic, MockView))},

        {"Single callback should render one element", fun() ->
            {Dynamic, _View} = render_dynamic_callbacks([1], MockDynamic, MockView),
            ?assertEqual([~"callback1"], Dynamic)
        end},

        {"Multiple callbacks should render in sequence", fun() ->
            {Dynamic, _View} = render_dynamic_callbacks([1, 2], MockDynamic, MockView),
            ?assertEqual([~"callback1", ~"callback2"], Dynamic)
        end},

        {"Callback returning function should be invoked with view", fun() ->
            MockDynamicWithFunction = {fun() -> fun(View) -> {~"from_function", View} end end},
            {Dynamic, _View} = render_dynamic_callbacks([1], MockDynamicWithFunction, MockView),
            ?assertEqual([~"from_function"], Dynamic)
        end}
    ].

create_test_view() ->
    % Create a minimal test view for internal function testing
    State = arizona_stateful:new(test_module, arizona_binder:new(#{id => ~"test"})),
    arizona_view:new(test_module, State, render, undefined).

-endif.

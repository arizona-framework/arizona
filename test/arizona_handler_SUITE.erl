-module(arizona_handler_SUITE).
-behaviour(ct_suite).
-include_lib("stdlib/include/assert.hrl").
-compile([export_all, nowarn_export_all]).

%% --------------------------------------------------------------------
%% Behaviour (ct_suite) callbacks
%% --------------------------------------------------------------------

all() ->
    [
        {group, handler_integration_tests}
    ].

groups() ->
    [
        {handler_integration_tests, [parallel], [
            handler_init_success_test,
            handler_with_layout_test,
            handler_error_handling_test
        ]}
    ].

init_per_suite(Config) ->
    % Config
    ServerPort = 8081,
    ViewRouteUrl = ~"/test",
    ErrorViewRouteUrl = ~"/error-test",
    ViewWithLayoutRouteUrl = ~"/layout-test",
    MockViewModule = arizona_handler_mock_view,
    MockErrorViewModule = arizona_handler_mock_error_view,
    MockViewWithLayoutModule = arizona_handler_mock_view_with_layout,
    MockLayoutModule = arizona_handler_mock_layout,
    LayoutRenderFun = render,
    LayoutSlotName = main_content,

    % Ensure applications are started
    {ok, _} = application:ensure_all_started(cowboy),
    % For httpc
    {ok, _} = application:ensure_all_started(inets),
    % Start arizona server
    {ok, _Pid} = arizona_server:start(#{
        transport_opts => [{port, ServerPort}],
        routes => [
            {live, ViewRouteUrl, MockViewModule, #{}},
            {live, ErrorViewRouteUrl, MockErrorViewModule, #{}},
            {live, ViewWithLayoutRouteUrl, MockViewWithLayoutModule, #{}}
        ]
    }),

    % Create mock modules for testing
    MockViewCode = merl:qquote(~""""
    -module('@module').
    -compile({parse_transform, arizona_parse_transform}).
    -behaviour(arizona_view).

    -export([mount/2]).
    -export([render/1]).

    mount(_Arg, _Req) ->
        arizona_view:new('@module', #{
            id => ~"test_id",
            title => ~"Arizona"
        }, none).

    render(Bindings) ->
        arizona_template:from_string(~"""
        <!DOCTYPE html>
        <html>
        <head>
            <title>{arizona_template:get_binding(title, Bindings)}</title>
        </head>
        <body>
            Hello, World!
        </body>
        </html>
        """).
    """", [{module, merl:term(MockViewModule)}]),

    % Create mock modules for testing
    MockErrorViewCode = merl:qquote(~""""
    -module('@module').
    -compile({parse_transform, arizona_parse_transform}).
    -behaviour(arizona_view).

    -export([mount/2]).
    -export([render/1]).

    mount(_Arg, _Req) ->
        error('@module').

    render(Bindings) ->
        arizona_template:from_string(~"""
        <!DOCTYPE html>
        <html>
        <head>
            <title>Error</title>
        </head>
        <body>
            Should not render!
        </body>
        </html>
        """).
    """", [{module, merl:term(MockErrorViewModule)}]),

    MockViewWithLayoutCode = merl:qquote(~""""
    -module('@module').
    -compile({parse_transform, arizona_parse_transform}).
    -behaviour(arizona_view).

    -export([mount/2]).
    -export([render/1]).

    mount(_Arg, _Req) ->
        Layout = {'@layout_module', '@layout_render_fun', '@layout_slot_name', #{
            title => ~"Arizona With Layout"
        }},
        arizona_view:new('@module', #{
            id => ~"test_id"
        }, Layout).

    render(_Bindings) ->
        arizona_template:from_string(~"""
        <h1>Mock View</h1>
        """).
    """", [
        {module, merl:term(MockViewWithLayoutModule)},
        {layout_module, merl:term(MockLayoutModule)},
        {layout_render_fun, merl:term(LayoutRenderFun)},
        {layout_slot_name, merl:term(LayoutSlotName)}
    ]),

    MockLayoutCode = merl:qquote(~""""
    -module('@module').
    -compile({parse_transform, arizona_parse_transform}).
    -export([render/1]).

    '@render_fun'(Bindings) ->
        SlotName = '@slot_name',
        arizona_template:from_string(~"""
        <!DOCTYPE html>
        <html>
        <head>
            <title>{arizona_template:get_binding(title, Bindings)}</title>
        </head>
        <body>
            {arizona_template:render_slot(arizona_template:get_binding(SlotName, Bindings))}
        </body>
        </html>
        """).
    """", [
        {module, merl:term(MockLayoutModule)},
        {render_fun, merl:term(LayoutRenderFun)},
        {slot_name, merl:term(LayoutSlotName)}
    ]),

    {ok, _ViewBinary} = merl:compile_and_load(MockViewCode),
    {ok, _ErrorViewBinary} = merl:compile_and_load(MockErrorViewCode),
    {ok, _ViewWithLayoutBinary} = merl:compile_and_load(MockViewWithLayoutCode),
    {ok, _LayoutBinary} = merl:compile_and_load(MockLayoutCode),

    [
        {server_port, ServerPort},
        {view_route_url, ViewRouteUrl},
        {error_view_route_url, ErrorViewRouteUrl},
        {view_with_layout_route_url, ViewWithLayoutRouteUrl},
        {mock_view_module, MockViewModule},
        {mock_error_view_module, MockErrorViewModule},
        {mock_view_with_layout_module, MockViewWithLayoutModule},
        {mock_layout_module, MockLayoutModule}
        | Config
    ].

end_per_suite(Config) ->
    % Stop arizona server
    arizona_server:stop(),

    {mock_view_module, MockViewModule} = proplists:lookup(mock_view_module, Config),
    {mock_error_view_module, MockErrorViewModule} = proplists:lookup(
        mock_error_view_module, Config
    ),
    {mock_view_with_layout_module, MockViewWithLayoutModule} = proplists:lookup(
        mock_view_with_layout_module, Config
    ),
    {mock_layout_module, MockLayoutModule} = proplists:lookup(mock_layout_module, Config),

    code:purge(MockViewModule),
    code:purge(MockErrorViewModule),
    code:purge(MockViewWithLayoutModule),
    code:purge(MockLayoutModule),

    code:delete(MockViewModule),
    code:delete(MockErrorViewModule),
    code:delete(MockViewWithLayoutModule),
    code:delete(MockLayoutModule),

    ok.

%% --------------------------------------------------------------------
%% Handler integration tests
%% --------------------------------------------------------------------

handler_init_success_test(Config) when is_list(Config) ->
    ct:comment("Handler should successfully initialize and process view requests"),
    {server_port, ServerPort} = proplists:lookup(server_port, Config),
    {view_route_url, ViewRouteUrl} = proplists:lookup(view_route_url, Config),
    HttpResponse = http_get(ServerPort, ViewRouteUrl),
    ?assertMatch({ok, {200, _HtmlBody}}, HttpResponse),
    {ok, {200, HtmlBody}} = HttpResponse,
    ?assert(binary:match(HtmlBody, ~"Hello, World!") =/= nomatch).

handler_with_layout_test(Config) when is_list(Config) ->
    ct:comment("Handler should successfully render views with layout integration"),
    {server_port, ServerPort} = proplists:lookup(server_port, Config),
    {view_with_layout_route_url, LayoutRouteUrl} = proplists:lookup(
        view_with_layout_route_url, Config
    ),
    HttpResponse = http_get(ServerPort, LayoutRouteUrl),
    ?assertMatch({ok, {200, _HtmlBody}}, HttpResponse),
    {ok, {200, HtmlBody}} = HttpResponse,
    ?assert(binary:match(HtmlBody, ~"Arizona With Layout") =/= nomatch),
    ?assert(binary:match(HtmlBody, ~"<h1>Mock View</h1>") =/= nomatch).

handler_error_handling_test(Config) when is_list(Config) ->
    ct:comment("Handler should gracefully handle view mounting errors"),
    {server_port, ServerPort} = proplists:lookup(server_port, Config),
    {error_view_route_url, ErrorRouteUrl} = proplists:lookup(error_view_route_url, Config),
    HttpResponse = http_get(ServerPort, ErrorRouteUrl),
    ?assertMatch({ok, {500, _ErrorBody}}, HttpResponse),
    {ok, {500, HtmlBody}} = HttpResponse,
    ?assert(binary:match(HtmlBody, ~"Error:") =/= nomatch).

%% --------------------------------------------------------------------
%% HTTP Helper Functions
%% --------------------------------------------------------------------

%% Simple GET request helper
http_get(ServerPort, RouteUrl) ->
    RequestUrl = io_lib:format("http://localhost:~p~s", [ServerPort, RouteUrl]),
    httpc:request(get, {RequestUrl, []}, [], [
        {body_format, binary},
        {full_result, false}
    ]).

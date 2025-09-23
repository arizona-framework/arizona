-module(arizona_websocket_SUITE).
-behaviour(ct_suite).
-include_lib("stdlib/include/assert.hrl").
-compile([export_all, nowarn_export_all]).

%% --------------------------------------------------------------------
%% Behaviour (ct_suite) callbacks
%% --------------------------------------------------------------------

all() ->
    [
        {group, websocket_tests}
    ].

groups() ->
    [
        {websocket_tests, [parallel], [
            initial_render_test,
            ping_test,
            unknown_message_test,
            invalid_json_test
        ]}
    ].

init_per_suite(Config) ->
    %% Setup test configuration
    ServerPort = 8082,
    ViewWithLayoutRouteUrl = ~"/websocket-layout-test",
    WebSocketRouteUrl = ~"/websocket",
    MockViewWithLayoutModule = arizona_websocket_mock_view_with_layout,
    MockLayoutModule = arizona_websocket_mock_layout,
    LayoutRenderFun = render,
    LayoutSlotName = main_content,

    % Start pg groups for testing (ignore if already started)
    PgLivePid =
        case pg:start(arizona_live) of
            {ok, Pid1} -> Pid1;
            {error, {already_started, Pid1}} -> Pid1
        end,
    PgPubSubPid =
        case pg:start(arizona_pubsub) of
            {ok, Pid2} -> Pid2;
            {error, {already_started, Pid2}} -> Pid2
        end,

    %% Ensure applications are started
    {ok, _} = application:ensure_all_started(cowboy),
    %% For gun WebSocket client
    {ok, _} = application:ensure_all_started(gun),
    %% Start arizona server
    ok = arizona_server:start(#{
        enabled => true,
        transport_opts => [{port, ServerPort}],
        routes => [
            {view, ViewWithLayoutRouteUrl, MockViewWithLayoutModule, #{}, []},
            {websocket, WebSocketRouteUrl, #{}, []}
        ]
    }),

    %% Create mock modules for testing
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
        arizona_template:from_html(~"""
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
        arizona_template:from_html(~"""
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

    {ok, _ViewWithLayoutBinary} = merl:compile_and_load(MockViewWithLayoutCode),
    {ok, _LayoutBinary} = merl:compile_and_load(MockLayoutCode),

    [
        {pg_live_pid, PgLivePid},
        {pg_pubsub_pid, PgPubSubPid},
        {server_port, ServerPort},
        {view_with_layout_route_url, ViewWithLayoutRouteUrl},
        {websocket_route_url, WebSocketRouteUrl},
        {mock_view_with_layout_module, MockViewWithLayoutModule},
        {mock_layout_module, MockLayoutModule}
        | Config
    ].

end_per_suite(Config) ->
    % Stop apps
    ok = arizona_server:stop(),
    ok = application:stop(gun),
    ok = application:stop(cowboy),

    {pg_live_pid, PgLivePid} = proplists:lookup(pg_live_pid, Config),
    {pg_pubsub_pid, PgPubSubPid} = proplists:lookup(pg_pubsub_pid, Config),
    is_process_alive(PgLivePid) andalso exit(PgLivePid, normal),
    is_process_alive(PgPubSubPid) andalso exit(PgPubSubPid, normal),

    {mock_view_with_layout_module, MockViewWithLayoutModule} = proplists:lookup(
        mock_view_with_layout_module, Config
    ),
    {mock_layout_module, MockLayoutModule} = proplists:lookup(mock_layout_module, Config),

    code:purge(MockViewWithLayoutModule),
    code:purge(MockLayoutModule),

    code:delete(MockViewWithLayoutModule),
    code:delete(MockLayoutModule),

    ok.

init_per_testcase(initial_render_test, Config) ->
    {server_port, ServerPort} = proplists:lookup(server_port, Config),
    {websocket_route_url, WebSocketRouteUrl} = proplists:lookup(websocket_route_url, Config),
    {view_with_layout_route_url, ViewWithLayoutRouteUrl} = proplists:lookup(
        view_with_layout_route_url, Config
    ),
    {ok, Conn} = ws_open_and_upgrade(ServerPort, WebSocketRouteUrl, ViewWithLayoutRouteUrl),
    [{conn, Conn} | Config];
init_per_testcase(_TestcaseName, Config) ->
    {server_port, ServerPort} = proplists:lookup(server_port, Config),
    {websocket_route_url, WebSocketRouteUrl} = proplists:lookup(websocket_route_url, Config),
    {view_with_layout_route_url, ViewWithLayoutRouteUrl} = proplists:lookup(
        view_with_layout_route_url, Config
    ),
    {ok, Conn} = ws_open_and_upgrade(ServerPort, WebSocketRouteUrl, ViewWithLayoutRouteUrl),
    ok = ws_init(Conn),
    [{conn, Conn} | Config].

end_per_testcase(_TestcaseName, Config) ->
    {conn, Conn} = proplists:lookup(conn, Config),
    ok = ws_close(Conn),
    ok.

%% --------------------------------------------------------------------
%% WebSocket tests
%% --------------------------------------------------------------------

initial_render_test(Config) when is_list(Config) ->
    ct:comment("Test WebSocket connection with gun client"),
    {conn, Conn} = proplists:lookup(conn, Config),
    InitialResponse = ws_send(Conn, []),
    ?assertMatch({text, _InitialJSON}, InitialResponse),
    {text, InitialJSON} = InitialResponse,
    ?assertMatch(
        #{
            ~"type" := ~"initial_render",
            ~"stateful_id" := ~"test_id",
            ~"structure" := #{~"test_id" := #{~"static" := Static, ~"dynamic" := Dynamic}}
        } when is_list(Static) andalso is_list(Dynamic),
        json:decode(InitialJSON)
    ).

ping_test(Config) when is_list(Config) ->
    ct:comment("Test ping/pong message handling"),
    {conn, Conn} = proplists:lookup(conn, Config),

    PingMessage = json:encode(#{type => ~"ping"}),
    PingResponse = ws_send(Conn, {text, PingMessage}),
    ?assertMatch({text, _PingJSON}, PingResponse),
    {text, PingJSON} = PingResponse,
    ?assertMatch(#{~"type" := ~"pong"}, json:decode(PingJSON)).

unknown_message_test(Config) when is_list(Config) ->
    ct:comment("Test handling of unknown message types"),
    {conn, Conn} = proplists:lookup(conn, Config),

    UnknownMessage = json:encode(#{type => ~"unknown_type", data => ~"test"}),
    ErrorResponse = ws_send(Conn, {text, UnknownMessage}),
    ?assertMatch({text, _ErrorJSON}, ErrorResponse),
    {text, ErrorJSON} = ErrorResponse,
    ErrorData = json:decode(ErrorJSON),
    ?assertMatch(#{~"type" := ~"error", ~"message" := ~"Unknown message type"}, ErrorData).

invalid_json_test(Config) when is_list(Config) ->
    ct:comment("Test handling of invalid JSON messages"),
    {conn, Conn} = proplists:lookup(conn, Config),

    InvalidJson = ~"invalid json{",
    ErrorResponse = ws_send(Conn, {text, InvalidJson}),
    ?assertMatch({text, _ErrorJSON}, ErrorResponse),
    {text, ErrorJSON} = ErrorResponse,
    ErrorData = json:decode(ErrorJSON),
    ?assertMatch(#{~"type" := ~"error", ~"message" := ~"Internal server error"}, ErrorData).

%% --------------------------------------------------------------------
%% Helper functions
%% --------------------------------------------------------------------

%% Open gun connection and upgrade to WebSocket
ws_open_and_upgrade(ServerPort, WebSocketRouteUrl, ViewRouteUrl) ->
    maybe
        %% Open connection
        {ok, ConnPid} ?= gun:open("localhost", ServerPort),
        {ok, http} ?= gun:await_up(ConnPid),
        %% Upgrade to WebSocket
        QueryString = io_lib:format("path=~s&qs=", [ViewRouteUrl]),
        WsUrl = io_lib:format("~s?~s", [WebSocketRouteUrl, QueryString]),
        StreamRef = gun:ws_upgrade(ConnPid, WsUrl),
        Conn = {ConnPid, StreamRef},
        {ws_upgrade_success, _Headers} = ws_wait_response(Conn),
        {ok, Conn}
    else
        {error, Reason} ->
            {error, Reason}
    end.

%% Initialize WebSocket connection with initial render
ws_init(Conn) ->
    case ws_send(Conn, []) of
        {text, _InitialResponse} ->
            ok;
        {error, Reason} ->
            {error, Reason}
    end.

%% Send WebSocket frames and wait for response
ws_send({ConnPid, StreamRef} = Conn, Frames) ->
    ok = gun:ws_send(ConnPid, StreamRef, Frames),
    ws_wait_response(Conn).

%% Wait for WebSocket response with timeout
ws_wait_response({ConnPid, StreamRef} = Conn) ->
    receive
        {gun_upgrade, ConnPid, StreamRef, [~"websocket"], Headers} ->
            {ws_upgrade_success, Headers};
        {gun_response, ConnPid, _, _, Status, Headers} ->
            {ws_upgrade_failed, Status, Headers};
        {gun_ws, ConnPid, StreamRef, Frames} ->
            Frames;
        {gun_error, ConnPid, StreamRef, Reason} ->
            ok = ws_close(Conn),
            {error, Reason}
    after 5000 ->
        ok = ws_close(Conn),
        {error, timeout}
    end.

%% Close WebSocket connection
ws_close({ConnPid, _StreamRef}) ->
    gun:close(ConnPid).

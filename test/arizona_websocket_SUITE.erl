-module(arizona_websocket_SUITE).
-behaviour(ct_suite).
-include_lib("stdlib/include/assert.hrl").
-compile([export_all, nowarn_export_all]).

%% --------------------------------------------------------------------
%% Behaviour (ct_suite) callbacks
%% --------------------------------------------------------------------

all() ->
    [
        {group, route_resolution},
        {group, websocket_init},
        {group, message_handling}
    ].

groups() ->
    [
        {route_resolution, [parallel], [
            extract_path_from_query_param,
            resolve_live_route_metadata
        ]},
        {websocket_init, [parallel], [
            websocket_init_with_correct_path
        ]},
        {message_handling, [parallel], [
            handle_ping_message,
            handle_unknown_message
        ]}
    ].

%% --------------------------------------------------------------------
%% Route resolution tests
%% --------------------------------------------------------------------

extract_path_from_query_param(_Config) ->
    % Create WebSocket request with path query parameter
    WebSocketReq = #{
        method => ~"GET",
        path => ~"/live/ws",
        % URL encoded "/users"
        qs => ~"path=%2Fusers",
        host => ~"localhost",
        port => 8080,
        scheme => ~"http",
        version => 'HTTP/1.1'
    },

    % Extract and decode the path parameter using cowboy_req:parse_qs/1
    PathParams = cowboy_req:parse_qs(WebSocketReq),
    LivePath = proplists:get_value(~"path", PathParams, ~"/"),

    ?assertEqual(~"/users", LivePath).

resolve_live_route_metadata(_Config) ->
    % Set up test routes
    TestRoutes = [
        {live, ~"/users", user_live, #{}},
        {live, ~"/posts", post_live, #{}},
        {live_websocket, ~"/live/ws"}
    ],

    Dispatch = arizona_server:compile_routes(TestRoutes),
    ok = persistent_term:put(arizona_dispatch, Dispatch),

    % Create WebSocket request
    WebSocketReq = #{
        method => ~"GET",
        path => ~"/live/ws",
        qs => ~"path=%2Fusers",
        host => ~"localhost",
        port => 8080,
        scheme => ~"http",
        version => 'HTTP/1.1'
    },

    % Extract path from query parameter
    PathParams = cowboy_req:parse_qs(WebSocketReq),
    LivePath = proplists:get_value(~"path", PathParams, ~"/"),

    % Create fake request with the LiveView path
    FakeReq = WebSocketReq#{path => LivePath},

    % Now resolve the correct route
    LiveRouteMetadata = arizona_server:get_route_metadata(FakeReq),

    % This should correctly resolve to the user_live handler
    ?assertEqual(live, arizona_server:get_route_type(LiveRouteMetadata)),
    ?assertEqual(user_live, arizona_server:get_route_handler(LiveRouteMetadata)).

%% --------------------------------------------------------------------
%% WebSocket init tests
%% --------------------------------------------------------------------

websocket_init_with_correct_path(_Config) ->
    % Set up test routes
    TestRoutes = [
        {live, ~"/users", user_live, #{}},
        {live_websocket, ~"/live/ws"}
    ],

    Dispatch = arizona_server:compile_routes(TestRoutes),
    ok = persistent_term:put(arizona_dispatch, Dispatch),

    % Create WebSocket request with path parameter
    WebSocketReq = #{
        method => ~"GET",
        path => ~"/live/ws",
        qs => ~"path=%2Fusers",
        host => ~"localhost",
        port => 8080,
        scheme => ~"http",
        version => 'HTTP/1.1'
    },

    % Simulate what arizona_websocket:init should do (fixed version)
    PathParams = cowboy_req:parse_qs(WebSocketReq),
    LivePath = proplists:get_value(~"path", PathParams, ~"/"),

    % Create fake request for route resolution
    FakeReq = WebSocketReq#{path => LivePath},
    RouteMetadata = arizona_server:get_route_metadata(FakeReq),
    LiveModule = arizona_server:get_route_handler(RouteMetadata),

    % Should get the correct LiveModule
    ?assertEqual(user_live, LiveModule).

%% --------------------------------------------------------------------
%% Message handling tests
%% --------------------------------------------------------------------

handle_ping_message(_Config) ->
    % Test ping message handling
    State = arizona_websocket:new_state(self()),

    {Commands, NewState} = arizona_websocket:handle_ping_message(State),

    % Should return pong command
    ?assertEqual([{text, json:encode(#{type => ~"pong"})}], Commands),
    ?assertEqual(arizona_websocket:get_live_pid(State), arizona_websocket:get_live_pid(NewState)).

handle_unknown_message(_Config) ->
    % Test unknown message handling
    State = arizona_websocket:new_state(self()),

    {Commands, NewState} = arizona_websocket:handle_unknown_message(State),

    % Should return error command
    ExpectedPayload = json:encode(#{
        type => ~"error",
        message => ~"Unknown message type"
    }),
    ?assertEqual([{text, ExpectedPayload}], Commands),
    ?assertEqual(arizona_websocket:get_live_pid(State), arizona_websocket:get_live_pid(NewState)).

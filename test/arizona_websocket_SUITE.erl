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
            resolve_live_route_metadata,
            test_init_function
        ]},
        {websocket_init, [parallel], [
            websocket_init_with_correct_path,
            test_websocket_init_function
        ]},
        {message_handling, [parallel], [
            handle_ping_message,
            handle_unknown_message,
            test_websocket_handle_with_event,
            test_websocket_handle_error,
            test_websocket_info,
            test_websocket_handle_ping_direct,
            test_websocket_handle_malformed_json,
            test_websocket_handle_missing_type,
            test_websocket_handle_with_real_event,
            test_websocket_init_with_real_live_process,
            test_init_with_empty_query_string,
            test_init_with_missing_path_param
        ]}
    ].

%% --------------------------------------------------------------------
%% Route resolution tests
%% --------------------------------------------------------------------

extract_path_from_query_param(_Config) ->
    % Create WebSocket request with path query parameter
    WebSocketReq = mock_websocket_request(),

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

test_init_function(_Config) ->
    % Set up test routes
    TestRoutes = [
        {live, ~"/users", user_live, #{}},
        {live_websocket, ~"/live/ws"}
    ],

    Dispatch = arizona_server:compile_routes(TestRoutes),
    ok = persistent_term:put(arizona_dispatch, Dispatch),

    % Create WebSocket request with path parameter
    WebSocketReq = mock_websocket_request(),

    % Test the actual init function
    {cowboy_websocket, Req, {LiveModule, ArizonaReq}} = arizona_websocket:init(WebSocketReq, #{}),

    % Should get the correct LiveModule and request
    ?assertEqual(user_live, LiveModule),
    ?assertEqual(WebSocketReq, Req),
    ?assertEqual(~"/users", arizona_request:get_path(ArizonaReq)).

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

test_websocket_init_function(_Config) ->
    % Test the actual websocket_init function with a real LiveView module
    LiveModule = test_live_component_with_info,
    ArizonaReq = arizona_request:new(#{
        method => ~"GET",
        path => ~"/test"
    }),

    % This should work since test_live_component_with_info is a real LiveView module
    {Commands, State} = arizona_websocket:websocket_init({LiveModule, ArizonaReq}),

    % Should return initial render command
    ?assertMatch([{text, _}], Commands),
    ?assert(is_pid(arizona_websocket:get_live_pid(State))).

%% --------------------------------------------------------------------
%% Message handling tests
%% --------------------------------------------------------------------

handle_ping_message(_Config) ->
    % Test ping message handling
    State = arizona_websocket:new_state(self()),

    {Commands, NewState} = arizona_websocket:handle_ping_message(State),

    % Should return pong command
    ExpectedPongPayload = json:encode(#{type => ~"pong"}),
    ?assertEqual([{text, ExpectedPongPayload}], Commands),
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

test_websocket_handle_with_event(_Config) ->
    % Test websocket_handle with an event message
    State = arizona_websocket:new_state(self()),

    EventMessage = iolist_to_binary(
        json:encode(#{
            type => ~"event",
            event => ~"click",
            params => #{~"target" => ~"button1"}
        })
    ),

    % This will test the message parsing but will fail when calling arizona_live:handle_event
    % since self() is not a real arizona_live process, it will return an error
    {Commands, _NewState} = arizona_websocket:websocket_handle({text, EventMessage}, State),

    % Should return an error due to arizona_live:handle_event failing
    ExpectedErrorPayload = json:encode(#{
        type => ~"error",
        message => ~"Internal server error"
    }),
    ?assertEqual([{text, ExpectedErrorPayload}], Commands).

test_websocket_handle_error(_Config) ->
    % Test websocket_handle with invalid JSON
    State = arizona_websocket:new_state(self()),

    InvalidJson = ~"invalid json {",

    % Should return error payload
    {Commands, _NewState} = arizona_websocket:websocket_handle({text, InvalidJson}, State),

    ExpectedErrorPayload = json:encode(#{
        type => ~"error",
        message => ~"Internal server error"
    }),

    ?assertEqual([{text, ExpectedErrorPayload}], Commands).

test_websocket_info(_Config) ->
    % Test websocket_info with arbitrary message
    State = arizona_websocket:new_state(self()),

    % Should return empty commands for unknown messages
    {Commands, NewState} = arizona_websocket:websocket_info(some_random_message, State),

    ?assertEqual([], Commands),
    ?assertEqual(arizona_websocket:get_live_pid(State), arizona_websocket:get_live_pid(NewState)).

test_websocket_handle_ping_direct(_Config) ->
    % Test websocket_handle with ping message through the main entry point
    State = arizona_websocket:new_state(self()),

    PingMessage = iolist_to_binary(json:encode(#{type => ~"ping"})),

    % Should return pong response
    {Commands, _NewState} = arizona_websocket:websocket_handle({text, PingMessage}, State),

    ExpectedPongPayload = json:encode(#{type => ~"pong"}),
    ?assertEqual([{text, ExpectedPongPayload}], Commands).

test_websocket_handle_malformed_json(_Config) ->
    % Test websocket_handle with malformed JSON
    State = arizona_websocket:new_state(self()),

    MalformedJson = ~"{invalid json",

    % Should return error response for malformed JSON
    {Commands, _NewState} = arizona_websocket:websocket_handle({text, MalformedJson}, State),

    ?assertEqual(1, length(Commands)),
    [{text, ErrorPayload}] = Commands,

    % Verify it contains error information
    ErrorBinary = iolist_to_binary(ErrorPayload),
    ?assert(binary:match(ErrorBinary, ~"error") =/= nomatch),
    ?assert(binary:match(ErrorBinary, ~"Internal server error") =/= nomatch).

test_websocket_handle_missing_type(_Config) ->
    % Test websocket_handle with message missing type field
    State = arizona_websocket:new_state(self()),

    MessageWithoutType = iolist_to_binary(
        json:encode(#{
            data => ~"some data"
        })
    ),

    % Should return error response for missing type
    {Commands, _NewState} = arizona_websocket:websocket_handle({text, MessageWithoutType}, State),

    ?assertEqual(1, length(Commands)),
    [{text, ErrorPayload}] = Commands,

    % Verify it contains error information
    ErrorBinary = iolist_to_binary(ErrorPayload),
    ?assert(binary:match(ErrorBinary, ~"error") =/= nomatch),
    ?assert(binary:match(ErrorBinary, ~"Unknown message type") =/= nomatch).

test_websocket_handle_with_real_event(_Config) ->
    % Test websocket_handle with a real LiveView process that can handle events
    {ok, LivePid} = arizona_live:start_link(test_live_component_with_info, arizona_socket:new(#{})),
    State = arizona_websocket:new_state(LivePid),

    % Mount the live process first
    Req = arizona_request:new(#{method => ~"GET", path => ~"/test"}),
    _Socket = arizona_live:mount(LivePid, Req),

    % Create a valid event message
    EventMessage = iolist_to_binary(
        json:encode(#{
            type => ~"event",
            event => ~"increment",
            params => #{}
        })
    ),

    % Should handle the event without crashing
    {Commands, _NewState} = arizona_websocket:websocket_handle({text, EventMessage}, State),

    % Should return commands (either success or error)
    ?assert(is_list(Commands)).

test_websocket_init_with_real_live_process(_Config) ->
    % Test websocket_init with a real LiveView process
    LiveModule = test_live_component_with_info,
    ArizonaReq = arizona_request:new(#{
        method => ~"GET",
        path => ~"/test"
    }),

    % This should work and create a real LiveView process
    {Commands, State} = arizona_websocket:websocket_init({LiveModule, ArizonaReq}),

    % Should return initial render command
    ?assertEqual(1, length(Commands)),
    [{text, InitialPayload}] = Commands,

    % Verify the payload contains the expected content
    PayloadBinary = iolist_to_binary(InitialPayload),
    ?assert(binary:match(PayloadBinary, ~"initial_render") =/= nomatch),
    ?assert(binary:match(PayloadBinary, ~"html") =/= nomatch),

    % Should have a valid live process
    LivePid = arizona_websocket:get_live_pid(State),
    ?assert(is_pid(LivePid)),
    ?assert(is_process_alive(LivePid)).

test_init_with_empty_query_string(_Config) ->
    % Test init with empty query string (should default to "/")
    TestRoutes = [
        {live, ~"/", root_live, #{}},
        {live_websocket, ~"/live/ws"}
    ],

    Dispatch = arizona_server:compile_routes(TestRoutes),
    ok = persistent_term:put(arizona_dispatch, Dispatch),

    % Create WebSocket request without query string
    WebSocketReq = mock_websocket_request_empty(),

    % Should default to root path "/"
    {cowboy_websocket, _Req, {LiveModule, ArizonaReq}} = arizona_websocket:init(WebSocketReq, #{}),

    ?assertEqual(root_live, LiveModule),
    ?assertEqual(~"/", arizona_request:get_path(ArizonaReq)).

test_init_with_missing_path_param(_Config) ->
    % Test init with query string but missing path parameter
    TestRoutes = [
        {live, ~"/", root_live, #{}},
        {live_websocket, ~"/live/ws"}
    ],

    Dispatch = arizona_server:compile_routes(TestRoutes),
    ok = persistent_term:put(arizona_dispatch, Dispatch),

    % Create WebSocket request with other params but no path
    WebSocketReq = mock_websocket_request_with_params(~"other=value"),

    % Should default to root path "/"
    {cowboy_websocket, _Req, {LiveModule, ArizonaReq}} = arizona_websocket:init(WebSocketReq, #{}),

    ?assertEqual(root_live, LiveModule),
    ?assertEqual(~"/", arizona_request:get_path(ArizonaReq)).

%% --------------------------------------------------------------------
%% Test helpers
%% --------------------------------------------------------------------

mock_websocket_request() ->
    mock_websocket_request(#{}).

mock_websocket_request(Opts) ->
    Path = maps:get(path, Opts, ~"path=%2Fusers"),
    QueryString = maps:get(qs, Opts, Path),
    #{
        method => ~"GET",
        path => ~"/live/ws",
        qs => QueryString,
        host => ~"localhost",
        port => 8080,
        scheme => ~"http",
        version => 'HTTP/1.1'
    }.

%% Helper to create WebSocket request for specific live path
mock_websocket_request_for_path(LivePath) ->
    EncodedPath = uri_string:quote(binary_to_list(LivePath)),
    mock_websocket_request(#{path => list_to_binary("path=" ++ EncodedPath)}).

%% Helper to create WebSocket request with no path parameter
mock_websocket_request_empty() ->
    mock_websocket_request(#{qs => ~""}).

%% Helper to create WebSocket request with custom query parameters
mock_websocket_request_with_params(QueryParams) ->
    mock_websocket_request(#{qs => QueryParams}).

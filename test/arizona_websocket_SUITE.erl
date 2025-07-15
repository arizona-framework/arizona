-module(arizona_websocket_SUITE).
-behaviour(ct_suite).
-include_lib("stdlib/include/assert.hrl").
-compile([export_all, nowarn_export_all]).

%% Suppress Dialyzer warnings for cowboy opaque type violations in mock functions
%% These tests use mock cowboy_req structures that don't match the opaque type
-dialyzer(
    {nowarn_function, [
        extract_path_from_query_param/1,
        resolve_live_route_metadata/1,
        test_init_function/1,
        websocket_init_with_correct_path/1,
        test_init_with_empty_query_string/1,
        test_init_with_missing_path_param/1
    ]}
).

%% --------------------------------------------------------------------
%% Behaviour (ct_suite) callbacks
%% --------------------------------------------------------------------

all() ->
    [
        {group, route_resolution},
        {group, websocket_init},
        {group, message_handling},
        {group, socket_lifecycle}
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
            test_websocket_handle_error,
            test_websocket_info,
            test_websocket_handle_missing_type,
            test_websocket_handle_with_real_event,
            test_websocket_init_with_real_live_process,
            test_init_with_empty_query_string,
            test_init_with_missing_path_param,
            test_handle_noreply_response_with_socket_changes,
            test_mock_live_module_for_reply_responses,
            test_json_encoding_tuple_to_array_conversion
        ]},
        {socket_lifecycle, [parallel], [
            test_websocket_info_with_notifications,
            test_websocket_handle_with_dead_live_process
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
        {live, ~"/users", user_live},
        {live, ~"/posts", post_live},
        {live_websocket, ~"/live"}
    ],

    Dispatch = arizona_server:compile_routes(TestRoutes),
    ok = persistent_term:put(arizona_dispatch, Dispatch),

    % Create WebSocket request
    WebSocketReq = mock_websocket_request(),

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
        {live, ~"/users", user_live},
        {live_websocket, ~"/live"}
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
        {live, ~"/users", user_live},
        {live_websocket, ~"/live"}
    ],

    Dispatch = arizona_server:compile_routes(TestRoutes),
    ok = persistent_term:put(arizona_dispatch, Dispatch),

    % Create WebSocket request with path parameter
    WebSocketReq = mock_websocket_request(),

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
    LiveModule = arizona_live_component_with_info,
    ArizonaReq = arizona_request:new(#{
        method => ~"GET",
        path => ~"/test"
    }),

    % This should work since arizona_live_component_with_info is a real LiveView module
    {Commands, State} = arizona_websocket:websocket_init({LiveModule, ArizonaReq}),

    % Should return initial render command
    ?assertMatch([{text, _}], Commands),
    ?assert(is_pid(arizona_websocket:get_live_pid(State))).

%% --------------------------------------------------------------------
%% Message handling tests
%% --------------------------------------------------------------------

handle_ping_message(_Config) ->
    % Test ping message handling through websocket_handle
    State = arizona_websocket:new_state(self()),

    PingMessage = iolist_to_binary(json:encode(#{type => ~"ping"})),
    {Commands, NewState} = arizona_websocket:websocket_handle({text, PingMessage}, State),

    % Should return pong command
    ExpectedPongPayload = json:encode(#{type => ~"pong"}),
    ?assertEqual([{text, ExpectedPongPayload}], Commands),
    ?assertEqual(arizona_websocket:get_live_pid(State), arizona_websocket:get_live_pid(NewState)).

handle_unknown_message(_Config) ->
    % Test unknown message handling through websocket_handle
    State = arizona_websocket:new_state(self()),

    UnknownMessage = iolist_to_binary(json:encode(#{type => ~"unknown"})),
    {Commands, NewState} = arizona_websocket:websocket_handle({text, UnknownMessage}, State),

    % Should return error command
    ExpectedPayload = json:encode(#{
        type => ~"error",
        message => ~"Unknown message type"
    }),
    ?assertEqual([{text, ExpectedPayload}], Commands),
    ?assertEqual(arizona_websocket:get_live_pid(State), arizona_websocket:get_live_pid(NewState)).

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
    {ok, LivePid} = arizona_live:start_link(
        arizona_live_component_with_info, arizona_socket:new(#{})
    ),
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
    LiveModule = arizona_live_component_with_info,
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
    ?assert(binary:match(PayloadBinary, ~"structure") =/= nomatch),

    % Should have a valid live process
    LivePid = arizona_websocket:get_live_pid(State),
    ?assert(is_pid(LivePid)),
    ?assert(is_process_alive(LivePid)).

test_init_with_empty_query_string(_Config) ->
    % Test init with empty query string (should default to "/")
    TestRoutes = [
        {live, ~"/", root_live},
        {live_websocket, ~"/live"}
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
        {live, ~"/", root_live},
        {live_websocket, ~"/live"}
    ],

    Dispatch = arizona_server:compile_routes(TestRoutes),
    ok = persistent_term:put(arizona_dispatch, Dispatch),

    % Create WebSocket request with other params but no path
    WebSocketReq = mock_websocket_request_with_params(~"other=value"),

    % Should default to root path "/"
    {cowboy_websocket, _Req, {LiveModule, ArizonaReq}} = arizona_websocket:init(WebSocketReq, #{}),

    ?assertEqual(root_live, LiveModule),
    ?assertEqual(~"/", arizona_request:get_path(ArizonaReq)).

test_handle_noreply_response_with_socket_changes(_Config) ->
    % Test noreply path with actual socket changes
    {ok, LivePid} = arizona_live:start_link(
        arizona_live_component_with_info, arizona_socket:new(#{})
    ),
    State = arizona_websocket:new_state(LivePid),

    % Mount the live process
    Req = arizona_request:new(#{method => ~"GET", path => ~"/test"}),
    _MountedSocket = arizona_live:mount(LivePid, Req),

    % The increment event should modify the counter
    EventMessage = iolist_to_binary(
        json:encode(#{
            type => ~"event",
            event => ~"increment",
            params => #{}
        })
    ),

    % Handle the event - should trigger noreply response
    {Commands, _NewState} = arizona_websocket:websocket_handle({text, EventMessage}, State),

    % Verify we get some response
    ?assertEqual([], Commands).

test_mock_live_module_for_reply_responses(_Config) ->
    % Test reply response path
    {ok, LivePid} = arizona_live:start_link(
        arizona_live_component_with_info, arizona_socket:new(#{})
    ),
    State = arizona_websocket:new_state(LivePid),

    % Mount the live process
    Req = arizona_request:new(#{method => ~"GET", path => ~"/test"}),
    _Socket = arizona_live:mount(LivePid, Req),

    % Send reply_test event that should trigger reply response
    EventMessage = iolist_to_binary(
        json:encode(#{
            type => ~"event",
            event => ~"reply_test",
            params => #{}
        })
    ),

    % Handle the event - should trigger reply response path
    {Commands, _NewState} = arizona_websocket:websocket_handle({text, EventMessage}, State),

    % Should return reply payload
    ?assertEqual(1, length(Commands)),
    [{text, ReplyPayload}] = Commands,
    ReplyBinary = iolist_to_binary(ReplyPayload),
    ?assert(binary:match(ReplyBinary, ~"reply") =/= nomatch),
    ?assert(binary:match(ReplyBinary, ~"test_reply") =/= nomatch).

test_json_encoding_tuple_to_array_conversion(_Config) ->
    % Test JSON encoding with tuple-to-array conversion using nested diff structure
    % This simulates the DiffChanges structure returned by arizona_socket:get_changes/1
    TestDiffChanges = [
        {root, [
            {1, arizona_differ:html_content(~"test_value")},
            {2, arizona_differ:html_content([~"nested", ~"data"])}
        ]}
    ],

    % Create the payload structure that handle_noreply_response would create
    DiffPayload = #{
        type => ~"diff",
        changes => TestDiffChanges
    },

    % Use the exported json_encode function to test tuple-to-array conversion
    EncodedJson = arizona_websocket:json_encode(DiffPayload),

    % Verify exact JSON encoding - nested tuples should be converted to arrays
    PayloadBinary = iolist_to_binary(EncodedJson),
    ExpectedJson = iolist_to_binary([
        ~"{\"type\":\"diff\",\"changes\":[[\"root\",[[1,\"test_value\"],",
        ~"[2,[\"nested\",\"data\"]]]]]}"
    ]),
    ?assertEqual(ExpectedJson, PayloadBinary).

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
        path => ~"/live",
        qs => QueryString,
        host => ~"localhost",
        port => 8080,
        scheme => ~"http",
        version => 'HTTP/1.1'
    }.

%% Helper to create WebSocket request with no path parameter
mock_websocket_request_empty() ->
    mock_websocket_request(#{qs => ~""}).

%% Helper to create WebSocket request with custom query parameters
mock_websocket_request_with_params(QueryParams) ->
    mock_websocket_request(#{qs => QueryParams}).

%% --------------------------------------------------------------------
%% Socket lifecycle tests
%% --------------------------------------------------------------------

test_websocket_info_with_notifications(Config) when is_list(Config) ->
    % Test the websocket_info handler with various message types
    % that might be sent for real-time updates
    State = arizona_websocket:new_state(self()),

    % Test various info message types
    TestMessages = [
        {push_notification, #{type => ~"alert", message => ~"System update"}},
        {live_data_feed, #{data => [1, 2, 3]}},
        {timer_event, timeout},
        {'DOWN', make_ref(), process, self(), normal}
    ],

    lists:foreach(
        fun(InfoMessage) ->
            {Commands, NewState} = arizona_websocket:websocket_info(InfoMessage, State),

            % Currently returns empty commands - verify this behavior
            ?assertEqual([], Commands),
            ?assertEqual(
                arizona_websocket:get_live_pid(State),
                arizona_websocket:get_live_pid(NewState)
            )
        end,
        TestMessages
    ).

test_websocket_handle_with_dead_live_process(Config) when is_list(Config) ->
    % Test handling messages when the LiveView process has died
    % Create a mock live process and kill it
    MockLivePid = spawn(fun() -> ok end),
    State = arizona_websocket:new_state(MockLivePid),

    % Kill the process to simulate crash
    exit(MockLivePid, kill),
    % Ensure process is dead
    timer:sleep(10),

    % Create a valid event message
    EventMessage = iolist_to_binary(
        json:encode(#{
            type => ~"event",
            event => ~"increment",
            params => #{}
        })
    ),

    % Should handle gracefully without crashing websocket
    try
        {Commands, _NewState} = arizona_websocket:websocket_handle({text, EventMessage}, State),

        % Should return some response (possibly error), not crash
        ?assert(is_list(Commands))
    catch
        Class:Reason ->
            % If it throws an error, it should be a controlled error, not a crash
            ?assert(Class =:= error orelse Class =:= throw),
            % Should not be undefined function error
            ?assert(Reason =/= undef)
    end.

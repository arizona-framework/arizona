-module(arizona_mcp_e2e_SUITE).
-include_lib("stdlib/include/assert.hrl").
-include_lib("common_test/include/ct.hrl").

-export([all/0, init_per_suite/1, end_per_suite/1]).
-export([
    post_initialize/1,
    post_tools_list/1,
    post_tools_call/1,
    post_streaming_progress/1,
    post_progress_without_accept_buffered/1,
    post_tools_list_paginates/1,
    post_resources_read/1,
    post_resources_templates_list/1,
    post_completion_complete/1,
    post_prompts_get/1,
    get_returns_405/1,
    post_disallowed_origin_403/1,
    session_initialize_returns_id/1,
    session_request_uses_session/1,
    session_delete/1,
    session_unknown_id_404/1,
    session_missing_id/1,
    session_delete_without_id/1,
    session_tool_crash_survives/1,
    session_get_without_id_400/1,
    session_channel_receives_push/1,
    session_resource_subscribe_pushes_update/1,
    session_set_level_filters_log/1,
    session_second_channel_409/1,
    session_survives_channel_disconnect/1,
    session_resumes_with_last_event_id/1,
    session_streaming_progress/1,
    auth_missing_token_401/1,
    auth_valid_token_ok/1,
    session_init_crash_internal_error/1
]).

-define(LISTENER, arizona_mcp_e2e).
-define(ALLOWED_ORIGIN, ~"http://localhost:3000").

all() ->
    [
        post_initialize,
        post_tools_list,
        post_tools_call,
        post_streaming_progress,
        post_progress_without_accept_buffered,
        post_tools_list_paginates,
        post_resources_read,
        post_resources_templates_list,
        post_completion_complete,
        post_prompts_get,
        get_returns_405,
        post_disallowed_origin_403,
        session_initialize_returns_id,
        session_request_uses_session,
        session_delete,
        session_unknown_id_404,
        session_missing_id,
        session_delete_without_id,
        session_tool_crash_survives,
        session_get_without_id_400,
        session_channel_receives_push,
        session_resource_subscribe_pushes_update,
        session_set_level_filters_log,
        session_second_channel_409,
        session_survives_channel_disconnect,
        session_resumes_with_last_event_id,
        session_streaming_progress,
        auth_missing_token_401,
        auth_valid_token_ok,
        session_init_crash_internal_error
    ].

init_per_suite(Config) ->
    {ok, _} = application:ensure_all_started(arizona),
    {ok, _} = application:ensure_all_started(roadrunner),
    Port = 15050 + erlang:unique_integer([positive, monotonic]) rem 1000,
    Auth = fun(Req) ->
        case roadrunner_req:header(~"authorization", Req) of
            ~"Bearer let-me-in" -> ok;
            _ -> {reject, 401}
        end
    end,
    Routes = [
        {mcp, ~"/mcp", arizona_mcp_test_server, #{origins => [?ALLOWED_ORIGIN]}},
        {mcp, ~"/mcp-session", arizona_mcp_test_server, #{
            origins => [?ALLOWED_ORIGIN],
            sessions => true
        }},
        {mcp, ~"/mcp-auth", arizona_mcp_test_server, #{
            origins => [?ALLOWED_ORIGIN],
            auth => Auth
        }},
        {mcp, ~"/mcp-session-crash", arizona_mcp_crashing_server, #{
            origins => [?ALLOWED_ORIGIN],
            sessions => true
        }},
        {mcp, ~"/mcp-paged", arizona_mcp_test_server, #{
            origins => [?ALLOWED_ORIGIN],
            page_size => 2
        }}
    ],
    {ok, _} = arizona_roadrunner_server:start(?LISTENER, #{
        transport_opts => [{port, Port}],
        routes => Routes
    }),
    [{port, Port} | Config].

end_per_suite(_Config) ->
    _ = arizona_roadrunner_server:stop(?LISTENER),
    _ = application:stop(arizona),
    ok.

%% --------------------------------------------------------------------
%% Tests
%% --------------------------------------------------------------------

post_initialize(Config) ->
    Body =
        ~"""
    {"jsonrpc":"2.0","id":1,"method":"initialize",
     "params":{"protocolVersion":"2025-06-18","capabilities":{},
               "clientInfo":{"name":"ct","version":"1"}}}
    """,
    Resp = post(Config, "/mcp", [], Body),
    ?assertEqual(200, status_code(Resp)),
    ?assertNotEqual(nomatch, binary:match(headers(Resp), ~"application/json")),
    #{~"result" := Result} = body_json(Resp),
    ?assertEqual(~"2025-06-18", maps:get(~"protocolVersion", Result)),
    ?assertEqual(
        #{~"name" => ~"arizona_test", ~"version" => ~"0.1.0"},
        maps:get(~"serverInfo", Result)
    ).

post_tools_list(Config) ->
    Body =
        ~"""
    {"jsonrpc":"2.0","id":2,"method":"tools/list","params":{}}
    """,
    Resp = post(Config, "/mcp", [], Body),
    ?assertEqual(200, status_code(Resp)),
    #{~"result" := #{~"tools" := Tools}} = body_json(Resp),
    Names = [maps:get(~"name", T) || T <- Tools],
    ?assert(lists:member(~"add", Names)),
    [Add | _] = Tools,
    ?assert(maps:is_key(~"inputSchema", Add)).

post_tools_call(Config) ->
    Body =
        ~"""
    {"jsonrpc":"2.0","id":3,"method":"tools/call",
     "params":{"name":"add","arguments":{"a":2,"b":3}}}
    """,
    Resp = post(Config, "/mcp", [], Body),
    ?assertEqual(200, status_code(Resp)),
    #{~"result" := Result} = body_json(Resp),
    ?assertEqual(
        #{~"content" => [#{~"type" => ~"text", ~"text" => ~"5"}], ~"isError" => false},
        Result
    ).

post_streaming_progress(Config) ->
    %% A token-bearing tools/call answers its POST as an SSE stream: the
    %% progress notifications (carrying the client's token) arrive before the
    %% result. Stateless mode runs the tool in a conn-side worker.
    Body =
        ~"""
    {"jsonrpc":"2.0","id":8,"method":"tools/call",
     "params":{"name":"progress","arguments":{},"_meta":{"progressToken":"tok-1"}}}
    """,
    Data = post_stream_until(Config, "/mcp", [], Body, ~"\"done\""),
    ?assertNotEqual(nomatch, binary:match(Data, ~"text/event-stream")),
    ?assertNotEqual(nomatch, binary:match(Data, ~"notifications/progress")),
    ?assertNotEqual(nomatch, binary:match(Data, ~"\"progressToken\":\"tok-1\"")),
    ?assertNotEqual(nomatch, binary:match(Data, ~"step 1")),
    ?assertNotEqual(nomatch, binary:match(Data, ~"step 2")),
    %% The progress events precede the final result on the wire.
    {Step1, _} = binary:match(Data, ~"step 1"),
    {Done, _} = binary:match(Data, ~"\"done\""),
    ?assert(Step1 < Done).

post_progress_without_accept_buffered(Config) ->
    %% A token-bearing tools/call from a client that does NOT accept SSE (no
    %% `Accept: text/event-stream`) falls back to a buffered JSON reply: the tool
    %% still runs, the progress notifications are simply dropped.
    Body =
        ~"""
    {"jsonrpc":"2.0","id":10,"method":"tools/call",
     "params":{"name":"progress","arguments":{},"_meta":{"progressToken":"tok-2"}}}
    """,
    Resp = post(Config, "/mcp", [], Body),
    ?assertEqual(200, status_code(Resp)),
    ?assertNotEqual(nomatch, binary:match(headers(Resp), ~"application/json")),
    #{~"result" := Result} = body_json(Resp),
    ?assertEqual(
        #{~"content" => [#{~"type" => ~"text", ~"text" => ~"done"}], ~"isError" => false},
        Result
    ).

post_tools_list_paginates(Config) ->
    %% On the page_size=2 route, the first tools/list page carries 2 tools and a
    %% nextCursor; following the cursor returns the next, different, page.
    Page1 =
        ~"""
    {"jsonrpc":"2.0","id":11,"method":"tools/list","params":{}}
    """,
    Resp1 = post(Config, "/mcp-paged", [], Page1),
    ?assertEqual(200, status_code(Resp1)),
    #{~"result" := #{~"tools" := Tools1, ~"nextCursor" := Cursor}} = body_json(Resp1),
    ?assertEqual(2, length(Tools1)),
    %% Follow the cursor for page 2.
    Page2 = json:encode(#{
        jsonrpc => ~"2.0",
        id => 12,
        method => ~"tools/list",
        params => #{cursor => Cursor}
    }),
    Resp2 = post(Config, "/mcp-paged", [], iolist_to_binary(Page2)),
    ?assertEqual(200, status_code(Resp2)),
    #{~"result" := #{~"tools" := Tools2}} = body_json(Resp2),
    ?assertEqual(2, length(Tools2)),
    Names1 = [maps:get(~"name", T) || T <- Tools1],
    Names2 = [maps:get(~"name", T) || T <- Tools2],
    ?assertNotEqual(Names1, Names2).

post_resources_read(Config) ->
    Body =
        ~"""
    {"jsonrpc":"2.0","id":5,"method":"resources/read",
     "params":{"uri":"mem://greeting"}}
    """,
    Resp = post(Config, "/mcp", [], Body),
    ?assertEqual(200, status_code(Resp)),
    #{~"result" := #{~"contents" := [Content]}} = body_json(Resp),
    ?assertEqual(~"mem://greeting", maps:get(~"uri", Content)),
    ?assertEqual(~"hello", maps:get(~"text", Content)).

post_resources_templates_list(Config) ->
    Body =
        ~"""
    {"jsonrpc":"2.0","id":13,"method":"resources/templates/list","params":{}}
    """,
    Resp = post(Config, "/mcp", [], Body),
    ?assertEqual(200, status_code(Resp)),
    #{~"result" := #{~"resourceTemplates" := [Template]}} = body_json(Resp),
    ?assertEqual(~"mem://user/{id}", maps:get(~"uriTemplate", Template)),
    ?assertEqual(~"user", maps:get(~"name", Template)).

post_completion_complete(Config) ->
    Body =
        ~"""
    {"jsonrpc":"2.0","id":16,"method":"completion/complete",
     "params":{"ref":{"type":"ref/prompt","name":"greet"},
               "argument":{"name":"who","value":"Ad"}}}
    """,
    Resp = post(Config, "/mcp", [], Body),
    ?assertEqual(200, status_code(Resp)),
    #{~"result" := #{~"completion" := #{~"values" := Values}}} = body_json(Resp),
    ?assertEqual([~"Ada", ~"Adam"], Values).

post_prompts_get(Config) ->
    Body =
        ~"""
    {"jsonrpc":"2.0","id":6,"method":"prompts/get",
     "params":{"name":"greet","arguments":{"who":"Ada"}}}
    """,
    Resp = post(Config, "/mcp", [], Body),
    ?assertEqual(200, status_code(Resp)),
    #{~"result" := #{~"messages" := [Message]}} = body_json(Resp),
    ?assertEqual(~"user", maps:get(~"role", Message)),
    #{~"content" := #{~"text" := Text}} = Message,
    ?assertEqual(~"Hello, Ada", Text).

get_returns_405(Config) ->
    Resp = request(Config, "GET", "/mcp", [], <<>>),
    ?assertEqual(405, status_code(Resp)),
    ?assertNotEqual(nomatch, binary:match(string:lowercase(headers(Resp)), ~"allow: post")).

post_disallowed_origin_403(Config) ->
    Body =
        ~"""
    {"jsonrpc":"2.0","id":4,"method":"ping"}
    """,
    Resp = post(Config, "/mcp", ["Origin: http://evil.example\r\n"], Body),
    ?assertEqual(403, status_code(Resp)).

%% --------------------------------------------------------------------
%% Session-mode tests (/mcp-session, sessions => true)
%% --------------------------------------------------------------------

session_initialize_returns_id(Config) ->
    Resp = post(Config, "/mcp-session", [], initialize_body()),
    ?assertEqual(200, status_code(Resp)),
    ?assert(byte_size(header_value(Resp, ~"mcp-session-id")) > 0).

session_request_uses_session(Config) ->
    SessionId = open_session(Config),
    Body =
        ~"""
    {"jsonrpc":"2.0","id":2,"method":"tools/list","params":{}}
    """,
    Resp = post(Config, "/mcp-session", session_header(SessionId), Body),
    ?assertEqual(200, status_code(Resp)),
    #{~"result" := #{~"tools" := Tools}} = body_json(Resp),
    ?assert(lists:member(~"add", [maps:get(~"name", T) || T <- Tools])).

session_delete(Config) ->
    SessionId = open_session(Config),
    Deleted = request(Config, "DELETE", "/mcp-session", session_header(SessionId), <<>>),
    ?assertEqual(204, status_code(Deleted)),
    %% After teardown the session is gone -> 404.
    Body =
        ~"""
    {"jsonrpc":"2.0","id":3,"method":"ping"}
    """,
    Resp = post(Config, "/mcp-session", session_header(SessionId), Body),
    ?assertEqual(404, status_code(Resp)).

session_unknown_id_404(Config) ->
    Body =
        ~"""
    {"jsonrpc":"2.0","id":4,"method":"ping"}
    """,
    Resp = post(Config, "/mcp-session", session_header(~"deadbeef"), Body),
    ?assertEqual(404, status_code(Resp)).

session_missing_id(Config) ->
    Body =
        ~"""
    {"jsonrpc":"2.0","id":2,"method":"tools/list","params":{}}
    """,
    Resp = post(Config, "/mcp-session", [], Body),
    ?assertEqual(200, status_code(Resp)),
    ?assertMatch(#{~"error" := #{~"code" := -32600}}, body_json(Resp)).

session_delete_without_id(Config) ->
    Resp = request(Config, "DELETE", "/mcp-session", [], <<>>),
    ?assertEqual(400, status_code(Resp)).

session_tool_crash_survives(Config) ->
    SessionId = open_session(Config),
    Crash =
        ~"""
    {"jsonrpc":"2.0","id":2,"method":"tools/call",
     "params":{"name":"crash","arguments":{}}}
    """,
    Crashed = post(Config, "/mcp-session", session_header(SessionId), Crash),
    ?assertEqual(200, status_code(Crashed)),
    ?assertMatch(#{~"error" := #{~"code" := -32603}}, body_json(Crashed)),
    %% The session survived the crashing tool and still serves.
    Ping =
        ~"""
    {"jsonrpc":"2.0","id":3,"method":"ping"}
    """,
    Resp = post(Config, "/mcp-session", session_header(SessionId), Ping),
    ?assertEqual(200, status_code(Resp)),
    ?assertMatch(#{~"result" := #{}}, body_json(Resp)).

session_get_without_id_400(Config) ->
    Resp = request(Config, "GET", "/mcp-session", [], <<>>),
    ?assertEqual(400, status_code(Resp)).

session_channel_receives_push(Config) ->
    SessionId = open_session(Config),
    {ok, Sock} = open_channel(Config, SessionId),
    %% Push from the server side (in-node) and read it off the SSE stream.
    ok = arizona_mcp:notify(SessionId, ~"notifications/message", #{~"text" => ~"hello-sse"}),
    Data = recv_until(Sock, ~"hello-sse", 5000),
    gen_tcp:close(Sock),
    ?assertNotEqual(nomatch, binary:match(Data, ~"notifications/message")),
    ?assertNotEqual(nomatch, binary:match(Data, ~"hello-sse")).

session_resource_subscribe_pushes_update(Config) ->
    SessionId = open_session(Config),
    {ok, Sock} = open_channel(Config, SessionId),
    %% Subscribe (the POST is served by the session process, which joins the
    %% per-uri channel), then announce the change server-side (in-node).
    SubBody =
        ~"""
    {"jsonrpc":"2.0","id":14,"method":"resources/subscribe",
     "params":{"uri":"mem://greeting"}}
    """,
    SubResp = post(Config, "/mcp-session", session_header(SessionId), SubBody),
    ?assertEqual(200, status_code(SubResp)),
    ok = arizona_mcp:resource_updated(~"mem://greeting"),
    Data = recv_until(Sock, ~"resources/updated", 5000),
    gen_tcp:close(Sock),
    ?assertNotEqual(nomatch, binary:match(Data, ~"notifications/resources/updated")),
    ?assertNotEqual(nomatch, binary:match(Data, ~"mem://greeting")).

session_set_level_filters_log(Config) ->
    SessionId = open_session(Config),
    {ok, Sock} = open_channel(Config, SessionId),
    %% Raise the threshold to warning via the wire, then log info (dropped) and
    %% error (delivered) in-node.
    SetBody =
        ~"""
    {"jsonrpc":"2.0","id":17,"method":"logging/setLevel","params":{"level":"warning"}}
    """,
    SetResp = post(Config, "/mcp-session", session_header(SessionId), SetBody),
    ?assertEqual(200, status_code(SetResp)),
    ok = arizona_mcp:log(SessionId, info, ~"info-drop"),
    ok = arizona_mcp:log(SessionId, error, ~"error-send"),
    Data = recv_until(Sock, ~"error-send", 5000),
    gen_tcp:close(Sock),
    ?assertNotEqual(nomatch, binary:match(Data, ~"notifications/message")),
    ?assertNotEqual(nomatch, binary:match(Data, ~"error-send")),
    ?assertEqual(nomatch, binary:match(Data, ~"info-drop")).

session_second_channel_409(Config) ->
    SessionId = open_session(Config),
    {ok, Sock} = open_channel(Config, SessionId),
    %% A second GET for the same session is rejected: one stream per session.
    Resp = request(Config, "GET", "/mcp-session", session_header(SessionId), <<>>),
    gen_tcp:close(Sock),
    ?assertEqual(409, status_code(Resp)).

session_survives_channel_disconnect(Config) ->
    SessionId = open_session(Config),
    {ok, Sock} = open_channel(Config, SessionId),
    gen_tcp:close(Sock),
    %% The session outlives the dropped channel and still serves requests.
    Body =
        ~"""
    {"jsonrpc":"2.0","id":7,"method":"ping"}
    """,
    Resp = post(Config, "/mcp-session", session_header(SessionId), Body),
    ?assertEqual(200, status_code(Resp)),
    ?assertMatch(#{~"result" := #{}}, body_json(Resp)).

session_resumes_with_last_event_id(Config) ->
    SessionId = open_session(Config),
    %% Two events emitted while no channel is attached: they buffer.
    ok = arizona_mcp:notify(SessionId, ~"notifications/message", #{~"seq" => 1}),
    ok = arizona_mcp:notify(SessionId, ~"notifications/message", #{~"seq" => 2}),
    %% Reconnect resuming from event 1: event 2 replays over the new stream.
    %% Read the whole response (the replay can arrive bundled with the head).
    Sock = send_channel_get(Config, SessionId, ["Last-Event-ID: 1\r\n"]),
    Data = recv_until(Sock, ~"\"seq\":2", 5000),
    gen_tcp:close(Sock),
    ?assertNotEqual(nomatch, binary:match(Data, ~"\"seq\":2")).

session_streaming_progress(Config) ->
    %% Same streaming contract in session mode -- the session runs the tool and
    %% relays progress + result to the POST's stream. Uses an integer token.
    SessionId = open_session(Config),
    Body =
        ~"""
    {"jsonrpc":"2.0","id":9,"method":"tools/call",
     "params":{"name":"progress","arguments":{},"_meta":{"progressToken":42}}}
    """,
    Data = post_stream_until(Config, "/mcp-session", session_header(SessionId), Body, ~"\"done\""),
    ?assertNotEqual(nomatch, binary:match(Data, ~"text/event-stream")),
    ?assertNotEqual(nomatch, binary:match(Data, ~"notifications/progress")),
    ?assertNotEqual(nomatch, binary:match(Data, ~"\"progressToken\":42")),
    ?assertNotEqual(nomatch, binary:match(Data, ~"\"done\"")).

%% --------------------------------------------------------------------
%% Auth-gated tests (/mcp-auth, auth => bearer hook)
%% --------------------------------------------------------------------

auth_missing_token_401(Config) ->
    Resp = post(Config, "/mcp-auth", [], initialize_body()),
    ?assertEqual(401, status_code(Resp)).

auth_valid_token_ok(Config) ->
    Headers = ["Authorization: Bearer let-me-in\r\n"],
    Resp = post(Config, "/mcp-auth", Headers, initialize_body()),
    ?assertEqual(200, status_code(Resp)).

%% A crashing init/1 on the session path is guarded: -32603, not a dead conn.
session_init_crash_internal_error(Config) ->
    Resp = post(Config, "/mcp-session-crash", [], initialize_body()),
    ?assertEqual(200, status_code(Resp)),
    ?assertMatch(#{~"error" := #{~"code" := -32603}}, body_json(Resp)).

%% --------------------------------------------------------------------
%% Helpers
%% --------------------------------------------------------------------

open_session(Config) ->
    Resp = post(Config, "/mcp-session", [], initialize_body()),
    header_value(Resp, ~"mcp-session-id").

open_channel(Config, SessionId) ->
    open_channel(Config, SessionId, []).

%% Open the server-to-client SSE channel and confirm the loop response opened
%% (a 200 header block means the channel attached). The socket stays open.
%% (Reads only the header block; reply data follows on later recvs.)
open_channel(Config, SessionId, ExtraHeaders) ->
    Sock = send_channel_get(Config, SessionId, ExtraHeaders),
    Head = recv_until(Sock, ~"\r\n\r\n", 5000),
    ?assertNotEqual(nomatch, binary:match(Head, ~"200 OK")),
    {ok, Sock}.

%% Connect and send the channel GET without reading the response -- the
%% caller reads the whole stream (used when replay events may arrive bundled
%% with the header block).
send_channel_get(Config, SessionId, ExtraHeaders) ->
    Port = ?config(port, Config),
    {ok, Sock} = gen_tcp:connect("localhost", Port, [binary, {active, false}]),
    Req = [
        "GET /mcp-session HTTP/1.1\r\n",
        "Host: localhost:",
        integer_to_list(Port),
        "\r\n",
        "Mcp-Session-Id: ",
        SessionId,
        "\r\n",
        "Accept: text/event-stream\r\n",
        ExtraHeaders,
        "\r\n"
    ],
    ok = gen_tcp:send(Sock, Req),
    Sock.

%% Send a POST and read its streamed (SSE) response until `Needle` appears. The
%% response has no Content-Length (it is chunked), so the buffered `recv_response`
%% does not apply.
post_stream_until(Config, Path, ExtraHeaders, Body, Needle) ->
    Port = ?config(port, Config),
    {ok, Sock} = gen_tcp:connect("localhost", Port, [binary, {active, false}]),
    Req = [
        "POST ",
        Path,
        " HTTP/1.1\r\n",
        "Host: localhost:",
        integer_to_list(Port),
        "\r\n",
        "Content-Type: application/json\r\n",
        %% The Streamable HTTP precondition for an SSE response.
        "Accept: application/json, text/event-stream\r\n",
        "Content-Length: ",
        integer_to_list(byte_size(Body)),
        "\r\n",
        ExtraHeaders,
        "\r\n",
        Body
    ],
    ok = gen_tcp:send(Sock, Req),
    Data = recv_until(Sock, Needle, 5000),
    gen_tcp:close(Sock),
    Data.

%% Accumulate socket data until `Needle` appears or time runs out.
recv_until(Sock, Needle, Timeout) ->
    recv_until(Sock, Needle, Timeout, <<>>).

recv_until(_Sock, _Needle, Timeout, Acc) when Timeout =< 0 ->
    Acc;
recv_until(Sock, Needle, Timeout, Acc) ->
    T0 = erlang:monotonic_time(millisecond),
    case gen_tcp:recv(Sock, 0, min(Timeout, 500)) of
        {ok, Chunk} ->
            Acc1 = <<Acc/binary, Chunk/binary>>,
            case binary:match(Acc1, Needle) of
                nomatch ->
                    Elapsed = erlang:monotonic_time(millisecond) - T0,
                    recv_until(Sock, Needle, Timeout - Elapsed, Acc1);
                _ ->
                    Acc1
            end;
        {error, timeout} ->
            Acc
    end.

session_header(SessionId) ->
    [<<"Mcp-Session-Id: ">>, SessionId, <<"\r\n">>].

initialize_body() ->
    ~"""
    {"jsonrpc":"2.0","id":1,"method":"initialize",
     "params":{"protocolVersion":"2025-06-18","capabilities":{}}}
    """.

%% Extract a response header value (case-insensitive on the name).
header_value(Resp, Name) ->
    Pattern = <<"(?i)", Name/binary, ":\\s*([^\r\n]+)">>,
    {match, [Value]} = re:run(headers(Resp), Pattern, [{capture, [1], binary}]),
    Value.

post(Config, Path, ExtraHeaders, Body) ->
    request(Config, "POST", Path, ExtraHeaders, Body).

request(Config, Method, Path, ExtraHeaders, Body) ->
    Port = ?config(port, Config),
    {ok, Sock} = gen_tcp:connect("localhost", Port, [binary, {active, false}]),
    Req = [
        Method,
        " ",
        Path,
        " HTTP/1.1\r\n",
        "Host: localhost:",
        integer_to_list(Port),
        "\r\n",
        "Content-Type: application/json\r\n",
        "Content-Length: ",
        integer_to_list(byte_size(Body)),
        "\r\n",
        ExtraHeaders,
        "\r\n",
        Body
    ],
    ok = gen_tcp:send(Sock, Req),
    Resp = recv_response(Sock, <<>>),
    gen_tcp:close(Sock),
    Resp.

%% Read until the full Content-Length body has arrived (responses are
%% buffered, so they always carry one), or the socket goes quiet.
recv_response(Sock, Acc) ->
    case complete(Acc) of
        true ->
            Acc;
        false ->
            case gen_tcp:recv(Sock, 0, 5000) of
                {ok, Chunk} -> recv_response(Sock, <<Acc/binary, Chunk/binary>>);
                {error, _} -> Acc
            end
    end.

complete(Resp) ->
    case binary:split(Resp, ~"\r\n\r\n") of
        [HeadersPart, Body] ->
            case content_length(HeadersPart) of
                undefined -> true;
                Len -> byte_size(Body) >= Len
            end;
        _ ->
            false
    end.

content_length(HeadersPart) ->
    Lower = string:lowercase(HeadersPart),
    case re:run(Lower, ~"content-length:\\s*(\\d+)", [{capture, [1], binary}]) of
        {match, [Bin]} -> binary_to_integer(Bin);
        nomatch -> undefined
    end.

status_code(Resp) ->
    [StatusLine | _] = binary:split(Resp, ~"\r\n"),
    [_Version, CodeBin | _] = binary:split(StatusLine, ~" ", [global]),
    binary_to_integer(CodeBin).

headers(Resp) ->
    [HeadersPart, _Body] = binary:split(Resp, ~"\r\n\r\n"),
    HeadersPart.

body_json(Resp) ->
    [_HeadersPart, Body] = binary:split(Resp, ~"\r\n\r\n"),
    json:decode(Body).

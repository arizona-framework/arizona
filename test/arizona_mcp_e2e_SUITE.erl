-module(arizona_mcp_e2e_SUITE).
-include_lib("stdlib/include/assert.hrl").
-include_lib("common_test/include/ct.hrl").

-export([all/0, init_per_suite/1, end_per_suite/1]).
-export([
    post_initialize/1,
    post_tools_list/1,
    post_tools_call/1,
    post_resources_read/1,
    post_prompts_get/1,
    get_returns_405/1,
    post_disallowed_origin_403/1
]).

-define(LISTENER, arizona_mcp_e2e).
-define(ALLOWED_ORIGIN, ~"http://localhost:3000").

all() ->
    [
        post_initialize,
        post_tools_list,
        post_tools_call,
        post_resources_read,
        post_prompts_get,
        get_returns_405,
        post_disallowed_origin_403
    ].

init_per_suite(Config) ->
    {ok, _} = application:ensure_all_started(arizona),
    {ok, _} = application:ensure_all_started(roadrunner),
    Port = 15050 + erlang:unique_integer([positive, monotonic]) rem 1000,
    Routes = [
        {mcp, ~"/mcp", arizona_mcp_test_server, #{origins => [?ALLOWED_ORIGIN]}}
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
%% Helpers
%% --------------------------------------------------------------------

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

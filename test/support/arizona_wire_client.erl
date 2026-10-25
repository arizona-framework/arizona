-module(arizona_wire_client).
-moduledoc """
**TEST SUPPORT.** A raw HTTP/1.1 and WebSocket client over `gen_tcp`, for suites
that must assert wire behaviour only (no calls into a server adapter).

`request/5` sends one request on a fresh connection and reads exactly one
response, framed by `content-length`, `transfer-encoding: chunked`, or the peer
closing. Header names come back lowercased, in wire order, duplicates kept (so
every `set-cookie` line is visible).

The WebSocket half connects to the `/ws` endpoint with `_az_path=<Path>`.
Frames are decoded off a per-socket buffer: a server may batch several frames
into one write, and a read may hold only part of one, so the undecoded tail is
kept for the next `ws_recv/1,2` instead of being dropped.
""".

-export([request/5]).
-export([request_head/4]).
-export([header/2]).
-export([headers/2]).
-export([ws_connect/3]).
-export([ws_upgrade_head/3]).
-export([ws_send_json/2]).
-export([ws_recv/1]).
-export([ws_recv/2]).
-export([ws_close/1]).

-define(TIMEOUT, 5000).

-type headers() :: [{binary(), binary()}].
-type ws_opts() :: #{
    %% Extra request headers (e.g. `Cookie`, `Origin`).
    headers => headers(),
    %% Extra raw query-string pairs appended after `_az_path`.
    query => [{binary(), binary()}]
}.
-type frame() ::
    {text, binary()}
    | {binary, binary()}
    | {unknown, binary()}
    | {close, non_neg_integer(), binary()}.

-export_type([headers/0]).
-export_type([ws_opts/0]).
-export_type([frame/0]).

%% --------------------------------------------------------------------
%% HTTP
%% --------------------------------------------------------------------

-spec request(Port, Method, Path, Headers, Body) -> {Status, headers(), binary()} when
    Port :: inet:port_number(),
    Method :: binary(),
    Path :: binary(),
    Headers :: headers(),
    Body :: iodata(),
    Status :: 100..599.
request(Port, Method, Path, Headers0, Body) ->
    {ok, Sock} = gen_tcp:connect("localhost", Port, [binary, {active, false}, {packet, raw}]),
    try
        Headers = default_headers(Port, Method, Headers0, iolist_size(Body)),
        ok = gen_tcp:send(Sock, [
            Method, ~" ", Path, ~" HTTP/1.1\r\n", header_lines(Headers), ~"\r\n", Body
        ]),
        {Head, Rest} = recv_head(Sock, <<>>),
        {Status, RespHeaders} = parse_head(Head),
        RespBody = read_body(Sock, Method, Status, RespHeaders, Rest),
        {Status, RespHeaders, RespBody}
    after
        gen_tcp:close(Sock)
    end.

-doc """
Sends one request and reads only the response head: for a response whose body
never ends (an event stream).
""".
-spec request_head(Port, Method, Path, Headers) -> {Status, headers()} when
    Port :: inet:port_number(),
    Method :: binary(),
    Path :: binary(),
    Headers :: headers(),
    Status :: 100..599.
request_head(Port, Method, Path, Headers0) ->
    {ok, Sock} = gen_tcp:connect("localhost", Port, [binary, {active, false}, {packet, raw}]),
    try
        Headers = default_headers(Port, Method, Headers0, 0),
        ok = gen_tcp:send(Sock, [
            Method, ~" ", Path, ~" HTTP/1.1\r\n", header_lines(Headers), ~"\r\n"
        ]),
        {Head, _Rest} = recv_head(Sock, <<>>),
        parse_head(Head)
    after
        gen_tcp:close(Sock)
    end.

-doc "The first value of header `Name` (lowercase), or `undefined`.".
-spec header(Name, headers()) -> binary() | undefined when
    Name :: binary().
header(Name, Headers) ->
    case lists:keyfind(Name, 1, Headers) of
        {Name, Value} -> Value;
        false -> undefined
    end.

-doc "Every value of header `Name` (lowercase), in wire order.".
-spec headers(Name, headers()) -> [binary()] when
    Name :: binary().
headers(Name, Headers) ->
    [V || {N, V} <:- Headers, N =:= Name].

%% Host is always sent (HTTP/1.1 requires it, and the Origin check compares
%% against it); a body gets its length so the server can frame it.
default_headers(Port, Method, Headers0, BodyLen) ->
    Host = [{~"host", <<"localhost:", (integer_to_binary(Port))/binary>>}],
    Len =
        case BodyLen > 0 orelse lists:member(Method, [~"POST", ~"PUT", ~"PATCH", ~"DELETE"]) of
            true -> [{~"content-length", integer_to_binary(BodyLen)}];
            false -> []
        end,
    Given = [string:lowercase(N) || {N, _} <- Headers0],
    [H || {N, _} = H <- Host ++ Len, not lists:member(N, Given)] ++ Headers0.

header_lines(Headers) ->
    [[N, ~": ", V, ~"\r\n"] || {N, V} <- Headers].

recv_head(Sock, Acc0) ->
    case binary:split(Acc0, ~"\r\n\r\n") of
        [Head, Rest] ->
            {Head, Rest};
        [_] ->
            {ok, Data} = gen_tcp:recv(Sock, 0, ?TIMEOUT),
            recv_head(Sock, <<Acc0/binary, Data/binary>>)
    end.

parse_head(Head) ->
    [StatusLine | Lines] = binary:split(Head, ~"\r\n", [global]),
    <<"HTTP/1.1 ", StatusBin:3/binary, _/binary>> = StatusLine,
    Headers = [
        {string:lowercase(string:trim(N)), string:trim(V)}
     || Line <- Lines,
        [N, V] <- [binary:split(Line, ~":")]
    ],
    {binary_to_integer(StatusBin), Headers}.

%% RFC 9112 §6.3: HEAD and 1xx/204/304 responses carry no body whatever their
%% headers say; otherwise chunked wins over content-length, else read to close.
read_body(_Sock, ~"HEAD", _Status, _Headers, _Rest) ->
    <<>>;
read_body(_Sock, _Method, Status, _Headers, _Rest) when
    Status < 200; Status =:= 204; Status =:= 304
->
    <<>>;
read_body(Sock, _Method, _Status, Headers, Rest) ->
    case {header(~"transfer-encoding", Headers), header(~"content-length", Headers)} of
        {~"chunked", _} -> read_chunked(Sock, Rest, <<>>);
        {_, undefined} -> read_to_close(Sock, Rest);
        {_, Len} -> read_exact(Sock, Rest, binary_to_integer(Len))
    end.

read_exact(Sock, Buf, Len) ->
    binary:part(fill(Sock, Buf, Len), 0, Len).

%% `Buf` grown from the socket until it holds at least `Len` bytes.
fill(_Sock, Buf, Len) when byte_size(Buf) >= Len ->
    Buf;
fill(Sock, Buf, Len) ->
    {ok, Data} = gen_tcp:recv(Sock, 0, ?TIMEOUT),
    fill(Sock, <<Buf/binary, Data/binary>>, Len).

read_to_close(Sock, Buf) ->
    case gen_tcp:recv(Sock, 0, ?TIMEOUT) of
        {ok, Data} -> read_to_close(Sock, <<Buf/binary, Data/binary>>);
        {error, closed} -> Buf
    end.

read_chunked(Sock, Buf, Acc) ->
    case binary:split(Buf, ~"\r\n") of
        [SizeLine, Rest] ->
            [SizeHex | _Ext] = binary:split(SizeLine, ~";"),
            case binary_to_integer(string:trim(SizeHex), 16) of
                0 ->
                    Acc;
                Size ->
                    <<Data:Size/binary, "\r\n", Tail/binary>> = fill(Sock, Rest, Size + 2),
                    read_chunked(Sock, Tail, <<Acc/binary, Data/binary>>)
            end;
        [_] ->
            {ok, Data} = gen_tcp:recv(Sock, 0, ?TIMEOUT),
            read_chunked(Sock, <<Buf/binary, Data/binary>>, Acc)
    end.

%% --------------------------------------------------------------------
%% WebSocket
%% --------------------------------------------------------------------

-doc """
Upgrades to the live route `Path`. Returns the open socket on a `101`, or the
refusal's `{error, {Status, Headers}}`. No frame is consumed: the connect frame
is the first `ws_recv/1`.
""".
-spec ws_connect(Port, Path, Opts) -> {ok, gen_tcp:socket()} | {error, {Status, headers()}} when
    Port :: inet:port_number(),
    Path :: binary(),
    Opts :: ws_opts(),
    Status :: 100..599.
ws_connect(Port, Path, Opts) ->
    {ok, Sock} = gen_tcp:connect("localhost", Port, [binary, {active, false}, {packet, raw}]),
    ok = gen_tcp:send(Sock, upgrade_request(Port, Path, Opts)),
    {Head, Rest} = recv_head(Sock, <<>>),
    case parse_head(Head) of
        {101, _Headers} ->
            ok = ws_put_buffer(Sock, Rest),
            {ok, Sock};
        {Status, Headers} ->
            ok = gen_tcp:close(Sock),
            {error, {Status, Headers}}
    end.

-doc "The status and headers an upgrade to the live route `Path` is answered with.".
-spec ws_upgrade_head(Port, Path, Opts) -> {Status, headers()} when
    Port :: inet:port_number(),
    Path :: binary(),
    Opts :: ws_opts(),
    Status :: 100..599.
ws_upgrade_head(Port, Path, Opts) ->
    {ok, Sock} = gen_tcp:connect("localhost", Port, [binary, {active, false}, {packet, raw}]),
    try
        ok = gen_tcp:send(Sock, upgrade_request(Port, Path, Opts)),
        {Head, _Rest} = recv_head(Sock, <<>>),
        parse_head(Head)
    after
        gen_tcp:close(Sock)
    end.

upgrade_request(Port, Path, Opts) ->
    Query = [[~"&", K, ~"=", V] || {K, V} <- maps:get(query, Opts, [])],
    Headers = [
        {~"host", <<"localhost:", (integer_to_binary(Port))/binary>>},
        {~"upgrade", ~"websocket"},
        {~"connection", ~"Upgrade"},
        {~"sec-websocket-key", base64:encode(crypto:strong_rand_bytes(16))},
        {~"sec-websocket-version", ~"13"}
        | maps:get(headers, Opts, [])
    ],
    [~"GET /ws?_az_path=", Path, Query, ~" HTTP/1.1\r\n", header_lines(Headers), ~"\r\n"].

-spec ws_send_json(gen_tcp:socket(), term()) -> ok.
ws_send_json(Sock, Term) ->
    gen_tcp:send(Sock, ws_encode_frame(1, iolist_to_binary(json:encode(Term)))).

-spec ws_recv(gen_tcp:socket()) -> frame() | timeout | {error, term()}.
ws_recv(Sock) ->
    ws_recv(Sock, ?TIMEOUT).

-spec ws_recv(gen_tcp:socket(), timeout()) -> frame() | timeout | {error, term()}.
ws_recv(Sock, Timeout) ->
    ws_recv_buffered(Sock, Timeout, ws_take_buffer(Sock)).

ws_recv_buffered(Sock, Timeout, Buf) ->
    case ws_decode(Buf) of
        {Frame, Rest} ->
            ok = ws_put_buffer(Sock, Rest),
            Frame;
        more ->
            case gen_tcp:recv(Sock, 0, Timeout) of
                {ok, Data} ->
                    ws_recv_buffered(Sock, Timeout, <<Buf/binary, Data/binary>>);
                {error, timeout} ->
                    %% Not terminal: the caller may read again with a longer one.
                    ok = ws_put_buffer(Sock, Buf),
                    timeout;
                {error, Reason} ->
                    ok = ws_put_buffer(Sock, Buf),
                    {error, Reason}
            end
    end.

ws_take_buffer(Sock) ->
    case erlang:erase({?MODULE, Sock}) of
        undefined -> <<>>;
        Buf -> Buf
    end.

ws_put_buffer(_Sock, <<>>) ->
    ok;
ws_put_buffer(Sock, Buf) ->
    undefined = erlang:put({?MODULE, Sock}, Buf),
    ok.

-spec ws_close(gen_tcp:socket()) -> ok.
ws_close(Sock) ->
    Mask = crypto:strong_rand_bytes(4),
    %% The peer may already have closed (a drained session); sending then is
    %% allowed to fail, closing our end is not.
    case gen_tcp:send(Sock, <<1:1, 0:3, 8:4, 1:1, 0:7, Mask/binary>>) of
        ok -> ok;
        {error, _} -> ok
    end,
    <<_/binary>> = ws_take_buffer(Sock),
    gen_tcp:close(Sock).

%% A client frame must be masked (RFC 6455 §5.3).
ws_encode_frame(Opcode, Payload) ->
    Mask = crypto:strong_rand_bytes(4),
    Masked = ws_mask(Payload, Mask),
    case byte_size(Payload) of
        L when L < 126 ->
            <<1:1, 0:3, Opcode:4, 1:1, L:7, Mask/binary, Masked/binary>>;
        L when L < 65536 ->
            <<1:1, 0:3, Opcode:4, 1:1, 126:7, L:16, Mask/binary, Masked/binary>>;
        L ->
            <<1:1, 0:3, Opcode:4, 1:1, 127:7, L:64, Mask/binary, Masked/binary>>
    end.

ws_mask(Payload, <<M0, M1, M2, M3>>) ->
    ws_mask(Payload, M0, M1, M2, M3, <<>>).

ws_mask(<<>>, _, _, _, _, Acc) ->
    Acc;
ws_mask(<<B, Rest/binary>>, M0, M1, M2, M3, Acc) ->
    ws_mask(Rest, M1, M2, M3, M0, <<Acc/binary, (B bxor M0)>>).

%% Decode ONE unmasked server frame off the front of `Data`, with the undecoded
%% tail, or `more`. All three RFC 6455 length forms: without the 64-bit one a
%% frame over 64 KiB would read as `more` until the timeout.
ws_decode(<<_Fin:1, _Rsv:3, 8:4, 0:1, Len:7, Rest/binary>>) when
    Len < 126, byte_size(Rest) >= Len
->
    <<Payload:Len/binary, Tail/binary>> = Rest,
    {ws_close_frame(Payload), Tail};
ws_decode(<<_Fin:1, _Rsv:3, Opcode:4, 0:1, Len:7, Rest/binary>>) when
    Len < 126, byte_size(Rest) >= Len
->
    ws_payload_frame(Opcode, Len, Rest);
ws_decode(<<_Fin:1, _Rsv:3, Opcode:4, 0:1, 126:7, Len:16, Rest/binary>>) when
    byte_size(Rest) >= Len
->
    ws_payload_frame(Opcode, Len, Rest);
ws_decode(<<_Fin:1, _Rsv:3, Opcode:4, 0:1, 127:7, Len:64, Rest/binary>>) when
    byte_size(Rest) >= Len
->
    ws_payload_frame(Opcode, Len, Rest);
ws_decode(_Data) ->
    more.

ws_payload_frame(Opcode, Len, Rest) ->
    <<Payload:Len/binary, Tail/binary>> = Rest,
    {{ws_opcode_to_type(Opcode), Payload}, Tail}.

ws_close_frame(<<Code:16, Reason/binary>>) ->
    {close, Code, Reason};
ws_close_frame(_Short) ->
    {close, 0, <<>>}.

ws_opcode_to_type(1) -> text;
ws_opcode_to_type(2) -> binary;
ws_opcode_to_type(_) -> unknown.

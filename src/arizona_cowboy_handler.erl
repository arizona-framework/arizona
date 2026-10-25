-module(arizona_cowboy_handler).
-moduledoc """
The Cowboy adapter's single handler: every request reaches it through the
listener's catch-all route, and it routes with `arizona_route_table`.

What each route kind answers comes from core -- `arizona_http:reply/3` for a
page, `arizona_ws:upgrade/3` for a WebSocket, `arizona_controller:dispatch/3`
for a controller -- and this module only maps it onto Cowboy, wrapping it in the
route's response transforms (`arizona_http_transforms`). It is also the
`cowboy_loop` handler of a controller's `{loop, ...}` reply and the
`cowboy_websocket` handler of a live session.
""".

-behaviour(cowboy_handler).

-include_lib("kernel/include/file.hrl").

%% --------------------------------------------------------------------
%% API function exports
%% --------------------------------------------------------------------

-export([init/2]).
-export([info/3]).
-export([terminate/3]).
-export([websocket_init/1]).
-export([websocket_handle/2]).
-export([websocket_info/2]).

%% --------------------------------------------------------------------
%% Ignore xref warnings
%% --------------------------------------------------------------------

%% Cowboy calls these once `init/2` hands it a loop or a WebSocket; the
%% `cowboy_loop` and `cowboy_websocket` behaviours are not declared beside
%% `cowboy_handler`, since all three define `init/2`.
-ignore_xref([info/3]).
-ignore_xref([websocket_init/1]).
-ignore_xref([websocket_handle/2]).
-ignore_xref([websocket_info/2]).

%% --------------------------------------------------------------------
%% API Functions
%% --------------------------------------------------------------------

-doc "Cowboy `init/2`: routes the request and answers it.".
-spec init(Req, Listener) -> Result when
    Req :: cowboy_req:req(),
    Listener :: atom(),
    Result ::
        {ok, Req, Listener}
        | {cowboy_loop, Req, map()}
        | {cowboy_websocket, Req, map(), map()}.
init(Req, Listener) ->
    %% Counted in flight until this process exits, so a drain waits for it.
    ok = arizona_pubsub:subscribe(arizona_cowboy_server:requests(Listener), self()),
    Table = arizona_cowboy_server:table(Listener),
    case arizona_route_table:match(cowboy_req:method(Req), cowboy_req:path(Req), Table) of
        not_found ->
            {ok, reply({404, [], <<>>}, Req), Listener};
        {method_not_allowed, Allow} ->
            {ok, reply({405, [{~"allow", join(Allow)}], <<>>}, Req), Listener};
        {ok, #{transforms := Transforms} = Entry, Bindings} ->
            Req1 = Req#{bindings => Bindings},
            case
                arizona_http_transforms:preflight(
                    Transforms, cowboy_req:method(Req1), cowboy_req:headers(Req1)
                )
            of
                none -> serve(Entry, Req1, Listener);
                Preflight -> {ok, reply(Preflight, Req1), Listener}
            end
    end.

-doc "Cowboy `cowboy_loop` `info/3`: hands a message to the controller's loop.".
-spec info(Msg, Req, State) -> {ok, Req, State} | {stop, Req, State} when
    Msg :: term(),
    Req :: cowboy_req:req(),
    State :: map().
info(Msg, Req, #{loop := {Handler, LoopState}} = State) ->
    Push = fun(Data) -> cowboy_req:stream_body(Data, nofin, Req) end,
    case Handler:handle_info(Msg, Push, LoopState) of
        {ok, LoopState1} ->
            {ok, Req, State#{loop => {Handler, LoopState1}}};
        {stop, LoopState1} ->
            ok = cowboy_req:stream_body(<<>>, fin, Req),
            {stop, Req, State#{loop => {Handler, LoopState1}, stopped => true}}
    end.

-doc """
Cowboy `terminate/3`. A loop that ends without having stopped itself lost its
client, which the controller hears as `{arizona_disconnect, Reason}`.
""".
-spec terminate(Reason, Req, State) -> ok when
    Reason :: term(),
    Req :: cowboy_req:req() | map(),
    State :: term().
terminate(Reason, _Req, #{loop := {Handler, LoopState}} = State) when
    not is_map_key(stopped, State)
->
    Gone = fun(_Data) -> {error, closed} end,
    case Handler:handle_info({arizona_disconnect, Reason}, Gone, LoopState) of
        {ok, _} -> ok;
        {stop, _} -> ok
    end;
terminate(_Reason, _Req, _State) ->
    ok.

-doc """
Cowboy `websocket_init/1`: joins the listener's session group (how a drain
reaches the session) and starts the Arizona socket.
""".
-spec websocket_init(State) -> {[cow_ws:frame()], State} when
    State :: map().
websocket_init(#{listener := Listener, ws := WsState} = State) ->
    ok = arizona_pubsub:subscribe(arizona_cowboy_server:sessions(Listener), self()),
    to_cowboy(arizona_ws:init(WsState), State).

-doc "Cowboy `websocket_handle/2`: text frames go to the socket.".
-spec websocket_handle(Frame, State) -> {[cow_ws:frame()], State} when
    Frame :: cow_ws:frame() | ping | pong,
    State :: map().
websocket_handle({text, Data}, #{socket := Sock} = State) ->
    to_cowboy(arizona_socket:handle_in(Data, Sock), State);
websocket_handle(_Frame, State) ->
    {[], State}.

-doc "Cowboy `websocket_info/2`: a drain, or a message for the socket.".
-spec websocket_info(Msg, State) -> {[cow_ws:frame()], State} when
    Msg :: term(),
    State :: map().
websocket_info({arizona_cowboy_drain, Deadline}, #{socket := Sock} = State) ->
    ok = arizona_socket:drain(Sock, Deadline),
    {[], State};
websocket_info(Msg, #{socket := Sock} = State) ->
    to_cowboy(arizona_socket:handle_info(Msg, Sock), State).

%% --------------------------------------------------------------------
%% Internal functions
%% --------------------------------------------------------------------

%% A plain OPTIONS reaches a route only when it was opened for a CORS
%% preflight; it answers with the URL's Allow set, never the route.
serve(#{cors_options := true} = Entry, #{method := ~"OPTIONS"} = Req, Listener) ->
    Allow = arizona_route_table:allow(
        cowboy_req:path(Req), arizona_cowboy_server:table(Listener)
    ),
    finish(Entry, Req, {204, [{~"allow", join(Allow)} || Allow =/= []], <<>>}, Listener);
serve(#{kind := live, target := #{handler := Handler} = Live} = Entry, Req, Listener) ->
    {Status, Headers, Body, ArzReq} = arizona_http:reply(
        Handler, arizona_cowboy_req:new(Req), Live
    ),
    finish(Entry, arizona_req:raw(ArzReq), {Status, Headers, Body}, Listener);
serve(#{kind := Kind, target := #{handler := Handler} = Controller} = Entry, Req, Listener) when
    Kind =:= controller; Kind =:= reload
->
    {Reply, ArzReq} = arizona_controller:dispatch(
        Controller, arizona_cowboy_req:new(Req), arizona_cowboy_server
    ),
    controller_reply(Entry, Handler, Reply, arizona_req:raw(ArzReq), Listener);
serve(#{kind := ws}, Req, Listener) ->
    case arizona_ws:upgrade(cowboy_req:parse_qs(Req), arizona_cowboy_req, Req) of
        {reply, Status, Headers, Raw} ->
            {ok, reply({Status, Headers, <<>>}, Raw), Listener};
        {upgrade, Headers, WsState} ->
            {Cookies, Others} = split_cookies(Headers),
            Req1 = cowboy_req:set_resp_headers(header_map(Others), with_cookies(Cookies, Req)),
            {cowboy_websocket, Req1, #{listener => Listener, ws => WsState},
                arizona_cowboy_server:ws_opts(Listener)}
    end;
serve(#{kind := asset, route := Route, target := Dir} = Entry, Req, Listener) ->
    Segments = maps:get(~"path", cowboy_req:bindings(Req), []),
    case asset_file(Dir, Segments) of
        undefined -> finish(Entry, Req, {404, [], <<>>}, Listener);
        File -> serve_file(Entry, Route, inside(Dir, File), Req, Listener)
    end.

serve_file(Entry, _Route, outside, Req, Listener) ->
    finish(Entry, Req, {404, [], <<>>}, Listener);
serve_file(Entry, Route, File, Req, Listener) ->
    case filelib:is_regular(File) of
        true ->
            {Type, SubType, _Params} = cow_mimetypes:all(iolist_to_binary(File)),
            Headers =
                [{~"content-type", <<Type/binary, "/", SubType/binary>>}] ++
                    cache_control(Route),
            {Status, Headers1, stream} = wrap(Entry, Req, {200, Headers, stream}),
            Size = filelib:file_size(File),
            {ok, reply({Status, Headers1, {sendfile, 0, Size, File}}, Req), Listener};
        false ->
            finish(Entry, Req, {404, [], <<>>}, Listener)
    end.

controller_reply(Entry, _Handler, {Status, Headers, Body}, Req, Listener) when
    is_integer(Status)
->
    finish(Entry, Req, {Status, Headers, Body}, Listener);
controller_reply(Entry, _Handler, {stream, Status, Headers, Fun}, Req, Listener) ->
    {Status1, Headers1} = wrap_head(Entry, Req, Status, Headers),
    Req1 = stream_reply(Status1, Headers1, Req),
    Fun(fun(Data, IsFin) -> cowboy_req:stream_body(Data, IsFin, Req1) end),
    {ok, Req1, Listener};
controller_reply(
    Entry, _Handler, {sendfile, Status, Headers, {Path, Offset, Length}}, Req, Listener
) ->
    {Status1, Headers1} = wrap_head(Entry, Req, Status, Headers),
    {ok, reply({Status1, Headers1, {sendfile, Offset, Length, Path}}, Req), Listener};
controller_reply(Entry, Handler, {loop, Status, Headers, LoopState}, Req, _Listener) ->
    {Status1, Headers1} = wrap_head(Entry, Req, Status, Headers),
    %% A client that goes away makes Cowboy shut this process down: trapped, the
    %% shutdown reaches `terminate/3`, which tells the controller.
    _WasTrapping = process_flag(trap_exit, true),
    {cowboy_loop, stream_reply(Status1, Headers1, Req), #{loop => {Handler, LoopState}}}.

finish(Entry, Req, Response, Listener) ->
    {ok, reply(wrap(Entry, Req, Response), Req), Listener}.

%% The transformed status and headers of a response whose body is not known up
%% front (stream, sendfile, loop, a static file): header-level transforms only.
wrap_head(Entry, Req, Status, Headers) ->
    {Status1, Headers1, stream} = wrap(Entry, Req, {Status, Headers, stream}),
    {Status1, Headers1}.

wrap(#{transforms := Transforms}, Req, Response) ->
    {Scheme, _ArzReq} = arizona_req:client_scheme(arizona_cowboy_req:new(Req)),
    Request = #{
        method => cowboy_req:method(Req),
        headers => cowboy_req:headers(Req),
        scheme => Scheme
    },
    arizona_http_transforms:wrap(Transforms, Request, Response).

reply({Status, Headers, Body}, Req) ->
    {Cookies, Others} = split_cookies(Headers),
    cowboy_req:reply(Status, header_map(Others), Body, with_cookies(Cookies, Req)).

stream_reply(Status, Headers, Req) ->
    {Cookies, Others} = split_cookies(Headers),
    cowboy_req:stream_reply(Status, header_map(Others), with_cookies(Cookies, Req)).

%% Cowboy takes cookies only through its `resp_cookies` map, one value per key,
%% and sends them in key order: integer keys keep Arizona's send order, which a
%% cookie written twice depends on (the browser keeps the last).
split_cookies(Headers) ->
    {Cookies, Others} = lists:partition(fun({Name, _}) -> Name =:= ~"set-cookie" end, Headers),
    {[Value || {_, Value} <- Cookies], Others}.

with_cookies([], Req) ->
    Req;
with_cookies(Cookies, Req) when length(Cookies) =< 32 ->
    Req#{resp_cookies => maps:from_list(lists:enumerate(Cookies))};
with_cookies(Cookies, Req) ->
    %% Past 32 keys the map stops iterating in key order, so the order cannot be
    %% kept. Keep what the browser would: the last line per cookie (name, domain,
    %% path), after which no two lines name the same cookie and order is moot.
    Req#{resp_cookies => maps:from_list(lists:enumerate(last_per_cookie(Cookies)))}.

last_per_cookie(Cookies) ->
    {Kept, _Seen} = lists:foldr(
        fun(Line, {Acc, Seen}) ->
            Id = cookie_id(Line),
            case Seen of
                #{Id := _} -> {Acc, Seen};
                #{} -> {[Line | Acc], Seen#{Id => true}}
            end
        end,
        {[], #{}},
        Cookies
    ),
    Kept.

cookie_id(Line) ->
    [Pair | Attrs] = binary:split(iolist_to_binary(Line), ~"; ", [global]),
    [Name | _] = binary:split(Pair, ~"="),
    Attr = fun(Key) ->
        case [V || A <- Attrs, [K, V] <- [binary:split(A, ~"=")], string:lowercase(K) =:= Key] of
            [Value | _] -> Value;
            [] -> undefined
        end
    end,
    {Name, Attr(~"domain"), Attr(~"path")}.

%% A field sent twice becomes one comma-joined line (RFC 9110 §5.3).
header_map(Headers) ->
    lists:foldl(
        fun({Name, Value}, Acc) ->
            case Acc of
                #{Name := Prev} -> Acc#{Name => [Prev, ~", ", Value]};
                #{} -> Acc#{Name => Value}
            end
        end,
        #{},
        Headers
    ).

%% The wildcard's segments, refused when any could leave the directory.
asset_file(Dir, Segments) ->
    Unsafe = fun(S) ->
        S =:= ~".." orelse S =:= ~"." orelse binary:match(S, [~"/", ~"\\", <<0>>]) =/= nomatch
    end,
    case Segments =/= [] andalso not lists:any(Unsafe, Segments) of
        true -> filename:join([Dir | Segments]);
        false -> undefined
    end.

%% `File` with its symlinks resolved when it stays inside `Dir` (also
%% resolved), else `outside`: a link must not serve what lies beyond the asset
%% directory, the rule roadrunner's static handler applies by default.
inside(Dir, File) ->
    case {real_path(Dir), real_path(File)} of
        {{ok, RealDir}, {ok, RealFile}} ->
            case string:prefix(RealFile, <<RealDir/binary, "/">>) of
                nomatch -> outside;
                _Rest -> RealFile
            end;
        _Unresolved ->
            outside
    end.

%% Paths as binaries throughout (asset segments arrive as binaries).
real_path(Path) ->
    resolve(filename:split(unicode:characters_to_binary(filename:absname(Path))), [], 0).

resolve(_Components, _Resolved, Links) when Links > 40 ->
    error;
resolve([], Resolved, _Links) ->
    {ok, filename:join(lists:reverse(Resolved))};
resolve([~"." | Rest], Resolved, Links) ->
    resolve(Rest, Resolved, Links);
resolve([~".." | Rest], [Root], Links) ->
    resolve(Rest, [Root], Links);
resolve([~".." | Rest], [_Last | Resolved], Links) ->
    resolve(Rest, Resolved, Links);
resolve([Component | Rest], Resolved, Links) ->
    Path = filename:join(lists:reverse([Component | Resolved])),
    case file:read_link_info(Path) of
        {ok, #file_info{type = symlink}} ->
            {ok, Target} = file:read_link(Path),
            Base = filename:join(lists:reverse(Resolved)),
            Next = unicode:characters_to_binary(filename:absname(Target, Base)),
            resolve(filename:split(Next) ++ Rest, [], Links + 1);
        {ok, _Info} ->
            resolve(Rest, [Component | Resolved], Links);
        {error, _Reason} ->
            error
    end.

cache_control({asset, _Path, _Source, #{cache_control := Value}}) ->
    [{~"cache-control", Value}];
cache_control(_Route) ->
    [].

to_cowboy({ok, Sock}, State) ->
    {[], State#{socket => Sock}};
to_cowboy({reply, Data, Sock}, State) ->
    {[{text, Data}], State#{socket => Sock}};
to_cowboy({reply_many, Frames, Sock}, State) ->
    {[{text, Data} || Data <- Frames], State#{socket => Sock}};
to_cowboy({close, Code, Reason, Sock}, State) ->
    {[{close, Code, Reason}], State#{socket => Sock}}.

join(Values) -> iolist_to_binary(lists:join(~", ", Values)).

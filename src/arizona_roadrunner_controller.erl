-module(arizona_roadrunner_controller).
-moduledoc """
Roadrunner handler for controller routes: runs `arizona_controller:dispatch/3`
and maps its reply onto roadrunner's response shapes.

A controller route (`{get, ...}`/`{post, ...}`/.../`{match, ...}`) compiles to this
dispatcher (the way `{live, ...}` routes go through `arizona_roadrunner_http`).
Roadrunner has every `t:arizona_controller:reply/0` kind, so each maps one to one; a
loop's state carries the controller module, so `handle_info/3` reaches it, and
roadrunner's `{roadrunner_disconnect, Reason}` reaches it as
`{arizona_disconnect, Reason}`.
""".

-behaviour(roadrunner_handler).

%% --------------------------------------------------------------------
%% API function exports
%% --------------------------------------------------------------------

-export([handle/1]).
-export([handle_info/3]).

%% --------------------------------------------------------------------
%% API Functions
%% --------------------------------------------------------------------

-doc """
Roadrunner `handle/1` callback. Dispatches the controller route and hands
roadrunner back the request the action returned, whose body reader a manual
body read advanced.
""".
-spec handle(Req) -> {Response, Req} when
    Req :: roadrunner_req:request(),
    Response :: roadrunner_handler:response().
handle(Req) ->
    #{arizona := #{handler := Handler} = Route} = roadrunner_req:state(Req),
    {Reply, ArzReq} = arizona_controller:dispatch(
        Route, arizona_roadrunner_req:new(Req), arizona_roadrunner_server
    ),
    {to_roadrunner(Handler, Reply), arizona_req:raw(ArzReq)}.

-doc """
Roadrunner `handle_info/3` callback for a `{loop, ...}` reply: hands each message
to the controller's own `handle_info/3`.
""".
-spec handle_info(Info, Push, State) -> {ok, State} | {stop, State} when
    Info :: term(),
    Push :: roadrunner_handler:push_fun(),
    State :: {module(), term()}.
handle_info({roadrunner_disconnect, Reason}, Push, State) ->
    handle_info({arizona_disconnect, Reason}, Push, State);
handle_info(Info, Push, {Handler, LoopState}) ->
    case Handler:handle_info(Info, Push, LoopState) of
        {ok, LoopState1} -> {ok, {Handler, LoopState1}};
        {stop, LoopState1} -> {stop, {Handler, LoopState1}}
    end.

%% --------------------------------------------------------------------
%% Internal functions
%% --------------------------------------------------------------------

%% A buffered reply without framing gets its `content-length` from roadrunner.
to_roadrunner(_Handler, {Status, Headers, Body}) when is_integer(Status) ->
    {Status, binary_headers(Headers), Body};
to_roadrunner(Handler, {loop, Status, Headers, State}) ->
    {loop, Status, binary_headers(Headers), {Handler, State}};
to_roadrunner(_Handler, {Kind, Status, Headers, Payload}) ->
    {Kind, Status, binary_headers(Headers), Payload}.

binary_headers(Headers) ->
    [{Name, iolist_to_binary(Value)} || {Name, Value} <- Headers].

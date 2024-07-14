-module(arizona_websocket).
-moduledoc false.
-behaviour(cowboy_websocket).

%% --------------------------------------------------------------------
%% Behaviour (cowboy_websocket) exports
%% --------------------------------------------------------------------

-export([init/2]).
-export([websocket_init/1]).
-export([websocket_handle/2]).
-export([websocket_info/2]).
-export([terminate/3]).

%% --------------------------------------------------------------------
%% API function exports
%% --------------------------------------------------------------------

-export([subscribe/1]).

%% --------------------------------------------------------------------
%% Types (and their exports)
%% --------------------------------------------------------------------

-opaque init_state() :: {Params :: [{binary(), binary() | true}],
                         {Mod :: module(), Fun :: atom(), Opts :: arizona:route_opts()}}.
-export_type([init_state/0]).

-opaque state() :: #{
    template => arizona_tpl_compile:block(),
    sockets => #{SocketId :: arizona_tpl_render:socket_target() := Socket :: arizona_socket:t()}
}.
-export_type([state/0]).
-elvis([{elvis_style, state_record_and_type, disable}]). % opaque not identified as "type"

%% --------------------------------------------------------------------
%% Behaviour (cowboy_websocket) callbacks
%% --------------------------------------------------------------------

-spec init(Req, term()) -> {cowboy_websocket, Req, InitState}
    when Req :: cowboy_req:req(),
         InitState :: init_state().
init(Req0, _State) ->
    Params = cowboy_req:parse_qs(Req0),
    {Req, Env} = arizona_server:route(Req0),
    #{handler_opts := {Mod, Fun, Opts}} = Env,
    {cowboy_websocket, Req, {Params, {Mod, Fun, Opts}}}.

-spec websocket_init(InitState) -> {Events, State}
    when InitState :: init_state(),
         Events :: cowboy_websocket:commands(),
         State :: state().
websocket_init({Params, {Mod, Fun, Opts}}) ->
    Macros = maps:get(macros, Opts, #{}),
    Tpl = arizona_tpl_compile:compile(Mod, Fun, Macros),
    Assigns = maps:get(assigns, Opts, #{}),
    {Html, Sockets} = arizona_tpl_render:mount(Tpl, Assigns),
    Reconnecting = proplists:get_value(<<"reconnecting">>, Params, <<"false">>),
    Events = case Reconnecting of
        <<"true">> ->
            [];
        <<"false">> ->
            [{text, json:encode([[~"init", Html]])}]
    end,
    State = #{
        template => Tpl,
        sockets => #{Id => arizona_socket:prune(Socket)
                     || Id := Socket <- Sockets}
    },
    subscribe(broadcast),
    send(init, {init, self()}),
    {Events, State}.

-spec websocket_handle(Event, State1) -> {Events, State2}
    when Event :: {text, binary()},
         Events :: cowboy_websocket:commands(),
         State1 :: state(),
         State2 :: state().
websocket_handle({text, Msg}, #{sockets := Sockets} = State) ->
    {Target, Event, Payload} = decode_msg(Msg),
    Socket0 = maps:get(Target, Sockets),
    View = arizona_socket:get_view(Socket0),
    Socket1 = arizona_live_view:handle_event(View, Event, Payload, Socket0),
    Socket = case arizona_socket:get_changes(Socket1) of
        Changes when map_size(Changes) > 0 ->
            Tpl = maps:get(template, State),
            Assigns = arizona_socket:get_assigns(Socket1),
            Patch = arizona_tpl_render:render_target(Target, Tpl, Changes, Assigns),
            arizona_socket:push_event(~"patch", [Target, Patch], Socket1);
        #{} ->
            Socket1
    end,
    {[{text, json:encode(arizona_socket:get_events(Socket))}],
        State#{sockets => Sockets#{Target => arizona_socket:prune(Socket)}}}.

-spec websocket_info(Info, State1) -> {Events, State2}
    when Info :: term(),
         Events :: cowboy_websocket:commands(),
         State1 :: state(),
         State2 :: state().
websocket_info(reload, Socket) ->
    {[{text, json:encode([[~"reload", []]])}], Socket};
websocket_info(_Info, Socket) ->
    {[], Socket}.

-spec terminate(Reason, Req, State) -> ok
    when Reason :: term(),
         Req :: cowboy_req:req(),
         State :: state().
terminate(_Reason, _Req, _State) ->
    send(terminate, {terminate, self()}).

%% --------------------------------------------------------------------
%% API function definitions
%% --------------------------------------------------------------------

-spec subscribe(Event) -> ok
    when Event :: term().
subscribe(Event) ->
    gproc:reg({p, l, {?MODULE, Event}}),
    ok.

%% --------------------------------------------------------------------
%% Private
%% --------------------------------------------------------------------

send(Event, Payload) ->
    gproc:send({p, l, {?MODULE, Event}}, Payload),
    ok.

decode_msg(Msg) ->
    case json:decode(Msg) of
        [Target, Event, Payload] ->
            {decode_target(Target), Event, Payload};
        [Target, Event] ->
            {decode_target(Target), Event, #{}}
    end.

decode_target(<<"root">>) ->
    root;
decode_target(Target) ->
    json:decode(Target).

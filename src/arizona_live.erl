-module(arizona_live).
-behaviour(gen_server).

%% API
-export([start_link/2]).
-export([mount/2, render/1, handle_event/3]).

%% Callback wrapper functions
-export([call_mount_callback/3, call_render_callback/2]).
-export([call_handle_event_callback/4, call_handle_info_callback/3]).

%% gen_server callbacks
-export([init/1, handle_call/3, handle_cast/2, handle_info/2, terminate/2, code_change/3]).

%% Behavior definition
-callback mount(Req, Socket) -> {Socket, Opts} when
    Req :: arizona_request:request(),
    Socket :: arizona_socket:socket(),
    Opts :: map().

-callback render(Socket) -> Socket when
    Socket :: arizona_socket:socket().

-callback handle_event(Event, Params, Socket) -> {noreply, Socket} | {reply, Reply, Socket} when
    Event :: binary(),
    Params :: map(),
    Socket :: arizona_socket:socket(),
    Reply :: term().

-callback handle_info(Info, Socket) -> {noreply, Socket} when
    Info :: term(),
    Socket :: arizona_socket:socket().

-optional_callbacks([handle_event/3, handle_info/2]).

%% State
-record(state, {
    module :: atom(),
    socket :: arizona_socket:socket()
}).

%% Types
-type state() :: #state{}.

%% API Functions

-spec start_link(Module, Socket) -> {ok, Pid} | {error, Reason} when
    Module :: atom(),
    Socket :: arizona_socket:socket(),
    Pid :: pid(),
    Reason :: term().
start_link(Module, Socket) ->
    gen_server:start_link(?MODULE, {Module, Socket}, []).

-spec mount(Pid, Req) -> {Socket, Opts} when
    Pid :: pid(),
    Req :: arizona_request:request(),
    Socket :: arizona_socket:socket(),
    Opts :: map().
mount(Pid, Req) ->
    gen_server:call(Pid, {mount, Req}).

-spec render(Pid) -> Socket when
    Pid :: pid(),
    Socket :: arizona_socket:socket().
render(Pid) ->
    gen_server:call(Pid, render).

-spec handle_event(Pid, Event, Params) -> {noreply, Socket} | {reply, Reply, Socket} when
    Pid :: pid(),
    Event :: binary(),
    Params :: map(),
    Socket :: arizona_socket:socket(),
    Reply :: term().
handle_event(Pid, Event, Params) ->
    gen_server:call(Pid, {handle_event, Event, Params}).

%% Callback wrapper functions

-spec call_mount_callback(Module, Req, Socket) -> {Socket, Opts} when
    Module :: atom(),
    Req :: arizona_request:request(),
    Socket :: arizona_socket:socket(),
    Opts :: map().
call_mount_callback(Module, Req, Socket) ->
    apply(Module, mount, [Req, Socket]).

-spec call_render_callback(Module, Socket) -> Socket when
    Module :: atom(),
    Socket :: arizona_socket:socket().
call_render_callback(Module, Socket) ->
    apply(Module, render, [Socket]).

-spec call_handle_event_callback(Module, Event, Params, Socket) ->
    {noreply, Socket} | {reply, Reply, Socket}
when
    Module :: atom(),
    Event :: binary(),
    Params :: map(),
    Socket :: arizona_socket:socket(),
    Reply :: term().
call_handle_event_callback(Module, Event, Params, Socket) ->
    case erlang:function_exported(Module, handle_event, 3) of
        true ->
            apply(Module, handle_event, [Event, Params, Socket]);
        false ->
            {noreply, Socket}
    end.

-spec call_handle_info_callback(Module, Info, Socket) -> {noreply, Socket} when
    Module :: atom(),
    Info :: term(),
    Socket :: arizona_socket:socket().
call_handle_info_callback(Module, Info, Socket) ->
    case erlang:function_exported(Module, handle_info, 2) of
        true ->
            apply(Module, handle_info, [Info, Socket]);
        false ->
            {noreply, Socket}
    end.

%% gen_server Callbacks

-spec init({Module, Socket}) -> {ok, State} when
    Module :: atom(),
    Socket :: arizona_socket:socket(),
    State :: state().
init({Module, Socket}) ->
    {ok, #state{module = Module, socket = Socket}}.

-spec handle_call(Message, From, State) -> {reply, Reply, State} when
    Message :: {mount, arizona_request:request()} | render | {handle_event, binary(), map()},
    From :: {pid(), term()},
    State :: state(),
    Reply :: term().
handle_call({mount, Req}, _From, #state{module = Module, socket = Socket} = State) ->
    {UpdatedSocket, Opts} = call_mount_callback(Module, Req, Socket),
    {reply, {UpdatedSocket, Opts}, State#state{socket = UpdatedSocket}};
handle_call(render, _From, #state{module = Module, socket = Socket} = State) ->
    UpdatedSocket = call_render_callback(Module, Socket),
    {reply, UpdatedSocket, State#state{socket = UpdatedSocket}};
handle_call({handle_event, Event, Params}, _From, #state{module = Module, socket = Socket} = State) ->
    case call_handle_event_callback(Module, Event, Params, Socket) of
        {noreply, UpdatedSocket} ->
            {reply, {noreply, UpdatedSocket}, State#state{socket = UpdatedSocket}};
        {reply, Reply, UpdatedSocket} ->
            {reply, {reply, Reply, UpdatedSocket}, State#state{socket = UpdatedSocket}}
    end.

-spec handle_cast(Request, State) -> {noreply, State} when
    Request :: term(),
    State :: state().
handle_cast(_Request, State) ->
    {noreply, State}.

-spec handle_info(Info, State) -> {noreply, State} when
    Info :: term(),
    State :: state().
handle_info(Info, #state{module = Module, socket = Socket} = State) ->
    {noreply, UpdatedSocket} = call_handle_info_callback(Module, Info, Socket),
    {noreply, State#state{socket = UpdatedSocket}}.

-spec terminate(Reason, State) -> ok when
    Reason :: term(),
    State :: state().
terminate(_Reason, _State) ->
    ok.

-spec code_change(OldVsn, State, Extra) -> {ok, State} when
    OldVsn :: term(),
    State :: state(),
    Extra :: term().
code_change(_OldVsn, State, _Extra) ->
    {ok, State}.

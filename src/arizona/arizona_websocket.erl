%% @author William Fank Thomé <willilamthome@hotmail.com>
%% @copyright 2023 William Fank Thomé
%% @doc WebSocket.

%% Copyright 2023 William Fank Thomé
%%
%% Licensed under the Apache License, Version 2.0 (the "License");
%% you may not use this file except in compliance with the License.
%% You may obtain a copy of the License at
%%
%%     http://www.apache.org/licenses/LICENSE-2.0
%%
%% Unless required by applicable law or agreed to in writing, software
%% distributed under the License is distributed on an "AS IS" BASIS,
%% WITHOUT WARRANTIES OR CONDITIONS OF ANY KIND, either express or implied.
%% See the License for the specific language governing permissions and
%% limitations under the License.
-module(arizona_websocket).

%% API
-export([ init/1, handle_msg/2, handle_info/2, terminate/3 ]).

%% Macros
-define(DEFAULT_PAYLOAD, #{}).

%% State
-record(state, { view :: module()
               , params :: params()
               , socket :: socket()
               }).

-type params() :: arizona_server_adapter:params().
-type socket() :: arizona_socket:t().

%%%=====================================================================
%%% API
%%%=====================================================================

init(Params) ->
    io:format("[WebSocket] init: ~p~n", [Params]),
    View = get_view(Params),
    Socket0 = arizona_socket:new(),
    {ok, Socket1} = arizona_live_view:mount(View, Params, Socket0),
    Bindings = arizona_socket:get_bindings(Socket1),
    RenderState = arizona_live_view:render_state(View, Bindings),
    Socket = arizona_socket:set_render_state(RenderState, Socket1),
    State = #state{
        view = View,
        params = Params,
        socket = Socket
    },
    {noreply, State}.

handle_msg(Msg, #state{view = View} = State0) ->
    io:format("[WebSocket] msg: ~p~n", [Msg]),
    {Event, Payload} = decode_msg(Msg),
    {ok, Socket0} = arizona_live_view:handle_event(
        View, Event, Payload, State0#state.socket
    ),
    Socket = push_patch_event(Socket0),
    Events = arizona_socket:get_events(Socket),
    State = State0#state{socket = prune_socket(Socket)},
    case Events =:= [] of
        true ->
            {noreply, State};
        false ->
            {reply, Events, State}
    end.

handle_info(Info, State) ->
    io:format("[WebSocket] info: ~p~n", [Info]),
    {noreply, State}.

terminate(Reason, _Req, _State) ->
    io:format("[WebSocket] terminate: ~p~n", [Reason]),
    ok.

%%%=====================================================================
%%% Internal functions
%%%=====================================================================

get_view(Params) ->
    {<<"view">>, View} = proplists:lookup(<<"view">>, Params),
    binary_to_existing_atom(View).

decode_msg(Msg0) ->
    {ok, Msg} = arizona_json:decode(Msg0),
    do_normalize_msg(Msg).

do_normalize_msg([Event, Payload]) ->
    {Event, Payload};
do_normalize_msg(Event) ->
    {Event, ?DEFAULT_PAYLOAD}.

%% Reply

push_patch_event(Socket0) ->
    Bindings = arizona_socket:get_bindings(Socket0),
    case map_size(Bindings) =:= 0 of
        true ->
            Socket0;
        false ->
            RenderState0 = arizona_socket:get_render_state(Socket0),
            RenderState = arizona_template:bind(Bindings, RenderState0),
            Socket = arizona_socket:set_render_state(RenderState, Socket0),
            case arizona_template:diff(RenderState) of
                {ok, Diff} ->
                    arizona_socket:push_event(<<"patch">>, Diff, Socket);
                none ->
                    Socket
            end
    end.

%% Prune

prune_socket(Socket) ->
    Funs = [ fun prune_events/1 ],
    lists:foldl(fun(Fun, Acc) -> Fun(Acc) end, Socket, Funs).

prune_events(Socket) ->
    arizona_socket:set_events([], Socket).

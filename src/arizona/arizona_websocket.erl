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

%%%=====================================================================
%%% API
%%%=====================================================================

init(Args) ->
    io:format("[WebSocket] init: ~p~n", [Args]),
    {ok, []}.

handle_msg(Msg, State) ->
    {Event, Payload} = decode_msg(Msg),
    io:format("[WebSocket] event: ~p | payload: ~p~n", [Event, Payload]),
    {ok, State}.

handle_info(Info, State) ->
    io:format("[WebSocket] info: ~p~n", [Info]),
    {ok, State}.

terminate(Reason, _Req, _State) ->
    io:format("[WebSocket] terminate: ~p~n", [Reason]),
    ok.

%%%=====================================================================
%%% Internal functions
%%%=====================================================================

decode_msg(Msg0) ->
    {ok, Msg} = arizona_json:decode(Msg0),
    do_normalize_msg(Msg).

do_normalize_msg([Event, Payload]) ->
    {Event, Payload};
do_normalize_msg(Event) ->
    {Event, ?DEFAULT_PAYLOAD}.

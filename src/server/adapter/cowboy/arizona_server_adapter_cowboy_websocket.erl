%% @author William Fank Thomé <willilamthome@hotmail.com>
%% @copyright 2023 William Fank Thomé
%% @doc Cowboy WebSocket.

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
-module(arizona_server_adapter_cowboy_websocket).

-behaviour(cowboy_websocket).

%% cowboy_websocket callbacks
-export([ init/2
        , websocket_init/1
        , websocket_handle/2
        , websocket_info/2
        , terminate/3
        ]).

%%%=====================================================================
%%% cowboy_websocket callbacks
%%%=====================================================================

init(Req0, _Args) ->
    Params = cowboy_req:parse_qs(Req0),
    % @todo Get options via arguments.
    % @see https://ninenines.eu/docs/en/cowboy/2.10/manual/cowboy_websocket/
    Opts = #{
        compress => true,
        idle_timeout => 60_000
    },
    case cowboy_req:parse_header(<<"sec-websocket-protocol">>, Req0) of
        undefined ->
            {cowboy_websocket, Req0, Params, Opts};
        Subprotocols ->
            case lists:keymember(<<"mqtt">>, 1, Subprotocols) of
                true ->
                    Req = cowboy_req:set_resp_header(
                        <<"sec-websocket-protocol">>,
                        <<"mqtt">>,
                        Req0
                    ),
                    {cowboy_websocket, Req, Params, Opts};
                false ->
                    Req = cowboy_req:reply(400, Req0),
                    {ok, Req, Params}
            end
    end.

websocket_init(Params) ->
    result(arizona_websocket:init(Params)).

websocket_handle({text, Msg}, State0) ->
    result(arizona_websocket:handle_msg(Msg, State0)).

websocket_info(Info, State0) ->
    result(arizona_websocket:handle_info(Info, State0)).

terminate(Reason, Req, State) ->
    arizona_websocket:terminate(Reason, Req, State).

%%%=====================================================================
%%% Internal functions
%%%=====================================================================

result({reply, Commands, State}) ->
    {Commands, State};
result({noreply, State}) ->
    {[], State}.

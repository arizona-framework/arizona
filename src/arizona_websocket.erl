%%
%% %CopyrightBegin%
%%
%% Copyright 2023-2024 William Fank Thomé
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
%%
%% %CopyrightEnd%
%%
-module(arizona_websocket).
-moduledoc false.

-behaviour(cowboy_websocket).

%% cowboy_websocket callbacks.
-export([init/2, websocket_init/1, websocket_handle/2,
         websocket_info/2, terminate/3]).

%% --------------------------------------------------------------------
%% cowboy_handler callbacks.
%% --------------------------------------------------------------------

init(Req, State) ->
    {cowboy_websocket, Req, State}.

websocket_init(State) ->
    io:format("[WebSocket] init: ~p~n", [State]),
    View = arizona_handler,
    Tpl = persistent_term:get({arz_tpl, View}),
    {ok, Assigns} = View:mount(),
    Html = arizona_tpl_render:render_block(Tpl, Assigns),
    Events = [[~"init", Html]],
    Socket = #{
        view => View,
        assigns => Assigns
    },
    {[{text, json:encode(Events)}], Socket}.

websocket_handle({text, Msg}, Socket0) ->
    io:format("[WebSocket] handle: ~p~n", [Msg]),
    {_Target, Event, Payload} = decode_msg(Msg),
    {Events, Socket} = arizona_handler:handle_event(Event, Payload, Socket0),
    {[{text, json:encode(Events)}], Socket}.

websocket_info(Info, Socket) ->
    io:format("[WebSocket] info: ~p~n", [Info]),
    {[], Socket}.

terminate(Reason, Req, _State) ->
    io:format("[WebSocket] terminate: ~p~n", [{Reason, Req}]),
    ok.

%% --------------------------------------------------------------------
%% API functions.
%% --------------------------------------------------------------------

% nothing here yet!

%% --------------------------------------------------------------------
%% Internal functions.
%% --------------------------------------------------------------------

decode_msg(Msg) ->
    case json:decode(Msg) of
        [Target, Event, Payload] ->
            {Target, Event, Payload};
        [Target, Event] ->
            {Target, Event, #{}}
    end.


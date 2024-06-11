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

%% API functions.
-export([push_event/3, push_change/3]).

%% --------------------------------------------------------------------
%% cowboy_handler callbacks.
%% --------------------------------------------------------------------

init(Req, State) ->
    {cowboy_websocket, Req, State}.

websocket_init(State) ->
    io:format("[WebSocket] init: ~p~n", [State]),
    % TODO: Get view from router.
    View = arizona_handler,
    Tpl = arizona_live_view:persist_get(View, #{}),
    % TODO: Return socket.
    {ok, Assigns} = View:mount(),
    Html = arizona_tpl_render:render_block(Tpl, Assigns),
    Events = [[~"init", Html]],
    Socket = #{
        view => View,
        assigns => Assigns,
        events => [],
        changes => #{}
    },
    {[{text, json:encode(Events)}], Socket}.

websocket_handle({text, Msg}, Socket0) ->
    io:format("[WebSocket] handle: ~p~n", [Msg]),
    % TODO: Get target socket.
    {_Target, Event, Payload} = decode_msg(Msg),
    % TODO: {reply, Msg, Socket}
    {noreply, Socket1} =
        arizona_handler:handle_event(Event, Payload, Socket0),
    Socket = case maps:get(changes, Socket1) of
        Changes when map_size(Changes) > 0 ->
            View = maps:get(view, Socket1),
            Tpl = arizona_live_view:persist_get(View, #{}),
            Assigns = maps:get(assigns, Socket1),
            Patch = arizona_tpl_render:render_changes(Tpl, Changes, Assigns),
            push_event(~"patch", [~"root", Patch],
                Socket1#{assigns => maps:merge(Assigns, Changes)});
        #{} ->
            Socket1
    end,
    {[{text, json:encode(maps:get(events, Socket))}],
        prune(Socket)}.

websocket_info(Info, Socket) ->
    io:format("[WebSocket] info: ~p~n", [Info]),
    {[], Socket}.

terminate(Reason, Req, _State) ->
    io:format("[WebSocket] terminate: ~p~n", [{Reason, Req}]),
    ok.

%% --------------------------------------------------------------------
%% API functions.
%% --------------------------------------------------------------------

push_event(Name, Payload, #{events := Events} = Socket) ->
    Socket#{events => [[Name, Payload] | Events]}.

push_change(Key, Value, #{changes := Changes} = Socket) ->
    Socket#{changes => Changes#{Key => Value}}.

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

prune(Socket) ->
    Socket#{
        events => [],
        changes => #{}
    }.


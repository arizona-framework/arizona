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
-export([subscribe/1]).

%% --------------------------------------------------------------------
%% cowboy_handler callbacks.
%% --------------------------------------------------------------------

init(Req0, _State = []) ->
    {ok, Req, Env} = arizona_server:route(Req0),
    #{handler_opts := {Mod, Fun, Opts}} = Env,
    {cowboy_websocket, Req, {Mod, Fun, Opts}}.

websocket_init({Mod, Fun, Opts}) ->
    io:format("[WebSocket] init: ~p~n", [{self(), {Mod, Fun, Opts}}]),
    Macros = maps:get(macros, Opts, #{}),
    Tpl = arizona_live_view:persist_get(Mod, Fun, Macros),
    Assigns = maps:get(assigns, Opts, #{}),
    {ok, {Html, Sockets}} = arizona_tpl_render:mount(Tpl, Assigns),
    Events = [[~"init", Html]],
    State = #{
        template => Tpl,
        sockets => #{Id => arizona_socket:prune(Socket)
                     || Id := Socket <- Sockets}
    },
    subscribe(broadcast),
    send(init, {init, self()}),
    {[{text, json:encode(Events)}], State}.

websocket_handle({text, Msg}, #{sockets := Sockets} = State) ->
    io:format("[WebSocket] handle: ~p~n", [Msg]),
    {Target, Event, Payload} = decode_msg(Msg),
    Socket0 = maps:get(Target, Sockets),
    View = maps:get(view, Socket0),
    % TODO: {reply, Msg, Socket}
    {noreply, Socket1} = View:handle_event(Event, Payload, Socket0),
    Socket = case maps:get(changes, Socket1) of
        Changes when map_size(Changes) > 0 ->
            Tpl = maps:get(template, State),
            Assigns = maps:get(assigns, Socket1),
            Patch = arizona_tpl_render:render_target(Target, Tpl, Changes, Assigns),
            arizona_socket:push_event(~"patch", [Target, Patch], Socket1);
        #{} ->
            Socket1
    end,
    Id = maps:get(id, Socket),
    {[{text, json:encode(maps:get(events, Socket))}],
        State#{sockets => Sockets#{Id => arizona_socket:prune(Socket)}}}.

websocket_info(reload, Socket) ->
    io:format("[WebSocket] reload~n", []),
    {[{text, json:encode([[~"reload", []]])}], Socket};
websocket_info(Info, Socket) ->
    io:format("[WebSocket] info: ~p~n", [{Info, Socket}]),
    {[], Socket}.

terminate(Reason, Req, _State) ->
    io:format("[WebSocket] terminate: ~p~n", [{Reason, Req}]),
    send(terminate, {terminate, self()}),
    ok.

%% --------------------------------------------------------------------
%% API functions.
%% --------------------------------------------------------------------

subscribe(Event) ->
    gproc:reg({p, l, {?MODULE, Event}}).

%% --------------------------------------------------------------------
%% Internal functions.
%% --------------------------------------------------------------------

send(Event, Payload) ->
    gproc:send({p, l, {?MODULE, Event}}, Payload).

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


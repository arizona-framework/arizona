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
-export([init/2]).
-export([websocket_init/1]).
-export([websocket_handle/2]).
-export([websocket_info/2]).
-export([terminate/3]).

%% API functions.
-export([subscribe/1]).

-opaque state() :: {Params :: [{binary(), binary() | true}],
                    {Mod :: module(), Fun :: atom(), Opts :: arizona:route_opts()}}.
-export_type([state/0]).

%% --------------------------------------------------------------------
%% cowboy_handler callbacks.
%% --------------------------------------------------------------------

-spec init(Req, State) -> {cowboy_websocket, Req, State}
    when Req :: cowboy_req:req(),
         State :: state().
init(Req0, _State) ->
    Params = cowboy_req:parse_qs(Req0),
    {ok, Req, Env} = arizona_server:route(Req0),
    #{handler_opts := {Mod, Fun, Opts}} = Env,
    {cowboy_websocket, Req, {Params, {Mod, Fun, Opts}}}.

-spec websocket_init(State) -> {Events, State}
    when State :: state(),
         Events :: cowboy_websocket:commands().
websocket_init({Params, {Mod, Fun, Opts}}) ->
    Macros = maps:get(macros, Opts, #{}),
    Tpl = arizona_live_view:persist_get(Mod, Fun, Macros),
    Assigns = maps:get(assigns, Opts, #{}),
    {ok, {Html, Sockets}} = arizona_tpl_render:mount(Tpl, Assigns),
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


-spec websocket_handle(Event, State) -> {Events, State}
    when Event :: {text, binary()},
         Events :: cowboy_websocket:commands(),
         State :: state().
websocket_handle({text, Msg}, #{sockets := Sockets} = State) ->
    {Target, Event, Payload} = decode_msg(Msg),
    Socket0 = maps:get(Target, Sockets),
    View = maps:get(view, Socket0),
    {noreply, Socket1} = arizona_live_view:handle_event(View, Event, Payload, Socket0),
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

-spec websocket_info(Event, State) -> {Events, State}
    when Event :: {text, binary()},
         Events :: cowboy_websocket:commands(),
         State :: state().
websocket_info(reload, Socket) ->
    {[{text, json:encode([[~"reload", []]])}], Socket};
websocket_info(_Info, Socket) ->
    {[], Socket}.

-spec terminate(Reason, Req, State) -> ok
    when Reason :: term(),
         Req :: cowboy_req:req(),
         State :: state().
terminate(_Reason, _Req, _State) ->
    send(terminate, {terminate, self()}),
    ok.

%% --------------------------------------------------------------------
%% API functions.
%% --------------------------------------------------------------------

-spec subscribe(Event) -> ok
    when Event :: term().
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


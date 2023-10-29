%% @author William Fank Thomé <willilamthome@hotmail.com>
%% @copyright 2023 William Fank Thomé
%% @doc Socket.

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
-module(arizona_socket).

%% API
-export([ new/0
        , get_render_state/1
        , set_render_state/2
        , get_bindings/1
        , set_bindings/2
        , get_changes/1
        , set_changes/2
        , bind/2
        , bind/3
        , get_events/1
        , set_events/2
        , push_event/2
        , push_event/3
        , prune/1
        ]).

%% Types
-export_type([ t/0, event_name/0, event_payload/0, event/0 ]).

-record(socket, { render_state :: render_state()
                , bindings :: bindings()
                , changes :: bindings()
                , events :: [event()]
                }).

-opaque t() :: #socket{}.

-type render_state() :: arizona_template_adapter:state().
-type bindings() :: arizona_template_adapter:bindings().
-type event_name() :: binary() | atom().
-type event_payload() :: map().
-type event() :: {event_name(), event_payload()} | event_name().

%%%=====================================================================
%%% API
%%%=====================================================================

new() ->
    #socket{ render_state = undefined
           , bindings = #{}
           , changes = #{}
           , events = []
           }.

get_render_state(#socket{render_state = RenderState}) ->
    RenderState.

set_render_state(RenderState, Socket) ->
    Socket#socket{render_state = RenderState}.

get_bindings(#socket{bindings = Bindings}) ->
    Bindings.

set_bindings(Bindings, Socket) ->
    Socket#socket{bindings = Bindings}.

get_changes(#socket{changes = Changes}) ->
    Changes.

set_changes(Changes, Socket) ->
    Socket#socket{changes = Changes}.

bind(Bindings, Socket) ->
    Socket#socket{
        bindings = maps:merge(Socket#socket.bindings, Bindings),
        changes = maps:merge(Socket#socket.changes, Bindings)
    }.

bind(Key, Val, Socket) ->
    bind(#{Key => Val}, Socket).

get_events(#socket{events = Events}) ->
    Events.

set_events(Events, Socket) ->
    Socket#socket{events = Events}.

push_event(Event, #socket{events = Events} = Socket) ->
    set_events([Event | Events], Socket).

push_event(Event, Payload, #socket{events = Events} = Socket) ->
    set_events([[Event, Payload] | Events], Socket).

prune(Socket) ->
    Socket#socket{
        changes = #{},
        events = []
    }.

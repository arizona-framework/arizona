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
-module(arizona_socket).
-moduledoc """
Components state.
""".

%% API functions.
-export([new/3]).
-export([put_assigns/2]).
-ignore_xref([put_assigns/2]).
-export([put_assign/3]).
-ignore_xref([put_assign/3]).
-export([get_assign/2]).
-ignore_xref([get_assign/2]).
-export([get_assign/3]).
-ignore_xref([get_assign/3]).
-export([push_event/3]).
-export([prune/1]).

-opaque t() :: map().
-export_type([t/0]).

-opaque assigns() :: map().
-export_type([assigns/0]).

%% --------------------------------------------------------------------
%% API functions.
%% --------------------------------------------------------------------

-spec new(Id, View, Assigns) -> Socket
    when Id :: atom(),
         View :: module(),
         Assigns :: assigns(),
         Socket :: t().
new(Id, View, Assigns) ->
    #{
        id => Id,
        view => View,
        assigns => Assigns,
        events => [],
        changes => #{}
    }.

-spec put_assigns(Assigns, Socket) -> Socket
    when Assigns :: assigns(),
         Socket :: t().
put_assigns(Map, Socket) ->
    maps:fold(fun put_assign/3, Socket, Map).

-spec put_assign(Key, Value, Socket) -> Socket
    when Key :: atom(),
         Value :: term(),
         Socket :: t().
put_assign(Key, Value, Socket) ->
    #{assigns := Assigns, changes := Changes} = Socket,
    case Assigns of
        #{Key := Value} ->
            Socket;
        #{} ->
            Socket#{
                assigns => Assigns#{Key => Value},
                changes => Changes#{Key => Value}
            }
    end.

-spec get_assign(Key, Socket) -> Got
    when Key :: atom(),
         Socket :: t(),
         Got :: term().
get_assign(Key, Socket) ->
    #{assigns := Assigns} = Socket,
    maps:get(Key, Assigns).

-spec get_assign(Key, Socket, Default) -> Got
    when Key :: atom(),
         Socket :: t(),
         Default :: term(),
         Got :: term().
get_assign(Key, Socket, Default) ->
    #{assigns := Assigns} = Socket,
    maps:get(Key, Assigns, Default).

-spec push_event(EventName, Payload, Socket) -> Socket
    when EventName :: binary(),
         Payload :: arizona:payload(),
         Socket :: t().
push_event(Name, Payload, #{events := Events} = Socket) ->
    Socket#{events => [[Name, Payload] | Events]}.

-spec prune(Socket) -> Socket
    when Socket :: t().
prune(Socket) ->
    Socket#{
        events => [],
        changes => #{}
    }.


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
-export([new/3, assign/2, assign/3, push_event/3, prune/1]).

%% --------------------------------------------------------------------
%% API functions.
%% --------------------------------------------------------------------

new(Id, View, Assigns) ->
    #{
        id => Id,
        view => View,
        assigns => Assigns,
        events => [],
        changes => #{}
    }.

assign(Map, Socket) ->
    maps:fold(fun assign/3, Socket, Map).

assign(Key, Value, #{assigns := Assigns, changes := Changes} = Socket) ->
    case Assigns of
        #{Key := Value} ->
            Socket;
        #{} ->
            Socket#{
                assigns => Assigns#{Key => Value},
                changes => Changes#{Key => Value}
            }
    end.

push_event(Name, Payload, #{events := Events} = Socket) ->
    Socket#{events => [[Name, Payload] | Events]}.

prune(Socket) ->
    Socket#{
        events => [],
        changes => #{}
    }.


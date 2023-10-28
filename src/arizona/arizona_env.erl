%% @author William Fank Thomé <willilamthome@hotmail.com>
%% @copyright 2023 William Fank Thomé
%% @doc Environment helpers.

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
-module(arizona_env).

-compile({no_auto_import, [ get/1 ]}).

%% API
-export([ get_all/0, get/1, get/2 ]).

%% Server
-export([ get_server/0, get_server/1, get_server/2 ]).

%%======================================================================
%% API
%%======================================================================

get_all() ->
    proplists:to_map(application:get_all_env(arizona)).

get(Key) ->
    maps:get(Key, get_all()).

get(Key, Default) ->
    maps:get(Key, get_all(), Default).

%%======================================================================
%% Server
%%======================================================================

get_server() ->
    get(server).

get_server(Key) ->
    maps:get(Key, get_server()).

get_server(Key, Default) ->
    maps:get(Key, get_server(), Default).

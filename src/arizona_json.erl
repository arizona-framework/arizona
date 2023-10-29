%% @author William Fank Thomé <willilamthome@hotmail.com>
%% @copyright 2023 William Fank Thomé
%% @doc JSON parser and generator.

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
-module(arizona_json).

%% API
-export([ encode/1, decode/1, try_encode/1, try_decode/1 ]).

%% Macros
-define(ADAPTER, (arizona_env:get_json(adapter))).

%%%=====================================================================
%%% API
%%%=====================================================================

encode(Term) ->
    {ok, JSON} = try_encode(Term),
    JSON.

decode(JSON) ->
    {ok, Term} = try_decode(JSON),
    Term.

try_encode(Term) ->
    ?ADAPTER:encode(Term).

try_decode(JSON) ->
    ?ADAPTER:decode(JSON).

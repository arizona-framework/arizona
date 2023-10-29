%% @author William Fank Thomé <willilamthome@hotmail.com>
%% @copyright 2023 William Fank Thomé
%% @doc Euneus JSON adapter.

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
-module(arizona_json_adapter_euneus).

-behaviour(arizona_json_adapter).

%% arizona_json_adapter callbacks
-export([ encode/1, decode/1 ]).

%%%=====================================================================
%%% arizona_json_adapter callbacks
%%%=====================================================================

encode(Term) ->
    euneus:encode(Term).

decode(JSON) ->
    euneus:decode(JSON).

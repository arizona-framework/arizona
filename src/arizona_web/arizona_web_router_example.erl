%% @author William Fank Thomé <willilamthome@hotmail.com>
%% @copyright 2023 William Fank Thomé
%% @doc Router example.

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
-module(arizona_web_router_example).

-behaviour(arizona_router_adapter).

%% arizona_router_adapter callbacks
-export([ match/2 ]).

%%%=====================================================================
%%% arizona_router_adapter callbacks
%%%=====================================================================

match(get, [ ]) ->
    {{live, arizona_web_live_view_example, []}, #{}};
match(Method, Path) ->
    {{arizona_web_controller_error, invalid_route, [Method, Path]}, #{}}.

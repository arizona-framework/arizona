%% @author William Fank Thomé <willilamthome@hotmail.com>
%% @copyright 2023 William Fank Thomé
%% @doc Template.

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
-module(arizona_template).

%% API
-export([ compile/1, bind/2, render/1, diff/1 ]).

%% Macros
-define(ADAPTER, (arizona_env:get_template(adapter))).

%%%=====================================================================
%%% API
%%%=====================================================================

compile(Input) ->
    ?ADAPTER:compile(Input).

bind(Bindings, State) ->
    ?ADAPTER:bind(Bindings, State).

render(State) ->
    ?ADAPTER:render(State).

diff(State) ->
    case ?ADAPTER:diff(State) of
        ChangedParts when map_size(ChangedParts) > 0 ->
            {ok, ChangedParts};
        #{} ->
            none
    end.

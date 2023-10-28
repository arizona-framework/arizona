%% @author William Fank Thomé <willilamthome@hotmail.com>
%% @copyright 2023 William Fank Thomé
%% @doc Cowboy server adapter.

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
-module(arizona_server_adapter_cowboy).

-behaviour(arizona_server_adapter).

%% arizona_server_adapter callbacks
-export([ start/1 ]).

%% Macros
-define(LISTENER, arizona_http_listener).

%%----------------------------------------------------------------------
%% ARIZONA_SERVER_ADAPTER CALLBACKS
%%----------------------------------------------------------------------

start(Args) ->
    Routes = [{'_', ?MODULE, []}],
    Dispatch = cowboy_router:compile([{'_', Routes}]),
    RanchOpts = [
        {port, maps:get(port, Args, 8080)}
    ],
    Opts = #{env => #{dispatch => Dispatch}},
    case cowboy:start_clear(?LISTENER, RanchOpts, Opts) of
        {ok, _} ->
            ok;
        {error, Reason} ->
            {error, Reason}
    end.

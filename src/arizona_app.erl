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
-module(arizona_app).
-moduledoc false.

-behaviour(application).

%% Application callbacks.
-export([start/2, stop/1]).

%% --------------------------------------------------------------------
%% Application callbacks.
%% --------------------------------------------------------------------

start(_StartType, _StartArgs) ->
    Dispatch = cowboy_router:compile([{'_', [
        % Static
        {"/favicon.ico", cowboy_static, {priv_file, arizona, "static/favicon.ico"}},
        {"/robots.txt", cowboy_static, {priv_file, arizona, "static/robots.txt"}},
        {"/assets/[...]", cowboy_static, {priv_dir, arizona, "static/assets"}},
        % Routes
        {"/", arizona_handler, []}
    ]}]),
    {ok, _} = cowboy:start_clear(arizona_http_listener,
        [{port, 8080}],
        #{env => #{dispatch => Dispatch}}
    ),
    arizona_sup:start_link().

stop(_State) ->
    ok.


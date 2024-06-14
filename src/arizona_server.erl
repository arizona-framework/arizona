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
-module(arizona_server).
-moduledoc false.

%% API functions.
-export([start/0, start/1]).

%% Macros
-define(LISTENER, arizona_http_listener).
-define(PERSISTENT_KEY, arizona_dispatch).

%% --------------------------------------------------------------------
%% API functions.
%% --------------------------------------------------------------------

start() ->
    start(#{
        schema => http,
        ip => {127,0,0,1},
        port => 8080
    }).

start(Args) ->
    Dispatch = cowboy_router:compile([{'_', [
        % Static
        {"/favicon.ico", cowboy_static, {priv_file, arizona, "static/favicon.ico"}},
        {"/robots.txt", cowboy_static, {priv_file, arizona, "static/robots.txt"}},
        {"/assets/[...]", cowboy_static, {priv_dir, arizona, "static/assets"}},
        % Handlers
        {"/websocket", arizona_websocket, []},
        {'_', arizona_handler, []}
    ]}]),
    persistent_term:put(?PERSISTENT_KEY, Dispatch),
    {ok, Pid} = cowboy:start_clear(?LISTENER,
        [{port, maps:get(port, Args)}],
        #{env => #{dispatch => {persistent_term, ?PERSISTENT_KEY}}}
    ),
    io:format("Arizona is running at ~s~n", [format_url(Args)]),
    {ok, Pid}.

%% --------------------------------------------------------------------
%% Internal functions.
%% --------------------------------------------------------------------

format_url(#{schema := Schema, ip := IP, port := Port}) ->
    io_lib:format("~w://~s:~p", [Schema, ip_to_string(IP), Port]).

ip_to_string(IP) ->
    case inet:ntoa(IP) of
        {error, einval} ->
            error({invalid_ip, IP});
        String ->
            String
    end.


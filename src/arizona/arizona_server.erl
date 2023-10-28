%% @author William Fank Thomé <willilamthome@hotmail.com>
%% @copyright 2023 William Fank Thomé
%% @doc Server.

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
-module(arizona_server).

%% API
-export([ start/0, stop/1 ]).

%% Macros
-define(ADAPTER, (arizona_env:get_server(adapter))).

%%%=====================================================================
%%% API
%%%=====================================================================

start() ->
    Args = arizona_env:get_server(args),
    case ?ADAPTER:start(Args) of
        ok ->
            URL = url_to_string(maps:get(url, Args)),
            io:format("Arizona is running at ~s~n", [URL]),
            ok;
        {error, Reason} ->
            {error, Reason}
    end.

stop(State) ->
    ?ADAPTER:stop(State).

%%%=====================================================================
%%% Internal functions
%%%=====================================================================

url_to_string(#{schema := Schema, ip := IP, port := Port}) ->
    io_lib:format("~w://~s:~p", [Schema, ip_to_string(IP), Port]).

ip_to_string(IP) ->
    case inet:ntoa(IP) of
        {error, einval} ->
            error({invalid_ip, IP});
        String ->
            String
    end.

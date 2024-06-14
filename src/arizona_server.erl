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
-export([start/0, start/1, route/1]).

%% Macros
-define(LISTENER, arizona_http_listener).
-define(PERSIST_KEY, arizona_dispatch).

%% --------------------------------------------------------------------
%% API functions.
%% --------------------------------------------------------------------

start() ->
    start(#{}).

start(Opts) ->
    do_start(normalize_opts(Opts)).

route(Req) ->
    #{path := Path} = cowboy_req:match_qs([path], Req),
    cowboy_router:execute(Req#{path => Path},
                          #{dispatch => {persistent_term, ?PERSIST_KEY}}).

%% --------------------------------------------------------------------
%% Internal functions.
%% --------------------------------------------------------------------

do_start(Opts) ->
    Dispatch = cowboy_router:compile([{'_', [
        % Static
        {"/favicon.ico", cowboy_static, {priv_file, arizona, "static/favicon.ico"}},
        {"/robots.txt", cowboy_static, {priv_file, arizona, "static/robots.txt"}},
        {"/assets/[...]", cowboy_static, {priv_dir, arizona, "static/assets"}},
        % Handlers
        {"/websocket", arizona_websocket, []}
    ] ++ maps:get(routes, Opts)}]),
    persistent_term:put(?PERSIST_KEY, Dispatch),
    Url = maps:get(url, Opts),
    {ok, Pid} = cowboy:start_clear(?LISTENER,
        [{port, maps:get(port, Url)}],
        #{env => #{dispatch => {persistent_term, ?PERSIST_KEY}}}
    ),
    io:format("Arizona is running at ~s~n", [format_url(Url)]),
    {ok, Pid}.

normalize_opts(Opts) when is_map(Opts) ->
    #{
        url => normalize_url(maps:get(url, Opts, #{})),
        routes => maps:get(routes, Opts, [])
     }.

normalize_url(Url) when is_map(Url) ->
    maps:merge(default_url(), Url).

default_url() ->
    #{
        schema => http,
        ip => {127,0,0,1},
        port => 8080
    }.

format_url(#{schema := Schema, ip := IP, port := Port}) ->
    io_lib:format("~w://~s:~p", [Schema, ip_to_string(IP), Port]).

ip_to_string(IP) ->
    case inet:ntoa(IP) of
        {error, einval} ->
            error({invalid_ip, IP});
        String ->
            String
    end.


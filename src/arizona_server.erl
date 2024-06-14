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
    do_start(norm_opts(Opts)).

route(Req) ->
    #{path := Path} = cowboy_req:match_qs([path], Req),
    cowboy_router:execute(Req#{path => Path},
                          #{dispatch => {persistent_term, ?PERSIST_KEY}}).

%% --------------------------------------------------------------------
%% Internal functions.
%% --------------------------------------------------------------------

do_start(#{scheme := http, transport := Transport, proto := Proto}) ->
    {ok, Pid} = cowboy:start_clear(?LISTENER, Transport, Proto),
    print_running(http),
    {ok, Pid};
do_start(#{scheme := https, transport := Transport, proto := Proto}) ->
    {ok, Pid} = cowboy:start_tls(?LISTENER, Transport, Proto),
    print_running(https),
    {ok, Pid}.

print_running(Scheme) ->
    Info = ranch:info(?LISTENER),
    {ip, Ip} = proplists:lookup(ip, Info),
    {port, Port} = proplists:lookup(port, Info),
    io:format("Arizona is running at ~w://~s:~p~n",
              [Scheme, ip_to_str(Ip), Port]).

ip_to_str(Ip) ->
    case inet:ntoa(Ip) of
        {error, einval} ->
            error({invalid_ip, Ip});
        Str ->
            Str
    end.


norm_opts(Opts) when is_map(Opts) ->
    #{
        scheme => maps:get(scheme, Opts, http),
        transport => norm_transport_opts(maps:get(transport, Opts, [])),
        proto => norm_proto_opts(maps:get(proto, Opts, #{}),
                                 maps:get(host, Opts, '_'),
                                 maps:get(routes, Opts, []))
    }.

norm_transport_opts([]) ->
    [{port, 8080}];
norm_transport_opts(Opts) when is_list(Opts) ->
    Opts.

norm_proto_opts(Opts, Host, Routes) when is_map(Opts) ->
    Dispatch = cowboy_router:compile([{Host, [
        {"/favicon.ico", cowboy_static, {priv_file, arizona, "static/favicon.ico"}},
        {"/robots.txt", cowboy_static, {priv_file, arizona, "static/robots.txt"}},
        {"/assets/[...]", cowboy_static, {priv_dir, arizona, "static/assets"}},
        {"/websocket", arizona_websocket, []}
    ] ++ Routes}]),
    persistent_term:put(?PERSIST_KEY, Dispatch),
    Opts#{env => #{dispatch => {persistent_term, ?PERSIST_KEY}}}.


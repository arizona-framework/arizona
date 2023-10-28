%% @author William Fank Thomé <willilamthome@hotmail.com>
%% @copyright 2023 William Fank Thomé
%% @doc Cowboy handler.

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
-module(arizona_server_adapter_cowboy_handler).

-behaviour(cowboy_handler).

%% cowboy_handler callbacks
-export([ init/2 ]).

%%%=====================================================================
%%% cowboy_handler callbacks
%%%=====================================================================

init(Req, State) ->
    Method = normalize_method(cowboy_req:method(Req)),
    Path = normalize_path(cowboy_req:path(Req)),
    {ok, Res} = arizona_handler:handle(Method, Path, Req),
    {ok, Res, State}.

%%%=====================================================================
%%% Internal functions
%%%=====================================================================

normalize_method(<<"GET">>) -> get;
normalize_method(<<"POST">>) -> post;
normalize_method(<<"PATCH">>) -> patch;
normalize_method(<<"DELETE">>) -> delete;
normalize_method(<<"PUT">>) -> put;
normalize_method(<<"CONNECT">>) -> connect;
normalize_method(<<"HEAD">>) -> head;
normalize_method(<<"OPTIONS">>) -> options;
normalize_method(<<"TRACE">>) -> trace.

normalize_path(Path) ->
    binary:split(Path, <<"/">>, [global, trim_all]).

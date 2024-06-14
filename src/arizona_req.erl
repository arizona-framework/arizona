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
-module(arizona_req).
-moduledoc false.

%% API functions.
-export([method/1, path/1, normalize_path/1, route/1]).

%% --------------------------------------------------------------------
%% API functions.
%% --------------------------------------------------------------------

method(Req) ->
    normalize_method(cowboy_req:method(Req)).

path(Req) ->
    normalize_path(cowboy_req:path(Req)).

normalize_path(Path) ->
    binary:split(Path, <<"/">>, [global, trim_all]).

route(Req) ->
    arizona_route:match(method(Req), path(Req)).

%% --------------------------------------------------------------------
%% Internal functions.
%% --------------------------------------------------------------------

normalize_method(<<"GET">>) -> get;
normalize_method(<<"POST">>) -> post;
normalize_method(<<"PATCH">>) -> patch;
normalize_method(<<"DELETE">>) -> delete;
normalize_method(<<"PUT">>) -> put;
normalize_method(<<"CONNECT">>) -> connect;
normalize_method(<<"HEAD">>) -> head;
normalize_method(<<"OPTIONS">>) -> options;
normalize_method(<<"TRACE">>) -> trace.


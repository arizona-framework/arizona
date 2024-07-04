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
-module(arizona_live_handler).
-moduledoc false.

-behaviour(cowboy_handler).

%% cowboy_handler callbacks.
-export([init/2]).

%% --------------------------------------------------------------------
%% cowboy_handler callbacks.
%% --------------------------------------------------------------------

-spec init(Req, State) -> {ok, Req, State}
    when Req :: cowboy_req:req(),
         State :: {Mod, Fun, Opts},
         Mod :: module(),
         Fun :: atom(),
         Opts :: arizona:route_opts().
init(Req0, {Mod, Fun, Opts} = State) ->
    Macros = maps:get(macros, Opts, #{}),
    Tpl = arizona_tpl_compile:compile(Mod, Fun, Macros),
    Assigns = maps:get(assigns, Opts, #{}),
    Html = arizona_tpl_render:render_block(Tpl, Assigns),
    Headers = #{<<"content-type">> => <<"text/html">>},
    Req = cowboy_req:reply(200, Headers, Html, Req0),
    {ok, Req, State}.

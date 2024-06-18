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
-module(arizona_live_view_SUITE).

%% ct callbacks.
-export([all/0, init_per_suite/1, end_per_suite/1]).

%% arizona_live_view callbacks.
-export([mount/1, render/1]).

%% Test cases.
-export([hello_world/1]).

%% Libs
-include_lib("common_test/include/ct.hrl").
-include_lib("stdlib/include/assert.hrl").
-include("live_view.hrl").
-include("live_view_test.hrl").

%% --------------------------------------------------------------------
%% arizona_live_view callbacks.
%% --------------------------------------------------------------------

all() ->
    [hello_world].

init_per_suite(Config) ->
    application:set_env([{arizona, [
        {endpoint, #{
            routes => [
                {"/helloworld", arizona_live_handler, {?MODULE, render, #{}}}
            ]
        }}
    ]}]),
    {ok, _} = application:ensure_all_started(arizona),
    Config.

end_per_suite(Config) ->
    Config.

%% --------------------------------------------------------------------
%% arizona_live_view callbacks.
%% --------------------------------------------------------------------

mount(Socket) ->
    {ok, Socket}.

render(Macros) ->
    ?LV(~s"""
    <html>
    <head>
    </head>
    <body>
        Hello, World!
    </body>
    </html>
    """).

%% --------------------------------------------------------------------
%% Test cases.
%% --------------------------------------------------------------------

hello_world(Config) when is_list(Config) ->
    {ok, Resp0} = httpc:request("http://localhost:8080/notfound"),
    ?assertHttpStatus(404, Resp0),
    {ok, Resp1} = httpc:request("http://localhost:8080/helloworld"),
    ?assertHttpStatus(200, Resp1),
    ?assertHttpBody("Hello, World!", Resp1).


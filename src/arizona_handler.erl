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
-module(arizona_handler).
-moduledoc false.

-behaviour(cowboy_handler).

%% cowboy_handler callbacks.
-export([init/2]).

%% Example functions.
-export([render/1]).

-include("live_view.hrl").

%% --------------------------------------------------------------------
%% cowboy_handler callbacks.
%% --------------------------------------------------------------------

init(Req0, State) ->
    Macros = #{title => <<"Arizona">>},
    {ok, Tpl} = arizona_live_view:compile(?MODULE, Macros),
    Assigns = #{count => 0},
    Html = arizona_tpl_render:render_block(Tpl, Assigns),
    Headers = #{<<"content-type">> => <<"text/html">>},
    Req = cowboy_req:reply(200, Headers, Html, Req0),
    {ok, Req, State}.

%% --------------------------------------------------------------------
%% Example functions.
%% --------------------------------------------------------------------

render(Macros) ->
    ?LV(~s"""
    {% TODO: Handle <!DOCTYPE html> in the parser. }
    <html lang="en">
    <head>
        <meta charset="UTF-8"/>
        <meta http-equiv="X-UA-Compatible" content="IE=edge"/>
        <meta name="viewport" content="width=device-width, initial-scale=1.0"/>
        <title>{_@title}</title>
        <script src="assets/js/arizona.js"></script>
    </head>
    <body>
        <div>Count: {_@count}</div>
        <button type="button" :onclick="incr">
            Increment
        </button>
    </body>
    </html>
    """).


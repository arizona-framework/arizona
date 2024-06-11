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
-export([mount/0, render/1, counter/1, handle_event/3]).

-include("live_view.hrl").

%% --------------------------------------------------------------------
%% cowboy_handler callbacks.
%% --------------------------------------------------------------------

init(Req0, State) ->
    View = ?MODULE,
    Tpl = arizona_live_view:persist_get(View, #{}),
    {ok, Assigns} = View:mount(),
    Html = arizona_tpl_render:render_block(Tpl, Assigns),
    Headers = #{~"content-type" => ~"text/html"},
    Req = cowboy_req:reply(200, Headers, Html, Req0),
    {ok, Req, State}.

%% --------------------------------------------------------------------
%% Example functions.
%% --------------------------------------------------------------------

mount() ->
    {ok, #{count => 0}}.

render(Macros0) ->
    Macros = Macros0#{
        title => maps:get(title, Macros0, ~"Arizona")
    },
    ?LV(~"""
    {% TODO: Handle <!DOCTYPE html> in the parser. }
    <html lang="en">
    <head>
        <meta charset="UTF-8"/>
        <meta http-equiv="X-UA-Compatible" content="IE=edge"/>
        <meta name="viewport" content="width=device-width, initial-scale=1.0"/>
        <title>{_@title}</title>
        <script src="assets/js/morphdom.min.js"></script>
        <script src="assets/js/arizona.js"></script>
        <script src="assets/js/example.js"></script>
    </head>
    <body>
        <.arizona_handler:counter
            count={_@count}
            btn_text="Increment"
        />
    </body>
    </html>
    """).

counter(Macros) ->
    ?LV(~s"""
    {% TODO: <div :statefull>...</div> }
    <div>Count: {_@count}</div>
    {% TODO: :onclick={_@event} }
    <button type="button" :onclick="incr">
        {_@btn_text}
    </button>
    """).

handle_event(<<"incr">>, #{}, #{assigns := Assigns} = Socket) ->
    io:format("[LV] incr: ~p~n", [Socket]),
    Count = maps:get(count, Assigns) + 1,
    Changes = #{count => Count},
    View = maps:get(view, Socket),
    Tpl = arizona_live_view:persist_get(View, #{}),
    Patch = [[K, V] || {K, V} <- arizona_tpl_render:render_changes(Tpl, Changes, Assigns)],
    Events = [[~"patch", [~"root", Patch]]],
    {Events, Socket#{
        assigns => Assigns#{count => Count}
    }}.


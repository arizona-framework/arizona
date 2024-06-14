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
-export([mount/1, render/1, counter/1, button/1, handle_event/3]).

-include("live_view.hrl").

%% --------------------------------------------------------------------
%% cowboy_handler callbacks.
%% --------------------------------------------------------------------

% TODO: View from rout.
init(Req0, State) ->
    View = ?MODULE,
    Tpl = arizona_live_view:persist_get(View, #{}),
    Assigns = #{},
    Html = arizona_tpl_render:render_block(Tpl, Assigns),
    Headers = #{~"content-type" => ~"text/html"},
    Req = cowboy_req:reply(200, Headers, Html, Req0),
    {ok, Req, State}.

%% --------------------------------------------------------------------
%% Example functions.
%% --------------------------------------------------------------------

mount(#{assigns := Assigns} = Socket) ->
    Count = maps:get(count, Assigns, 0),
    {ok, arizona_websocket:assign(count, Count, Socket)}.

render(Macros0) ->
    Macros = Macros0#{
        title => maps:get(title, Macros0, ~"Arizona")
    },
    ?LV(~"""
    <!DOCTYPE html>
    <html lang="en">
    <head>
        <meta charset="UTF-8">
        <meta http-equiv="X-UA-Compatible" content="IE=edge">
        <meta name="viewport" content="width=device-width, initial-scale=1.0">
        <title>{_@title}</title>
        <script src="assets/js/morphdom.min.js"></script>
        <script src="assets/js/arizona.js"></script>
        <script src="assets/js/example.js"></script>
    </head>
    <body>
        <.counter
            count={_@count}
            btn_text="Increment #1"
            event="incr"
        />
        <.counter
            count={99}
            btn_text="Increment #2"
            event="decr"
        />
    </body>
    </html>
    """).

counter(Macros) ->
    ?LV(~s"""
    <div :stateful>
        <div>Count: {_@count}</div>
        <.button event={_@event} text={_@btn_text} />
    </div>
    """).

button(Macros) ->
    ?LV(~s"""
    {% NOTE: On this example, :onclick is and expression to be }
    {%       dynamic. It could be just, e.g., :onclick="incr". }
    <button type="button" :onclick={arizona_js:send(_@event)}>
        {_@text}
    </button>
    """).

handle_event(<<"incr">>, #{}, #{assigns := Assigns} = Socket) ->
    Count = maps:get(count, Assigns) + 1,
    {noreply, arizona_websocket:assign(count, Count, Socket)};
handle_event(<<"decr">>, #{}, #{assigns := Assigns} = Socket) ->
    Count = maps:get(count, Assigns) - 1,
    {noreply, arizona_websocket:assign(count, Count, Socket)}.


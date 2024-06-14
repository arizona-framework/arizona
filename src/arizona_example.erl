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
-module(arizona_example).
-moduledoc false.

%% arizona_live_view callbacks.
-export([mount/1, render/1, handle_event/3]).

%% Component functions.
-export([counter/1, button/1]).

%% Libs
-include("live_view.hrl").

%% --------------------------------------------------------------------
%% arizona_live_view callbacks.
%% --------------------------------------------------------------------

mount(#{assigns := Assigns} = Socket) ->
    Count = maps:get(count, Assigns, 0),
    {ok, arizona_socket:assign(count, Count, Socket)}.

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

handle_event(<<"incr">>, #{}, #{assigns := Assigns} = Socket) ->
    Count = maps:get(count, Assigns) + 1,
    {noreply, arizona_socket:assign(count, Count, Socket)};
handle_event(<<"decr">>, #{}, #{assigns := Assigns} = Socket) ->
    Count = maps:get(count, Assigns) - 1,
    {noreply, arizona_socket:assign(count, Count, Socket)}.

%% --------------------------------------------------------------------
%% Component functions.
%% --------------------------------------------------------------------

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


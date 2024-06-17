-module(arzex_live_counter).
-behaviour(arizona_live_view).

%% arizona_live_view callbacks.
-export([mount/1, render/1, handle_event/3]).

%% Component functions.
-export([counter/1, button/1]).

%% Libs
-include_lib("arizona/include/live_view.hrl").

%% --------------------------------------------------------------------
%% arizona_live_view callbacks.
%% --------------------------------------------------------------------

mount(#{assigns := Assigns} = Socket) ->
    Count = maps:get(count, Assigns, 55),
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

        {% TODO: Auto-inject Arizona scripts. }
        <script src="assets/js/morphdom.min.js"></script>
        <script src="assets/js/arizona.js"></script>
        <script src="assets/js/arizona-live-reload.js"></script>

        <script src="assets/js/main.js"></script>
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


-module(arizona_simple_live_component).
-behaviour(arizona_live).
-export([mount/2, render/1]).

%% Simple component for testing basic arizona_handler functionality

mount(_Req, Socket) ->
    Socket.

render(Socket) ->
    arizona_html:render_live(~"""
    <div class="simple-component">
        <h1>Simple Live Component</h1>
        <p>This is a basic arizona_live component.</p>
    </div>
    """, Socket).

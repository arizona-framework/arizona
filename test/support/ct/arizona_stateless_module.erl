-module(arizona_stateless_module).
%-compile({parse_transform, arizona_parse_transform}).
-arizona_parse_transform([basic_render/1]).
-arizona_parse_transform([render_with_bindings/1]).
-arizona_parse_transform([render_simple/1]).
-export([basic_render/1]).
-export([render_with_bindings/1]).
-export([render_simple/1]).

%% Basic stateless component render functions for testing

basic_render(Socket) ->
    %% Return socket from arizona_html:render_stateless
    arizona_html:render_stateless(~"""
    <div class="stateless-basic">
        <h2>Basic Stateless Component</h2>
    </div>
    """, Socket).

render_with_bindings(Socket) ->
    %% Return socket from arizona_html:render_stateless
    arizona_html:render_stateless(~"""
    <div class="stateless-with-bindings">
        <h2>{arizona_socket:get_temp_binding(title, Socket)}</h2>
        <p>{arizona_socket:get_temp_binding(content, Socket)}</p>
    </div>
    """, Socket).

render_simple(Socket) ->
    %% Return socket from arizona_html:render_stateless
    arizona_html:render_stateless(~"""
    <span>{arizona_socket:get_temp_binding(message, Socket)}</span>
    """, Socket).

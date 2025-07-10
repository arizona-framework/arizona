-module(arizona_stateless_module).
-compile({parse_transform, arizona_parse_transform}).
-compile([export_all, nowarn_export_all]).

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

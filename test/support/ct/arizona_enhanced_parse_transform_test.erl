-module(arizona_enhanced_parse_transform_test).
-compile([nowarn_unused_vars]).
%-compile({parse_transform, arizona_parse_transform}).
-arizona_parse_transform([render/1]).

-export([render/1]).

render(Socket) ->
    Count = arizona_socket:get_binding(count, Socket),
    UserName = arizona_socket:get_binding(user_name, Socket, "Anonymous"),
    arizona_html:render_stateful(~"""
    <div class="container">
        <h1>Hello {UserName}!</h1>
        <p>Count: {Count}</p>
        <button onclick="increment">+</button>
    </div>
    """, Socket).

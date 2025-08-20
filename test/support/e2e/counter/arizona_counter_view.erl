-module(arizona_counter_view).
-behaviour(arizona_view).
-compile({parse_transform, arizona_parse_transform}).
-export([mount/1]).
-export([render/1]).

mount(Req) ->
    Bindings = #{id => ~"view"},
    Layout =
        {arizona_counter_layout, render, main_content, #{
            active_url => arizona_request:get_path(Req)
        }},
    arizona_view:new(?MODULE, Bindings, Layout).

render(Bindings) ->
    arizona_template:from_string(~"""
    <div id="{arizona_template:get_binding(id, Bindings)}">
        {arizona_template:render_stateful(arizona_counter_stateful, #{
            id => ~"counter",
            count => 0
        })}
    </div>
    """).

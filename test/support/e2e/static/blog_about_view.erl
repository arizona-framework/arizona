-module(blog_about_view).
-behaviour(arizona_view).
-compile({parse_transform, arizona_parse_transform}).
-export([mount/1, render/1]).

mount(Req) ->
    {ReqBindings, _Req} = arizona_request:get_bindings(Req),
    Bindings = #{id => ~"about"},
    Layout =
        {blog_layout, render, main_content, #{
            page_title => maps:get(title, ReqBindings),
            nav_active => ~"about"
        }},
    arizona_view:new(?MODULE, Bindings, Layout).

render(Bindings) ->
    arizona_template:from_string(~"""
    <div id="{arizona_template:get_binding(id, Bindings)}" class="about">
        <h1>About Me</h1>
        <p>This is a static blog built with Arizona framework!</p>
        <p>Arizona provides a great foundation for both live applications and static sites.</p>
    </div>
    """).

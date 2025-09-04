-module(arizona_blog_about_view).
-behaviour(arizona_view).
-compile({parse_transform, arizona_parse_transform}).
-export([mount/2, render/1]).

mount(PageBindings, _Req) ->
    Bindings = #{id => ~"about"},
    Layout =
        {arizona_blog_layout, render, main_content, #{
            page_title => maps:get(title, PageBindings),
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

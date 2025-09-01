-module(blog_home_view).
-behaviour(arizona_view).
-compile({parse_transform, arizona_parse_transform}).
-export([mount/2, render/1]).

mount(PageBindings, _Req) ->
    Bindings = #{
        id => ~"home",
        posts => [
            #{
                title => ~"Hello World",
                url => ~"/post/hello-world"
            },
            #{
                title => ~"Arizona Static Site Generation",
                url => ~"/post/arizona-static"
            }
        ]
    },
    Layout =
        {blog_layout, render, main_content, #{
            page_title => maps:get(title, PageBindings),
            nav_active => ~"home"
        }},
    arizona_view:new(?MODULE, Bindings, Layout).

render(Bindings) ->
    arizona_template:from_string(~""""
    <div id="{arizona_template:get_binding(id, Bindings)}" class="home">
        <h1>Welcome to My Blog</h1>
        <div class="posts">
            {arizona_template:render_list(fun(#{url := URL, title := Title}) ->
                arizona_template:from_string(~"""
                <article class="post-preview">
                    <h2><a href="{URL}">{Title}</a></h2>
                </article>
                """)
            end, arizona_template:get_binding(posts, Bindings))}
        </div>
    </div>
    """").

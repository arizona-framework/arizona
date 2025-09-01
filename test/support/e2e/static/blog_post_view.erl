-module(blog_post_view).
-behaviour(arizona_view).
-compile({parse_transform, arizona_parse_transform}).
-export([mount/2, render/1]).

mount(Posts, Req) ->
    {ReqBindings, _UpdatedReq} = arizona_request:get_bindings(Req),
    PostId = maps:get(post_id, ReqBindings),
    case Posts of
        #{PostId := Post} ->
            Bindings = Post#{
                id => ~"post"
            },
            Layout =
                {blog_layout, render, main_content, #{
                    page_title => arizona_template:get_binding(title, Post),
                    nav_active => ~"blog"
                }},
            arizona_view:new(?MODULE, Bindings, Layout);
        #{} ->
            error({404, PostId})
    end.

render(Bindings) ->
    arizona_template:from_string(~""""
    <article id="{arizona_template:get_binding(id, Bindings)}" class="post">
        <header>
            <h1>{arizona_template:get_binding(title, Bindings)}</h1>
        </header>
        <div class="content">
            <p>{arizona_template:get_binding(content, Bindings)}</p>
        </div>
    </article>
    """").

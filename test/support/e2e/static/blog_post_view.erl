-module(blog_post_view).
-behaviour(arizona_view).
-compile({parse_transform, arizona_parse_transform}).
-export([mount/1, render/1]).

mount(Req) ->
    {ReqBindings, _Req} = arizona_request:get_bindings(Req),
    % In a real implementation, you might load post data from files or database
    Bindings = ReqBindings#{
        id => ~"post"
        % title, post_id, content will be provided via static config bindings
    },
    Layout =
        {blog_layout, render, main_content, #{
            page_title => arizona_template:get_binding(title, Bindings),
            nav_active => ~"blog"
        }},
    arizona_view:new(?MODULE, Bindings, Layout).

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

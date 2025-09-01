-module(blog_layout).
-compile({parse_transform, arizona_parse_transform}).
-export([render/1]).

render(Bindings) ->
    arizona_template:from_string(~""""
    <!DOCTYPE html>
    <html lang="en">
    <head>
        <meta charset="UTF-8">
        <meta name="viewport" content="width=device-width, initial-scale=1.0">
        <title>{arizona_template:get_binding(page_title, Bindings)}</title>
        <style>
            body \{ font-family: Arial, sans-serif; max-width: 800px; margin: 0 auto; padding: 20px; }
            nav \{ margin-bottom: 2rem; }
            nav a \{ margin-right: 1rem; text-decoration: none; color: #333; }
            nav a.active \{ font-weight: bold; color: #007acc; }
            .post-preview \{ margin-bottom: 1.5rem; border-bottom: 1px solid #eee; padding-bottom: 1rem; }
            .post-preview h2 \{ margin: 0 0 0.5rem 0; }
            .post \{ max-width: 700px; }
            .post header \{ margin-bottom: 1.5rem; }
            .post h1 \{ color: #333; }
            footer \{ margin-top: 3rem; padding-top: 2rem; border-top: 1px solid #eee; color: #666; text-align: center; }
        </style>
    </head>
    <body>
        <nav>
            {
                NavActive = arizona_template:get_binding(nav_active, Bindings),
                arizona_template:render_list(fun({URL, Slug, Label}) ->
                   arizona_template:from_string(~"""
                   <a
                       href="{URL}"
                       class="{case NavActive of Slug -> ~"active"; _ -> ~"" end}"
                   >
                       {Label}
                   </a>
                   """)
                end, links())
            }
        </nav>
        <main>
            {arizona_template:render_slot(arizona_template:get_binding(main_content, Bindings))}
        </main>
        <footer>
            <p>Generated with Arizona Static Site Generator</p>
        </footer>
    </body>
    </html>
    """").

% Internal functions

links() ->
    [
        {~"/", ~"home", ~"Home"},
        {~"/about", ~"about", ~"About"}
    ].

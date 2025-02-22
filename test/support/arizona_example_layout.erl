-module(arizona_example_layout).

-export([render/1]).

render(View) ->
    arizona_render:component_template(View, ~"""
    <!DOCTYPE html>
    <html lang="en">
    <head>
        <meta charset="UTF-8">
        <meta http-equiv="X-UA-Compatible" content="IE=edge">
        <meta name="viewport" content="width=device-width, initial-scale=1.0">
        <title>{arizona_view:get_assign(title, View)}</title>
        {arizona_html:scripts()}
    </head>
    <body>
        {arizona_view:get_assign(inner_content, View)}
    </body>
    </html>
    """).

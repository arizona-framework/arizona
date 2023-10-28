-module(arizona_web_template_example).

%% API
-export([ render/2 ]).

%% Libs
-include("arizona_live_view.hrl").

%%%=====================================================================
%%% API
%%%=====================================================================

render(Bindings0, InnerContent) ->
    Bindings = Bindings0#{
        title => <<"Arizona Example">>,
        inner_content => InnerContent
    },
    ?LV(<<"
    <!DOCTYPE html>
    <html lang=\"en\">
    <head>
        <meta charset=\"UTF-8\">
        <meta http-equiv=\"X-UA-Compatible\" content=\"IE=edge\">
        <meta name=\"viewport\" content=\"width=device-width, initial-scale=1.0\">
        <title><%= @title .%></title>
        <script src=\"assets/main.js\"></script>
    </head>
    <body>
        <%= @inner_content .%>
    </body>
    </html>
    ">>).

-module(test_layout).

%% Layout callbacks
-export([render/1]).

%% Simple test layout for arizona_live testing

render(Socket) ->
    arizona_html:render_stateless(~"""
    <html>
    <head>
        <title>Arizona Live Test</title>
    </head>
    <body>
        <header>
            <h1>Test Layout</h1>
        </header>
        <main>
            {arizona_html:render_slot(main_content, Socket)}
        </main>
        <footer>
            <p>Arizona Framework</p>
        </footer>
    </body>
    </html>
    """, Socket).

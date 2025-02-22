-module(arizona_view_handler_SUITE).
-behaviour(ct_suite).
-behaviour(arizona_view).
-include_lib("stdlib/include/assert.hrl").
-compile([export_all, nowarn_export_all]).

%% --------------------------------------------------------------------
%% Behaviour (ct_suite) callbacks
%% --------------------------------------------------------------------

all() ->
    [hello_world].

init_per_suite(Config) ->
    application:set_env([
        {arizona, [
            {endpoint, #{
                routes => [
                    {"/hello-world", arizona_view_handler,
                        {?MODULE,
                            #{
                                title => ~"Arizona",
                                id => ~"helloWorld",
                                name => ~"World"
                            },
                            #{layout => arizona_example_layout}}}
                ]
            }}
        ]}
    ]),
    {ok, _} = application:ensure_all_started(arizona),
    Config.

end_per_suite(Config) ->
    Config.

%% --------------------------------------------------------------------
%% Behaviour (arizona_live_view) callbacks
%% --------------------------------------------------------------------

-spec mount(Assigns, Socket) -> {ok, View} | ignore when
    Assigns :: arizona_view:assigns(),
    Socket :: arizona_socket:socket(),
    View :: arizona_view:view().
mount(Assigns, _Socket) ->
    View = arizona_view:new(?MODULE, Assigns),
    {ok, View}.

-spec render(View) -> Token when
    View :: arizona_view:view(),
    Token :: arizona_render:token().
render(View) ->
    arizona_render:view_template(View, ~""""
    <main id="{arizona_view:get_assign(id, View)}">
        Hello, {arizona_view:get_assign(name, View)}!
    </main>
    """").

-spec handle_event(Event, Payload, View0) -> View1 when
    Event :: arizona_view:event(),
    Payload :: arizona_view:payload(),
    View0 :: arizona_view:view(),
    View1 :: arizona_view:view().
handle_event(_Event, _Payload, View) ->
    View.

%% --------------------------------------------------------------------
%% Tests
%% --------------------------------------------------------------------

hello_world(Config) when is_list(Config) ->
    ?assertEqual({404, ~""}, request("/404")),
    ?assertEqual({200, ~"""
    <!DOCTYPE html>
    <html lang="en">
    <head>
        <meta charset="UTF-8">
        <meta http-equiv="X-UA-Compatible" content="IE=edge">
        <meta name="viewport" content="width=device-width, initial-scale=1.0">
        <title>Arizona</title>
        <script src="assets/js/arizona/patch.js"></script>
        <script src="assets/js/arizona/worker.js"></script>
        <script src="assets/js/arizona/main.js"></script>
    </head>
    <body> <main id="helloWorld">
        Hello, World!
    </main></body>
    </html>
    """}, request("/hello-world")).

%% --------------------------------------------------------------------
%% Test support
%% --------------------------------------------------------------------

request(Uri) ->
    Url = io_lib:format("http://localhost:8080~s", [Uri]),
    Headers = [],
    HttpOptions = [],
    Options = [{body_format, binary}, {full_result, false}],
    {ok, Response} = httpc:request(get, {Url, Headers}, HttpOptions, Options),
    Response.

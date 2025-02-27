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
                    {"/hello-world/:id", arizona_view_handler,
                        {?MODULE,
                            #{
                                data_dir => proplists:get_value(data_dir, Config),
                                title => ~"Arizona",
                                name => ~"Joe"
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

-spec handle_params(PathParams, QueryString) -> Return when
    PathParams :: arizona:path_params(),
    QueryString :: arizona:query_string(),
    Return :: arizona:handle_params_ret().
handle_params(PathParams, QueryString) ->
    QueryParams = arizona:parse_query_string(QueryString),
    {true, #{
        id => arizona:get_path_param(id, PathParams),
        name => arizona:get_query_param(name, QueryParams)
    }}.

-spec mount(Bindings, Socket) -> Return when
    Bindings :: arizona:bindings(),
    Socket :: arizona:socket(),
    Return :: arizona:mount_ret().
mount(Bindings, _Socket) ->
    View = arizona_view:new(?MODULE, Bindings),
    {ok, View}.

-spec render(View) -> Token when
    View :: arizona:view(),
    Token :: arizona:rendered_view_template().
render(View) ->
    arizona:render_view_template(View, ~""""
    <main id="{arizona:get_binding(id, View)}">
        Hello, {arizona:get_binding(name, View)}!
    </main>
    """").

-spec handle_event(EventName, Payload, View) -> Return when
    EventName :: arizona:event_name(),
    Payload :: arizona:event_payload(),
    View :: arizona:view(),
    Return :: arizona:handle_event_ret().
handle_event(_EventName, _Payload, View) ->
    {noreply, View}.

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
        <title>Arizona</title> <script src="assets/js/arizona/morphdom.min.js"></script>
    <script src="assets/js/arizona/main.js"></script></head>
    <body> <main id="helloWorld">
        Hello, World!
    </main></body>
    </html>
    """}, request("/hello-world/helloWorld?name=World")).

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

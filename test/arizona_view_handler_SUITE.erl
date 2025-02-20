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
                    {"/helloworld", arizona_view_handler,
                        {?MODULE,
                            #{
                                title => ~"Arizona",
                                id => ~"app",
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

%% --------------------------------------------------------------------
%% Tests
%% --------------------------------------------------------------------

hello_world(Config) when is_list(Config) ->
    Resp0 = httpc:request("http://localhost:8080/notfound"),
    ?assert(is_status(404, Resp0)),
    Resp1 = httpc:request("http://localhost:8080/helloworld"),
    ?assert(is_status(200, Resp1)),
    ?assert(is_body("Hello, World!", Resp1)).

%% --------------------------------------------------------------------
%% Test support
%% --------------------------------------------------------------------

is_body(Pattern, {ok, {{_HttpVersion, _StatusCode, _String}, _HttpHeaders, HttpBodyResult}}) ->
    nomatch =/= string:find(HttpBodyResult, Pattern);
is_body(_Pattern, _Result) ->
    false.

is_status(StatusCode, {ok, {{_HttpVersion, StatusCode, _String}, _HttpHeaders, _HttpBodyResult}}) ->
    true;
is_status(_StatusCode, _Result) ->
    false.

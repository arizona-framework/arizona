-module(arizona_http_SUITE).
-include_lib("stdlib/include/assert.hrl").

-export([all/0]).
-export([groups/0]).

-export([halt_keeps_redirect_and_cookie/1]).
-export([render_threads_cookie/1]).
-export([error_page_prod_serves_minimal_500/1]).
-export([error_page_dev_serves_rich_page/1]).
-export([error_page_logs_the_crash/1]).

all() ->
    [{group, render}, {group, error_page}].

groups() ->
    [
        {render, [parallel], [
            halt_keeps_redirect_and_cookie,
            render_threads_cookie
        ]},
        %% Sequential: these toggle the global `arizona_reload_url` persistent_term
        %% (the dev/prod signal), which must not race the parallel render tests.
        {error_page, [], [
            error_page_prod_serves_minimal_500,
            error_page_dev_serves_rich_page,
            error_page_logs_the_crash
        ]}
    ].

%% --------------------------------------------------------------------
%% Response stash survives the render pipeline
%% --------------------------------------------------------------------

%% A middleware that halts with a redirect and a stashed cookie. render/3 leaves
%% the halt intact (the transport decodes the redirect), and the cookie rides
%% along on the same request -- this is the login path.
halt_keeps_redirect_and_cookie(Config) when is_list(Config) ->
    Req = arizona_req_test_adapter:new(#{}),
    Mw = [
        fun(R, _B) ->
            R1 = arizona_req:put_resp_cookie(R, ~"session", ~"abc", #{path => ~"/"}),
            {halt, arizona_req:redirect(R1, 303, ~"/")}
        end
    ],
    {halt, HaltReq} = arizona_http:render(arizona_about, Req, #{middlewares => Mw}),
    ?assertEqual({303, ~"/"}, arizona_req:halted_redirect(HaltReq)),
    ?assertEqual(
        [{~"session", ~"abc", #{path => ~"/"}}],
        arizona_req:resp_cookies(HaltReq)
    ).

%% A middleware that stashes a cookie then continues. The rendered result threads
%% the request out so the transport can flush the cookie onto the page response.
render_threads_cookie(Config) when is_list(Config) ->
    Req = arizona_req_test_adapter:new(#{}),
    Mw = [
        fun(R, B) ->
            R1 = arizona_req:put_resp_cookie(R, ~"theme", ~"dark", #{path => ~"/"}),
            {cont, R1, B}
        end
    ],
    {ok, 200, _Body, ReqOut} = arizona_http:render(arizona_about, Req, #{middlewares => Mw}),
    ?assertEqual(
        [{~"theme", ~"dark", #{path => ~"/"}}],
        arizona_req:resp_cookies(ReqOut)
    ).

%% --------------------------------------------------------------------
%% Error page: production must not leak internals; crashes must be logged
%% --------------------------------------------------------------------

%% Opts that crash `arizona_crashable` inside the render pipeline's try/catch.
crash_opts() ->
    #{bindings => #{crash_on_mount => true}}.

%% Production (no dev `reload_url`): a render crash yields a minimal 500 with no
%% exception class/reason/stack -- the rich page must never leak to a visitor.
error_page_prod_serves_minimal_500(Config) when is_list(Config) ->
    persistent_term:erase(arizona_reload_url),
    Req = arizona_req_test_adapter:new(#{}),
    {error, 500, Body, _} = arizona_http:render(arizona_crashable, Req, crash_opts()),
    Bin = iolist_to_binary(Body),
    ?assertEqual(nomatch, binary:match(Bin, ~"crash_on_mount")),
    ?assertEqual(nomatch, binary:match(Bin, ~"arizona_crashable")),
    ?assertNotEqual(nomatch, binary:match(Bin, ~"Internal Server Error")).

%% Development (`reload_url` set): the rich page with the exception detail is
%% still served, so the dev experience is unchanged.
error_page_dev_serves_rich_page(Config) when is_list(Config) ->
    persistent_term:put(arizona_reload_url, ~"/arizona/reload"),
    try
        Req = arizona_req_test_adapter:new(#{}),
        {error, 500, Body, _} = arizona_http:render(arizona_crashable, Req, crash_opts()),
        Bin = iolist_to_binary(Body),
        ?assertNotEqual(nomatch, binary:match(Bin, ~"crash_on_mount"))
    after
        persistent_term:erase(arizona_reload_url)
    end.

%% The crash is logged (arizona catches it before roadrunner's crash logger).
error_page_logs_the_crash(Config) when is_list(Config) ->
    persistent_term:erase(arizona_reload_url),
    HandlerId = arizona_http_suite_log_capture,
    ok = logger:add_handler(HandlerId, arizona_test_log_handler, #{
        level => error,
        config => #{pid => self()}
    }),
    try
        Req = arizona_req_test_adapter:new(#{}),
        {error, 500, _Body, _} = arizona_http:render(arizona_crashable, Req, crash_opts()),
        ?assert(received_crash_log(1000))
    after
        logger:remove_handler(HandlerId)
    end.

%% Drain forwarded log events until one carries the crash reason, else time out.
received_crash_log(Timeout) ->
    receive
        {arizona_test_log_handler, #{msg := {_, Args}}} when is_list(Args) ->
            case lists:member(crash_on_mount, Args) of
                true -> true;
                false -> received_crash_log(Timeout)
            end;
        {arizona_test_log_handler, _} ->
            received_crash_log(Timeout)
    after Timeout ->
        false
    end.

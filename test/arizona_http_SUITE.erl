-module(arizona_http_SUITE).
-include_lib("stdlib/include/assert.hrl").

-export([all/0]).
-export([groups/0]).

-export([halt_keeps_redirect_and_cookie/1]).
-export([reply_sends_flush_lines_then_content_type/1]).
-export([reply_turns_a_halt_into_its_halt_reply/1]).
-export([render_threads_cookie/1]).
-export([render_answers_the_targets_content_type/1]).
-export([render_refuses_a_cross_target_layout/1]).
-export([renderers_name_their_content_type/1]).
-export([error_page_prod_serves_plain_text_500/1]).
-export([error_page_dev_without_page_serves_plain_text/1]).
-export([error_page_dev_renders_the_configured_page/1]).
-export([error_page_without_backend_is_refused/1]).
-export([error_page_logs_the_crash/1]).
-export([error_page_custom_target_type/1]).

%% An `error_page` that returns no template.
-export([bare_error_page/1]).

all() ->
    [{group, render}, {group, error_page}].

groups() ->
    [
        {render, [parallel], [
            halt_keeps_redirect_and_cookie,
            reply_sends_flush_lines_then_content_type,
            reply_turns_a_halt_into_its_halt_reply,
            render_threads_cookie,
            render_answers_the_targets_content_type,
            render_refuses_a_cross_target_layout,
            renderers_name_their_content_type
        ]},
        %% Sequential: these toggle the global `arizona_reload_url` persistent_term
        %% (the dev/prod signal), which must not race the parallel render tests.
        {error_page, [], [
            error_page_prod_serves_plain_text_500,
            error_page_dev_without_page_serves_plain_text,
            error_page_dev_renders_the_configured_page,
            error_page_without_backend_is_refused,
            error_page_logs_the_crash,
            error_page_custom_target_type
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

%% reply/3 is what an adapter sends: the stash in send order, then the page's
%% media type, with the body render/3 produced.
reply_sends_flush_lines_then_content_type(Config) when is_list(Config) ->
    Mw = [
        fun(R, B) ->
            {cont, arizona_req:put_resp_cookie(R, ~"theme", ~"dark", #{path => ~"/"}), B}
        end
    ],
    {200, Headers, Body, _Req} = arizona_http:reply(
        arizona_about, arizona_req_test_adapter:new(#{}), #{middlewares => Mw}
    ),
    ?assertEqual(
        [{~"set-cookie", ~"theme=dark; Path=/"}, {~"content-type", arizona_html:content_type()}],
        Headers
    ),
    ?assertNotEqual(<<>>, iolist_to_binary(Body)).

reply_turns_a_halt_into_its_halt_reply(Config) when is_list(Config) ->
    Mw = [fun(R, _B) -> {halt, arizona_req:redirect(R, 303, ~"/login")} end],
    ?assertMatch(
        {303, [{~"location", ~"/login"}], <<>>, _},
        arizona_http:reply(arizona_about, arizona_req_test_adapter:new(#{}), #{middlewares => Mw})
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
    {ok, 200, _ContentType, _Body, ReqOut} = arizona_http:render(arizona_about, Req, #{
        middlewares => Mw
    }),
    ?assertEqual(
        [{~"theme", ~"dark", #{path => ~"/"}}],
        arizona_req:resp_cookies(ReqOut)
    ).

%% The page is served under the media type of its render target: a `?json` view
%% is a JSON document, an HTML view (bare or in a layout) an HTML one.
render_answers_the_targets_content_type(Config) when is_list(Config) ->
    Req = arizona_req_test_adapter:new(#{}),
    {ok, 200, NativeType, NativeBody, _} =
        arizona_http:render(arizona_json_counter_demo, Req, #{}),
    ?assertEqual(~"application/json", NativeType),
    ?assertMatch(#{~"type" := ~"Column"}, json:decode(iolist_to_binary(NativeBody))),
    {ok, 200, HtmlType, _, _} = arizona_http:render(arizona_about, Req, #{}),
    ?assertEqual(~"text/html; charset=utf-8", HtmlType),
    {ok, 200, LayoutType, _, _} = arizona_http:render(arizona_local_nested, Req, #{
        bindings => #{title => ~"Nested"},
        layouts => [{arizona_layout, render}]
    }),
    ?assertEqual(~"text/html; charset=utf-8", LayoutType).

%% A layout of one target around a page of another would be a document no client
%% parses, so the render refuses it and the request gets the error page.
render_refuses_a_cross_target_layout(Config) when is_list(Config) ->
    Req = arizona_req_test_adapter:new(#{}),
    ?assertMatch(
        {error, 500, ~"text/plain; charset=utf-8", _, _},
        arizona_http:render(arizona_json_counter_demo, Req, #{
            bindings => #{title => ~"Native"},
            layouts => [{arizona_layout, render}]
        })
    ).

renderers_name_their_content_type(Config) when is_list(Config) ->
    ?assertEqual(~"text/html; charset=utf-8", arizona_html:content_type()),
    ?assertEqual(~"application/json", arizona_json:content_type()).

%% --------------------------------------------------------------------
%% Error page: production must not leak internals; crashes must be logged
%% --------------------------------------------------------------------

%% Opts that crash `arizona_crashable` inside the render pipeline's try/catch.
crash_opts() ->
    #{bindings => #{crash_on_mount => true}}.

%% Production (no dev `reload_url`): a render crash yields a bare plain-text 500
%% with no exception class/reason/stack, whatever `error_page` the listener sets.
error_page_prod_serves_plain_text_500(Config) when is_list(Config) ->
    persistent_term:erase(arizona_reload_url),
    Req = arizona_req_test_adapter:new(#{}),
    [
        ?assertMatch(
            {error, 500, ~"text/plain; charset=utf-8", ~"Internal Server Error", _},
            arizona_http:render(arizona_crashable, Req, Opts)
        )
     || Opts <- [crash_opts(), (crash_opts())#{error_page => {arizona_error_page, render}}]
    ].

%% Development with no `error_page` configured: the crash detail as plain text,
%% naming the reason and the frame it was raised in.
error_page_dev_without_page_serves_plain_text(Config) when is_list(Config) ->
    with_dev(fun() ->
        Req = arizona_req_test_adapter:new(#{}),
        {error, 500, ContentType, Body, _} = arizona_http:render(
            arizona_crashable, Req, crash_opts()
        ),
        ?assertEqual(~"text/plain; charset=utf-8", ContentType),
        Bin = iolist_to_binary(Body),
        ?assertNotEqual(nomatch, binary:match(Bin, ~"crash_on_mount")),
        ?assertMatch(
            {match, _},
            re:run(Bin, ~"\n  arizona_crashable:mount/1 [^\n]*arizona_crashable.erl:\\d+")
        ),
        ?assertEqual(nomatch, binary:match(Bin, ~"<"))
    end).

%% Development with an `error_page` configured renders that template.
error_page_dev_renders_the_configured_page(Config) when is_list(Config) ->
    with_dev(fun() ->
        Req = arizona_req_test_adapter:new(#{}),
        Opts = (crash_opts())#{error_page => {arizona_error_page, render}},
        {error, 500, ContentType, Body, _} = arizona_http:render(arizona_crashable, Req, Opts),
        ?assertEqual(~"text/html; charset=utf-8", ContentType),
        Bin = iolist_to_binary(Body),
        ?assertNotEqual(nomatch, binary:match(Bin, ~"<title>500 -- Server Error</title>")),
        ?assertNotEqual(nomatch, binary:match(Bin, ~"crash_on_mount"))
    end).

%% An error page is a template, so its backend names the media type; a value
%% with no backend is refused rather than served under a guessed type.
error_page_without_backend_is_refused(Config) when is_list(Config) ->
    with_dev(fun() ->
        Req = arizona_req_test_adapter:new(#{}),
        Opts = (crash_opts())#{error_page => {?MODULE, bare_error_page}},
        try arizona_http:render(arizona_crashable, Req, Opts) of
            Result -> ct:fail({not_raised, Result})
        catch
            error:Reason:ST ->
                ?assertMatch(
                    {error_page_not_a_template, {?MODULE, bare_error_page}, #{s := [~"oops"]}},
                    Reason
                ),
                Msg = iolist_to_binary(erl_error:format_exception(error, Reason, ST)),
                ?assertNotEqual(nomatch, binary:match(Msg, ~"which is not a template"))
        end
    end).

bare_error_page(#{error_info := #{class := error}}) ->
    #{s => [~"oops"], d => []}.

with_dev(Fun) ->
    persistent_term:put(arizona_reload_url, ~"/arizona/reload"),
    try
        Fun()
    after
        persistent_term:erase(arizona_reload_url)
    end.

%% A listener's custom error page is served under its own target's media type.
error_page_custom_target_type(Config) when is_list(Config) ->
    persistent_term:put(arizona_reload_url, ~"/arizona/reload"),
    try
        Req = arizona_req_test_adapter:new(#{}),
        Opts = (crash_opts())#{error_page => {arizona_json_error_page, render}},
        {error, 500, ContentType, Body, _} = arizona_http:render(arizona_crashable, Req, Opts),
        ?assertEqual(~"application/json", ContentType),
        ?assertMatch(#{~"type" := ~"Text"}, json:decode(iolist_to_binary(Body)))
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
        {error, 500, _ContentType, _Body, _} = arizona_http:render(
            arizona_crashable, Req, crash_opts()
        ),
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

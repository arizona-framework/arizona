-module(arizona_http_SUITE).
-include_lib("stdlib/include/assert.hrl").

-export([all/0]).
-export([groups/0]).

-export([halt_keeps_redirect_and_cookie/1]).
-export([render_threads_cookie/1]).

all() ->
    [{group, render}].

groups() ->
    [
        {render, [parallel], [
            halt_keeps_redirect_and_cookie,
            render_threads_cookie
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

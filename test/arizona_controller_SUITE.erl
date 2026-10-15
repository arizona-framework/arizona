-module(arizona_controller_SUITE).
-include_lib("stdlib/include/assert.hrl").

-export([all/0]).
-export([groups/0]).

-export([reply_effects_builds_effects_payload/1]).
-export([reply_effects_status_and_content_type/1]).
-export([reply_effects_with_status_for_error_leg/1]).
-export([reply_redirect_emits_navigate_effect/1]).
-export([set_cookie_layers_onto_reply/1]).
-export([reply_effects_refuses_navigate_flash/1]).
-export([put_flash_with_full_navigate_reaches_target/1]).
-export([init_per_suite/1]).
-export([end_per_suite/1]).

all() ->
    [{group, reply}].

groups() ->
    [
        {reply, [parallel], [
            reply_effects_builds_effects_payload,
            reply_effects_status_and_content_type,
            reply_effects_with_status_for_error_leg,
            reply_redirect_emits_navigate_effect,
            set_cookie_layers_onto_reply,
            reply_effects_refuses_navigate_flash,
            put_flash_with_full_navigate_reaches_target
        ]}
    ].

init_per_suite(Config) ->
    ok = application:set_env(arizona, secret_key, ~"test-secret-key-0123456789abcdef"),
    Config.

end_per_suite(_Config) ->
    ok = application:unset_env(arizona, secret_key).

%% --------------------------------------------------------------------
%% reply_effects/1
%% --------------------------------------------------------------------

%% The body is the same {"e": [...]} wire payload the WebSocket sends -- the
%% arizona_effect tuples unwrapped to their bare op-code lists (set_attr = 7).
reply_effects_builds_effects_payload(Config) when is_list(Config) ->
    {_Status, _Headers, Body} = arizona_controller:reply_effects([
        arizona_js:set_attr(~"#error", ~"hidden", ~"")
    ]),
    ?assertEqual(
        #{~"e" => [[7, ~"#error", ~"hidden", ~""]]},
        json:decode(iolist_to_binary(Body))
    ).

reply_effects_status_and_content_type(Config) when is_list(Config) ->
    {Status, Headers, _Body} = arizona_controller:reply_effects([
        arizona_js:set_title(~"Saved")
    ]),
    ?assertEqual(200, Status),
    ?assertEqual(~"application/json", proplists:get_value(~"content-type", Headers)).

%% The error leg: a non-2xx (so a fetch form keeps its fields) that still carries
%% effects (push_event = op 0), via reply_effects/2.
reply_effects_with_status_for_error_leg(Config) when is_list(Config) ->
    {Status, _Headers, Body} = arizona_controller:reply_effects(422, [
        arizona_js:push_event(~"invalid")
    ]),
    ?assertEqual(422, Status),
    ?assertEqual(#{~"e" => [[0, ~"invalid"]]}, json:decode(iolist_to_binary(Body))).

%% --------------------------------------------------------------------
%% reply_redirect/1
%% --------------------------------------------------------------------

%% A redirect is delivered as a navigate effect (op code 10), not an HTTP 3xx --
%% a fetch-followed 3xx can't drive a SPA navigation.
reply_redirect_emits_navigate_effect(Config) when is_list(Config) ->
    {200, _Headers, Body} = arizona_controller:reply_redirect(~"/login"),
    ?assertEqual(
        #{~"e" => [[10, ~"/login"]]},
        json:decode(iolist_to_binary(Body))
    ).

%% --------------------------------------------------------------------
%% Cookies layer on with the roadrunner_resp builders
%% --------------------------------------------------------------------

%% The real Set-Cookie -- the reason fetch exists -- rides on the reply via the
%% existing roadrunner_resp:set_cookie/4, with no controller-specific plumbing.
set_cookie_layers_onto_reply(Config) when is_list(Config) ->
    Resp0 = arizona_controller:reply_effects([arizona_js:set_title(~"Saved")]),
    {200, Headers, _Body} = roadrunner_resp:set_cookie(Resp0, ~"sid", ~"new", #{
        http_only => true, path => ~"/"
    }),
    Cookie = proplists:get_value(~"set-cookie", Headers),
    ?assertNotEqual(undefined, Cookie),
    ?assertNotEqual(nomatch, binary:match(Cookie, ~"sid=new")).

%% --------------------------------------------------------------------
%% A navigate/patch flash has no carrier in a fetch response
%% --------------------------------------------------------------------

%% The `flash` opt is carried in-process by the WebSocket, for an effect a live
%% handler returns. A fetch response reaches the browser instead, which would
%% navigate without it -- the message silently lost, its content shipped in the
%% body. Refused, wherever the navigate sits.
reply_effects_refuses_navigate_flash(Config) when is_list(Config) ->
    Flash = #{~"info" => ~"Saved"},
    Nav = arizona_js:navigate(~"/done", #{flash => Flash}),
    Refused = [
        [Nav],
        [arizona_js:set_title(~"t"), arizona_js:patch(~"/done", #{flash => Flash})],
        [arizona_js:transition(Nav)],
        [arizona_js:on_key(enter, [Nav])],
        [arizona_js:fetch(~"/f", #{on_error => Nav})]
    ],
    [
        ?assertError({flash_outside_live_effect, _}, arizona_controller:reply_effects(Effects))
     || Effects <- Refused
    ],
    ?assertError(
        {flash_outside_live_effect, navigate}, arizona_controller:reply_effects(422, [Nav])
    ),
    ?assertMatch(
        {200, _, _}, arizona_controller:reply_effects([arizona_js:navigate(~"/done", #{})])
    ).

%% What carries a flash out of a controller: `put_flash/3` through the request
%% round trip sets the signed cookie on this response, and a FULL navigation makes
%% the browser's next GET carry it. (A SPA navigate would not: the socket's request
%% predates the cookie.)
put_flash_with_full_navigate_reaches_target(Config) when is_list(Config) ->
    ArzReq = arizona_req:put_flash(arizona_req_test_adapter:new(#{}), info, ~"Saved"),
    Resp = arizona_roadrunner_resp:flush(
        ArzReq,
        arizona_controller:reply_effects([arizona_js:navigate(~"/done", #{full => true})])
    ),
    {200, Headers, Body} = Resp,
    ?assertEqual(
        #{~"e" => [[10, ~"/done", #{~"full" => true}]]}, json:decode(iolist_to_binary(Body))
    ),
    [Cookie] = [V || {~"set-cookie", V} <- Headers],
    <<"az_flash=", Rest/binary>> = Cookie,
    [Value | _] = binary:split(Rest, ~";"),
    ?assertEqual(#{~"info" => ~"Saved"}, arizona_flash:decode(Value)).

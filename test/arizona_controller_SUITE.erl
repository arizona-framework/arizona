-module(arizona_controller_SUITE).
-include_lib("stdlib/include/assert.hrl").

-export([all/0]).
-export([groups/0]).

-export([reply_effects_builds_effects_payload/1]).
-export([reply_effects_status_and_content_type/1]).
-export([reply_effects_with_status_for_error_leg/1]).
-export([reply_redirect_emits_navigate_effect/1]).
-export([reply_redirect_off_origin_loads_full_page/1]).
-export([reply_redirect_reads_location_as_a_browser_does/1]).
-export([reply_redirect_refuses_unsafe_scheme/1]).
-export([dispatch_sends_flushed_lines_ahead_of_the_reply/1]).
-export([dispatch_hands_the_action_state_and_bindings/1]).
-export([dispatch_halt_skips_the_action/1]).
-export([dispatch_flushes_the_returned_request/1]).
-export([dispatch_refuses_a_bad_reply_before_committing/1]).
-export([dispatch_refuses_a_loop_without_handle_info/1]).
-export([dispatch_names_a_missing_action/1]).
-export([unsupported_reply_names_kind_and_adapter/1]).
-export([reply_effects_refuses_navigate_flash/1]).
-export([put_flash_with_full_navigate_reaches_target/1]).
-export([init_per_suite/1]).
-export([end_per_suite/1]).
-export([reply_effects_flattens_nested_commands/1]).

all() ->
    [{group, reply}].

groups() ->
    [
        {reply, [parallel], [
            reply_effects_builds_effects_payload,
            reply_effects_status_and_content_type,
            reply_effects_with_status_for_error_leg,
            reply_redirect_emits_navigate_effect,
            reply_redirect_off_origin_loads_full_page,
            reply_redirect_reads_location_as_a_browser_does,
            reply_redirect_refuses_unsafe_scheme,
            dispatch_sends_flushed_lines_ahead_of_the_reply,
            dispatch_hands_the_action_state_and_bindings,
            dispatch_halt_skips_the_action,
            dispatch_flushes_the_returned_request,
            dispatch_refuses_a_bad_reply_before_committing,
            dispatch_refuses_a_loop_without_handle_info,
            dispatch_names_a_missing_action,
            unsupported_reply_names_kind_and_adapter,
            reply_effects_refuses_navigate_flash,
            put_flash_with_full_navigate_reaches_target,
            reply_effects_flattens_nested_commands
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

%% A response is a command list like any other: a conditional item that yields a
%% list of commands, or `[]` for none, is flattened into it.
reply_effects_flattens_nested_commands(Config) when is_list(Config) ->
    {200, _Headers, Body} = arizona_controller:reply_effects([
        [],
        arizona_js:set_title(~"t"),
        [arizona_js:hide(~"#a"), []]
    ]),
    ?assertEqual(
        #{~"e" => [[14, ~"t"], [3, ~"#a"]]},
        json:decode(iolist_to_binary(Body))
    ),
    {200, _, EmptyBody} = arizona_controller:reply_effects([[]]),
    ?assertEqual(#{~"e" => []}, json:decode(iolist_to_binary(EmptyBody))).

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

%% A Location off this origin cannot be SPA-navigated (the client would
%% pushState it, which throws), so it goes out as a full-page navigation. The
%% scheme-relative forms count as off-origin: `/\host` is read like `//host`.
reply_redirect_off_origin_loads_full_page(Config) when is_list(Config) ->
    lists:foreach(
        fun(Location) ->
            {200, _Headers, Body} = arizona_controller:reply_redirect(Location),
            ?assertEqual(
                #{~"e" => [[10, Location, #{~"full" => true}]]},
                json:decode(iolist_to_binary(Body)),
                Location
            )
        end,
        [
            ~"https://idp.example.com/authorize?client_id=x",
            ~"//idp.example.com/authorize",
            ~"/\\idp.example.com/authorize",
            ~"login"
        ]
    ).

%% A browser drops tab, LF and CR from a URL before parsing it, so a path that
%% hides one between its slashes is a scheme-relative URL to another host. One
%% whose second character merely LOOKS odd stays a path.
reply_redirect_reads_location_as_a_browser_does(Config) when is_list(Config) ->
    lists:foreach(
        fun({Location, Expected}) ->
            {200, _Headers, Body} = arizona_controller:reply_redirect(Location),
            ?assertEqual(#{~"e" => [Expected]}, json:decode(iolist_to_binary(Body)), Location)
        end,
        [
            {~"/\t/evil.com", [10, ~"/\t/evil.com", #{~"full" => true}]},
            {~"/\n/evil.com", [10, ~"/\n/evil.com", #{~"full" => true}]},
            {~"/\r\\evil.com", [10, ~"/\r\\evil.com", #{~"full" => true}]},
            {~" //evil.com", [10, ~" //evil.com", #{~"full" => true}]},
            {~"/%2F%2Fevil", [10, ~"/%2F%2Fevil"]},
            {~"/ /evil", [10, ~"/ /evil"]}
        ]
    ).

%% Only http(s) may leave the page. A `javascript:` (or `data:`) Location would run
%% in the page if the client loaded it, and a controller redirecting to a
%% request-supplied `next` parameter would hand that to any attacker, so it is
%% refused at the call -- in every spelling a browser reads as that scheme.
reply_redirect_refuses_unsafe_scheme(Config) when is_list(Config) ->
    lists:foreach(
        fun(Location) ->
            ?assertError({unsafe_redirect, Location}, arizona_controller:reply_redirect(Location)),
            ?assertMatch(
                #{general := [_ | _]},
                arizona_controller:format_error({unsafe_redirect, Location}, [])
            )
        end,
        [
            ~"javascript:alert(1)",
            ~"JavaScript:alert(1)",
            ~" javascript:alert(1)",
            ~"java\tscript:alert(1)",
            ~"data:text/html,<script>alert(1)</script>",
            ~"vbscript:x"
        ]
    ).

%% --------------------------------------------------------------------
%% dispatch/3
%% --------------------------------------------------------------------

%% The cookie an action writes on its request goes out ahead of the reply's own
%% headers, the way every adapter sends it.
dispatch_sends_flushed_lines_ahead_of_the_reply(Config) when is_list(Config) ->
    {{200, Headers, _Body}, _Req} = dispatch(arizona_fetch_push_controller, handle, #{}, []),
    ?assertMatch(
        [{~"set-cookie", <<"pushed=1; ", _/binary>>}, {~"content-type", ~"application/json"}],
        Headers
    ).

dispatch_hands_the_action_state_and_bindings(Config) when is_list(Config) ->
    Bind = fun(R, B) -> {cont, R, B#{session => #{~"user_id" => ~"u7"}}} end,
    ?assertMatch(
        {{200, _, ~"MARK"}, _},
        dispatch(arizona_state_echo_controller, handle, #{marker => ~"MARK"}, [])
    ),
    ?assertMatch(
        {{200, _, <<"session=anon binding=u7">>}, _},
        dispatch(arizona_session_echo_controller, handle, #{}, [Bind])
    ).

dispatch_halt_skips_the_action(Config) when is_list(Config) ->
    Gate = fun(R, _B) -> {halt, arizona_req:redirect(R, 303, ~"/login")} end,
    ?assertMatch(
        {{303, [{~"location", ~"/login"}], <<>>}, _},
        dispatch(arizona_fetch_push_controller, handle, #{}, [Gate])
    ).

%% What is flushed is the request the action returned: a write it discards never
%% reaches the response.
dispatch_flushes_the_returned_request(Config) when is_list(Config) ->
    {{200, Written, _}, _} = dispatch(arizona_session_write_controller, handle, #{}, []),
    ?assertMatch([{~"set-cookie", <<"az_session=", _/binary>>} | _], Written),
    Session = arizona_session:encode(#{~"user_id" => ~"u1"}),
    Req = arizona_req_test_adapter:new(#{cookies => [{~"az_session", Session}]}),
    Route = route(arizona_logout_controller, handle_dropped, #{}, []),
    {{200, Dropped, _}, _} = arizona_controller:dispatch(Route, Req, arizona_roadrunner_server),
    ?assertEqual([{~"content-type", ~"application/json"}], Dropped).

%% A reply no adapter could send is refused before the flush commits anything.
dispatch_refuses_a_bad_reply_before_committing(Config) when is_list(Config) ->
    ?assertError({bad_reply, not_a_reply}, dispatch(arizona_bad_reply_controller, handle, #{}, [])).

dispatch_refuses_a_loop_without_handle_info(Config) when is_list(Config) ->
    ?assertError(
        {missing_loop_callback, arizona_bad_reply_controller},
        dispatch(arizona_bad_reply_controller, loop, #{}, [])
    ).

dispatch_names_a_missing_action(Config) when is_list(Config) ->
    ?assertError(
        {missing_action, arizona_users_controller, nope, 2},
        dispatch(arizona_users_controller, nope, #{}, [])
    ).

%% A reply kind the adapter does not declare is refused, naming both.
unsupported_reply_names_kind_and_adapter(Config) when is_list(Config) ->
    Route = route(arizona_loop_controller, handle, #{}, []),
    Req = arizona_req_test_adapter:new(#{cookies => []}),
    {error, Reason, ST} =
        try arizona_controller:dispatch(Route, Req, arizona_fake_server) of
            _ -> error(not_raised)
        catch
            error:R:S -> {error, R, S}
        end,
    ?assertEqual({unsupported_reply, loop, arizona_fake_server}, Reason),
    Message = iolist_to_binary(erl_error:format_exception(error, Reason, ST)),
    ?assertNotEqual(
        nomatch, binary:match(Message, ~"arizona_fake_server server adapter cannot send a loop")
    ).

dispatch(Handler, Action, State, Middlewares) ->
    arizona_controller:dispatch(
        route(Handler, Action, State, Middlewares),
        arizona_req_test_adapter:new(#{cookies => []}),
        arizona_roadrunner_server
    ).

route(Handler, Action, State, Middlewares) ->
    #{handler => Handler, action => Action, state => State, middlewares => Middlewares}.

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

%% What carries a flash out of a controller: `put_flash/3` on the returned
%% request sets the signed cookie on this response, and a FULL navigation makes
%% the browser's next GET carry it. (A SPA navigate would not: the socket's request
%% predates the cookie.)
put_flash_with_full_navigate_reaches_target(Config) when is_list(Config) ->
    ArzReq = arizona_req:put_flash(arizona_req_test_adapter:new(#{}), info, ~"Saved"),
    {200, _ReplyHeaders, Body} = arizona_controller:reply_effects([
        arizona_js:navigate(~"/done", #{full => true})
    ]),
    Headers = arizona_req:flush(ArzReq),
    ?assertEqual(
        #{~"e" => [[10, ~"/done", #{~"full" => true}]]}, json:decode(iolist_to_binary(Body))
    ),
    [Cookie] = [V || {~"set-cookie", V} <- Headers],
    <<"az_flash=", Rest/binary>> = Cookie,
    [Value | _] = binary:split(Rest, ~";"),
    ?assertEqual(#{~"info" => ~"Saved"}, arizona_flash:decode(Value)).

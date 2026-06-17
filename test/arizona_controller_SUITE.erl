-module(arizona_controller_SUITE).
-include_lib("stdlib/include/assert.hrl").

-export([all/0]).
-export([groups/0]).

-export([reply_effects_builds_effects_payload/1]).
-export([reply_effects_status_and_content_type/1]).
-export([reply_redirect_emits_navigate_effect/1]).
-export([set_cookie_layers_onto_reply/1]).

all() ->
    [{group, reply}].

groups() ->
    [
        {reply, [parallel], [
            reply_effects_builds_effects_payload,
            reply_effects_status_and_content_type,
            reply_redirect_emits_navigate_effect,
            set_cookie_layers_onto_reply
        ]}
    ].

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

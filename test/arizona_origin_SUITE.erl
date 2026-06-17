-module(arizona_origin_SUITE).
-include_lib("stdlib/include/assert.hrl").

-export([all/0]).
-export([groups/0]).
-export([init_per_suite/1]).
-export([end_per_suite/1]).

-export([same_origin_passes/1]).
-export([same_origin_with_port_passes/1]).
-export([cross_origin_forbidden/1]).
-export([missing_origin_passes/1]).
-export([scheme_less_origin_forbidden/1]).
-export([null_origin_forbidden/1]).
-export([allowlisted_origin_passes/1]).
-export([disabled_allows_any_origin/1]).

all() ->
    [{group, check}].

groups() ->
    %% Sequential: the allowlist/disabled cases toggle shared app env.
    [
        {check, [], [
            same_origin_passes,
            same_origin_with_port_passes,
            cross_origin_forbidden,
            missing_origin_passes,
            scheme_less_origin_forbidden,
            null_origin_forbidden,
            allowlisted_origin_passes,
            disabled_allows_any_origin
        ]}
    ].

init_per_suite(Config) ->
    Config.

end_per_suite(_Config) ->
    application:unset_env(arizona, check_origin),
    application:unset_env(arizona, csrf_origins),
    ok.

same_origin_passes(Config) when is_list(Config) ->
    ?assertEqual(ok, arizona_origin:check(~"https://app.example", ~"app.example")).

same_origin_with_port_passes(Config) when is_list(Config) ->
    ?assertEqual(ok, arizona_origin:check(~"http://localhost:4041", ~"localhost:4041")).

cross_origin_forbidden(Config) when is_list(Config) ->
    ?assertEqual(forbidden, arizona_origin:check(~"https://evil.example", ~"app.example")).

missing_origin_passes(Config) when is_list(Config) ->
    %% No Origin header (native clients, CLI, top-level GET navigations) -> allowed.
    ?assertEqual(ok, arizona_origin:check(undefined, ~"app.example")).

scheme_less_origin_forbidden(Config) when is_list(Config) ->
    ?assertEqual(forbidden, arizona_origin:check(~"app.example", ~"app.example")).

null_origin_forbidden(Config) when is_list(Config) ->
    ?assertEqual(forbidden, arizona_origin:check(~"null", ~"app.example")).

allowlisted_origin_passes(Config) when is_list(Config) ->
    application:set_env(arizona, csrf_origins, [~"https://trusted.example"]),
    try
        ?assertEqual(ok, arizona_origin:check(~"https://trusted.example", ~"app.example"))
    after
        application:unset_env(arizona, csrf_origins)
    end.

disabled_allows_any_origin(Config) when is_list(Config) ->
    application:set_env(arizona, check_origin, false),
    try
        ?assertEqual(ok, arizona_origin:check(~"https://evil.example", ~"app.example"))
    after
        application:unset_env(arizona, check_origin)
    end.

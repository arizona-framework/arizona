-module(arizona_origin_SUITE).
-include_lib("stdlib/include/assert.hrl").

-export([all/0]).
-export([groups/0]).
-export([init_per_suite/1]).
-export([end_per_suite/1]).

-export([same_origin_passes/1]).
-export([same_origin_with_port_passes/1]).
-export([same_host_different_port_forbidden/1]).
-export([same_origin_ipv6_authority_passes/1]).
-export([cross_origin_forbidden/1]).
-export([https_request_forbids_http_origin/1]).
-export([http_request_allows_https_origin/1]).
-export([missing_origin_passes/1]).
-export([scheme_less_origin_forbidden/1]).
-export([null_origin_forbidden/1]).
-export([mixed_case_origin_passes/1]).
-export([invalid_utf8_authority_forbidden/1]).
-export([invalid_utf8_scheme_forbidden/1]).
-export([invalid_utf8_allowlist_compare_forbidden/1]).
-export([allowlisted_origin_passes/1]).
-export([allowlisted_origin_case_insensitive/1]).
-export([allowlisted_charlist_entry_passes/1]).
-export([allowlist_mixes_binary_and_charlist_entries/1]).
-export([allowlist_non_origin_entry_names_itself/1]).
-export([allowlist_follows_env_change/1]).
-export([allowlist_resolves_env_reference/1]).
-export([disabled_allows_any_origin/1]).

all() ->
    [{group, check}].

groups() ->
    %% Sequential: the allowlist/disabled cases toggle shared app env.
    [
        {check, [], [
            same_origin_passes,
            same_origin_with_port_passes,
            same_host_different_port_forbidden,
            same_origin_ipv6_authority_passes,
            cross_origin_forbidden,
            https_request_forbids_http_origin,
            http_request_allows_https_origin,
            missing_origin_passes,
            scheme_less_origin_forbidden,
            null_origin_forbidden,
            mixed_case_origin_passes,
            invalid_utf8_authority_forbidden,
            invalid_utf8_scheme_forbidden,
            invalid_utf8_allowlist_compare_forbidden,
            allowlisted_origin_passes,
            allowlisted_origin_case_insensitive,
            allowlisted_charlist_entry_passes,
            allowlist_mixes_binary_and_charlist_entries,
            allowlist_non_origin_entry_names_itself,
            allowlist_follows_env_change,
            allowlist_resolves_env_reference,
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
    ?assertEqual(ok, arizona_origin:check(~"https://app.example", ~"app.example", https)).

same_origin_with_port_passes(Config) when is_list(Config) ->
    ?assertEqual(ok, arizona_origin:check(~"http://localhost:4041", ~"localhost:4041", http)).

same_host_different_port_forbidden(Config) when is_list(Config) ->
    %% The authority includes the port, so the same host on a different port is a
    %% distinct origin and rejected.
    ?assertEqual(
        forbidden, arizona_origin:check(~"https://app.example:8443", ~"app.example", https)
    ).

same_origin_ipv6_authority_passes(Config) when is_list(Config) ->
    %% An IPv6 authority (bracketed host + port) compares whole against the Host.
    ?assertEqual(ok, arizona_origin:check(~"http://[::1]:8080", ~"[::1]:8080", http)).

cross_origin_forbidden(Config) when is_list(Config) ->
    ?assertEqual(forbidden, arizona_origin:check(~"https://evil.example", ~"app.example", https)).

https_request_forbids_http_origin(Config) when is_list(Config) ->
    %% Scheme is part of the origin. An active network attacker can serve
    %% `http://app.example` with no certificate (any host not HSTS-pinned is
    %% reachable that way) and post from it to the real HTTPS site, which the
    %% browser answers with the same cookies -- `Secure` ones included. Authority
    %% equality alone would wave that through.
    ?assertEqual(forbidden, arizona_origin:check(~"http://app.example", ~"app.example", https)),
    ?assertEqual(ok, arizona_origin:check(~"https://app.example", ~"app.example", https)).

http_request_allows_https_origin(Config) when is_list(Config) ->
    %% The reverse is not an attack, it is the standard proxy topology: a
    %% TLS-terminating proxy that forwards without `X-Forwarded-Proto` leaves the
    %% backend seeing plain HTTP while the browser truthfully reports https.
    %% Rejecting it would break that deployment and protect nothing (the hop the
    %% check can see is already unencrypted).
    ?assertEqual(ok, arizona_origin:check(~"https://app.example", ~"app.example", http)),
    ?assertEqual(ok, arizona_origin:check(~"http://app.example", ~"app.example", http)).

missing_origin_passes(Config) when is_list(Config) ->
    %% No Origin header (native clients, CLI, top-level GET navigations) -> allowed.
    ?assertEqual(ok, arizona_origin:check(undefined, ~"app.example", https)).

scheme_less_origin_forbidden(Config) when is_list(Config) ->
    ?assertEqual(forbidden, arizona_origin:check(~"app.example", ~"app.example", https)).

null_origin_forbidden(Config) when is_list(Config) ->
    ?assertEqual(forbidden, arizona_origin:check(~"null", ~"app.example", https)).

mixed_case_origin_passes(Config) when is_list(Config) ->
    %% Scheme and authority both compare case-insensitively -- the property the
    %% byte-wise ASCII compare has to keep after dropping `string:equal/3`.
    ?assertEqual(ok, arizona_origin:check(~"HTTPS://App.Example", ~"app.example", https)),
    ?assertEqual(ok, arizona_origin:check(~"https://app.example", ~"APP.EXAMPLE", https)).

invalid_utf8_authority_forbidden(Config) when is_list(Config) ->
    %% A client may put any byte in the `Origin` header (roadrunner permits
    %% >= 0x80), and an origin is ASCII by RFC 6454 -- so an invalid-UTF-8
    %% authority is simply not the request's own. It must read as a clean
    %% `forbidden`, not raise `badarg` out of the middleware into a 500: the
    %% Unicode casefold in the authority compare was the first of three such
    %% raises, and the one on the primary path (hit before the allowlist).
    ?assertEqual(
        forbidden,
        arizona_origin:check(<<"http://app.example", 16#FF>>, ~"app.example", http)
    ).

invalid_utf8_scheme_forbidden(Config) when is_list(Config) ->
    %% Same on the scheme half, which an https request compares separately (and
    %% reaches before the authority).
    ?assertEqual(
        forbidden,
        arizona_origin:check(<<16#FF, "://app.example">>, ~"app.example", https)
    ).

invalid_utf8_allowlist_compare_forbidden(Config) when is_list(Config) ->
    %% And on the allowlist fallback, reached when the same-origin compare says
    %% no without ever touching the bad byte (a cross-origin request).
    application:set_env(arizona, csrf_origins, [~"https://trusted.example"]),
    try
        ?assertEqual(
            forbidden,
            arizona_origin:check(<<"https://app.example", 16#FF>>, ~"other.example", https)
        )
    after
        application:unset_env(arizona, csrf_origins)
    end.

allowlisted_origin_passes(Config) when is_list(Config) ->
    application:set_env(arizona, csrf_origins, [~"https://trusted.example"]),
    try
        ?assertEqual(ok, arizona_origin:check(~"https://trusted.example", ~"app.example", https))
    after
        application:unset_env(arizona, csrf_origins)
    end.

allowlisted_origin_case_insensitive(Config) when is_list(Config) ->
    %% Browser Origins are lowercase; an uppercase allowlist entry must still match
    %% (consistent with the case-insensitive same-origin compare), not silently 403.
    application:set_env(arizona, csrf_origins, [~"HTTPS://Trusted.Example"]),
    try
        ?assertEqual(ok, arizona_origin:check(~"https://trusted.example", ~"app.example", https))
    after
        application:unset_env(arizona, csrf_origins)
    end.

allowlisted_charlist_entry_passes(Config) when is_list(Config) ->
    %% `"https://trusted.example"` is the easiest sys.config typo there is, and
    %% it sits on the security path: a charlist entry used to build an allowlist
    %% that could never match (`lists:member/2` against a binary Origin), so the
    %% operator's allowlist silently did nothing. Accept every iodata spelling.
    application:set_env(arizona, csrf_origins, ["https://trusted.example"]),
    try
        ?assertEqual(ok, arizona_origin:check(~"https://trusted.example", ~"app.example", https)),
        ?assertEqual(
            forbidden, arizona_origin:check(~"https://evil.example", ~"app.example", https)
        )
    after
        application:unset_env(arizona, csrf_origins)
    end.

allowlist_mixes_binary_and_charlist_entries(Config) when is_list(Config) ->
    %% A half-converted list is the realistic shape of that typo.
    application:set_env(arizona, csrf_origins, [~"https://a.example", "https://b.example"]),
    try
        ?assertEqual(ok, arizona_origin:check(~"https://a.example", ~"app.example", https)),
        ?assertEqual(ok, arizona_origin:check(~"https://b.example", ~"app.example", https)),
        ?assertEqual(
            forbidden, arizona_origin:check(~"https://c.example", ~"app.example", https)
        )
    after
        application:unset_env(arizona, csrf_origins)
    end.

allowlist_non_origin_entry_names_itself(Config) when is_list(Config) ->
    %% An entry that is not an origin in any spelling cannot be guessed at, so it
    %% fails naming itself rather than as an opaque `bad_generator`/`badarg` from
    %% somewhere inside the compare.
    application:set_env(arizona, csrf_origins, [~"https://a.example", not_an_origin]),
    try
        ?assertError(
            {invalid_csrf_origin, not_an_origin},
            arizona_origin:check(~"https://evil.example", ~"app.example", https)
        )
    after
        application:unset_env(arizona, csrf_origins)
    end.

allowlist_follows_env_change(Config) when is_list(Config) ->
    %% The lowercased allowlist is cached (keyed by the raw app-env value), so
    %% repeated checks must not go stale: a changed `csrf_origins` app env takes
    %% effect on the next request, both granting a newly listed origin and
    %% revoking a delisted one.
    application:set_env(arizona, csrf_origins, [~"https://a.example"]),
    try
        ?assertEqual(ok, arizona_origin:check(~"https://a.example", ~"app.example", https)),
        ?assertEqual(
            forbidden, arizona_origin:check(~"https://b.example", ~"app.example", https)
        ),
        application:set_env(arizona, csrf_origins, [~"https://b.example"]),
        ?assertEqual(ok, arizona_origin:check(~"https://b.example", ~"app.example", https)),
        ?assertEqual(
            forbidden, arizona_origin:check(~"https://a.example", ~"app.example", https)
        )
    after
        application:unset_env(arizona, csrf_origins)
    end.

allowlist_resolves_env_reference(Config) when is_list(Config) ->
    %% A `{env, ...}` reference as the csrf_origins value resolves through the
    %% cache exactly as the direct read did.
    Var = "ARIZONA_ORIGIN_SUITE_ALLOWLIST",
    true = os:putenv(Var, "https://proxy.example"),
    application:set_env(arizona, csrf_origins, {env, Var, []}),
    try
        ?assertEqual(ok, arizona_origin:check(~"https://proxy.example", ~"app.example", https)),
        ?assertEqual(
            forbidden, arizona_origin:check(~"https://other.example", ~"app.example", https)
        )
    after
        application:unset_env(arizona, csrf_origins),
        os:unsetenv(Var)
    end.

disabled_allows_any_origin(Config) when is_list(Config) ->
    application:set_env(arizona, check_origin, false),
    try
        ?assertEqual(ok, arizona_origin:check(~"https://evil.example", ~"app.example", https))
    after
        application:unset_env(arizona, check_origin)
    end.

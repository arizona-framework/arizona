-module(arizona_origin).
-moduledoc """
Origin checking for CSRF defense.

Rejects a state-changing request or a WebSocket upgrade whose `Origin` header neither
matches the request `Host` + scheme (same-origin) nor appears in the configured
allowlist. A missing `Origin` -- native (`?native`) clients, CLI tools, top-level GET
navigations -- is allowed: those carry no ambient cross-site authority to abuse.

Used by the `arizona_middleware:check_origin/2` step, which the router applies to
`live` and `controller` routes **by default** (off by exception, not by omission).

## Scheme

An origin is a scheme + authority, and the `Host` header carries no scheme, so the
caller passes the request's own (client-facing) scheme. It is enforced in the
direction that matters: an **HTTPS** request refuses a plain-`http` Origin on the
same authority. Without that, an active network attacker who serves a page at
`http://your-app` (they need no certificate, and any host not pinned by HSTS is
reachable that way) could post it cross-scheme to the real HTTPS site -- the
browser attaches the same cookies, including `Secure` ones, and the authority
alone matches.

A **plain-HTTP** request accepts either scheme, because a TLS-terminating proxy
that forwards without `X-Forwarded-Proto` leaves the backend looking like plain
HTTP while the browser correctly reports `https`. Rejecting there would break that
deployment for no gain -- the hop the check can see is already unencrypted.

## Config (`arizona` app env)

- `check_origin` (`boolean()`, default `true`) -- global switch; `false` allows every origin.
- `csrf_origins` (`[binary()]`, default `[]`) -- extra trusted origins beyond same-origin
  (e.g. a reverse proxy that rewrites `Host`, or a known partner origin).
""".

%% --------------------------------------------------------------------
%% API function exports
%% --------------------------------------------------------------------

-export([check/3]).

%% --------------------------------------------------------------------
%% API Functions
%% --------------------------------------------------------------------

-doc """
Checks an `Origin` header value against the request `Host` and client-facing
`Scheme`. Returns `ok` when the origin is trusted (same-origin, allowlisted,
missing, or checking disabled) and `forbidden` otherwise.
""".
-spec check(Origin, Host, Scheme) -> ok | forbidden when
    Origin :: binary() | undefined,
    Host :: binary() | undefined,
    Scheme :: arizona_req:scheme().
check(Origin, Host, Scheme) ->
    case arizona_config:get_env(check_origin, true) of
        false ->
            warn_disabled(),
            ok;
        _Enabled ->
            do_check(Origin, Host, Scheme)
    end.

%% --------------------------------------------------------------------
%% Internal functions
%% --------------------------------------------------------------------

%% Warn once per node when Origin checking is globally disabled, so an operator never
%% runs with CSRF protection off without a trace in the logs.
warn_disabled() ->
    case persistent_term:get({?MODULE, disabled_warned}, false) of
        true ->
            ok;
        false ->
            persistent_term:put({?MODULE, disabled_warned}, true),
            logger:warning(
                "arizona CSRF Origin checking is globally disabled "
                "(application env check_origin=false)"
            )
    end.

do_check(undefined, _Host, _Scheme) ->
    ok;
do_check(Origin, Host, Scheme) ->
    case same_origin(Origin, Host, Scheme) orelse allowlisted(Origin) of
        true -> ok;
        false -> forbidden
    end.

%% Compare the Origin's authority (host[:port], after stripping `scheme://`) to the
%% Host header, case-insensitively, plus the scheme rule below. Browsers include/omit
%% the default port consistently in both Origin and Host, so plain authority equality
%% holds. A scheme-less or `null` Origin never matches.
same_origin(_Origin, undefined, _Scheme) ->
    false;
same_origin(Origin, Host, Scheme) ->
    case binary:split(Origin, ~"://") of
        [OriginScheme, Authority] ->
            same_scheme(OriginScheme, Scheme) andalso string:equal(Authority, Host, true);
        _ ->
            false
    end.

%% An HTTPS request demands an https Origin: the plain-HTTP page on the same
%% authority is a different origin that an active network attacker can serve
%% without a certificate, and the browser sends the same cookies to both. A
%% plain-HTTP request takes either, because a TLS-terminating proxy that drops
%% `X-Forwarded-Proto` is indistinguishable from real plain HTTP here.
same_scheme(_OriginScheme, http) ->
    true;
same_scheme(OriginScheme, https) ->
    string:equal(OriginScheme, ~"https", true).

%% Match case-insensitively, consistent with `same_origin/2`: browser Origins are
%% already lowercase, so an uppercase `csrf_origins` entry is an operator typo that
%% should still match its lowercased origin, not silently 403.
allowlisted(Origin) ->
    Lower = string:lowercase(Origin),
    lists:member(Lower, [string:lowercase(O) || O <- arizona_config:get_env(csrf_origins, [])]).

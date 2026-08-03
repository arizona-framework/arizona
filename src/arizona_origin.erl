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
  (e.g. a reverse proxy that rewrites `Host`, or a known partner origin). A string entry
  (`"https://app.example"`) is accepted as well; anything that is not an origin in some
  iodata spelling raises `{invalid_csrf_origin, Entry}` naming it.
""".

%% --------------------------------------------------------------------
%% API function exports
%% --------------------------------------------------------------------

-export([check/3]).
-export([format_error/2]).

%% Called by `erl_error` via the `error_info` annotation, not directly.
-ignore_xref([format_error/2]).

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

-doc """
Renders the error raised when a `csrf_origins` entry is not an origin. Picked up
by `erl_error` via the `error_info` annotation.
""".
-spec format_error(Reason, Stacktrace) -> ErrorInfo when
    Reason :: term(),
    Stacktrace :: [tuple()],
    ErrorInfo :: #{general := iolist()}.
format_error({invalid_csrf_origin, Origin}, _ST) ->
    #{
        general => io_lib:format(
            "the `csrf_origins` application env entry ~0tp is not an origin. Each entry "
            "is an origin binary such as <<\"https://app.example\">> (a string like "
            "\"https://app.example\" is accepted too); an entry that is neither can "
            "never match a request's `Origin` header.",
            [Origin]
        )
    }.

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
            same_scheme(OriginScheme, Scheme) andalso ascii_equal(Authority, Host);
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
    ascii_equal(OriginScheme, ~"https").

%% Match case-insensitively, consistent with `same_origin/2`: browser Origins are
%% already lowercase, so an uppercase `csrf_origins` entry is an operator typo that
%% should still match its lowercased origin, not silently 403.
allowlisted(Origin) ->
    lists:member(ascii_lowercase(Origin), allowlist()).

%% The lowercased allowlist, cached in persistent_term keyed by the RAW app-env
%% value (a cheap read), so the `{env, ...}` resolution and per-entry lowercasing
%% run once per distinct config value instead of on every request. A changed
%% `csrf_origins` app env still takes effect (the raw value no longer matches the
%% cached key); what stays fixed -- `arizona_config`'s resolved-at-startup
%% semantics, the same staleness stance `warn_disabled/0` takes -- is the resolved
%% value of an unchanged `{env, ...}` reference whose underlying variable is
%% mutated at runtime.
allowlist() ->
    Raw = application:get_env(arizona, csrf_origins, []),
    case persistent_term:get({?MODULE, allowlist}, undefined) of
        {Raw, Lowered} ->
            Lowered;
        _Other ->
            Lowered = [normalize_origin(O) || O <- arizona_config:resolve(Raw)],
            persistent_term:put({?MODULE, allowlist}, {Raw, Lowered}),
            Lowered
    end.

%% Normalize one configured origin to the lowercased binary the compare needs.
%% The documented type is `[binary()]`, but `"https://app.example"` is the single
%% easiest sys.config typo and this is the security path, so every iodata
%% spelling is accepted rather than left to misbehave: a charlist entry used to
%% build an allowlist that could never match (`lists:member/2` compares it
%% against a binary `Origin`), silently doing nothing an operator asked for. An
%% entry that is not iodata at all cannot be guessed at, so it fails naming
%% itself instead of surfacing as a `badarg` from inside the compare.
normalize_origin(Origin) when is_binary(Origin) ->
    ascii_lowercase(Origin);
normalize_origin(Origin) ->
    try
        ascii_lowercase(iolist_to_binary(Origin))
    catch
        error:badarg ->
            erlang:error({invalid_csrf_origin, Origin}, none, [
                {error_info, #{module => ?MODULE}}
            ])
    end.

%% Case-insensitive compare over BYTES, not Unicode codepoints. An origin is
%% ASCII by RFC 6454, so Unicode casefolding buys nothing -- and it actively
%% breaks here: `string:equal/3` and `string:lowercase/1` raise `badarg` on
%% invalid UTF-8, which a client is free to put in an `Origin` header (the
%% transport permits bytes >= 0x80 in header values). That turned a request
%% that must simply be refused into a `500`. Byte-wise is also the cheaper
%% compare on the per-request path.
ascii_equal(A, A) ->
    true;
ascii_equal(A, B) ->
    ascii_lowercase(A) =:= ascii_lowercase(B).

ascii_lowercase(Bin) ->
    <<<<(ascii_lower_byte(C))>> || <<C>> <= Bin>>.

ascii_lower_byte(C) when C >= $A, C =< $Z -> C + 32;
ascii_lower_byte(C) -> C.

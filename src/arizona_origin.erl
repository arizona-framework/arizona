-module(arizona_origin).
-moduledoc """
Origin checking for CSRF defense.

Rejects a state-changing request or a WebSocket upgrade whose `Origin` header neither
matches the request `Host` (same-origin) nor appears in the configured allowlist. A
missing `Origin` -- native (`?native`) clients, CLI tools, top-level GET navigations --
is allowed: those carry no ambient cross-site authority to abuse.

Used by the `arizona_middleware:check_origin/2` step, which the router applies to
`live` and `controller` routes **by default** (off by exception, not by omission).

## Config (`arizona` app env)

- `check_origin` (`boolean()`, default `true`) -- global switch; `false` allows every origin.
- `csrf_origins` (`[binary()]`, default `[]`) -- extra trusted origins beyond same-origin
  (e.g. a reverse proxy that rewrites `Host`, or a known partner origin).
""".

%% --------------------------------------------------------------------
%% API function exports
%% --------------------------------------------------------------------

-export([check/2]).

%% --------------------------------------------------------------------
%% API Functions
%% --------------------------------------------------------------------

-doc """
Checks an `Origin` header value against the request `Host`. Returns `ok` when the
origin is trusted (same-origin, allowlisted, missing, or checking disabled) and
`forbidden` otherwise.
""".
-spec check(Origin, Host) -> ok | forbidden when
    Origin :: binary() | undefined,
    Host :: binary() | undefined.
check(Origin, Host) ->
    case application:get_env(arizona, check_origin, true) of
        false ->
            warn_disabled(),
            ok;
        _Enabled ->
            do_check(Origin, Host)
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

do_check(undefined, _Host) ->
    ok;
do_check(Origin, Host) ->
    case same_origin(Origin, Host) orelse allowlisted(Origin) of
        true -> ok;
        false -> forbidden
    end.

%% Compare the Origin's authority (host[:port], after stripping `scheme://`) to the
%% Host header, case-insensitively. Browsers include/omit the default port consistently
%% in both Origin and Host, so plain authority equality holds. A scheme-less or `null`
%% Origin never matches.
same_origin(_Origin, undefined) ->
    false;
same_origin(Origin, Host) ->
    case binary:split(Origin, ~"://") of
        [_Scheme, Authority] -> string:equal(Authority, Host, true);
        _ -> false
    end.

allowlisted(Origin) ->
    lists:member(Origin, application:get_env(arizona, csrf_origins, [])).

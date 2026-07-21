-module(arizona_req).
-moduledoc """
HTTP request abstraction with adapter pattern for different web servers.

Provides a unified interface for HTTP requests behind a transport
behaviour (roadrunner is the built-in implementation). Features lazy
loading of request data with caching to avoid re-parsing across accesses.

## Adapter pattern

A web-server adapter implements the mandatory parsing callbacks:

- `parse_bindings/1` -- extract route-pattern path bindings
- `parse_params/1` -- parse the query string as a proplist
- `parse_cookies/1` -- parse the Cookie header as a proplist
- `parse_headers/1` -- headers as a map
- `read_body/1` -- consume the request body

Plus one optional callback for live-capable transports:

- `resolve_route/3` -- resolve a SPA-navigate target. Required for
  WebSocket sessions, omitted for HTTP-only adapters.

The adapter module is stored inside the request record itself, so every
accessor dispatches to the correct backend, and `arizona_socket`
recovers the adapter via `arizona_req:adapter/1` for navigate.

## Lazy loading

Fields other than `method` and `path` are populated on first access and
cached. Accessors return `{Value, Request1}` -- the updated request
carries the cached value. Callers thread the returned request forward.

```erlang
Method = arizona_req:method(Req).          %% eager, no thread
{Bs,     Req1} = arizona_req:bindings(Req).
{Params, Req2} = arizona_req:params(Req1).
```
""".

%% This module is the HTTP request API surface: one lazy accessor per request
%% field plus the response stash (headers, cookies, status, flash). That is a
%% cohesive set of small accessors, so it legitimately exceeds the god-module
%% function count.
-elvis([{elvis_style, no_god_modules, disable}]).

%% --------------------------------------------------------------------
%% API function exports
%% --------------------------------------------------------------------

-export([new/3]).
-export([adapter/1]).
-export([method/1]).
-export([path/1]).
-export([request_id/1]).
-export([raw/1]).
-export([set_raw/2]).
-export([bindings/1]).
-export([params/1]).
-export([cookies/1]).
-export([headers/1]).
-export([user_agent/1]).
-export([body/1]).
-export([call_resolve_route/4]).
-export([redirect/2]).
-export([redirect/3]).
-export([halted_redirect/1]).
-export([put_resp_header/3]).
-export([put_resp_cookie/4]).
-export([put_resp_status/2]).
-export([resp_headers/1]).
-export([resp_cookies/1]).
-export([resp_status/1]).
-export([put_flash/3]).
-export([flash/1]).
-export([flash_out/1]).
-export([consume_flash/1]).
-export([seed_flash/2]).
-export([put_session/3]).
-export([delete_session/2]).
-export([clear_session/1]).
-export([session/1]).
-export([get_session/2]).
-export([get_session/3]).
-export([read_session/1]).
-export([session_id/1]).
-export([session_error/1]).

%% --------------------------------------------------------------------
%% Ignore xref warnings
%% --------------------------------------------------------------------

-ignore_xref([method/1]).
-ignore_xref([path/1]).
-ignore_xref([request_id/1]).
-ignore_xref([set_raw/2]).
-ignore_xref([bindings/1]).
-ignore_xref([params/1]).
-ignore_xref([cookies/1]).
-ignore_xref([headers/1]).
-ignore_xref([user_agent/1]).
-ignore_xref([body/1]).
-ignore_xref([redirect/2]).
-ignore_xref([redirect/3]).
-ignore_xref([halted_redirect/1]).
-ignore_xref([put_resp_header/3]).
-ignore_xref([put_resp_cookie/4]).
-ignore_xref([put_resp_status/2]).
-ignore_xref([resp_headers/1]).
-ignore_xref([resp_cookies/1]).
-ignore_xref([resp_status/1]).
-ignore_xref([put_flash/3]).
-ignore_xref([flash/1]).
-ignore_xref([put_session/3]).
-ignore_xref([delete_session/2]).
-ignore_xref([clear_session/1]).
-ignore_xref([session/1]).
-ignore_xref([get_session/2]).
-ignore_xref([get_session/3]).
-ignore_xref([session_id/1]).
-ignore_xref([session_error/1]).

%% --------------------------------------------------------------------
%% Types exports
%% --------------------------------------------------------------------

-export_type([request/0]).
-export_type([adapter/0]).
-export_type([raw/0]).
-export_type([method/0]).
-export_type([path/0]).
-export_type([bindings/0]).
-export_type([params/0]).
-export_type([cookies/0]).
-export_type([headers/0]).
-export_type([body/0]).
-export_type([redirect_status/0]).
-export_type([resp_status/0]).
-export_type([flash/0]).
-export_type([session/0]).
-export_type([resp_cookie_opts/0]).
-export_type([qs/0]).

%% --------------------------------------------------------------------
%% Types definitions
%% --------------------------------------------------------------------

-opaque request() :: #{
    adapter := adapter(),
    raw := raw(),
    method := method(),
    path := path(),
    request_id => binary(),
    bindings => bindings(),
    params => params(),
    cookies => cookies(),
    headers => headers(),
    body => body(),
    redirect => {redirect_status(), binary()},
    resp_headers => [{binary(), iodata()}],
    resp_cookies => [{binary(), binary(), resp_cookie_opts()}],
    resp_status => resp_status(),
    flash_out => flash(),
    flash_in => flash(),
    flash_consumed => boolean(),
    session_in => session(),
    session_out => session(),
    session_dirty => boolean(),
    session_id => binary(),
    session_error => {error, term()}
}.

-nominal adapter() :: module().
-nominal raw() :: dynamic().
-nominal method() :: binary().
-nominal path() :: binary().
-nominal bindings() :: #{binary() => dynamic()}.
-nominal params() :: [{binary(), binary() | true}].
-nominal cookies() :: [{binary(), binary()}].
-nominal headers() :: #{binary() => iodata()}.
-nominal body() :: binary().

%% HTTP redirect status codes (RFC 9110 §15.4). The most common are
%% 301 (moved permanently), 302 (found), 303 (see other), 307
%% (temporary redirect), 308 (permanent redirect). The full 3xx range
%% is allowed so rarer codes (300 multiple choices, 304 not modified,
%% etc.) remain expressible -- callers pick the semantics they want.
-nominal redirect_status() :: 300..399.

%% Any HTTP response status. Stashed by `put_resp_status/2` and used as the OK
%% status of a rendered page (default 200) -- e.g. a 401 from an auth gate.
-nominal resp_status() :: 100..599.

%% A flash payload: short-lived display messages carried across one redirect.
%% Keys are binaries (an atom key passed to `put_flash/3` is normalized); values
%% are anything `json:encode/1` accepts (typically display strings).
-nominal flash() :: #{binary() => term()}.

%% A session payload: durable state carried across requests in an encrypted cookie.
%% Keys are binaries (an atom key passed to `put_session/3` is normalized); values
%% are anything `json:encode/1` accepts. Encrypted (confidential), but a cookie store
%% cannot be revoked before expiry -- keep sessions small (an id, light state).
-nominal session() :: #{binary() => term()}.

%% Response cookie options stashed by `put_resp_cookie/4` and serialized by
%% the transport. Mirrors the transport cookie serializer's options.
-nominal resp_cookie_opts() :: #{
    domain => binary(),
    path => binary(),
    max_age => non_neg_integer(),
    expires => binary(),
    secure => boolean(),
    http_only => boolean(),
    same_site => strict | lax | none
}.

%% Query string passed to `resolve_route/3` for SPA navigate.
-nominal qs() :: binary().

%% --------------------------------------------------------------------
%% Behaviour callbacks
%% --------------------------------------------------------------------

-callback parse_bindings(raw()) -> bindings().
-callback parse_params(raw()) -> params().
-callback parse_cookies(raw()) -> cookies().
-callback parse_headers(raw()) -> headers().
-callback read_body(raw()) -> {body(), raw()}.

%% Optional: live-capable adapters export this for SPA navigate.
%% Returns `{ok, Handler, RouteOpts, NavRequest}` when the path resolves to a
%% live (page) route, or `error` when it does not -- no matching route, a
%% non-live route (controller/asset/ws), or a method mismatch. The path is
%% client-supplied (`_az_path` on the WS upgrade, or a navigate/patch frame), so
%% the callback must never crash on it; the caller turns `error` into a 404
%% upgrade rejection or a full-page navigate. HTTP-only adapters can omit it.
-callback resolve_route(path(), qs(), raw()) ->
    {ok, module(), arizona_live:route_opts(), request()} | error.

-optional_callbacks([resolve_route/3]).

%% --------------------------------------------------------------------
%% API Functions
%% --------------------------------------------------------------------

-doc """
Creates a request backed by `Adapter` wrapping `Raw`.

`Opts` must contain `method` and `path`; any of
`bindings | params | cookies | headers | body` may be pre-populated to
skip their adapter callbacks on first access.
""".
-spec new(Adapter, Raw, Opts) -> request() when
    Adapter :: adapter(),
    Raw :: raw(),
    Opts :: #{
        method := method(),
        path := path(),
        request_id => binary(),
        bindings => bindings(),
        params => params(),
        cookies => cookies(),
        headers => headers(),
        body => body()
    }.
new(Adapter, Raw, #{method := Method, path := Path} = Opts) when is_atom(Adapter) ->
    Base = #{adapter => Adapter, raw => Raw, method => Method, path => Path},
    maps:merge(Base, maps:without([method, path], Opts)).

-doc "Returns the adapter module that backs the request.".
-spec adapter(request()) -> adapter().
adapter(#{adapter := Adapter}) -> Adapter.

-doc "Returns the HTTP method (e.g. `~\"GET\"`, `~\"POST\"`).".
-spec method(request()) -> method().
method(#{method := Method}) -> Method.

-doc "Returns the request path (no query string).".
-spec path(request()) -> path().
path(#{path := Path}) -> Path.

-doc """
Returns the adapter-supplied request ID (e.g. roadrunner's 16-char
hex correlation token), or `undefined` if the adapter doesn't
populate one.

Useful for live-view handlers and crash-report glue that need to
correlate Arizona-side events with the adapter's access logs.
""".
-spec request_id(request()) -> binary() | undefined.
request_id(#{request_id := Id}) -> Id;
request_id(_Req) -> undefined.

-doc "Returns the raw, transport-native request value the adapter wraps.".
-spec raw(request()) -> raw().
raw(#{raw := Raw}) -> Raw.

-doc """
Replaces the raw value inside `Request` and clears all lazy caches.

Useful after a transport-specific mutation (e.g. writing a reply on
halt) that returns an updated raw. Cached fields are dropped because
they may no longer reflect the new raw.
""".
-spec set_raw(request(), raw()) -> request().
set_raw(Req, Raw) ->
    maps:without([bindings, params, cookies, headers, body], Req#{raw => Raw}).

-doc """
Returns route-pattern path bindings. Lazy-loaded via the adapter on
first access, cached in the returned request for subsequent calls.
""".
-spec bindings(request()) -> {bindings(), request()}.
bindings(#{bindings := Bindings} = Req) ->
    {Bindings, Req};
bindings(#{adapter := Adapter, raw := Raw} = Req) ->
    Bindings = Adapter:parse_bindings(Raw),
    {Bindings, Req#{bindings => Bindings}}.

-doc """
Returns parsed query-string params as a proplist. Lazy-loaded and cached.

Preserves repeat keys (`?tag=a&tag=b` returns two entries).
""".
-spec params(request()) -> {params(), request()}.
params(#{params := Params} = Req) ->
    {Params, Req};
params(#{adapter := Adapter, raw := Raw} = Req) ->
    Params = Adapter:parse_params(Raw),
    {Params, Req#{params => Params}}.

-doc "Returns parsed cookies as a proplist. Lazy-loaded and cached.".
-spec cookies(request()) -> {cookies(), request()}.
cookies(#{cookies := Cookies} = Req) ->
    {Cookies, Req};
cookies(#{adapter := Adapter, raw := Raw} = Req) ->
    Cookies = Adapter:parse_cookies(Raw),
    {Cookies, Req#{cookies => Cookies}}.

-doc "Returns request headers as a map. Lazy-loaded and cached.".
-spec headers(request()) -> {headers(), request()}.
headers(#{headers := Headers} = Req) ->
    {Headers, Req};
headers(#{adapter := Adapter, raw := Raw} = Req) ->
    Headers = Adapter:parse_headers(Raw),
    {Headers, Req#{headers => Headers}}.

-doc """
Returns the `User-Agent` request header (or `<<>>` if absent).

A view that serves both browsers (HTML) and native apps (a native JSON tree)
reads this via an `arizona_middleware:extract([user_agent])` middleware and
branches in `render/1`. Match it directly (you know your own native client's
UA), or classify it with the pure helpers in `arizona_user_agent` (`browser/1`,
`os/1`, `mobile/1`). The framework decides and injects nothing -- you own the
binding and the branch. For an explicit signal instead, send a query param and
extract it with `arizona_middleware:extract([params])`.
""".
-spec user_agent(request()) -> {binary(), request()}.
user_agent(Req0) ->
    {Headers, Req1} = headers(Req0),
    {iolist_to_binary(maps:get(~"user-agent", Headers, <<>>)), Req1}.

-doc """
Consumes the request body. Lazy-loaded; the underlying raw request is
updated because reading a body is mutating in most transports.
""".
-spec body(request()) -> {body(), request()}.
body(#{body := Body} = Req) ->
    {Body, Req};
body(#{adapter := Adapter, raw := Raw} = Req) ->
    {Body, Raw1} = Adapter:read_body(Raw),
    {Body, Req#{raw => Raw1, body => Body}}.

%% Internal use only -- invoked by `arizona_socket:handle_navigate/3`
%% and `arizona_ws:prepare/3`. Crashes with `undef` if `Adapter` did
%% not implement the optional `resolve_route/3` callback.
-doc false.
-spec call_resolve_route(Adapter, Path, Qs, Raw) ->
    {ok, module(), arizona_live:route_opts(), request()} | error
when
    Adapter :: adapter(),
    Path :: path(),
    Qs :: qs(),
    Raw :: raw().
call_resolve_route(Adapter, Path, Qs, Raw) ->
    Adapter:resolve_route(Path, Qs, Raw).

-doc """
Stashes a 302 redirect intent in `Request` so downstream transports
translate it uniformly.

Middleware authors should return `{halt, arizona_req:redirect(Req,
Location)}` instead of writing a transport-specific response
directly. HTTP transports read the intent and emit a proper redirect
reply; WS navigate transports emit an `arizona_js:navigate/1`
client effect so the browser pushes the new URL.
""".
-spec redirect(Request, Location) -> Request when
    Request :: request(),
    Location :: binary().
redirect(Req, Location) ->
    redirect(Req, 302, Location).

-doc "Like `redirect/2` but lets callers pick the 3xx status code.".
-spec redirect(Request, Status, Location) -> Request when
    Request :: request(),
    Status :: redirect_status(),
    Location :: binary().
redirect(Req, Status, Location) when
    is_integer(Status), Status >= 300, Status =< 399, is_binary(Location)
->
    Req#{redirect => {Status, Location}}.

-doc """
Returns the stashed redirect intent, or `undefined` when no
`redirect/2,3` was called on this request. Transports use this to
translate a middleware `{halt, Req}` return into the appropriate
client-visible response.
""".
-spec halted_redirect(Request) -> {redirect_status(), binary()} | undefined when
    Request :: request().
halted_redirect(#{redirect := Redirect}) -> Redirect;
halted_redirect(_) -> undefined.

-doc """
Stashes a response header on `Request`. Transports flush stashed headers
onto the outgoing response (redirect, rendered page, or halt). Repeated
calls accumulate (newest first).
""".
-spec put_resp_header(Request, Name, Value) -> Request when
    Request :: request(),
    Name :: binary(),
    Value :: iodata().
put_resp_header(Req, Name, Value) when is_binary(Name) ->
    Headers = maps:get(resp_headers, Req, []),
    Req#{resp_headers => [{Name, Value} | Headers]}.

-doc """
Stashes a response cookie on `Request`, serialized by the transport when the
response is built. Lets a middleware set a cookie alongside a `redirect/2,3`
or a rendered page. `Opts` follows `resp_cookie_opts()`.
""".
-spec put_resp_cookie(Request, Name, Value, Opts) -> Request when
    Request :: request(),
    Name :: binary(),
    Value :: binary(),
    Opts :: resp_cookie_opts().
put_resp_cookie(Req, Name, Value, Opts) when
    is_binary(Name), is_binary(Value), is_map(Opts)
->
    Cookies = maps:get(resp_cookies, Req, []),
    Req#{resp_cookies => [{Name, Value, Opts} | Cookies]}.

-doc "Returns the stashed response headers (newest first), or `[]`.".
-spec resp_headers(Request) -> [{binary(), iodata()}] when
    Request :: request().
resp_headers(#{resp_headers := Headers}) -> Headers;
resp_headers(_) -> [].

-doc """
Returns the stashed response cookies (newest first), or `[]`, plus the flash and
session cookies when warranted: a freshly `put_flash/3`-set flash serializes to a
signed `Set-Cookie` (a consumed incoming flash with no new one serializes to a
clearing cookie); a written session (`put_session/3`/`delete_session/2`) serializes
to an encrypted `Set-Cookie`, a cleared one (`clear_session/1`) to a clearing
cookie, and an untouched session to nothing. Transports flush this list verbatim.
""".
-spec resp_cookies(Request) -> [{binary(), binary(), resp_cookie_opts()}] when
    Request :: request().
resp_cookies(Req) ->
    Stashed = maps:get(resp_cookies, Req, []),
    FlashOut = maps:get(flash_out, Req, #{}),
    Consumed = maps:get(flash_consumed, Req, false),
    WithFlash =
        case arizona_flash:resp_cookie(FlashOut, Consumed) of
            none -> Stashed;
            FlashCookie -> Stashed ++ [FlashCookie]
        end,
    case session_resp_cookie(Req) of
        none -> WithFlash;
        SessionCookie -> WithFlash ++ [SessionCookie]
    end.

%% The session `Set-Cookie` for this response. In store mode this also commits the write
%% (the response is where a written session is persisted): a non-empty dirty session is
%% put under its id and the signed id is emitted; a cleared one (logout) is deleted and
%% the cookie cleared; an untouched session emits nothing. Cookie mode encrypts the map
%% into the cookie as before.
session_resp_cookie(Req) ->
    SessionOut = maps:get(session_out, Req, #{}),
    Dirty = maps:get(session_dirty, Req, false),
    case session_store() of
        undefined ->
            arizona_session:resp_cookie(SessionOut, Dirty);
        Store ->
            store_resp_cookie(Store, Req, SessionOut, Dirty)
    end.

store_resp_cookie(_Store, _Req, _SessionOut, false) ->
    none;
store_resp_cookie(Store, Req, SessionOut, true) when map_size(SessionOut) > 0 ->
    Id = maps:get(session_id, Req),
    ok = Store:put(Id, SessionOut, arizona_session:max_age()),
    arizona_session:set_cookie_id(Id);
store_resp_cookie(Store, Req, _SessionOut, true) ->
    %% Cleared (logout): drop the store entry if we know the id, and clear the cookie.
    ok = maybe_delete_stored(Store, maps:get(session_id, Req, undefined)),
    arizona_session:clear_cookie().

maybe_delete_stored(_Store, undefined) -> ok;
maybe_delete_stored(Store, Id) -> Store:delete(Id).

-doc """
Stashes the HTTP status for a rendered page. Lets a view or middleware return
a non-200 status (e.g. `401` from an auth gate, `404` from a not-found page)
while still rendering a body. Ignored for redirects and halts, which carry
their own status.
""".
-spec put_resp_status(Request, Status) -> Request when
    Request :: request(),
    Status :: resp_status().
put_resp_status(Req, Status) when is_integer(Status), Status >= 100, Status =< 599 ->
    Req#{resp_status => Status}.

-doc "Returns the stashed response status, or `undefined`.".
-spec resp_status(Request) -> resp_status() | undefined when
    Request :: request().
resp_status(#{resp_status := Status}) -> Status;
resp_status(_) -> undefined.

-doc """
Stashes a flash message to carry across one redirect (the Post/Redirect/Get
pattern). On the response it serializes to a signed, short-lived `Set-Cookie`;
the next request reads it once via `flash/1` (or the auto-injected `flash`
binding) and it is then cleared. `Key` may be an atom or binary (normalized to a
binary); repeated calls accumulate, last write wins per key.

Requires `secret_key` in the `arizona` application env (used to sign the cookie);
errors if it is unset.
""".
-spec put_flash(Request, Key, Value) -> Request when
    Request :: request(),
    Key :: atom() | binary(),
    Value :: term().
put_flash(Req, Key, Value) ->
    %% Fail fast at the call site if signing is unconfigured, rather than later
    %% when the response serializes the cookie.
    _ = arizona_flash:secret(),
    FlashOut = maps:get(flash_out, Req, #{}),
    Req#{flash_out => FlashOut#{arizona_flash:key(Key) => Value}}.

-doc """
Returns the incoming flash for this request (the message a `put_flash/3` set on the
prior request before a redirect), or `#{}` when there is none.

The app-facing flash reader. The framework materializes the flash before your view
runs -- decoded from the signed cookie on a fresh request, or carried in-process
across a WebSocket SPA navigate -- so a view reads it here or through the
auto-injected `flash` binding. Pair it with `put_flash/3` (the writer).
""".
-spec flash(Request) -> flash() when
    Request :: request().
flash(#{flash_in := Flash}) -> Flash;
flash(_) -> #{}.

-doc """
Returns the outgoing flash this request has staged via `put_flash/3` (destined for
the *next* request), or `#{}` when none was set.

The companion to `flash/1`: where `flash/1` reads the **incoming** flash (what this
request received), `flash_out/1` reads the **outgoing** one (what it has queued to
send). The framework carries this map onward -- a signed cookie for a full-page
redirect, or in-process across a WebSocket SPA navigate -- so it is primarily an
introspection/testing helper: a test can assert what a middleware or handler
flashed before a redirect without decoding the cookie.
""".
-spec flash_out(Request) -> flash() when
    Request :: request().
flash_out(#{flash_out := Flash}) -> Flash;
flash_out(_) -> #{}.

%% Internal use only -- the consuming cookie reader behind the `fetch_flash`
%% middleware and the HTTP render pipeline: reads, verifies, and decodes the flash
%% cookie into the request, marking it consumed so the response clears it
%% (read-once). Returns `{Flash, Req}` and consumes -- the destructive counterpart
%% to the non-consuming `read_session/1`. Idempotent (a second call returns the
%% cached value without re-marking). App code reads the flash with `flash/1` or the
%% `flash` binding.
-doc false.
-spec consume_flash(Request) -> {flash(), Request} when
    Request :: request().
consume_flash(#{flash_in := Flash} = Req) ->
    {Flash, Req};
consume_flash(Req0) ->
    {Cookies, Req} = cookies(Req0),
    case lists:keyfind(arizona_flash:cookie_name(), 1, Cookies) of
        {_, Value} ->
            Flash = arizona_flash:decode(Value),
            {Flash, Req#{flash_in => Flash, flash_consumed => true}};
        false ->
            {#{}, Req#{flash_in => #{}}}
    end.

%% Internal use only -- carry plumbing invoked by `arizona_socket` to seed the
%% incoming side of the follow-up request with a flash a halting middleware set
%% (the `flash_out` map) -- so `flash/1` reads it with no cookie -- when carried
%% in-process across a WebSocket SPA navigate, where a `{halt, redirect}` has no
%% `Set-Cookie` leg. Merges onto any flash already present (last write wins per
%% key). App code sets a flash with `put_flash/3` and reads it with `flash/1`; it
%% never calls this.
-doc false.
-spec seed_flash(Request, Flash) -> Request when
    Request :: request(),
    Flash :: flash().
seed_flash(Req, Flash) when is_map(Flash) ->
    Current = maps:get(flash_in, Req, #{}),
    Req#{flash_in => maps:merge(Current, Flash)}.

-doc """
Writes `Key => Value` into the session. On the response the session serializes to a
freshly encrypted `Set-Cookie` (durable -- it persists until overwritten or
cleared). `Key` may be an atom or binary (normalized to a binary); repeated calls
accumulate, last write wins per key.

The first write seeds the pending session from the **incoming** session (reading the
cookie if not already read), so a write merges onto existing state even without the
`fetch_session` middleware. Requires `secret_key` in the `arizona` application env
(used to encrypt the cookie); errors if it is unset.
""".
-spec put_session(Request, Key, Value) -> Request when
    Request :: request(),
    Key :: atom() | binary(),
    Value :: term().
put_session(Req0, Key, Value) ->
    %% Fail fast at the call site if encryption is unconfigured, rather than later
    %% when the response serializes the cookie.
    _ = arizona_crypto:secret(),
    {Current, Req} = ensure_session_out(Req0),
    Req#{
        session_out => Current#{arizona_session:key(Key) => Value},
        session_dirty => true
    }.

-doc """
Removes `Key` from the session and marks it written. On the response the session
serializes to the re-encrypted (still non-empty) cookie, or a clearing cookie if
this removed the last key.
""".
-spec delete_session(Request, Key) -> Request when
    Request :: request(),
    Key :: atom() | binary().
delete_session(Req0, Key) ->
    _ = arizona_crypto:secret(),
    {Current, Req} = ensure_session_out(Req0),
    Req#{
        session_out => maps:remove(arizona_session:key(Key), Current),
        session_dirty => true
    }.

-doc """
Clears the entire session (logout). On the response a clearing `Set-Cookie` is sent
regardless of the incoming session. Unlike `put_session/3` it does not require
`secret_key` -- clearing encrypts nothing, so logout always works.
""".
-spec clear_session(Request) -> Request when
    Request :: request().
clear_session(Req) ->
    Req#{session_out => #{}, session_dirty => true}.

-doc """
Returns the effective session for this request: the pending written state
(`session_out`) when it has been written this request, otherwise the incoming
session (`session_in`, `#{}` if not yet read). A view or controller sees the session
including its own writes.
""".
-spec session(Request) -> session() when
    Request :: request().
session(#{session_dirty := true, session_out := Session}) -> Session;
session(#{session_in := Session}) -> Session;
session(_) -> #{}.

-doc """
Returns the opaque session id for this request in store mode (`session_store`
configured), or `undefined` in cookie mode or when there is no session yet. Apps can
record it to revoke the session later via the store's `delete/1`.
""".
-spec session_id(Request) -> binary() | undefined when
    Request :: request().
session_id(#{session_id := Id}) -> Id;
session_id(_) -> undefined.

-doc """
Returns `{error, Reason}` when reading the server-side session failed this request, or
`undefined` when it did not.

Set in store mode when the store's `get/1` answers `{error, Reason}` (the backing store
was unreachable) -- **not** for a genuinely absent, expired, or revoked session, nor a
tampered/forged cookie, all of which read as a signed-out request with no error. Lets an
app at the request boundary (a controller, or a middleware after `fetch_session`) log the
outage or serve a degraded response; the auth decision is unaffected, since a failed read
still yields an empty session. The `{error, _}` wrapper keeps any `Reason` (including
`undefined`) distinct from the no-error sentinel.
""".
-spec session_error(Request) -> {error, term()} | undefined when
    Request :: request().
session_error(#{session_error := Error}) -> Error;
session_error(_) -> undefined.

-doc """
Looks up `Key` in the effective session, returning `{ok, Value}` or `error`. `Key`
may be an atom or binary (normalized). Reads the post-write state when the session
was written earlier this request.
""".
-spec get_session(Request, Key) -> {ok, term()} | error when
    Request :: request(),
    Key :: atom() | binary().
get_session(Req, Key) ->
    BinKey = arizona_session:key(Key),
    case session(Req) of
        #{BinKey := Value} -> {ok, Value};
        #{} -> error
    end.

-doc "Like `get_session/2` but returns `Default` instead of `error` when absent.".
-spec get_session(Request, Key, Default) -> term() when
    Request :: request(),
    Key :: atom() | binary(),
    Default :: term().
get_session(Req, Key, Default) ->
    case get_session(Req, Key) of
        {ok, Value} -> Value;
        error -> Default
    end.

-doc """
Reads, authenticates, and decodes the incoming session cookie into the request.
Returns the session map (`#{}` when absent, tampered, malformed, or expired) and the
updated request. Idempotent and **non-consuming**: unlike `consume_flash/1` it does not
mark the session for clearing -- a durable session survives a read untouched.

Run by the `fetch_session` middleware (opt-in); apps normally use `session/1`,
`get_session/2,3`, or the `session` binding rather than calling this directly.
""".
-spec read_session(Request) -> {session(), Request} when
    Request :: request().
read_session(#{session_in := Session} = Req) ->
    {Session, Req};
read_session(Req0) ->
    {Cookies, Req} = cookies(Req0),
    case lists:keyfind(arizona_session:cookie_name(), 1, Cookies) of
        {_, Value} ->
            read_session_value(Value, Req);
        false ->
            {#{}, Req#{session_in => #{}}}
    end.

%% Decode the cookie per mode: cookie mode is the encrypted session map; store mode is a
%% signed opaque id resolved through the store. A missing/revoked/expired store entry
%% reads as an empty session with no id, so a later write mints a fresh one rather than
%% resurrecting a revoked id. A store *failure* ({error, Reason}) also reads empty (a
%% failed read must never grant access), but records the error so an app can tell an outage
%% from a signed-out request (session_error/1); like absence it stashes no session_id, so an
%% outage-time write mints a fresh id rather than trusting one the store could not confirm.
read_session_value(Value, Req) ->
    case session_store() of
        undefined ->
            Session = arizona_session:decode(Value),
            {Session, Req#{session_in => Session}};
        Store ->
            case arizona_session:decode_id(Value) of
                {ok, Id} ->
                    case Store:get(Id) of
                        {ok, Session} ->
                            {Session, Req#{session_in => Session, session_id => Id}};
                        no_session ->
                            {#{}, Req#{session_in => #{}}};
                        {error, _Reason} = Error ->
                            {#{}, Req#{session_in => #{}, session_error => Error}}
                    end;
                error ->
                    {#{}, Req#{session_in => #{}}}
            end
    end.

%% The configured server-side session store module, or `undefined` for the default
%% cookie store (the session map encrypted into the cookie).
session_store() ->
    arizona_config:get_env(session_store, undefined).

%% Returns the pending session (session_out), seeded from the incoming session on
%% first write, plus the (possibly cookie-read) request. Idempotent: once written,
%% session_out is authoritative and read_session is not re-run.
ensure_session_out(#{session_out := Out} = Req) ->
    {Out, Req};
ensure_session_out(Req0) ->
    {In, Req} = read_session(Req0),
    {In, ensure_session_id(Req#{session_out => In})}.

%% In store mode a write needs an id: reuse the one `read_session` resolved, else mint a
%% fresh one (a new session). No-op in cookie mode or when an id is already present.
ensure_session_id(#{session_id := _} = Req) ->
    Req;
ensure_session_id(Req) ->
    case session_store() of
        undefined -> Req;
        _ -> Req#{session_id => arizona_session:new_id()}
    end.

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
-export([read_flash/1]).

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
    flash_consumed => boolean()
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
%% Returns the route's `{Handler, RouteOpts, NavRequest}` triple.
%% HTTP-only adapters can omit it; `arizona_socket:handle_navigate`
%% only runs against adapters that supply the callback.
-callback resolve_route(path(), qs(), raw()) ->
    {module(), arizona_live:route_opts(), request()}.

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
    {module(), arizona_live:route_opts(), request()}
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
Returns the stashed response cookies (newest first), or `[]`, plus the flash
cookie when one is warranted: a freshly `put_flash/3`-set flash serializes to a
signed `Set-Cookie`, and a consumed incoming flash (no new one set) serializes to
a clearing cookie. Transports flush this list verbatim.
""".
-spec resp_cookies(Request) -> [{binary(), binary(), resp_cookie_opts()}] when
    Request :: request().
resp_cookies(Req) ->
    Stashed = maps:get(resp_cookies, Req, []),
    FlashOut = maps:get(flash_out, Req, #{}),
    Consumed = maps:get(flash_consumed, Req, false),
    case arizona_flash:resp_cookie(FlashOut, Consumed) of
        none -> Stashed;
        Cookie -> Stashed ++ [Cookie]
    end.

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
Returns the incoming flash read from the request, or `#{}`. Populated by
`read_flash/1` (which the HTTP render pipeline runs before the view), so views
can also read it through the auto-injected `flash` binding.
""".
-spec flash(Request) -> flash() when
    Request :: request().
flash(#{flash_in := Flash}) -> Flash;
flash(_) -> #{}.

-doc """
Reads, verifies, and decodes the incoming flash cookie into the request, marking
it consumed so the response clears it (read-once). Returns the flash map (`#{}`
when absent, unsigned-mismatch, or malformed) and the updated request. Idempotent:
a second call returns the cached value without re-marking.

Run by the HTTP render pipeline; apps normally use `flash/1` or the `flash`
binding rather than calling this directly.
""".
-spec read_flash(Request) -> {flash(), Request} when
    Request :: request().
read_flash(#{flash_in := Flash} = Req) ->
    {Flash, Req};
read_flash(Req0) ->
    {Cookies, Req} = cookies(Req0),
    case lists:keyfind(arizona_flash:cookie_name(), 1, Cookies) of
        {_, Value} ->
            Flash = arizona_flash:decode(Value),
            {Flash, Req#{flash_in => Flash, flash_consumed => true}};
        false ->
            {#{}, Req#{flash_in => #{}}}
    end.

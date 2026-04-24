-module(arizona_req).
-moduledoc """
HTTP request abstraction with adapter pattern for different web servers.

Provides a unified interface for HTTP requests across transport adapters
(Cowboy today; others pluggable via the behaviour). Features lazy loading
of request data with caching to avoid re-parsing across accesses.

## Adapter pattern

A web-server adapter implements the behaviour callbacks:

- `parse_bindings/1` -- extract route-pattern path bindings
- `parse_params/1` -- parse the query string as a proplist
- `parse_cookies/1` -- parse the Cookie header as a proplist
- `parse_headers/1` -- headers as a map
- `read_body/1` -- consume the request body

The adapter module is stored inside the request record itself, so every
accessor dispatches to the correct backend.

## Lazy loading

Fields other than `method` and `path` are populated on first access and
cached. Accessors return `{Value, Request1}` -- the updated request
carries the cached value. Callers thread the returned request forward.

```erlang
Method = arizona_req:method(Req).          %% eager, no thread
{Bs,     Req1} = arizona_req:bindings(Req).
{Params, Req2} = arizona_req:params(Req1).
```

## Middleware runner

`apply_middlewares/3` threads a request and bindings map through a list
of middleware steps. Each step returns either `{cont, Request, Bindings}`
or `{halt, Request}`. The runner stops on the first halt.

## Example adapter implementation

```erlang
-module(my_server_req).
-behaviour(arizona_req).
-export([new/1, parse_bindings/1, parse_params/1, parse_cookies/1,
         parse_headers/1, read_body/1]).

new(RawReq) ->
    arizona_req:new(?MODULE, RawReq, #{
        method => my_server:method(RawReq),
        path => my_server:path(RawReq)
    }).

parse_bindings(RawReq) -> my_server:route_params(RawReq).
%% ... etc.
```
""".

%% --------------------------------------------------------------------
%% API function exports
%% --------------------------------------------------------------------

-export([new/3]).
-export([method/1]).
-export([path/1]).
-export([raw/1]).
-export([set_raw/2]).
-export([bindings/1]).
-export([params/1]).
-export([cookies/1]).
-export([headers/1]).
-export([body/1]).
-export([apply_middlewares/3]).
-export([redirect/2]).
-export([redirect/3]).
-export([halted_redirect/1]).

%% --------------------------------------------------------------------
%% Ignore xref warnings
%% --------------------------------------------------------------------

-ignore_xref([method/1]).
-ignore_xref([path/1]).
-ignore_xref([set_raw/2]).
-ignore_xref([bindings/1]).
-ignore_xref([params/1]).
-ignore_xref([cookies/1]).
-ignore_xref([headers/1]).
-ignore_xref([body/1]).
-ignore_xref([redirect/2]).
-ignore_xref([redirect/3]).
-ignore_xref([halted_redirect/1]).

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
-export_type([middleware/0]).
-export_type([middleware_result/0]).
-export_type([redirect_status/0]).

%% --------------------------------------------------------------------
%% Types definitions
%% --------------------------------------------------------------------

-opaque request() :: #{
    adapter := adapter(),
    raw := raw(),
    method := method(),
    path := path(),
    bindings => bindings(),
    params => params(),
    cookies => cookies(),
    headers => headers(),
    body => body(),
    redirect => {redirect_status(), binary()}
}.

-nominal adapter() :: module().
-nominal raw() :: dynamic().
-nominal method() :: binary().
-nominal path() :: binary().
-nominal bindings() :: #{atom() => dynamic()}.
-nominal params() :: [{binary(), binary() | true}].
-nominal cookies() :: [{binary(), binary()}].
-nominal headers() :: #{binary() => iodata()}.
-nominal body() :: binary().

-nominal middleware() ::
    fun((request(), az:bindings()) -> middleware_result()) | {module(), atom()}.
-nominal middleware_result() :: {cont, request(), az:bindings()} | {halt, request()}.

%% HTTP redirect status codes (RFC 9110 §15.4). The most common are
%% 301 (moved permanently), 302 (found), 303 (see other), 307
%% (temporary redirect), 308 (permanent redirect). The full 3xx range
%% is allowed so rarer codes (300 multiple choices, 304 not modified,
%% etc.) remain expressible -- callers pick the semantics they want.
-nominal redirect_status() :: 300..399.

%% --------------------------------------------------------------------
%% Behaviour callbacks
%% --------------------------------------------------------------------

-callback parse_bindings(raw()) -> bindings().
-callback parse_params(raw()) -> params().
-callback parse_cookies(raw()) -> cookies().
-callback parse_headers(raw()) -> headers().
-callback read_body(raw()) -> {body(), raw()}.

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
        bindings => bindings(),
        params => params(),
        cookies => cookies(),
        headers => headers(),
        body => body()
    }.
new(Adapter, Raw, #{method := Method, path := Path} = Opts) when is_atom(Adapter) ->
    Base = #{adapter => Adapter, raw => Raw, method => Method, path => Path},
    maps:merge(Base, maps:without([method, path], Opts)).

-doc "Returns the HTTP method (e.g. `~\"GET\"`, `~\"POST\"`).".
-spec method(request()) -> method().
method(#{method := Method}) -> Method.

-doc "Returns the request path (no query string).".
-spec path(request()) -> path().
path(#{path := Path}) -> Path.

-doc "Returns the raw, transport-native request value the adapter wraps.".
-spec raw(request()) -> raw().
raw(#{raw := Raw}) -> Raw.

-doc """
Replaces the raw value inside `Request` and clears all lazy caches.

Useful after a transport-specific mutation (e.g. a
`cowboy_req:reply/4` on halt) that returns an updated raw. Cached
fields are dropped because they may no longer reflect the new raw.
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
Consumes the request body. Lazy-loaded; the underlying raw request is
updated because reading a body is mutating in most transports.
""".
-spec body(request()) -> {body(), request()}.
body(#{body := Body} = Req) ->
    {Body, Req};
body(#{adapter := Adapter, raw := Raw} = Req) ->
    {Body, Raw1} = Adapter:read_body(Raw),
    {Body, Req#{raw => Raw1, body => Body}}.

-doc """
Runs `Middlewares` left-to-right, threading `Request` and `Bindings`
through each step. Stops on the first `{halt, Request}` and returns it.
""".
-spec apply_middlewares(Middlewares, Request, Bindings) -> middleware_result() when
    Middlewares :: [middleware()],
    Request :: request(),
    Bindings :: az:bindings().
apply_middlewares([], Req, Bindings) ->
    {cont, Req, Bindings};
apply_middlewares([Mw | Rest], Req, Bindings) ->
    case call(Mw, Req, Bindings) of
        {cont, Req1, Bindings1} -> apply_middlewares(Rest, Req1, Bindings1);
        {halt, _Req1} = Halt -> Halt
    end.

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

%% --------------------------------------------------------------------
%% Internal functions
%% --------------------------------------------------------------------

call({Mod, Fun}, Req, Bindings) -> Mod:Fun(Req, Bindings);
call(Fun, Req, Bindings) -> Fun(Req, Bindings).

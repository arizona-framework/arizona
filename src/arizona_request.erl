-module(arizona_request).
-moduledoc ~"""
HTTP request abstraction with adapter pattern for different web servers.

Provides a unified interface for HTTP requests across different web server
adapters (Cowboy, etc.). Features lazy-loading of request data and caching
to avoid re-parsing. Uses adapter behavior pattern for pluggable web server support.

## Adapter Pattern

Web server adapters implement the behavior callbacks:
- `parse_bindings/1` - Extract path bindings from routes
- `parse_params/1` - Parse query string parameters
- `parse_cookies/1` - Parse HTTP cookies
- `parse_headers/1` - Parse HTTP headers
- `read_body/1` - Read and return request body

## Lazy Loading

Request data is parsed on-demand and cached:
1. Initial request contains only basic info (method, path)
2. First access triggers adapter parsing via callbacks
3. Subsequent accesses return cached data
4. Returns updated request with cached data

## Example Usage

```erlang
%% Create request via adapter (e.g., arizona_cowboy_request)
Request = arizona_cowboy_request:new(CowboyReq),

%% Access request data (lazy-loaded and cached)
Method = arizona_request:get_method(Request),
Path = arizona_request:get_path(Request),
{Bindings, Request1} = arizona_request:get_bindings(Request),
{Params, Request2} = arizona_request:get_params(Request1),
{Body, Request3} = arizona_request:get_body(Request2).
```

## Adapter Implementation Example

```erlang
-module(my_server_request).
-behaviour(arizona_request).

parse_bindings(RawRequest) ->
    %% Extract path bindings from your server's request format
    #{user_id => ~"123", action => ~"edit"}.
```
""".

%% --------------------------------------------------------------------
%% API function exports
%% --------------------------------------------------------------------

-export([new/3]).
-export([get_bindings/1]).
-export([get_params/1]).
-export([get_cookies/1]).
-export([get_method/1]).
-export([get_path/1]).
-export([get_headers/1]).
-export([get_body/1]).
-export([get_raw_request/1]).

%% --------------------------------------------------------------------
%% Ignore xref warnings
%% --------------------------------------------------------------------

-ignore_xref([get_bindings/1]).
-ignore_xref([get_params/1]).
-ignore_xref([get_cookies/1]).
-ignore_xref([get_method/1]).
-ignore_xref([get_path/1]).
-ignore_xref([get_headers/1]).
-ignore_xref([get_body/1]).
-ignore_xref([get_raw_request/1]).

%% --------------------------------------------------------------------
%% Types exports
%% --------------------------------------------------------------------

-export_type([request/0]).
-export_type([adapter/0]).
-export_type([raw_request/0]).
-export_type([method/0]).
-export_type([path/0]).
-export_type([bindings/0]).
-export_type([params/0]).
-export_type([cookies/0]).
-export_type([headers/0]).
-export_type([body/0]).

%% --------------------------------------------------------------------
%% Types definitions
%% --------------------------------------------------------------------

%% Internal request record structure
-record(request, {
    adapter :: adapter(),
    raw_request :: raw_request(),
    method :: method(),
    path :: path(),
    bindings :: bindings() | undefined,
    params :: params() | undefined,
    cookies :: cookies() | undefined,
    headers :: headers() | undefined,
    body :: body() | undefined
}).

-opaque request() :: #request{}.
-nominal adapter() :: module().
-nominal raw_request() :: dynamic().
-nominal method() :: binary().
-nominal path() :: binary().
-nominal bindings() :: arizona_binder:map().
-nominal params() :: [{binary(), binary() | true}].
-nominal cookies() :: [{binary(), binary()}].
-nominal headers() :: #{binary() => iodata()}.
-nominal body() :: binary().

%% --------------------------------------------------------------------
%% Behavior callback definitions
%% --------------------------------------------------------------------

-callback parse_bindings(RawRequest) -> Bindings when
    RawRequest :: raw_request(),
    Bindings :: bindings().

-callback parse_params(RawRequest) -> Params when
    RawRequest :: raw_request(),
    Params :: params().

-callback parse_cookies(RawRequest) -> Cookies when
    RawRequest :: raw_request(),
    Cookies :: cookies().

-callback parse_headers(RawRequest) -> Headers when
    RawRequest :: raw_request(),
    Headers :: headers().

-callback read_body(RawRequest) -> {Body, UpdatedRawRequest} when
    RawRequest :: raw_request(),
    Body :: body(),
    UpdatedRawRequest :: raw_request().

%% --------------------------------------------------------------------
%% API function definitions
%% --------------------------------------------------------------------

-doc ~"""
Creates a new request with adapter and options.

Initializes request with adapter module, raw request object, and
optional pre-computed values. Used by adapter modules to create
request instances.
""".
-spec new(Adapter, RawRequest, Opts) -> Request when
    Adapter :: adapter(),
    RawRequest :: raw_request(),
    Opts :: map(),
    Request :: request().
new(Adapter, RawRequest, Opts) when is_atom(Adapter), is_map(Opts) ->
    #request{
        adapter = Adapter,
        raw_request = RawRequest,
        method = maps:get(method, Opts, ~"GET"),
        path = maps:get(path, Opts, ~"/"),
        bindings = maps:get(bindings, Opts, #{}),
        params = maps:get(params, Opts, []),
        cookies = maps:get(cookies, Opts, []),
        headers = maps:get(headers, Opts, #{}),
        body = maps:get(body, Opts, undefined)
    }.

-doc ~"""
Returns the HTTP method.

Gets the HTTP method (GET, POST, PUT, DELETE, etc.) as a binary.
""".
-spec get_method(Request) -> Method when
    Request :: request(),
    Method :: method().
get_method(#request{method = Method}) ->
    Method.

-doc ~"""
Returns the request path.

Gets the URL path (e.g., `~"/users/123/edit"`) as a binary.
""".
-spec get_path(Request) -> Path when
    Request :: request(),
    Path :: path().
get_path(#request{path = Path}) ->
    Path.

-doc ~"""
Gets path bindings with lazy loading.

Returns route path bindings (e.g., from `"/users/:id"` routes).
Lazy-loads via adapter on first access, caches for subsequent calls.
""".
-spec get_bindings(Request) -> {Bindings, Request1} when
    Request :: request(),
    Bindings :: bindings(),
    Request1 :: request().
get_bindings(#request{bindings = undefined} = Request) ->
    Adapter = Request#request.adapter,
    RawRequest = Request#request.raw_request,
    Bindings = Adapter:parse_bindings(RawRequest),
    Request1 = Request#request{bindings = Bindings},
    {Bindings, Request1};
get_bindings(#request{bindings = Bindings} = Request) ->
    {Bindings, Request}.

-doc ~"""
Gets query parameters with lazy loading.

Returns parsed query string parameters as key-value pairs.
Lazy-loads via adapter on first access, caches for subsequent calls.
""".
-spec get_params(Request) -> {Params, Request1} when
    Request :: request(),
    Params :: params(),
    Request1 :: request().
get_params(#request{params = undefined} = Request) ->
    Adapter = Request#request.adapter,
    RawRequest = Request#request.raw_request,
    Params = Adapter:parse_params(RawRequest),
    Request1 = Request#request{params = Params},
    {Params, Request1};
get_params(#request{params = Params} = Request) ->
    {Params, Request}.

-doc ~"""
Gets HTTP cookies with lazy loading.

Returns parsed HTTP cookies as key-value pairs.
Lazy-loads via adapter on first access, caches for subsequent calls.
""".
-spec get_cookies(Request) -> {Cookies, Request1} when
    Request :: request(),
    Cookies :: cookies(),
    Request1 :: request().
get_cookies(#request{cookies = undefined} = Request) ->
    Adapter = Request#request.adapter,
    RawRequest = Request#request.raw_request,
    Cookies = Adapter:parse_cookies(RawRequest),
    Request1 = Request#request{cookies = Cookies},
    {Cookies, Request1};
get_cookies(#request{cookies = Cookies} = Request) ->
    {Cookies, Request}.

-doc ~"""
Gets HTTP headers with lazy loading.

Returns HTTP headers as a map of header names to values.
Lazy-loads via adapter on first access, caches for subsequent calls.
""".
-spec get_headers(Request) -> {Headers, Request1} when
    Request :: request(),
    Headers :: headers(),
    Request1 :: request().
get_headers(#request{headers = undefined} = Request) ->
    Adapter = Request#request.adapter,
    RawRequest = Request#request.raw_request,
    Headers = Adapter:parse_headers(RawRequest),
    Request1 = Request#request{headers = Headers},
    {Headers, Request1};
get_headers(#request{headers = Headers} = Request) ->
    {Headers, Request}.

-doc ~"""
Gets request body with lazy loading.

Returns the complete request body as binary. May update the
raw request object during reading (streaming protocols).
Lazy-loads via adapter on first access, caches for subsequent calls.
""".
-spec get_body(Request) -> {Body, Request1} when
    Request :: request(),
    Body :: body(),
    Request1 :: request().
get_body(#request{body = undefined} = Request) ->
    Adapter = Request#request.adapter,
    RawRequest = Request#request.raw_request,
    {Body, RawRequest1} = Adapter:read_body(RawRequest),
    Request1 = Request#request{body = Body, raw_request = RawRequest1},
    {Body, Request1};
get_body(#request{body = Body} = Request) ->
    {Body, Request}.

-doc ~"""
Returns the underlying raw request object.

Provides access to the original web server's request object
for adapter implementations or advanced usage.
""".
-spec get_raw_request(Request) -> RawRequest when
    Request :: request(),
    RawRequest :: raw_request().
get_raw_request(#request{raw_request = RawRequest}) ->
    RawRequest.

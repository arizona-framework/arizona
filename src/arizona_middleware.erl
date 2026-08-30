-module(arizona_middleware).
-moduledoc """
The request-to-bindings middleware pipeline.

A middleware reduces an incoming request into the plain bindings a
`mount/1` handler consumes, so handlers (and the live process) stay free
of any request or transport coupling. Each step receives the request and
the bindings gathered so far and returns either `{cont, Request, Bindings}`
to continue or `{halt, Request}` to stop.

## Running

`apply_middlewares/3` threads a request and bindings map through a list of
steps left-to-right, stopping on the first halt. Transports
(`arizona_http`, `arizona_ws`, `arizona_socket`) run it before mounting a
view; a route lists its steps under the `middlewares` key of
`t:arizona_live:route_opts/0`.

A halting step says what the client sees by stashing a redirect
(`arizona_req:redirect/2,3`) or a status (`arizona_req:put_resp_status/2`,
as `check_origin/2` does with its `403`). A bare `{halt, Request}` carrying
neither is answered `403 Forbidden` -- the same on a page render, a
controller, and a WebSocket upgrade.

## Built-in steps

- `extract/1` -- copy selected request data (path bindings, params,
  headers, ...) into bindings so a handler reads it with `?get(Key)`.
- `put_request/2` -- the escape hatch: expose the whole request under the
  `request` binding for lazy access.
- `fetch_flash/2` -- read the incoming flash (set on the prior request via
  `arizona_req:put_flash/3`) into the `flash` binding, consuming it.
- `fetch_session/2` -- read the durable session into the `session` binding, without
  consuming it.
- `check_origin/2` -- CSRF defense: halt with `403` when the request `Origin` is not
  trusted (see `arizona_origin`). The router applies it to `live`/`controller` routes by
  default, so you rarely list it by hand.

## HTTP response transforms

The list accepts a second kind of entry: an **HTTP response transform**,
built by `etag/0`, `compress/0`, `cors/1`, `security_headers/0,1`, or the
`http_transform/1` escape hatch. A transform is not a request-to-bindings
step: it wraps the whole HTTP exchange (the server applies it in its own
response pipeline), so it can see and rewrite the response -- the thing a
step, which runs before any render and also on paths with no HTTP response
at all, cannot. Order has phase semantics: steps run in list order during
the request phase; transforms wrap the exchange in list order, first
outermost -- and the framework's own compression is prepended outside them
all, so a transform sees the response before it is re-encoded. On a
pipeline run with no HTTP response to wrap (the WebSocket upgrade, a
navigate/patch), a transform is inert and skipped.

## Declaring steps

A step is either a fun of two arguments or a `{Module, Function}` pair naming a
2-arity function. Which of the two you can write depends on where the route is
declared.

A route declared in Erlang takes both forms, so it can call a builder like
`extract/1` inline:

```erlang
#{middlewares => [arizona_middleware:extract([path_bindings, params])]}
```

A route declared in `config/sys.config` takes only the pair. An application
config file is read by `file:consult/1`, which parses *terms*, and
`arizona_middleware:extract([path_bindings])` is a function call rather than a
term -- so the file fails to parse and the node refuses to boot:

```text
===> Error reading file .../config/sys.config: 1: bad term
```

The reported line is always 1, because the whole file is a single term, so it
points nowhere near the offending expression. Note that `fun Module:Function/Arity`
with literal parts *is* a term (which is why the reloader's
`callback => fun arizona_reloader:reload_erl/1` consults fine); it is specifically
the call that is not.

So a parameterised step -- `extract/1`, or a builder of your own -- needs a named
2-arity function for a config file to spell:

```erlang
%% config/sys.config -- an entry in the server's `routes` list
{live, ~"/users/:id", user_page, #{middlewares => [{my_app_mw, path_bindings}]}}
```

```erlang
%% src/my_app_mw.erl
path_bindings(Req, Bindings) ->
    Extract = arizona_middleware:extract([path_bindings]),
    Extract(Req, Bindings).
```

`{arizona_middleware, extract}` is not that wrapper. It type-checks, then fails at
request time with `undef`: the pair names `extract/2`, and `extract/1` is the
builder.
""".

%% --------------------------------------------------------------------
%% API function exports
%% --------------------------------------------------------------------

-export([apply_middlewares/3]).
-export([extract/1]).
-export([put_request/2]).
-export([fetch_flash/2]).
-export([fetch_session/2]).
-export([check_origin/2]).
-export([etag/0]).
-export([compress/0]).
-export([cors/1]).
-export([security_headers/0]).
-export([security_headers/1]).
-export([http_transform/1]).

%% --------------------------------------------------------------------
%% Ignore xref warnings
%% --------------------------------------------------------------------

-ignore_xref([extract/1]).
-ignore_xref([put_request/2]).
-ignore_xref([fetch_flash/2]).
-ignore_xref([fetch_session/2]).
-ignore_xref([check_origin/2]).
%% HTTP response transform constructors: public API for route declarations;
%% no internal callers by design (the router matches the tagged term).
-ignore_xref([etag/0]).
-ignore_xref([compress/0]).
-ignore_xref([cors/1]).
-ignore_xref([security_headers/0]).
-ignore_xref([security_headers/1]).
-ignore_xref([http_transform/1]).

%% --------------------------------------------------------------------
%% Types exports
%% --------------------------------------------------------------------

-export_type([middleware/0]).
-export_type([http_transform/0]).
-export_type([middleware_result/0]).
-export_type([extract_key/0]).

%% --------------------------------------------------------------------
%% Types definitions
%% --------------------------------------------------------------------

%% A pipeline step. The `{Module, Function}` pair names a 2-arity function and is
%% the only form writable as a term, so it is what a `sys.config` route must use;
%% see "Declaring steps" in the module doc.
-nominal middleware() ::
    fun((arizona_req:request(), az:bindings()) -> middleware_result())
    | {module(), atom()}
    | http_transform().

%% An HTTP response transform (see the module doc): a tagged term, so a
%% sys.config route can spell it literally (`{http_transform, etag}`) where a
%% constructor call cannot be written. The router maps the named transforms to
%% the server's own middlewares at route-compile time; `{custom, Entry}`
%% carries a server-shaped middleware entry verbatim. A plain -type, not
%% -nominal: the tag is already the discriminator, and the nominal made
%% dialyzer mis-type a constructor-built value flowing into
%% `apply_middlewares/3` (a false "no local return" at every such call site).
-type http_transform() ::
    {http_transform,
        etag
        | compress
        | security_headers
        | {cors, map()}
        | {security_headers, map()}
        | {custom, term()}}.
-nominal middleware_result() ::
    {cont, arizona_req:request(), az:bindings()} | {halt, arizona_req:request()}.

%% Keys accepted by `extract/1`; each copies one piece of the request into
%% bindings (see the function doc).
-nominal extract_key() ::
    path_bindings | params | headers | cookies | body | method | user_agent.

%% --------------------------------------------------------------------
%% API Functions
%% --------------------------------------------------------------------

-doc """
Runs `Middlewares` left-to-right, threading `Request` and `Bindings`
through each step. Stops on the first `{halt, Request}` and returns it.
""".
-spec apply_middlewares(Middlewares, Request, Bindings) -> middleware_result() when
    Middlewares :: [middleware()],
    Request :: arizona_req:request(),
    Bindings :: az:bindings().
apply_middlewares([], Req, Bindings) ->
    {cont, Req, Bindings};
apply_middlewares([{http_transform, _} | Rest], Req, Bindings) ->
    %% A response transform wraps the HTTP exchange; on a pipeline run with no
    %% HTTP response to wrap (WS upgrade, navigate/patch, a custom
    %% resolve_route result) it is inert. Statically declared routes never get
    %% here -- the router splits transforms out at compile time.
    apply_middlewares(Rest, Req, Bindings);
apply_middlewares([Mw | Rest], Req, Bindings) ->
    case call(Mw, Req, Bindings) of
        {cont, Req1, Bindings1} -> apply_middlewares(Rest, Req1, Bindings1);
        {halt, _Req1} = Halt -> Halt
    end.

-doc """
Builds a middleware that copies selected request data into bindings, so a
`mount/1` handler can read it with `?get(Key)` without touching the request.

Each key targets a binding:

- `path_bindings` -- route path bindings merged into the bindings map (so
  `/users/:id` is read as `?get(<<"id">>)`)
- `params` / `headers` / `cookies` / `body` / `method` / `user_agent` --
  stored under the same-named key

```erlang
#{middlewares => [arizona_middleware:extract([path_bindings, params])]}
```

That call builds the middleware where the route is declared, so it can only be
written in Erlang. A route declared in `config/sys.config` must name a 2-arity
wrapper around it instead -- see "Declaring steps" in the module doc.
""".
-spec extract(Keys) -> middleware() when Keys :: [extract_key()].
extract(Keys) ->
    fun(Req, Bindings) ->
        {Req1, Bindings1} = lists:foldl(fun extract_into/2, {Req, Bindings}, Keys),
        {cont, Req1, Bindings1}
    end.

-doc """
Middleware that exposes the whole request to a handler under the `request`
binding (read with `?get(request)`).

The escape hatch for a handler that needs lazy access to many request fields;
it couples that handler to HTTP. Prefer `extract/1` for specific data.

The `request` binding is mount-scoped: it is read during `mount/1`, and because
mount returns a fresh bindings map (construct, don't merge) it does not persist
into the live process or carry across an SPA navigate.

```erlang
#{middlewares => [{arizona_middleware, put_request}]}
```
""".
-spec put_request(arizona_req:request(), az:bindings()) -> middleware_result().
put_request(Req, Bindings) ->
    {cont, Req, Bindings#{request => Req}}.

-doc """
Middleware that reads the incoming flash into the `flash` binding (read with
`?get(flash)`, a map; `#{}` when there is none).

Opt in on routes that display flash messages. Reading consumes the flash: the
response clears its cookie, so a refresh shows nothing. Pair it with
`arizona_req:put_flash/3` before a redirect (the Post/Redirect/Get pattern).

```erlang
#{middlewares => [{arizona_middleware, fetch_flash}]}
```
""".
-spec fetch_flash(arizona_req:request(), az:bindings()) -> middleware_result().
fetch_flash(Req, Bindings) ->
    {Flash, Req1} = arizona_req:consume_flash(Req),
    {cont, Req1, Bindings#{flash => Flash}}.

-doc """
Middleware that reads the durable session into the `session` binding (read with
`?get(session)`, a map; `#{}` when there is none).

Opt in on routes that need the session. Unlike `fetch_flash/2`, reading does **not**
consume it: the session survives untouched, and the response re-emits the cookie
only when the app writes it (`arizona_req:put_session/3`) or clears it
(`clear_session/1`). It runs on both the GET render and the WS upgrade, so a live
view is seeded with the session at mount.

A server-side store outage is left on the request (`arizona_req:session_error/1`), not
the `session` binding (which stays `#{}`, an ordinary signed-out read). A route that wants
the outage visible in a view can add its own middleware after this one that lifts
`arizona_req:session_error(Req)` into a binding.

```erlang
#{middlewares => [{arizona_middleware, fetch_session}]}
```
""".
-spec fetch_session(arizona_req:request(), az:bindings()) -> middleware_result().
fetch_session(Req, Bindings) ->
    {Session, Req1} = arizona_req:read_session(Req),
    {cont, Req1, Bindings#{session => Session}}.

-doc """
CSRF defense: halt with `403` when the request `Origin` is not trusted (same-origin or
allowlisted -- see `arizona_origin`), continue otherwise. A missing `Origin` (native
clients, CLI tools, top-level GET navigations) continues.

The router prepends this to `live` and `controller` routes by default; opt a route out
with `check_origin => false`, or disable globally with the `check_origin` app env.

The Origin is compared against the request's scheme as well as its `Host`, so an
HTTPS request refuses a plain-`http` Origin. The scheme is the connection's own
(`arizona_req:scheme/1`), upgraded to `https` when a proxy in front sets
`X-Forwarded-Proto: https` -- an upgrade-only read, so the header cannot be forged
into weakening the check.

```erlang
#{middlewares => [{arizona_middleware, check_origin}]}
```
""".
-spec check_origin(arizona_req:request(), az:bindings()) -> middleware_result().
check_origin(Req0, Bindings) ->
    {Headers, Req} = arizona_req:headers(Req0),
    Origin = maps:get(~"origin", Headers, undefined),
    Host = maps:get(~"host", Headers, undefined),
    case arizona_origin:check(Origin, Host, client_scheme(Req, Headers)) of
        ok -> {cont, Req, Bindings};
        forbidden -> {halt, arizona_req:put_resp_status(Req, 403)}
    end.

-doc """
HTTP response transform: derive a weak `ETag` from the response body and
answer a matching `If-None-Match` with `304` (RFC 7232). The validator is
weak on purpose, so the framework's compression re-encoding the body
outside it cannot invalidate the tag.
""".
-spec etag() -> http_transform().
etag() ->
    {http_transform, etag}.

-doc """
HTTP response transform: gzip/deflate response compression. Only needed on
routes the framework does not already compress -- controller routes; `live`
and `asset` routes get compression from the server's `compress` option
(default on), and listing it there again is a no-op.
""".
-spec compress() -> http_transform().
compress() ->
    {http_transform, compress}.

-doc """
HTTP response transform: CORS -- answers the browser's preflight `OPTIONS`
and decorates cross-origin responses with `Access-Control-*` headers per
`Config` (the server middleware's own config map; `origins` at minimum).
Distinct from `check_origin/2`, which REJECTS cross-origin requests --
list this on a route that deliberately serves them (usually beside
`check_origin => false`).
""".
-spec cors(Config) -> http_transform() when
    Config :: map().
cors(Config) when is_map(Config) ->
    {http_transform, {cors, Config}}.

-doc """
HTTP response transform: a default-safe security-header set
(`x-content-type-options`, `x-frame-options`, `referrer-policy`, ...)
added to every response, leaving headers the handler already set
untouched. `security_headers/1` overrides individual headers.
""".
-spec security_headers() -> http_transform().
security_headers() ->
    {http_transform, security_headers}.

-spec security_headers(Config) -> http_transform() when
    Config :: map().
security_headers(Config) when is_map(Config) ->
    {http_transform, {security_headers, Config}}.

-doc """
Escape hatch for a response transform the named constructors do not cover:
wraps a server-shaped middleware entry (for the roadrunner server, a module
implementing `roadrunner_middleware`, a fun, or a `{Callable, Config}`
pair) verbatim into the route's response pipeline. Unlike the named
transforms this couples the route to the server backend -- prefer the
constructors when one exists.
""".
-spec http_transform(Entry) -> http_transform() when
    Entry :: term().
http_transform(Entry) ->
    {http_transform, {custom, Entry}}.

%% --------------------------------------------------------------------
%% Internal functions
%% --------------------------------------------------------------------

%% The scheme the *browser* used, which is what an Origin must be compared against:
%% TLS on this connection, or a proxy in front reporting the original request was
%% HTTPS. Note the asymmetry -- this only ever upgrades http to https, so a forged
%% `X-Forwarded-Proto` can tighten the Origin check (rejecting the forger's own
%% request) but never loosen it, which is why the header needs no trusted-proxy
%% configuration to be safe here.
client_scheme(Req, Headers) ->
    case arizona_req:scheme(Req) of
        https -> https;
        http -> forwarded_scheme(Headers)
    end.

%% `X-Forwarded-Proto` is a comma-separated chain when several proxies append to
%% it; the client-facing hop is the first entry.
forwarded_scheme(#{~"x-forwarded-proto" := Proto}) ->
    [First | _] = binary:split(iolist_to_binary(Proto), ~","),
    case string:equal(string:trim(First), ~"https", true) of
        true -> https;
        false -> http
    end;
forwarded_scheme(#{}) ->
    http.

call({Mod, Fun}, Req, Bindings) -> Mod:Fun(Req, Bindings);
call(Fun, Req, Bindings) -> Fun(Req, Bindings).

%% One step of `extract/1`'s fold: copy the keyed request datum into bindings,
%% threading the lazy request forward. `path_bindings` spreads the path-binding
%% map; the rest land under their key; `method` is eager (no request update).
extract_into(path_bindings, {Req, Bindings}) ->
    {PathBindings, Req1} = arizona_req:bindings(Req),
    {Req1, maps:merge(Bindings, PathBindings)};
extract_into(params, {Req, Bindings}) ->
    {Params, Req1} = arizona_req:params(Req),
    {Req1, Bindings#{params => Params}};
extract_into(headers, {Req, Bindings}) ->
    {Headers, Req1} = arizona_req:headers(Req),
    {Req1, Bindings#{headers => Headers}};
extract_into(cookies, {Req, Bindings}) ->
    {Cookies, Req1} = arizona_req:cookies(Req),
    {Req1, Bindings#{cookies => Cookies}};
extract_into(body, {Req, Bindings}) ->
    {Body, Req1} = arizona_req:body(Req),
    {Req1, Bindings#{body => Body}};
extract_into(user_agent, {Req, Bindings}) ->
    {UserAgent, Req1} = arizona_req:user_agent(Req),
    {Req1, Bindings#{user_agent => UserAgent}};
extract_into(method, {Req, Bindings}) ->
    {Req, Bindings#{method => arizona_req:method(Req)}}.

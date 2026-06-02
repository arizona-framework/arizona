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

## Built-in steps

- `extract/1` -- copy selected request data (path bindings, params,
  headers, ...) into bindings so a handler reads it with `?get(Key)`.
- `put_request/2` -- the escape hatch: expose the whole request under the
  `request` binding for lazy access.

```erlang
#{middlewares => [arizona_middleware:extract([path_bindings, params])]}
```
""".

%% --------------------------------------------------------------------
%% API function exports
%% --------------------------------------------------------------------

-export([apply_middlewares/3]).
-export([extract/1]).
-export([put_request/2]).

%% --------------------------------------------------------------------
%% Ignore xref warnings
%% --------------------------------------------------------------------

-ignore_xref([extract/1]).
-ignore_xref([put_request/2]).

%% --------------------------------------------------------------------
%% Types exports
%% --------------------------------------------------------------------

-export_type([middleware/0]).
-export_type([middleware_result/0]).
-export_type([extract_key/0]).

%% --------------------------------------------------------------------
%% Types definitions
%% --------------------------------------------------------------------

-nominal middleware() ::
    fun((arizona_req:request(), az:bindings()) -> middleware_result()) | {module(), atom()}.
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

%% --------------------------------------------------------------------
%% Internal functions
%% --------------------------------------------------------------------

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

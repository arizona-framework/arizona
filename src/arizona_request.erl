-module(arizona_request).
-moduledoc ~"""
Provides HTTP request abstraction for Arizona LiveView applications.

## Overview

The request module provides a unified interface for HTTP request handling across
different server implementations. It abstracts away server-specific details and
provides a consistent API for accessing request data in Arizona LiveView applications.

## Features

- **Server Abstraction**: Unified interface for different HTTP servers (currently Cowboy)
- **Lazy Loading**: Efficient lazy loading of request data to minimize memory usage
- **Type Safety**: Comprehensive type definitions for request components
- **Immutable Updates**: Functional approach to request data manipulation
- **Comprehensive Access**: Full access to all HTTP request components
- **Raw Access**: Direct access to underlying server request when needed

## Key Functions

- `new/1`: Create new Arizona request from options map
- `from_cowboy/1`: Convert Cowboy request to Arizona request format
- `get_bindings/1`, `get_params/1`, `get_cookies/1`: Access request data components
- `get_method/1`, `get_path/1`, `get_headers/1`: Access HTTP request metadata
- `get_body/1`, `get_raw_request/1`: Access request body and raw server data

## Request Components

- **Method**: HTTP method (GET, POST, PUT, DELETE, etc.)
- **Path**: Request path without query parameters
- **Bindings**: URL path parameter bindings from routing
- **Params**: Query string parameters
- **Cookies**: HTTP cookies
- **Headers**: HTTP request headers
- **Body**: Request body content
- **Raw**: Server-specific request object for advanced usage

## Lazy Loading

The module implements lazy loading for expensive operations like body reading
and header parsing, ensuring optimal performance for common use cases.
""".

%% --------------------------------------------------------------------
%% API function exports
%% --------------------------------------------------------------------

-export([new/1]).
-export([from_cowboy/1]).
-export([get_bindings/1]).
-export([get_params/1]).
-export([get_cookies/1]).
-export([get_method/1]).
-export([get_path/1]).
-export([get_headers/1]).
-export([get_body/1]).
-export([get_raw_request/1]).

%% --------------------------------------------------------------------
%% Types exports
%% --------------------------------------------------------------------

-export_type([request/0]).

%% --------------------------------------------------------------------
%% Types definitions
%% --------------------------------------------------------------------

%% Internal request record structure
-record(request, {
    method :: binary(),
    path :: binary(),
    bindings :: #{atom() => binary()} | undefined,
    params :: [{binary(), binary()}] | undefined,
    cookies :: [{binary(), binary()}] | undefined,
    headers :: #{binary() => binary()} | undefined,
    body :: binary() | undefined,
    % For server-specific data
    raw :: {cowboy_req, cowboy_req:req()} | undefined
}).

-doc ~"""
Opaque request type representing HTTP request data.

Contains all HTTP request components including method, path, parameters,
headers, body, and server-specific raw data. Supports lazy loading for
efficient memory usage.
""".
-opaque request() :: #request{}.

%% --------------------------------------------------------------------
%% API function definitions
%% --------------------------------------------------------------------

-doc ~"""
Create a new Arizona request from options map.

Creates a new request instance with the provided options. All fields are
optional and will use sensible defaults if not provided.

## Examples

```erlang
1> arizona_request:new(#{method => <<"POST">>, path => <<"/users">>}).
#request{method = <<"POST">>, path = <<"/users">>, ...}
2> arizona_request:new(#{}).
#request{method = <<"GET">>, path = <<"/">>, ...}
```
""".
-spec new(Opts) -> Request when
    Opts :: map(),
    Request :: request().
new(Opts) when is_map(Opts) ->
    #request{
        method = maps:get(method, Opts, ~"GET"),
        path = maps:get(path, Opts, ~"/"),
        bindings = maps:get(bindings, Opts, #{}),
        params = maps:get(params, Opts, []),
        cookies = maps:get(cookies, Opts, []),
        headers = maps:get(headers, Opts, #{}),
        body = maps:get(body, Opts, undefined),
        raw = maps:get(raw, Opts, undefined)
    }.

-doc ~"""
Create Arizona request from Cowboy request.

Converts a Cowboy HTTP request to Arizona request format with lazy loading
for expensive operations. Most request data is loaded on-demand to optimize
performance.

## Examples

```erlang
1> arizona_request:from_cowboy(CowboyReq).
#request{method = <<"GET">>, path = <<"/users">>, ...}
```
""".
-spec from_cowboy(CowboyReq) -> Request when
    CowboyReq :: cowboy_req:req(),
    Request :: request().
from_cowboy(CowboyReq) ->
    #request{
        method = cowboy_req:method(CowboyReq),
        path = cowboy_req:path(CowboyReq),
        % Lazy load
        bindings = undefined,
        % Lazy load
        params = undefined,
        % Lazy load
        cookies = undefined,
        % Lazy load
        headers = undefined,
        % Lazy load
        body = undefined,
        raw = {cowboy_req, CowboyReq}
    }.

-doc ~"""
Get URL path parameter bindings from the request.

Retrieves path parameter bindings from URL routing, such as user_id from
a route like `/users/:user_id`. Uses lazy loading for Cowboy requests.

## Examples

```erlang
1> {Bindings, Req2} = arizona_request:get_bindings(Req).
{#{user_id => <<"123">>}, #request{...}}
```
""".
-spec get_bindings(Request) -> {Bindings, Request1} when
    Request :: request(),
    Bindings :: #{atom() => binary()},
    Request1 :: request().
get_bindings(#request{bindings = undefined, raw = {cowboy_req, CowboyReq}} = Req) ->
    Bindings = cowboy_req:bindings(CowboyReq),
    Req1 = Req#request{bindings = Bindings},
    {Bindings, Req1};
get_bindings(#request{bindings = Bindings} = Req) ->
    {Bindings, Req}.

-doc ~"""
Get query string parameters from the request.

Parses and returns query string parameters as a list of key-value pairs.
Uses lazy loading for Cowboy requests to optimize performance.

## Examples

```erlang
1> {Params, Req2} = arizona_request:get_params(Req).
{[{<<"tab">>, <<"account">>}, {<<"page">>, <<"1">>}], #request{...}}
```
""".
-spec get_params(Request) -> {Params, Request1} when
    Request :: request(),
    Params :: [{binary(), binary()}],
    Request1 :: request().
get_params(#request{params = undefined, raw = {cowboy_req, CowboyReq}} = Req) ->
    Params = cowboy_req:parse_qs(CowboyReq),
    Req1 = Req#request{params = Params},
    {Params, Req1};
get_params(#request{params = Params} = Req) ->
    {Params, Req}.

-doc ~"""
Get HTTP cookies from the request.

Parses and returns HTTP cookies as a list of key-value pairs. Uses lazy
loading for Cowboy requests to optimize performance.

## Examples

```erlang
1> {Cookies, Req2} = arizona_request:get_cookies(Req).
{[{<<"session_id">>, <<"abc123">>}], #request{...}}
```
""".
-spec get_cookies(Request) -> {Cookies, Request1} when
    Request :: request(),
    Cookies :: [{binary(), binary()}],
    Request1 :: request().
get_cookies(#request{cookies = undefined, raw = {cowboy_req, CowboyReq}} = Req) ->
    Cookies = cowboy_req:parse_cookies(CowboyReq),
    Req1 = Req#request{cookies = Cookies},
    {Cookies, Req1};
get_cookies(#request{cookies = Cookies} = Req) ->
    {Cookies, Req}.

-doc ~"""
Get HTTP method from the request.

Returns the HTTP method (GET, POST, PUT, DELETE, etc.) as a binary string.
This field is eagerly loaded for all request types.

## Examples

```erlang
1> arizona_request:get_method(Req).
<<"POST">>
```
""".
-spec get_method(Request) -> Method when
    Request :: request(),
    Method :: binary().
get_method(#request{method = Method}) ->
    Method.

-doc ~"""
Get request path from the request.

Returns the request path without query parameters as a binary string.
This field is eagerly loaded for all request types.

## Examples

```erlang
1> arizona_request:get_path(Req).
<<"/users/123">>
```
""".
-spec get_path(Request) -> Path when
    Request :: request(),
    Path :: binary().
get_path(#request{path = Path}) ->
    Path.

-doc ~"""
Get HTTP headers from the request.

Retrieves all HTTP headers as a map of header names to values. Uses lazy
loading for Cowboy requests to optimize performance.

## Examples

```erlang
1> {Headers, Req2} = arizona_request:get_headers(Req).
{#{<<"content-type">> => <<"application/json">>}, #request{...}}
```
""".
-spec get_headers(Request) -> {Headers, Request1} when
    Request :: request(),
    Headers :: #{binary() => binary()},
    Request1 :: request().
get_headers(#request{headers = undefined, raw = {cowboy_req, CowboyReq}} = Req) ->
    Headers = cowboy_req:headers(CowboyReq),
    Req1 = Req#request{headers = Headers},
    {Headers, Req1};
get_headers(#request{headers = Headers} = Req) ->
    {Headers, Req}.

-doc ~"""
Get request body from the request.

Reads and returns the request body as a binary. Uses lazy loading for
Cowboy requests and handles the case where no body is present.

## Examples

```erlang
1> {Body, Req2} = arizona_request:get_body(Req).
{<<"{\"name\":\"John\"}">>, #request{...}}
2> {Body, Req3} = arizona_request:get_body(EmptyReq).
{<<"">>, #request{...}}
```
""".
-spec get_body(Request) -> {Body, Request1} when
    Request :: request(),
    Body :: binary(),
    Request1 :: request().
get_body(#request{body = undefined, raw = {cowboy_req, CowboyReq}} = Req) ->
    {ok, Body, CowboyReq1} = cowboy_req:read_body(CowboyReq),
    Req1 = Req#request{body = Body, raw = {cowboy_req, CowboyReq1}},
    {Body, Req1};
get_body(#request{body = Body} = Req) when Body =/= undefined ->
    {Body, Req};
get_body(#request{} = Req) ->
    {~"", Req}.

-doc ~"""
Get the raw server-specific request object.

Returns the underlying server request object for advanced usage when the
Arizona request abstraction is insufficient. Currently supports Cowboy requests.

## Examples

```erlang
1> arizona_request:get_raw_request(Req).
{cowboy_req, CowboyReq}
2> arizona_request:get_raw_request(DirectReq).
undefined
```
""".
-spec get_raw_request(Request) -> RawRequest when
    Request :: request(),
    RawRequest :: {cowboy_req, cowboy_req:req()} | undefined.
get_raw_request(#request{raw = Raw}) ->
    Raw.

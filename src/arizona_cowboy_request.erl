-module(arizona_cowboy_request).
-moduledoc ~"""
Cowboy web server adapter implementing the `arizona_request` behavior.

Provides the adapter layer between the Arizona framework and the Cowboy
web server, implementing all required callbacks for request data parsing.
Handles lazy-loading by delegating to appropriate Cowboy request functions.

## Adapter Implementation

Maps Arizona request operations to Cowboy equivalents:
- Path bindings: `cowboy_req:bindings/1`
- Query parameters: `cowboy_req:parse_qs/1`
- HTTP cookies: `cowboy_req:parse_cookies/1`
- HTTP headers: `cowboy_req:headers/1`
- Request body: `cowboy_req:read_body/1`

## Usage Pattern

Typically used by Arizona handlers to create request abstractions:

```erlang
%% In arizona_handler or arizona_websocket
init(CowboyReq, State) ->
    ArizonaRequest = arizona_cowboy_request:new(CowboyReq),
    View = arizona_view:call_mount_callback(ViewModule, MountArg, ArizonaRequest),
    %% ... continue with view processing
```

## Lazy Loading

Only method and path are extracted immediately for performance.
Other request data (bindings, params, cookies, headers, body) are
loaded on-demand when accessed through `arizona_request` functions.
""".
-behaviour(arizona_request).

%% --------------------------------------------------------------------
%% API function exports
%% --------------------------------------------------------------------

-export([new/1]).

%% --------------------------------------------------------------------
%% Behaviour (arizona_request) exports
%% --------------------------------------------------------------------

-export([parse_bindings/1]).
-export([parse_params/1]).
-export([parse_cookies/1]).
-export([parse_headers/1]).
-export([read_body/1]).

%% --------------------------------------------------------------------
%% API function definitions
%% --------------------------------------------------------------------

-doc ~"""
Creates an Arizona request from a Cowboy request.

Extracts method and path immediately, but leaves other data for
lazy-loading through the adapter callbacks.
""".
-spec new(CowboyRequest) -> Request when
    CowboyRequest :: cowboy_req:req(),
    Request :: arizona_request:request().
new(CowboyRequest) ->
    arizona_request:new(?MODULE, CowboyRequest, #{
        method => cowboy_req:method(CowboyRequest),
        path => cowboy_req:path(CowboyRequest),
        % Lazy load
        bindings => undefined,
        % Lazy load
        params => undefined,
        % Lazy load
        cookies => undefined,
        % Lazy load
        headers => undefined,
        % Lazy load
        body => undefined
    }).

%% --------------------------------------------------------------------
%% Behaviour (arizona_request) callbacks
%% --------------------------------------------------------------------

-doc ~"""
Parses path bindings from Cowboy request.

Behavior callback that extracts route path bindings
(e.g., from `/users/:id` patterns) using `cowboy_req:bindings/1`.
""".
-spec parse_bindings(CowboyRequest) -> Bindings when
    CowboyRequest :: cowboy_req:req(),
    Bindings :: arizona_request:bindings().
parse_bindings(CowboyRequest) ->
    cowboy_req:bindings(CowboyRequest).

-doc ~"""
Parses query string parameters from Cowboy request.

Behavior callback that extracts URL query parameters
using `cowboy_req:parse_qs/1`.
""".
-spec parse_params(CowboyRequest) -> Params when
    CowboyRequest :: cowboy_req:req(),
    Params :: arizona_request:params().
parse_params(CowboyRequest) ->
    cowboy_req:parse_qs(CowboyRequest).

-doc ~"""
Parses HTTP cookies from Cowboy request.

Behavior callback that extracts HTTP cookies
using `cowboy_req:parse_cookies/1`.
""".
-spec parse_cookies(CowboyRequest) -> Cookies when
    CowboyRequest :: cowboy_req:req(),
    Cookies :: arizona_request:cookies().
parse_cookies(CowboyRequest) ->
    cowboy_req:parse_cookies(CowboyRequest).

-doc ~"""
Parses HTTP headers from Cowboy request.

Behavior callback that extracts HTTP headers
using `cowboy_req:headers/1`.
""".
-spec parse_headers(CowboyRequest) -> Headers when
    CowboyRequest :: cowboy_req:req(),
    Headers :: arizona_request:headers().
parse_headers(CowboyRequest) ->
    cowboy_req:headers(CowboyRequest).

-doc ~"""
Reads the complete request body from Cowboy request.

Behavior callback that reads the full request body
using `cowboy_req:read_body/1`. Returns the body and
updated Cowboy request (for streaming protocols).
""".
-spec read_body(CowboyRequest) -> {Body, UpdatedCowboyRequest} when
    CowboyRequest :: cowboy_req:req(),
    Body :: arizona_request:body(),
    UpdatedCowboyRequest :: cowboy_req:req().
read_body(CowboyRequest) ->
    {ok, Body, CowboyRequest1} = cowboy_req:read_body(CowboyRequest),
    {Body, CowboyRequest1}.

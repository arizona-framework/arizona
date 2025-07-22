-module(arizona_request).

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
-nominal bindings() :: #{atom() => dynamic()}.
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

-spec get_method(Request) -> Method when
    Request :: request(),
    Method :: method().
get_method(#request{method = Method}) ->
    Method.

-spec get_path(Request) -> Path when
    Request :: request(),
    Path :: path().
get_path(#request{path = Path}) ->
    Path.

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

-spec get_raw_request(Request) -> RawRequest when
    Request :: request(),
    RawRequest :: raw_request().
get_raw_request(#request{raw_request = RawRequest}) ->
    RawRequest.

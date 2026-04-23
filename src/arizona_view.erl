-module(arizona_view).
-moduledoc """
Behaviour for route-mounted Arizona handlers (pages, not embedded
components).

Adds a single mount callback over the shared `arizona_handler`
contract: `mount/2` takes both the initial `Bindings` and an
`arizona_req:request()`, giving a view access to URL path bindings,
query params, cookies, headers, and body via the request accessors.

The other lifecycle callbacks (`render/1`, `handle_event/3`,
`handle_info/2`, `unmount/1`) come from `arizona_handler`. Views
don't implement `handle_update/2` in practice -- they're
route-level and never receive parent prop updates -- but the shared
behaviour allows it for forward compatibility.

Handlers include `arizona_view.hrl`, which declares both behaviours
(`arizona_handler` and `arizona_view`), enables the parse transform,
and brings in the common macros.

## Required callbacks

- `mount/2` -- runs once on instance creation with
  `(Bindings, Request)`; returns initial bindings plus any reset
  values
- `render/1` -- from `arizona_handler`

## Optional callbacks

- `handle_event/3`, `handle_info/2`, `unmount/1` -- from
  `arizona_handler`
""".

%% --------------------------------------------------------------------
%% API function exports
%% --------------------------------------------------------------------

-export([call_mount/3]).

%% --------------------------------------------------------------------
%% Behaviour callbacks
%% --------------------------------------------------------------------

-doc """
Initializes a view instance. Called once when the framework mounts
the handler at the top of a route.

`Bindings` carries the route's static `bindings` config (plus
anything middleware layered on top). `Request` is the current
`arizona_req:request()` -- use its accessors to reach URL path
bindings, query params, cookies, headers, and body.
""".
-callback mount(Bindings, Request) -> arizona_stateful:mount_ret() when
    Bindings :: arizona_stateful:bindings(),
    Request :: az:request().

%% --------------------------------------------------------------------
%% API Functions
%% --------------------------------------------------------------------

-doc """
Invokes the required `mount/2` callback on a view handler module.
""".
-spec call_mount(Handler, Bindings, Request) -> arizona_stateful:mount_ret() when
    Handler :: module(),
    Bindings :: arizona_stateful:bindings(),
    Request :: az:request().
call_mount(H, Bindings, Req) ->
    H:mount(Bindings, Req).

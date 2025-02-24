-module(arizona_layout).
-moduledoc ~"""
The `arizona_layout` behaviour defines the interface for creating layouts
in the Arizona framework.

Layouts are used to wrap views in a consistent structure, such as a common
HTML template with headers, footers, and navigation. Unlike views, layouts
**do not maintain state** and are **rendered only once** when the view is
first mounted. Subsequent updates and re-renders only affect the wrapped view,
not the layout.

To implement a layout in Arizona, a module must define the following callbacks:

- `c:mount/2`: Initializes the layout with bindings and a WebSocket connection.
- `c:render/1`: Renders the layout's template **once**, wrapping the view's content.

Layouts provide a way to create consistent UI structures across multiple views,
such as headers, footers, or navigation bars.
""".

%% --------------------------------------------------------------------
%% Callback support function exports
%% --------------------------------------------------------------------

-export([mount/3]).
-export([render/1]).

%% --------------------------------------------------------------------
%% Callback definitions
%% --------------------------------------------------------------------

-doc ~"""
Initializes the layout when it is first rendered.

This callback sets up the layout's bindings and prepares it to wrap the view's
content. The layout is rendered only once, and its state remains static throughout
the lifecycle of the view.

## Parameters

- `Bindings`: A map (`t:arizona:bindings/0`) containing the initial data for the layout.
- `Socket`: The WebSocket connection (`t:arizona:socket/0`) associated with the layout.

## Returns

The initialized view state (`t:arizona:view/0`) for the layout.
""".
-callback mount(Bindings, Socket) -> View when
    Bindings :: arizona:bindings(),
    Socket :: arizona:socket(),
    View :: arizona:view().

-doc ~"""
Renders the layout's template, wrapping the view's content.

This callback is invoked only once when the layout is first rendered.
Subsequent updates and re-renders only affect the wrapped view, not the layout.

## Parameters

- `View`: The current view state (`t:arizona:view/0`), which includes the layout's
  bindings and the view's content.

## Returns

The rendered template as `t:arizona:rendered_layout_template/0`.
""".
-callback render(View) -> Rendered when
    View :: arizona:view(),
    Rendered :: arizona:rendered_layout_template().

%% --------------------------------------------------------------------
%% Callback support function definitions
%% --------------------------------------------------------------------

-doc ~"""
Delegates to the `c:mount/2` callback defined in the layout module (`Mod`).

This function is used internally by Arizona to initialize the layout.

## Parameters

- `Mod`: The module name of the layout being mounted. This must be a valid atom.
- `Bindings`: A map (`t:arizona:bindings/0`) containing the initial data for the layout.
- `Socket`: The WebSocket connection (`t:arizona:socket/0`) associated with the layout.

## Returns

The initialized view state (`t:arizona:view/0`) for the layout.
""".
-spec mount(Mod, Bindings, Socket) -> View when
    Mod :: module(),
    Bindings :: arizona:bindings(),
    Socket :: arizona:socket(),
    View :: arizona:view().
mount(Mod, Bindings, Socket) when is_atom(Mod), is_map(Bindings) ->
    erlang:apply(Mod, mount, [Bindings, Socket]).

-doc ~"""
Delegates to the `c:render/1` callback defined in the layout module (`Mod`).

This function is used internally by Arizona to render the layout's template once
when the view is first mounted.

## Parameters

- `View`: The current view state (`t:arizona:view/0`), which includes the layout's
  bindings and the view's content.

## Returns

The rendered template as `t:arizona:rendered_layout_template/0`.
""".
-spec render(View) -> Rendered when
    View :: arizona:view(),
    Rendered :: arizona:rendered_layout_template().
render(View) ->
    Mod = arizona_view:module(View),
    erlang:apply(Mod, render, [View]).

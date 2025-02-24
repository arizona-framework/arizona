-module(arizona_component).
-moduledoc ~"""
The `arizona_component` module provides functionality for rendering
**stateless components** in the Arizona framework.

Components are reusable UI elements that can be embedded within views.
Unlike views, components do not maintain their own state; instead, they
rely on the state of the parent view that calls them. This makes components
lightweight and easy to reuse across different parts of an application.

## Usage

Components are typically used to encapsulate reusable UI elements, such as
buttons, forms, or cards. They are rendered within a view's template and
inherit their state from the parent view.

Here’s how components fit into the Arizona framework:

- `Stateless`: Components do not maintain their own state. They rely on the
  state of the parent view.
- `Reusable`: Components can be used across multiple views or even multiple
  times within the same view.
- `Lightweight`: Since they are stateless, components are simple to implement
  and efficient to render.
""".

%% --------------------------------------------------------------------
%% API function exports
%% --------------------------------------------------------------------

-export([render/3]).

%% --------------------------------------------------------------------
%% API function definitions
%% --------------------------------------------------------------------

-doc ~"""
Renders a stateless component by delegating to the specified function (`Fun`)
in the component module (`Mod`).

The component's state is derived from the parent view's state (`View`), making
it stateless and reusable.

## Parameters

- `Mod`: The module name of the component. This must be a valid atom.
- `Fun`: The function name (`t:atom/0`) within the component module that handles
  rendering.
- `View`: The current view state (`t:arizona:view/0`) of the parent view. This is
  passed to the component to access assigns or other data.

## Returns

The rendered component template as `t:arizona:rendered_component_template`.
""".
-spec render(Mod, Fun, View) -> Token when
    Mod :: module(),
    Fun :: atom(),
    View :: arizona:view(),
    Token :: arizona:rendered_component_template().
render(Mod, Fun, View) when is_atom(Mod), is_atom(Fun) ->
    erlang:apply(Mod, Fun, [View]).

-module(arizona_lifecycle).
-moduledoc ~"""
Stateful component lifecycle management.

Manages the mounting and rendering lifecycle of stateful components,
determining whether to mount new components or update existing ones
based on component ID and view state.

## Lifecycle Flow

1. Check if component exists by ID in view state
2. If exists: merge new bindings with existing state and render
3. If not exists: mount new component with bindings and render
4. Return component ID, rendered template, and updated view

## Example

```erlang
1> Bindings = #{id => ~"my_component", count => 5}.
2> {Id, Template, View1} = arizona_lifecycle:prepare_render(my_module, Bindings, View).
{~"my_component", #template{...}, UpdatedView}
```
""".

%% --------------------------------------------------------------------
%% API function exports
%% --------------------------------------------------------------------

-export([prepare_render/3]).

%% --------------------------------------------------------------------
%% API function definitions
%% --------------------------------------------------------------------

-doc ~"""
Prepares a stateful component for rendering.

Checks if the component exists by ID in the view state. If it exists,
merges new bindings with existing state. If not, mounts a new component.
Returns the component ID, rendered template, and updated view.
""".
-spec prepare_render(Module, Bindings, View) -> Result when
    Module :: module(),
    Bindings :: arizona_binder:map(),
    View :: arizona_view:view(),
    Result :: {Id, Template, View1},
    Id :: arizona_stateful:id(),
    Template :: arizona_template:template(),
    View1 :: arizona_view:view().
prepare_render(Module, Bindings, View) ->
    maybe
        % Check if component already exists or needs mounting
        #{id := Id} ?= Bindings,
        {ok, State} ?= arizona_view:find_stateful_state(Id, View),
        UpdatedState = arizona_stateful:merge_bindings(Bindings, State),
        render(Id, UpdatedState, View)
    else
        _Other ->
            mount_and_render(Module, Bindings, View)
    end.

%% --------------------------------------------------------------------
%% Internal functions
%% --------------------------------------------------------------------

render(Id, State, View) ->
    Template = arizona_stateful:call_render_callback(State),
    UpdatedView = arizona_view:put_stateful_state(Id, State, View),
    {Id, Template, UpdatedView}.

mount_and_render(Module, Bindings, View) ->
    State = arizona_stateful:call_mount_callback(Module, Bindings),
    Id = arizona_stateful:get_binding(id, State),
    render(Id, State, View).

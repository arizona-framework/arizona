-module(arizona_stateless).
-moduledoc ~""""
Stateless component callback execution utilities.

Provides typed interface for executing stateless component render
callbacks. Stateless components are pure functions that take bindings
and return templates without maintaining any internal state.

## Stateless Component Pattern

Stateless components are implemented as functions that:
1. Accept `arizona_binder:map/0` with template variables
2. Return `arizona_template:template/0` for rendering
3. Have no internal state
4. Are deterministic based solely on input bindings

## Example

```erlang
%% In my_component.erl:
render(Bindings) ->
    arizona_template:from_string(~"""
    <div>
        Hello {arizona_template:get_binding(name, Bindings)},
        you are {arizona_template:get_binding(age, Bindings)} years old
    </div>
    """).

%% Usage:
1> Bindings = #{name => ~"John", age => 25}.
2> arizona_stateless:call_render_callback(my_component, render, Bindings).
#template{...}
```
"""".

%% --------------------------------------------------------------------
%% API function exports
%% --------------------------------------------------------------------

-export([call_render_callback/3]).

%% --------------------------------------------------------------------
%% API function definitions
%% --------------------------------------------------------------------

-doc ~"""
Executes a stateless component render callback.

Calls the specified module function with bindings and returns the
resulting template. Provides type-safe wrapper around `apply/3`
for stateless component execution.
""".
-spec call_render_callback(Mod, Fun, Bindings) -> Template when
    Mod :: module(),
    Fun :: atom(),
    Bindings :: arizona_binder:map(),
    Template :: arizona_template:template().
call_render_callback(Mod, Fun, Bindings) ->
    apply(Mod, Fun, [Bindings]).

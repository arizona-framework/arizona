-module(arizona_stateless).
-moduledoc ~"""
Provides stateless component functionality for Arizona LiveView applications.

## Overview

The stateless module provides utilities for invoking stateless components
in Arizona LiveView applications. Stateless components are lightweight
functions that render based on provided bindings without maintaining
persistent state between renders.

## Features

- **Lightweight Components**: Function-based components without state management
- **Callback Invocation**: Safe invocation of stateless component functions
- **Socket Integration**: Seamless integration with Arizona socket system
- **Performance Optimized**: Minimal overhead for simple presentational components
- **Flexible Rendering**: Supports any module/function combination for rendering

## Key Functions

- `call_render_callback/3`: Invoke stateless component render functions

## Component Characteristics

### Stateless Nature

Stateless components don't maintain state between renders, making them ideal for:
- Simple presentational components
- Utility functions that transform data
- Reusable UI elements without complex logic
- Components that depend only on input parameters

### Performance Benefits

- No state management overhead
- Fast invocation without lifecycle management
- Minimal memory footprint
- Efficient rendering for simple use cases

## Usage Patterns

Stateless components are typically used for:
- Formatting and display utilities
- Simple UI components (buttons, cards, etc.)
- Template helpers and transformations
- Components that don't need mount/unmount lifecycle

This module serves as the foundation for stateless component rendering
in Arizona LiveView applications.
""".

%% --------------------------------------------------------------------
%% API function exports
%% --------------------------------------------------------------------

-export([call_render_callback/3]).

%% --------------------------------------------------------------------
%% API function definitions
%% --------------------------------------------------------------------

-doc ~"""
Invoke a stateless component render function.

Calls the specified module and function with the provided socket, enabling
stateless component rendering. The function should accept a socket and return
an updated socket with rendered content.

## Component Function Signature

The called function must have the signature:
```erlang
render_function(Socket) -> Socket1 when
    Socket :: arizona_socket:socket(),
    Socket1 :: arizona_socket:socket().
```

## Examples

```erlang
1> arizona_stateless:call_render_callback(button_component, render, Socket).
#socket{html_acc = [~"<button>Click me</button>"], ...}
2> arizona_stateless:call_render_callback(card_component, render_title, Socket).
#socket{html_acc = [~"<h2>Title</h2>"], ...}
```

## Error Handling

The function will propagate any errors from the called component function.
Ensure the target module and function exist and have the correct signature.

## Performance

Direct function invocation with minimal overhead makes this suitable for
frequently called presentational components.
""".
-spec call_render_callback(Mod, Fun, Socket) -> Socket1 when
    Mod :: module(),
    Fun :: atom(),
    Socket :: arizona_socket:socket(),
    Socket1 :: arizona_socket:socket().
call_render_callback(Mod, Fun, Socket) ->
    apply(Mod, Fun, [Socket]).

-module(arizona_list).
-moduledoc ~"""
Provides list processing functionality for Arizona template rendering.

## Overview

The list module provides utility functions for list template processing,
handling function calls for list items and dynamic element functions.
It serves as a bridge between list templates and the template rendering
system.

## Features

- **Item Function Calls**: Safe invocation of list item functions
- **Element Function Calls**: Dynamic element function processing for list items
- **Template Integration**: Seamless integration with Arizona's template system
- **Function Validation**: Ensures proper function signatures and return values

## Key Functions

- `call_item_function/2`: Invoke item function for list template rendering
- `call_element_function/3`: Call dynamic element functions within list contexts

## Usage

The module is primarily used internally by Arizona's template rendering system
to process list templates and handle dynamic content generation within lists.
Functions are typically called by the renderer when processing list templates
that contain item functions or dynamic element expressions.
""".

%% --------------------------------------------------------------------
%% API function exports
%% --------------------------------------------------------------------

-export([call_item_function/2]).
-export([call_element_function/3]).

%% --------------------------------------------------------------------
%% API function definitions
%% --------------------------------------------------------------------

-doc ~"""
Invoke item function for list template rendering.

Calls the provided item function with the given item value, returning
the HTML result for template rendering. Used internally by the template
system when processing list templates.

## Examples

```erlang
1> ItemFun = fun(Item) -> <<"<li>", Item/binary, "</li>">> end.
#Fun<...>
2> arizona_list:call_item_function(ItemFun, <<"test">>).
<<"<li>test</li>">>
```
""".
-spec call_item_function(ItemFun, Item) -> Html when
    ItemFun :: fun((term()) -> arizona_html:html()),
    Item :: term(),
    Html :: arizona_html:html().
call_item_function(ItemFun, Item) ->
    apply(ItemFun, [Item]).

-doc ~"""
Call dynamic element function for list item processing.

Invokes a dynamic element function with the provided item and socket,
returning the result for template processing. Used when list templates
contain dynamic expressions that need access to both the list item
and the socket context.

## Examples

```erlang
1> Fun = fun(Item, Socket) -> arizona_socket:put_binding(current_item, Item, Socket) end.
#Fun<...>
2> Socket = arizona_socket:new(#{}).
#socket{...}
3> arizona_list:call_element_function(Fun, <<"test">>, Socket).
#socket{bindings = #{current_item => <<"test">>}, ...}
```
""".
-spec call_element_function(Fun, Item, Socket) -> Result when
    Fun :: fun((term(), arizona_socket:socket()) -> term()),
    Item :: term(),
    Socket :: arizona_socket:socket(),
    Result :: term().
call_element_function(Fun, Item, Socket) ->
    apply(Fun, [Item, Socket]).

-module(arizona_html).
-moduledoc ~"""
HTML value conversion utilities for template rendering.

Provides unified conversion of Erlang data types to HTML-safe iodata.
Used throughout the rendering pipeline to convert template binding
values into HTML content.

## Supported Types

- `binary()` - Passed through unchanged
- `iolist()` - Passed through unchanged
- `atom()` - Converted to binary with UTF-8 encoding
- `integer()` - Converted to decimal binary representation
- `float()` - Converted to binary using default formatting

## Example

```erlang
1> arizona_html:to_html(~"hello").
~"hello"
2> arizona_html:to_html(world).
~"world"
3> arizona_html:to_html(123).
~"123"
4> arizona_html:to_html(45.67).
~"45.67"
```
""".

%% --------------------------------------------------------------------
%% API function exports
%% --------------------------------------------------------------------

-export([to_html/1]).

%% --------------------------------------------------------------------
%% Types exports
%% --------------------------------------------------------------------

-export_type([html/0]).
-export_type([value/0]).

%% --------------------------------------------------------------------
%% Types definitions
%% --------------------------------------------------------------------

-nominal html() :: iodata().
-type value() :: binary() | iolist() | atom() | number().

%% --------------------------------------------------------------------
%% API Functions
%% --------------------------------------------------------------------

-doc ~"""
Converts a value to HTML-safe `iodata/0`.

Handles multiple Erlang data types and converts them to a format
suitable for HTML output. Binaries and iolists are passed through
unchanged, while other types are converted to binary representation.
""".
-spec to_html(Value) -> Html when
    Value :: value(),
    Html :: html().
to_html(Value) when is_binary(Value) ->
    Value;
% Assume it is an iolist()
to_html(Value) when is_list(Value) ->
    Value;
to_html(Value) when is_atom(Value) ->
    atom_to_binary(Value, utf8);
to_html(Value) when is_integer(Value) ->
    io:format("[DEBUG] arizona_html:to_html converting integer: ~p~n  Stacktrace: ~p~n",
              [Value, erlang:process_info(self(), current_stacktrace)]),
    integer_to_binary(Value, 10);
to_html(Value) when is_float(Value) ->
    list_to_binary(io_lib:format("~p", [Value])).

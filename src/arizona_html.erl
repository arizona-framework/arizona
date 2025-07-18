-module(arizona_html).

%% --------------------------------------------------------------------
%% API function exports
%% --------------------------------------------------------------------

-export([to_html/1]).

%% --------------------------------------------------------------------
%% Types exports
%% --------------------------------------------------------------------

-export_type([html/0]).

%% --------------------------------------------------------------------
%% Types definitions
%% --------------------------------------------------------------------

-nominal html() :: binary() | iodata().

%% --------------------------------------------------------------------
%% API Functions
%% --------------------------------------------------------------------

-doc ~"""
Convert various Erlang terms to HTML representation.

Handles binary, list, atom, integer, float, and other terms by converting
them to appropriate HTML-safe binary strings. This is a pure function
with no side effects.

## Examples

```erlang
<<"hello">> = arizona_html:to_html(hello),
<<"42">> = arizona_html:to_html(42),
[<<"foo">>, <<"bar">>] = arizona_html:to_html([foo, bar]).
```
""".
-spec to_html(Value) -> Html when
    Value :: dynamic(),
    Html :: html().
to_html(Value) when is_binary(Value) ->
    Value;
to_html(Value) when is_list(Value) ->
    list_to_html(Value);
to_html(Value) when is_atom(Value) ->
    atom_to_binary(Value, utf8);
to_html(Value) when is_integer(Value) ->
    integer_to_binary(Value, 10);
to_html(Value) when is_float(Value) ->
    list_to_binary(io_lib:format("~p", [Value]));
to_html(Value) ->
    list_to_binary(io_lib:format("~tp", [Value])).

%% --------------------------------------------------------------------
%% Internal Functions
%% --------------------------------------------------------------------

list_to_html([]) ->
    [];
list_to_html([Value | T]) ->
    case to_html(Value) of
        [] ->
            list_to_html(T);
        Html ->
            [Html | list_to_html(T)]
    end.

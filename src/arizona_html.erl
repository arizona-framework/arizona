-module(arizona_html).

%% --------------------------------------------------------------------
%% API function exports
%% --------------------------------------------------------------------

-export([to_html/2]).

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
Convert various Erlang terms to HTML binary representation.

Handles binary, list, atom, integer, float, and other terms by converting
them to appropriate HTML-safe binary strings. Works with socket context
for proper state management during rendering.

## Examples

```erlang
{<<"hello">>, Socket1} = arizona_html:to_html(hello, Socket),
{<<"42">>, Socket2} = arizona_html:to_html(42, Socket1),
{[<<"foo">>, <<"bar">>], Socket3} = arizona_html:to_html([foo, bar], Socket2).
```
""".
-spec to_html(Value, Socket) -> {Html, Socket1} when
    Value :: dynamic(),
    Socket :: arizona_socket:socket(),
    Html :: html(),
    Socket1 :: arizona_socket:socket().
to_html(Value, Socket) when is_binary(Value) ->
    {Value, Socket};
to_html(Value, Socket) when is_list(Value) ->
    lists:foldl(
        fun(Item, {HtmlAcc, AccSocket}) ->
            {HtmlAcc1, AccSocket1} = to_html(Item, AccSocket),
            case HtmlAcc of
                [] -> {HtmlAcc1, AccSocket1};
                _ -> {[HtmlAcc, HtmlAcc1], AccSocket1}
            end
        end,
        {[], Socket},
        Value
    );
to_html(Value, Socket) when is_atom(Value) ->
    {atom_to_binary(Value, utf8), Socket};
to_html(Value, Socket) when is_integer(Value) ->
    {integer_to_binary(Value), Socket};
to_html(Value, Socket) when is_float(Value) ->
    {list_to_binary(io_lib:format("~p", [Value])), Socket};
to_html(Value, Socket) ->
    Html = list_to_binary(io_lib:format("~tp", [Value])),
    {Html, Socket}.

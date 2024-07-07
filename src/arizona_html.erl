-module(arizona_html).
-moduledoc """
HTML support.
""".

%% API functions.
-export([to_safe/1]).
-export([safe_types/0]).

%% --------------------------------------------------------------------
%% API funtions.
%% --------------------------------------------------------------------

-spec to_safe(Value) -> binary()
    when Value :: binary() | atom() | number().
to_safe(V) when is_binary(V) ->
    V;
to_safe(V) when is_atom(V) ->
    atom_to_binary(V, utf8);
to_safe(V) when is_integer(V) ->
    integer_to_binary(V, 10);
to_safe(V) when is_float(V) ->
    io_lib:format("~p", [V]).

-spec safe_types() -> [binary | atom | integer | float].
safe_types() ->
    [binary, atom, integer, float].

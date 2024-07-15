-module(arizona_html).
-moduledoc """
HTML support.
""".

%% --------------------------------------------------------------------
%% API function exports
%% --------------------------------------------------------------------

-export([to_safe/1]).

%% --------------------------------------------------------------------
%% Types (and their exports)
%% --------------------------------------------------------------------

-type safe_type() :: binary() | atom() | number().
-export_type([safe_type/0]).

%% --------------------------------------------------------------------
%% API function definitions
%% --------------------------------------------------------------------

-spec to_safe(Value) -> binary()
    when Value :: safe_type().
to_safe(V) when is_binary(V) ->
    V;
to_safe(V) when is_atom(V) ->
    atom_to_binary(V, utf8);
to_safe(V) when is_integer(V) ->
    integer_to_binary(V, 10);
to_safe(V) when is_float(V) ->
    io_lib:format("~p", [V]).

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

-nominal html() :: iodata().

%% --------------------------------------------------------------------
%% API Functions
%% --------------------------------------------------------------------

-spec to_html(Value) -> Html when
    Value :: dynamic(),
    Html :: html().
to_html(Value) when is_binary(Value) ->
    Value;
% Assume it is an iolist()
to_html(Value) when is_list(Value) ->
    Value;
to_html(Value) when is_atom(Value) ->
    atom_to_binary(Value, utf8);
to_html(Value) when is_integer(Value) ->
    integer_to_binary(Value, 10);
to_html(Value) when is_float(Value) ->
    list_to_binary(io_lib:format("~p", [Value])).

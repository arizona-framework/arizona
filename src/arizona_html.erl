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

-module(arizona_binder).

%% --------------------------------------------------------------------
%% API function exports
%% --------------------------------------------------------------------

-export([new/1]).
-export([get/2]).
-export([get/3]).
-export([find/2]).
-export([put/3]).
-export([keys/1]).
-export([is_empty/1]).

%% --------------------------------------------------------------------
%% Types exports
%% --------------------------------------------------------------------

-export_type([key/0]).
-export_type([value/0]).
-export_type([default_fun/0]).
-export_type([bindings/0]).

%% --------------------------------------------------------------------
%% Types definitions
%% --------------------------------------------------------------------

-opaque bindings() :: #{key() => value()}.
-nominal key() :: atom().
-type value() :: dynamic().
-nominal default_fun() :: fun(() -> value()).

%% --------------------------------------------------------------------
%% API Functions
%% --------------------------------------------------------------------

-spec new(InitialBindings) -> Bindings when
    InitialBindings :: map(),
    Bindings :: bindings().
new(InitialBindings) when is_map(InitialBindings) ->
    InitialBindings.

-spec get(Key, Bindings) -> Value when
    Key :: key(),
    Bindings :: bindings(),
    Value :: value().
get(Key, Bindings) when is_atom(Key), is_map(Bindings) ->
    maps:get(Key, Bindings).

-spec get(Key, Bindings, DefaultFun) -> Value when
    Key :: key(),
    Bindings :: bindings(),
    DefaultFun :: default_fun(),
    Value :: value().
get(Key, Bindings, DefaultFun) when is_atom(Key), is_map(Bindings), is_function(DefaultFun, 0) ->
    case Bindings of
        #{Key := Value} ->
            Value;
        #{} ->
            DefaultFun()
    end.

-spec find(Key, Bindings) -> Result when
    Key :: key(),
    Bindings :: bindings(),
    Result :: {ok, value()} | error.
find(Key, Bindings) when is_atom(Key), is_map(Bindings) ->
    case Bindings of
        #{Key := Value} ->
            {ok, Value};
        #{} ->
            error
    end.

-spec put(Key, Value, Bindings) -> Bindings1 when
    Key :: key(),
    Value :: value(),
    Bindings :: bindings(),
    Bindings1 :: bindings().
put(Key, Value, Bindings) when is_atom(Key), is_map(Bindings) ->
    Bindings#{Key => Value}.

-spec keys(Bindings) -> Keys when
    Bindings :: bindings(),
    Keys :: [key()].
keys(Bindings) when is_map(Bindings) ->
    maps:keys(Bindings).

-spec is_empty(Bindings) -> IsEmpty when
    Bindings :: bindings(),
    IsEmpty :: boolean().
is_empty(Bindings) when is_map(Bindings) ->
    maps:size(Bindings) =:= 0.

-module(arizona_binder).

%% --------------------------------------------------------------------
%% API function exports
%% --------------------------------------------------------------------

-export([new/0]).
-export([get/2]).
-export([get/3]).
-export([find/2]).
-export([put/3]).
-export([merge/2]).
-export([remove/2]).
-export([keys/1]).
-export([values/1]).
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

-type key() :: atom().
-type value() :: dynamic().
-type default_fun() :: fun(() -> value()).
-opaque bindings() :: #{key() => value()}.

%% --------------------------------------------------------------------
%% API Functions
%% --------------------------------------------------------------------

-spec new() -> Bindings when
    Bindings :: bindings().
new() ->
    #{}.

-spec get(Key, Bindings) -> Value when
    Key :: key(),
    Bindings :: bindings(),
    Value :: value().
get(Key, Bindings) when is_atom(Key), is_map(Bindings) ->
    maps:get(Key, Bindings).

-spec get(Key, Bindings, Default) -> Value when
    Key :: key(),
    Bindings :: bindings(),
    Default :: default_fun(),
    Value :: value().
get(Key, Bindings, Default) when is_atom(Key), is_map(Bindings), is_function(Default, 0) ->
    case Bindings of
        #{Key := Value} ->
            Value;
        #{} ->
            Default()
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

-spec merge(Bindings1, Bindings2) -> Bindings when
    Bindings1 :: bindings(),
    Bindings2 :: bindings(),
    Bindings :: bindings().
merge(Bindings1, Bindings2) when is_map(Bindings1), is_map(Bindings2) ->
    maps:merge(Bindings1, Bindings2).

-spec remove(Key, Bindings) -> Bindings1 when
    Key :: key(),
    Bindings :: bindings(),
    Bindings1 :: bindings().
remove(Key, Bindings) when is_atom(Key), is_map(Bindings) ->
    maps:remove(Key, Bindings).

-spec keys(Bindings) -> Keys when
    Bindings :: bindings(),
    Keys :: [key()].
keys(Bindings) when is_map(Bindings) ->
    maps:keys(Bindings).

-spec values(Bindings) -> Values when
    Bindings :: bindings(),
    Values :: [value()].
values(Bindings) when is_map(Bindings) ->
    maps:values(Bindings).

-spec is_empty(Bindings) -> IsEmpty when
    Bindings :: bindings(),
    IsEmpty :: boolean().
is_empty(Bindings) when is_map(Bindings) ->
    maps:size(Bindings) =:= 0.

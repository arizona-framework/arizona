-module(arizona_binder).
-moduledoc ~"""
Template variable binding management and access utilities.

Provides a type-safe abstraction layer over Erlang maps for managing
template variable bindings. Used throughout the rendering pipeline to
access template data with atom keys and any dynamic values.

## Key Features

- Type-safe access to template variables
- Default value functions for missing keys
- Safe lookup operations with ok/error tuples
- Opaque bindings type for encapsulation
- Direct map conversion for interoperability

## Example

```erlang
1> Bindings = arizona_binder:new(#{name => ~"John", age => 25}).
#{name => ~"John", age => 25}
2> arizona_binder:get(name, Bindings).
~"John"
3> arizona_binder:find(email, Bindings).
error
4> arizona_binder:get(email, Bindings, fun() -> ~"unknown" end).
~"unknown"
```
""".
-compile({nowarn_redefined_builtin_type, [map/0]}).

%% --------------------------------------------------------------------
%% API function exports
%% --------------------------------------------------------------------

-export([new/1]).
-export([to_map/1]).
-export([get/2]).
-export([get/3]).
-export([find/2]).
-export([put/3]).
-export([keys/1]).
-export([is_empty/1]).

%% --------------------------------------------------------------------
%% Types exports
%% --------------------------------------------------------------------

-export_type([bindings/0]).
-export_type([map/0]).
-export_type([key/0]).
-export_type([value/0]).
-export_type([default_fun/0]).

%% --------------------------------------------------------------------
%% Types definitions
%% --------------------------------------------------------------------

-opaque bindings() :: #{key() => value()}.
-nominal map() :: #{key() => value()}.
-nominal key() :: atom().
-type value() :: dynamic().
-nominal default_fun() :: fun(() -> value()).

%% --------------------------------------------------------------------
%% API Functions
%% --------------------------------------------------------------------

-doc ~"""
Creates new bindings from a map.

Converts a regular map to opaque bindings type. Keys must be atoms
and values can be any dynamic content.
""".
-spec new(InitialBindings) -> Bindings when
    InitialBindings :: map(),
    Bindings :: bindings().
new(InitialBindings) when is_map(InitialBindings) ->
    InitialBindings.

-doc ~"""
Converts bindings back to a regular map.

Provides direct access to the underlying map representation
for interoperability with standard map operations.
""".
-spec to_map(Bindings) -> Map when
    Bindings :: bindings(),
    Map :: map().
to_map(Bindings) when is_map(Bindings) ->
    Bindings.

-doc ~"""
Gets a value by key, throwing if not found.

Returns the value associated with the key. Raises `{badkey, Key}`
exception if the key is not present in the bindings.
""".
-spec get(Key, Bindings) -> Value when
    Key :: key(),
    Bindings :: bindings(),
    Value :: value().
get(Key, Bindings) when is_atom(Key), is_map(Bindings) ->
    maps:get(Key, Bindings).

-doc ~"""
Gets a value by key with default function fallback.

Returns the value if the key exists, otherwise calls the default
function to generate a fallback value. Useful for optional bindings.
""".
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

-doc ~"""
Safely looks up a value by key.

Returns `{ok, Value}` if the key exists, or `error` if not found.
Provides exception-free lookup for optional bindings.
""".
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

-doc ~"""
Adds or updates a key-value pair in the bindings.

Returns new bindings with the specified key-value pair added
or updated. The original bindings are not modified.
""".
-spec put(Key, Value, Bindings) -> Bindings1 when
    Key :: key(),
    Value :: value(),
    Bindings :: bindings(),
    Bindings1 :: bindings().
put(Key, Value, Bindings) when is_atom(Key), is_map(Bindings) ->
    Bindings#{Key => Value}.

-doc ~"""
Returns a list of all binding keys.

Extracts all atom keys from the bindings in arbitrary order.
Useful for iterating over available template variables.
""".
-spec keys(Bindings) -> Keys when
    Bindings :: bindings(),
    Keys :: [key()].
keys(Bindings) when is_map(Bindings) ->
    maps:keys(Bindings).

-doc ~"""
Checks if the bindings are empty.

Returns `true` if no key-value pairs exist, `false` otherwise.
Useful for conditional template logic.
""".
-spec is_empty(Bindings) -> IsEmpty when
    Bindings :: bindings(),
    IsEmpty :: boolean().
is_empty(Bindings) when is_map(Bindings) ->
    maps:size(Bindings) =:= 0.

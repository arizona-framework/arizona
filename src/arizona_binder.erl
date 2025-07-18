-module(arizona_binder).
-moduledoc ~"""
Provides pure variable binding operations for Arizona templates.

## Overview

The binder module provides pure, reusable binding operations without side effects.
It handles variable binding access, modification, and merging operations that
can be used across different Arizona modules.

## Key Features

- **Pure Functions**: No side effects, only data transformation
- **Reusable**: Can be used by arizona_stateful, arizona_socket, etc.
- **Safe Access**: Provides error handling for missing bindings
- **Conditional Defaults**: Supports default function execution when keys are missing
- **Comprehensive Operations**: Get, put, find, merge, and more

## Usage

```erlang
% Basic binding access
Value = arizona_binder:get(title, Bindings),

% Conditional binding with default function
Greeting = arizona_binder:get(greeting, Bindings, fun() ->
    <<"Default Greeting">>
end),

% Safe binding lookup
case arizona_binder:find(optional_key, Bindings) of
    {ok, Value} -> Value;
    error -> <<"Not found">>
end,

% Binding modification
NewBindings = arizona_binder:put(title, <<"New Title">>, Bindings),

% Merge bindings
MergedBindings = arizona_binder:merge(Bindings1, Bindings2).
```

## Integration

This module is designed to be used by other Arizona modules that need
binding operations, particularly arizona_template, arizona_stateful,
and arizona_socket.
""".

%% --------------------------------------------------------------------
%% API function exports
%% --------------------------------------------------------------------

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

-nominal key() :: atom().
-nominal value() :: dynamic().
-nominal default_fun() :: fun(() -> value()).
-opaque bindings() :: #{key() => value()}.

%% --------------------------------------------------------------------
%% API Functions
%% --------------------------------------------------------------------

-doc ~"""
Get a binding value.

Retrieves the value associated with the given key from the bindings map.
This is a pure function with no side effects.

## Parameters

- `Key`: The binding key to look up
- `Bindings`: Map of variable bindings

## Returns

The value associated with the key.

## Raises

`badkey` error if the key is not found in the bindings map.

## Example

```erlang
Title = arizona_binder:get(title, #{title => ~"Hello World"}).
% Returns: ~"Hello World"
```
""".
-spec get(key(), bindings()) -> value().
get(Key, Bindings) when is_atom(Key), is_map(Bindings) ->
    maps:get(Key, Bindings).

-doc ~"""
Get a binding value with conditional default function.

Retrieves the value associated with the given key from the bindings map.
If the key is not found, executes the default function to compute a fallback value.
This is a pure function with no side effects.

## Parameters

- `Key`: The binding key to look up
- `Bindings`: Map of variable bindings
- `Default`: Zero-arity function to compute default value when key is missing

## Returns

Either the bound value or the result of the default function.

## Example

```erlang
% Key exists - returns bound value
Greeting = arizona_binder:get(greeting, #{greeting => ~"Hello"}, fun() ->
    ~"Default"
end).
% Returns: ~"Hello"

% Key missing - executes default function
Greeting = arizona_binder:get(greeting, #{}, fun() ->
    ~"Default Greeting"
end).
% Returns: ~"Default Greeting"
```
""".
-spec get(key(), bindings(), default_fun()) -> value().
get(Key, Bindings, Default) when is_atom(Key), is_map(Bindings), is_function(Default, 0) ->
    case Bindings of
        #{Key := Value} ->
            Value;
        #{} ->
            Default()
    end.

-doc ~"""
Find a binding value with safe lookup.

Safely looks up a binding value without raising an error if the key is not found.
This is a pure function with no side effects.

## Parameters

- `Key`: The binding key to look up
- `Bindings`: Map of variable bindings

## Returns

- `{ok, Value}` if the key exists
- `error` if the key is not found

## Example

```erlang
case arizona_binder:find(title, Bindings) of
    {ok, Title} -> Title;
    error -> ~"No title"
end.
```
""".
-spec find(key(), bindings()) -> {ok, value()} | error.
find(Key, Bindings) when is_atom(Key), is_map(Bindings) ->
    case Bindings of
        #{Key := Value} ->
            {ok, Value};
        #{} ->
            error
    end.

-doc ~"""
Put a binding value.

Adds or updates a binding in the bindings map.
This is a pure function with no side effects.

## Parameters

- `Key`: The binding key to set
- `Value`: The value to bind
- `Bindings`: Map of variable bindings

## Returns

Updated bindings map with the new key-value pair.

## Example

```erlang
NewBindings = arizona_binder:put(title, ~"New Title", Bindings).
```
""".
-spec put(key(), value(), bindings()) -> bindings().
put(Key, Value, Bindings) when is_atom(Key), is_map(Bindings) ->
    Bindings#{Key => Value}.

-doc ~"""
Merge two bindings maps.

Merges two bindings maps, with the second map taking precedence
for duplicate keys. This is a pure function with no side effects.

## Parameters

- `Bindings1`: First bindings map
- `Bindings2`: Second bindings map (takes precedence)

## Returns

Merged bindings map.

## Example

```erlang
MergedBindings = arizona_binder:merge(
    #{title => ~"Old Title"},
    #{title => ~"New Title", author => ~"John"}
).
% Returns: #{title => ~"New Title", author => ~"John"}
```
""".
-spec merge(bindings(), bindings()) -> bindings().
merge(Bindings1, Bindings2) when is_map(Bindings1), is_map(Bindings2) ->
    maps:merge(Bindings1, Bindings2).

-doc ~"""
Remove a binding.

Removes a binding from the bindings map.
This is a pure function with no side effects.

## Parameters

- `Key`: The binding key to remove
- `Bindings`: Map of variable bindings

## Returns

Updated bindings map with the key removed.

## Example

```erlang
NewBindings = arizona_binder:remove(title, Bindings).
```
""".
-spec remove(key(), bindings()) -> bindings().
remove(Key, Bindings) when is_atom(Key), is_map(Bindings) ->
    maps:remove(Key, Bindings).

-doc ~"""
Get all binding keys.

Returns a list of all keys in the bindings map.
This is a pure function with no side effects.

## Parameters

- `Bindings`: Map of variable bindings

## Returns

List of all binding keys.

## Example

```erlang
Keys = arizona_binder:keys(#{title => ~"Title", author => ~"Author"}).
% Returns: [title, author] (order may vary)
```
""".
-spec keys(bindings()) -> [key()].
keys(Bindings) when is_map(Bindings) ->
    maps:keys(Bindings).

-doc ~"""
Get all binding values.

Returns a list of all values in the bindings map.
This is a pure function with no side effects.

## Parameters

- `Bindings`: Map of variable bindings

## Returns

List of all binding values.

## Example

```erlang
Values = arizona_binder:values(#{title => ~"Title", author => ~"Author"}).
% Returns: [~"Title", ~"Author"] (order may vary)
```
""".
-spec values(bindings()) -> [value()].
values(Bindings) when is_map(Bindings) ->
    maps:values(Bindings).

-doc ~"""
Check if bindings map is empty.

Returns true if the bindings map contains no key-value pairs.
This is a pure function with no side effects.

## Parameters

- `Bindings`: Map of variable bindings

## Returns

`true` if the bindings map is empty, `false` otherwise.

## Example

```erlang
?assert(arizona_binder:is_empty(#{})),
?assertNot(arizona_binder:is_empty(#{key => value})).
```
""".
-spec is_empty(bindings()) -> boolean().
is_empty(Bindings) when is_map(Bindings) ->
    maps:size(Bindings) =:= 0.

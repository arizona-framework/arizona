-module(arizona_fingerprint).
-moduledoc ~"""
Provides fingerprinting functionality for Arizona LiveView component change detection.

## Overview

The fingerprint module generates and compares fingerprints for Arizona LiveView
components to efficiently detect when components need to be remounted. It uses
fast hashing algorithms to create unique identifiers for component state that
can be quickly compared across renders.

## Features

- **Fast Hashing**: Uses erlang:phash2 for optimal performance
- **Component Identity**: Generates unique fingerprints for component state
- **Change Detection**: Efficiently compares fingerprints to detect changes
- **Remount Optimization**: Enables smart remounting decisions for stateful components
- **Lightweight**: Minimal memory footprint and computational overhead

## Key Functions

- `generate/1`: Generate fingerprint from component data
- `match/2`: Compare fingerprints for equality

## Use Cases

### Component Remounting

Fingerprints are used to determine when stateful components need to be remounted:
- When component module changes
- When critical component bindings change
- When component structure changes

### Performance Optimization

By comparing fingerprints instead of deep data structures, Arizona can quickly
determine if a component has changed without expensive comparisons.

## Algorithm

The module uses erlang:phash2 for fingerprint generation because:
- It's significantly faster than cryptographic hash functions
- It provides sufficient uniqueness for component comparison
- It's designed for Erlang term hashing
- It has minimal collision risk for typical use cases

The fingerprint is a binary representation of the hash value, optimized for
storage and comparison performance.
""".

%% --------------------------------------------------------------------
%% API function exports
%% --------------------------------------------------------------------

-export([generate/1]).
-export([match/2]).

%% --------------------------------------------------------------------
%% Types exports
%% --------------------------------------------------------------------

-export_type([fingerprint/0]).

%% --------------------------------------------------------------------
%% Types definitions
%% --------------------------------------------------------------------

-doc ~"""
Opaque fingerprint type representing a component's identity.

A binary hash value that uniquely identifies the current state of a component
for change detection purposes. Generated using erlang:phash2 for optimal
performance.
""".
-opaque fingerprint() :: binary().

%% --------------------------------------------------------------------
%% API function definitions
%% --------------------------------------------------------------------

-doc ~"""
Generate a fingerprint from component data.

Creates a unique binary fingerprint from the provided key data using
erlang:phash2 for fast hashing. The fingerprint can be used for efficient
component change detection and remount decisions.

## Examples

```erlang
1> arizona_fingerprint:generate({my_component, #{name => ~"John"}}).
~"123456789"
2> arizona_fingerprint:generate([item1, item2, item3]).
~"987654321"
```

## Performance

Uses erlang:phash2 which is optimized for Erlang terms and provides excellent
performance characteristics for component fingerprinting.

## Uniqueness

While not cryptographically secure, the fingerprint provides sufficient
uniqueness for component comparison with minimal collision risk.
""".
-spec generate(Key) -> Fingerprint when
    Key :: term(),
    Fingerprint :: fingerprint().
generate(Key) ->
    % Use erlang:phash2 - much faster than crypto:hash
    % Combine module and props in a simple way
    Hash = erlang:phash2(Key),

    % Convert to binary (no base64 encoding needed)
    integer_to_binary(Hash).

-doc ~"""
Compare two fingerprints for equality.

Performs a simple binary comparison of two fingerprints to determine if they
represent the same component state. Returns true if the fingerprints match,
false otherwise.

## Examples

```erlang
1> Fp1 = arizona_fingerprint:generate({my_component, #{name => ~"John"}}).
~"123456789"
2> Fp2 = arizona_fingerprint:generate({my_component, #{name => ~"John"}}).
~"123456789"
3> arizona_fingerprint:match(Fp1, Fp2).
true
4> Fp3 = arizona_fingerprint:generate({my_component, #{name => ~"Jane"}}).
~"987654321"
5> arizona_fingerprint:match(Fp1, Fp3).
false
```

## Performance

Direct binary comparison provides optimal performance for fingerprint matching.
""".
-spec match(OldFingerprint, NewFingerprint) -> Match when
    OldFingerprint :: fingerprint(),
    NewFingerprint :: fingerprint(),
    Match :: boolean().
match(OldFingerprint, NewFingerprint) ->
    OldFingerprint =:= NewFingerprint.

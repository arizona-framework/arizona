-module(arizona_fingerprint).

-export([generate/1]).
-export([match/2]).

-opaque fingerprint() :: binary().
-export_type([fingerprint/0]).

generate(Key) ->
    % Use erlang:phash2 - much faster than crypto:hash
    % Combine module and props in a simple way
    Hash = erlang:phash2(Key),
    
    % Convert to binary (no base64 encoding needed)
    integer_to_binary(Hash).

match(OldFingerprint, NewFingerprint) ->
    OldFingerprint =:= NewFingerprint.

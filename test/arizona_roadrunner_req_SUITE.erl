-module(arizona_roadrunner_req_SUITE).
-include_lib("stdlib/include/assert.hrl").

-export([all/0]).
-export([groups/0]).

-export([parse_bindings_percent_decodes_captures/1]).
-export([parse_bindings_decodes_encoded_slash/1]).
-export([parse_bindings_keeps_malformed_raw/1]).
-export([parse_bindings_empty_is_empty/1]).

all() ->
    [{group, parse_bindings}].

groups() ->
    [
        {parse_bindings, [parallel], [
            parse_bindings_percent_decodes_captures,
            parse_bindings_decodes_encoded_slash,
            parse_bindings_keeps_malformed_raw,
            parse_bindings_empty_is_empty
        ]}
    ].

%% --------------------------------------------------------------------
%% parse_bindings/1 -- percent-decode captured path segments
%% --------------------------------------------------------------------

%% The router matches raw path bytes, so a `:param` capture arrives
%% percent-encoded. parse_bindings decodes each value so a handler's
%% `?get(~"name")` sees the real characters (matching how query params arrive).
parse_bindings_percent_decodes_captures(Config) when is_list(Config) ->
    Req = #{bindings => #{~"name" => ~"jos%C3%A9", ~"id" => ~"42"}},
    ?assertEqual(
        #{~"name" => <<"josé"/utf8>>, ~"id" => ~"42"},
        arizona_roadrunner_req:parse_bindings(Req)
    ).

%% An encoded slash within a segment decodes to a literal `/` in the value (the
%% router already split on raw `/`, so this is value data, not path structure) --
%% consistent with query-param decoding.
parse_bindings_decodes_encoded_slash(Config) when is_list(Config) ->
    Req = #{bindings => #{~"path" => ~"a%2Fb"}},
    ?assertEqual(#{~"path" => ~"a/b"}, arizona_roadrunner_req:parse_bindings(Req)).

%% A malformed encoding (bad %-escape or non-UTF-8 bytes) is left as the raw
%% segment rather than crashing the request on an attacker-controllable path.
parse_bindings_keeps_malformed_raw(Config) when is_list(Config) ->
    Req = #{bindings => #{~"bad_escape" => ~"a%ZZ", ~"bad_utf8" => ~"b%C3"}},
    ?assertEqual(
        #{~"bad_escape" => ~"a%ZZ", ~"bad_utf8" => ~"b%C3"},
        arizona_roadrunner_req:parse_bindings(Req)
    ).

parse_bindings_empty_is_empty(Config) when is_list(Config) ->
    ?assertEqual(#{}, arizona_roadrunner_req:parse_bindings(#{bindings => #{}})),
    ?assertEqual(#{}, arizona_roadrunner_req:parse_bindings(#{})).

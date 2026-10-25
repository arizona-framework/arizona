-module(arizona_cookie_SUITE).
-include_lib("stdlib/include/assert.hrl").

-export([all/0]).

-export([serializes_name_value_and_attributes_in_order/1]).
-export([false_flags_and_absent_opts_add_nothing/1]).
-export([rejects_what_would_break_the_header/1]).
-export([matches_roadrunner_byte_for_byte/1]).

all() ->
    [
        serializes_name_value_and_attributes_in_order,
        false_flags_and_absent_opts_add_nothing,
        rejects_what_would_break_the_header,
        matches_roadrunner_byte_for_byte
    ].

serializes_name_value_and_attributes_in_order(Config) when is_list(Config) ->
    ?assertEqual(
        <<
            "sid=abc; Domain=example.com; Path=/; Max-Age=60; "
            "Expires=Wed, 21 Oct 2026 07:28:00 GMT; Secure; HttpOnly; SameSite=Lax"
        >>,
        arizona_cookie:serialize(~"sid", ~"abc", #{
            same_site => lax,
            http_only => true,
            secure => true,
            expires => ~"Wed, 21 Oct 2026 07:28:00 GMT",
            max_age => 60,
            path => ~"/",
            domain => ~"example.com"
        })
    ).

false_flags_and_absent_opts_add_nothing(Config) when is_list(Config) ->
    ?assertEqual(~"a=", arizona_cookie:serialize(~"a", <<>>, #{})),
    ?assertEqual(
        ~"a=1", arizona_cookie:serialize(~"a", ~"1", #{secure => false, http_only => false})
    ).

rejects_what_would_break_the_header(Config) when is_list(Config) ->
    ?assertError({invalid_cookie_name, <<>>}, arizona_cookie:serialize(<<>>, ~"v", #{})),
    ?assertError({invalid_cookie_name, ~"a b"}, arizona_cookie:serialize(~"a b", ~"v", #{})),
    ?assertError({invalid_cookie_value, ~"v;x"}, arizona_cookie:serialize(~"a", ~"v;x", #{})),
    ?assertError(
        {invalid_cookie_attr, path, ~"/;Domain=evil"},
        arizona_cookie:serialize(~"a", ~"v", #{path => ~"/;Domain=evil"})
    ),
    ?assertError(
        {invalid_cookie_attr, domain, ~"a b"},
        arizona_cookie:serialize(~"a", ~"v", #{domain => ~"a b"})
    ),
    ?assertError(
        {invalid_cookie_attr, expires, ~"x\r\nSet-Cookie: y=1"},
        arizona_cookie:serialize(~"a", ~"v", #{expires => ~"x\r\nSet-Cookie: y=1"})
    ).

%% Moving serialization out of the roadrunner adapter must not change a byte:
%% `arizona_session` budgets `session_max_bytes` against this exact format, and
%% a validation that drifted would let an attribute-smuggling value through.
%% Every byte is tried in every position, plus random option maps.
matches_roadrunner_byte_for_byte(Config) when is_list(Config) ->
    Bytes = lists:seq(0, 255),
    Cases =
        [{<<"n", B>>, ~"v", #{}} || B <- Bytes] ++
            [{~"n", <<"v", B>>, #{}} || B <- Bytes] ++
            [{~"n", ~"v", #{domain => <<"d", B>>}} || B <- Bytes] ++
            [{~"n", ~"v", #{path => <<"/", B>>}} || B <- Bytes] ++
            [{~"n", ~"v", #{expires => <<"e", B>>}} || B <- Bytes] ++
            [{~"n", ~"v", random_opts(rand:seed_s(exsss, {I, I, I}))} || I <- lists:seq(1, 500)],
    [
        ?assertEqual(
            {Name, Value, Opts, roadrunner(Name, Value, Opts)},
            {Name, Value, Opts, arizona(Name, Value, Opts)}
        )
     || {Name, Value, Opts} <- Cases
    ],
    ok.

arizona(Name, Value, Opts) ->
    try
        {ok, arizona_cookie:serialize(Name, Value, Opts)}
    catch
        error:Reason -> {error, Reason}
    end.

roadrunner(Name, Value, Opts) ->
    try
        {ok, iolist_to_binary(roadrunner_cookie:serialize(Name, Value, Opts))}
    catch
        error:Reason -> {error, Reason}
    end.

random_opts(Seed) ->
    Choices = [
        {domain, [~"example.com", ~"a.b"]},
        {path, [~"/", ~"/x/y"]},
        {max_age, [0, 1, 604800]},
        {expires, [~"Wed, 21 Oct 2026 07:28:00 GMT"]},
        {secure, [true, false]},
        {http_only, [true, false]},
        {same_site, [strict, lax, none]}
    ],
    {Opts, _} = lists:foldl(
        fun({Key, Values}, {Acc, S0}) ->
            {Pick, S1} = rand:uniform_s(length(Values) + 1, S0),
            case Pick > length(Values) of
                true -> {Acc, S1};
                false -> {Acc#{Key => lists:nth(Pick, Values)}, S1}
            end
        end,
        {#{}, Seed},
        Choices
    ),
    Opts.

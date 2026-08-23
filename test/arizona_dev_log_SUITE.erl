-module(arizona_dev_log_SUITE).
-include_lib("stdlib/include/assert.hrl").

-export([all/0]).
-export([groups/0]).
-export([init_per_suite/1]).
-export([end_per_suite/1]).

-export([captures_log_events/1]).
-export([filters_by_level/1]).
-export([filters_by_grep_case_insensitively/1]).
-export([invalid_grep_reports_in_band/1]).
-export([count_takes_the_newest/1]).
-export([excludes_a_marked_process/1]).
-export([captures_a_process_the_marked_one_spawned/1]).
-export([install_is_idempotent/1]).
-export([since_returns_only_newer_entries/1]).
-export([cursor_holds_position_when_nothing_matched/1]).
-export([ring_keeps_the_newest_when_full/1]).

all() ->
    [{group, capture}, {group, bounds}].

groups() ->
    [
        %% Every case here tags its own entries with a unique token and reads
        %% them back by `grep`, so they share the one global buffer safely.
        {capture, [parallel], [
            captures_log_events,
            filters_by_level,
            filters_by_grep_case_insensitively,
            invalid_grep_reports_in_band,
            count_takes_the_newest,
            excludes_a_marked_process,
            captures_a_process_the_marked_one_spawned,
            install_is_idempotent,
            since_returns_only_newer_entries,
            cursor_holds_position_when_nothing_matched
        ]},
        %% Sequential and last: filling the ring evicts everything else, which
        %% would pull entries out from under a parallel case mid-assertion.
        {bounds, [], [
            ring_keeps_the_newest_when_full
        ]}
    ].

init_per_suite(Config) ->
    %% Start the app rather than creating the table here: `arizona_sup` has to
    %% own it, because a table owned by the transient init_per_suite process
    %% dies with it -- the same lifetime requirement the real wiring exists for.
    {ok, _Started} = application:ensure_all_started(arizona),
    ok = arizona_dev_log:install(),
    Config.

end_per_suite(Config) ->
    %% `arizona_app:prep_stop/1` removes the handler on the way down, so there
    %% is nothing to clean up here.
    ok = application:stop(arizona),
    Config.

%% --------------------------------------------------------------------
%% capture
%% --------------------------------------------------------------------

captures_log_events(Config) when is_list(Config) ->
    Token = token(),
    logger:error("captured ~ts", [Token]),
    ?assertMatch([_], entries(Token)).

filters_by_level(Config) when is_list(Config) ->
    Token = token(),
    logger:error("err ~ts", [Token]),
    logger:notice("note ~ts", [Token]),
    %% `level` keeps events at least as severe, so `error` drops the notice.
    {ok, Errors, _} = arizona_dev_log:tail(#{grep => Token, level => error, count => 100}),
    ?assertMatch([_], Errors),
    ?assertNotEqual(nomatch, binary:match(iolist_to_binary(Errors), ~"err ")),
    ?assertEqual(nomatch, binary:match(iolist_to_binary(Errors), ~"note ")),
    %% `notice` is less severe, so it keeps both.
    {ok, Both, _} = arizona_dev_log:tail(#{grep => Token, level => notice, count => 100}),
    ?assertMatch([_, _], Both).

filters_by_grep_case_insensitively(Config) when is_list(Config) ->
    Token = token(),
    logger:error("MiXeDcAsE ~ts", [Token]),
    {ok, Found, _} = arizona_dev_log:tail(#{grep => ~"mixedcase", count => 100}),
    ?assertNotEqual(nomatch, binary:match(iolist_to_binary(Found), Token)).

invalid_grep_reports_in_band(Config) when is_list(Config) ->
    %% A bad pattern is agent input, not a bug: it must come back as a message
    %% the tool can relay rather than crashing the dispatch.
    ?assertMatch(
        {error, Message} when is_binary(Message),
        arizona_dev_log:tail(#{grep => ~"[unclosed"})
    ).

count_takes_the_newest(Config) when is_list(Config) ->
    Token = token(),
    logger:error("first ~ts", [Token]),
    logger:error("second ~ts", [Token]),
    logger:error("third ~ts", [Token]),
    {ok, Newest, _} = arizona_dev_log:tail(#{grep => Token, count => 1}),
    ?assertMatch([_], Newest),
    ?assertNotEqual(nomatch, binary:match(iolist_to_binary(Newest), ~"third")).

excludes_a_marked_process(Config) when is_list(Config) ->
    Token = token(),
    ok = arizona_dev_log:mark_self(),
    logger:error("from the tool call ~ts", [Token]),
    %% This is what keeps an agent from reading its own `eval` output back as
    %% the app's behaviour.
    ?assertEqual([], entries(Token)).

captures_a_process_the_marked_one_spawned(Config) when is_list(Config) ->
    Token = token(),
    ok = arizona_dev_log:mark_self(),
    Self = self(),
    _Pid = spawn(fun() ->
        logger:error("from the spawned proc ~ts", [Token]),
        Self ! logged
    end),
    receive
        logged -> ok
    after 5000 -> ct:fail(spawned_process_never_logged)
    end,
    %% Metadata is per-process and not inherited, so a process the tool call
    %% spawns is still captured -- which is where a crash actually surfaces.
    ?assertMatch([_], entries(Token)).

install_is_idempotent(Config) when is_list(Config) ->
    %% Every MCP session calls it, so a second call must not fail.
    ?assertEqual(ok, arizona_dev_log:install()),
    ?assertEqual(ok, arizona_dev_log:install()).

since_returns_only_newer_entries(Config) when is_list(Config) ->
    Token = token(),
    Self = self(),
    Log = fun(Text) ->
        _Pid = spawn(fun() ->
            logger:error("~ts ~ts", [Text, Token]),
            Self ! logged
        end),
        receive
            logged -> ok
        after 5000 -> ct:fail(never_logged)
        end
    end,
    Log(~"before"),
    {ok, First, Cursor} = arizona_dev_log:tail(#{grep => Token, count => 100}),
    ?assertMatch([_], First),
    Log(~"after"),
    %% The whole point of the cursor: a second read returns only what is new,
    %% instead of the caller re-reading its own previous window every poll.
    {ok, Second, _} = arizona_dev_log:tail(#{grep => Token, count => 100, since => Cursor}),
    ?assertMatch([_], Second),
    ?assertNotEqual(nomatch, binary:match(iolist_to_binary(Second), ~"after")),
    ?assertEqual(nomatch, binary:match(iolist_to_binary(Second), ~"before")).

cursor_holds_position_when_nothing_matched(Config) when is_list(Config) ->
    %% A read past the end must carry the cursor forward, not reset it: returning
    %% 0 would make the caller's next poll replay the entire buffer.
    {ok, _Entries, Cursor} = arizona_dev_log:tail(#{count => 1}),
    {ok, [], Same} = arizona_dev_log:tail(#{count => 1, since => Cursor, grep => ~"zzz-nope"}),
    ?assert(Same >= Cursor).

%% --------------------------------------------------------------------
%% bounds
%% --------------------------------------------------------------------

ring_keeps_the_newest_when_full(Config) when is_list(Config) ->
    Token = token(),
    Size = 1024,
    Overflow = 100,
    Total = Size + Overflow,
    Self = self(),
    %% From an unmarked process: this suite's own process may have been marked
    %% by an earlier case, and a marked writer is dropped by design.
    _Pid = spawn(fun() ->
        lists:foreach(
            fun(N) -> logger:error("fill ~p ~ts", [N, Token]) end,
            lists:seq(1, Total)
        ),
        Self ! filled
    end),
    receive
        filled -> ok
    after 30000 -> ct:fail(fill_never_finished)
    end,
    Entries = entries(Token),
    ?assert(length(Entries) =< Size),
    Text = iolist_to_binary(Entries),
    %% The newest survived and the earliest were evicted: a bounded ring, not a
    %% buffer that stops accepting once full.
    ?assertNotEqual(nomatch, binary:match(Text, fill(Total, Token))),
    ?assertEqual(nomatch, binary:match(Text, fill(1, Token))).

%% --------------------------------------------------------------------
%% Helpers
%% --------------------------------------------------------------------

%% Cases share one global buffer, so each tags its entries and reads them back
%% by tag rather than assuming it starts empty. The trailing delimiter is load
%% bearing: `grep` is a regex, so a bare `tok38` also matches another case's
%% `tok388` and one case reads the other's entries.
token() ->
    iolist_to_binary(io_lib:format("tok-~p-end", [erlang:unique_integer([positive])])).

fill(N, Token) ->
    iolist_to_binary(io_lib:format("fill ~p ~ts", [N, Token])).

entries(Token) ->
    {ok, Entries, _Cursor} = arizona_dev_log:tail(#{grep => Token, count => 5000}),
    Entries.

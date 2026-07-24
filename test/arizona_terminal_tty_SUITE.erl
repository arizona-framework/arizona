-module(arizona_terminal_tty_SUITE).
-include_lib("stdlib/include/assert.hrl").

-export([all/0]).
-export([init_per_suite/1]).
-export([end_per_suite/1]).
-export([serve_reaps_view_and_reader_on_eof/1]).
-export([serve_quits_on_stop_key/1]).
-export([serve_loops_through_multiple_keys/1]).
-export([serve_handles_push_then_eof/1]).
-export([serve_quits_on_push_quit_effect/1]).
-export([serve_exits_on_linked_death/1]).
-export([serve_tolerates_dead_view/1]).
-export([serve_forwards_wide_codepoint_list/1]).

%% arizona_terminal_tty is a thin local-TTY transport over arizona_terminal_session:
%% start/3 enters raw mode (needs a real controlling TTY) and serve/2 runs the event
%% loop, reaping the session's live view and the input reader on *every* exit path.
%%
%% Only serve/2 is transport-testable without a TTY (the module exports it for exactly
%% that). These cases drive serve/2 directly with pre-seeded loop messages and assert
%% the loop's dispatch (key/push/EXIT), its quit-vs-continue verdicts, and the reaping
%% guarantee. The byte decoding the review flagged (split escapes, multibyte UTF-8, C0
%% control bytes) is not this module's job -- it forwards raw reads to the session's
%% driver -- and is covered by arizona_terminal_io_SUITE.

all() ->
    [
        serve_reaps_view_and_reader_on_eof,
        serve_quits_on_stop_key,
        serve_loops_through_multiple_keys,
        serve_handles_push_then_eof,
        serve_quits_on_push_quit_effect,
        serve_exits_on_linked_death,
        serve_tolerates_dead_view,
        serve_forwards_wide_codepoint_list
    ].

init_per_suite(Config) ->
    %% The demo view subscribes to a pubsub channel at mount, so the pg scope must be
    %% running (the arizona app is not started in this suite).
    case erlang:whereis(arizona_pubsub) of
        undefined ->
            {ok, Pid} = pg:start_link(arizona_pubsub),
            unlink(Pid);
        _ ->
            ok
    end,
    Config.

end_per_suite(Config) when is_list(Config) ->
    Config.

%% --------------------------------------------------------------------
%% serve/2 exit paths -- the loop must reap the live view and input reader.
%% --------------------------------------------------------------------

%% eof (stdin closed / a read error) ends the loop; serve/2 reaps both processes.
%% Otherwise the view outlives the transport (keeps ticking, stays subscribed) and the
%% reader stays blocked in io:get_chars, stealing subsequent stdin.
serve_reaps_view_and_reader_on_eof(Config) when is_list(Config) ->
    #{session := Session, view := ViewPid, reader := Reader, vmon := VMon, rmon := RMon} =
        serve_fixture(),
    self() ! {term_input, eof},
    ok = arizona_terminal_tty:serve(Session, Reader),
    ?assert(down_within(VMon, ViewPid)),
    ?assert(down_within(RMon, Reader)).

%% A stop-mapping key (Ctrl-D in the demo driver) makes the loop quit; serve/2 reaps on
%% that exit path too. Discriminates the key->stop->quit branch: an unmapped key would
%% continue the loop and never return here.
serve_quits_on_stop_key(Config) when is_list(Config) ->
    #{session := Session, view := ViewPid, reader := Reader, vmon := VMon, rmon := RMon} =
        serve_fixture(),
    self() ! {term_input, [4]},
    ok = arizona_terminal_tty:serve(Session, Reader),
    ?assert(down_within(VMon, ViewPid)),
    ?assert(down_within(RMon, Reader)).

%% Two successive key reads must both be applied: the loop has to recurse on the
%% continued session after the first key, not exit. Observed through a capturing Out --
%% the second frame reflects the selection advanced twice (index 0 -> 1 -> 2). If the
%% loop exited after the first key->cont, only one frame would be painted and the
%% second selection assertion would fail.
serve_loops_through_multiple_keys(Config) when is_list(Config) ->
    Self = self(),
    Out = fun(Io) ->
        Self ! {out, iolist_to_binary(Io)},
        ok
    end,
    {ok, Session} = arizona_terminal_session:start(
        arizona_term_demo, #{}, arizona_term_demo_driver, [], Out
    ),
    ViewPid = arizona_terminal_session:pid(Session),
    Reader = spawn(fun() -> timer:sleep(infinity) end),
    RMon = monitor(process, Reader),
    %% Drain the initial paint so only the two key frames remain to collect.
    _Initial = next_out(),
    self() ! {term_input, ~"j"},
    self() ! {term_input, ~"j"},
    self() ! {term_input, eof},
    ok = arizona_terminal_tty:serve(Session, Reader),
    [Frame1, Frame2] = collect_frames(),
    ?assert(contains(Frame1, ~"> Options")),
    ?assert(contains(Frame2, ~"> Send message")),
    ?assert(down_within(RMon, Reader)),
    ?assertNot(is_process_alive(ViewPid)).

%% A push (timer tick / broadcast) repaints and continues the loop; a following eof ends
%% it. Exercises the {arizona_push, _, _} continue branch.
serve_handles_push_then_eof(Config) when is_list(Config) ->
    #{session := Session, view := ViewPid, reader := Reader, vmon := VMon, rmon := RMon} =
        serve_fixture(),
    self() ! {arizona_push, [], [arizona_term_demo_effects:log(~"tick")]},
    self() ! {term_input, eof},
    ok = arizona_terminal_tty:serve(Session, Reader),
    ?assert(down_within(VMon, ViewPid)),
    ?assert(down_within(RMon, Reader)).

%% A push carrying a quit effect stops the loop just like a stop key: the driver's paint
%% returns `stop`, handle_push returns `quit`, and serve/2 reaps. If the quit verdict
%% were ignored the loop would block on the next receive and serve/2 would never return.
serve_quits_on_push_quit_effect(Config) when is_list(Config) ->
    #{session := Session, view := ViewPid, reader := Reader, vmon := VMon, rmon := RMon} =
        serve_fixture(),
    self() ! {arizona_push, [], [arizona_term_demo_effects:quit()]},
    ok = arizona_terminal_tty:serve(Session, Reader),
    ?assert(down_within(VMon, ViewPid)),
    ?assert(down_within(RMon, Reader)).

%% A linked process death arrives as an {'EXIT', _, _} message (the tty run/3 traps
%% exits); the loop returns and serve/2 reaps both processes. Here the live view is
%% still alive, so serve/2's teardown stops it the normal way.
serve_exits_on_linked_death(Config) when is_list(Config) ->
    #{session := Session, view := ViewPid, reader := Reader, vmon := VMon, rmon := RMon} =
        serve_fixture(),
    Dummy = spawn(fun() -> ok end),
    self() ! {'EXIT', Dummy, boom},
    ok = arizona_terminal_tty:serve(Session, Reader),
    ?assert(down_within(VMon, ViewPid)),
    ?assert(down_within(RMon, Reader)).

%% A mid-session view crash: the live view itself dies. Its {'EXIT', _, _} ends the
%% loop, then serve/2's teardown tries to stop an already-dead view -- stop_session/1
%% must swallow the exit:noproc gen_server:stop raises, so serve/2 still returns ok and
%% the reader is reaped. Removing that catch makes serve/2 crash with noproc here.
serve_tolerates_dead_view(Config) when is_list(Config) ->
    Prev = process_flag(trap_exit, true),
    try
        {ok, Session} = arizona_terminal_session:start(
            arizona_term_demo, #{}, arizona_term_demo_driver, [], fun(_Io) -> ok end
        ),
        ViewPid = arizona_terminal_session:pid(Session),
        Reader = spawn(fun() -> timer:sleep(infinity) end),
        RMon = monitor(process, Reader),
        %% Kill the linked view; trapping turns its death into the {'EXIT', ViewPid, _}
        %% the loop consumes, and guarantees it is dead before serve/2's stop_session.
        exit(ViewPid, kill),
        ok = arizona_terminal_tty:serve(Session, Reader),
        ?assertNot(is_process_alive(ViewPid)),
        ?assert(down_within(RMon, Reader))
    after
        process_flag(trap_exit, Prev)
    end.

%% --------------------------------------------------------------------
%% Read shape -- the reader forwards a codepoint list, not a byte binary.
%% --------------------------------------------------------------------

%% A local TTY in unicode mode delivers keys as a codepoint list; a non-Latin-1 key
%% ("€" is codepoint 8364) exceeds a byte, so iolist_to_binary would reject it with
%% badarg and take the loop (and its linked view) down. serve/2 must forward the list
%% intact to the session, which UTF-8 encodes it; the byte decoder then drops the
%% multibyte input, so the loop simply continues to the following eof.
serve_forwards_wide_codepoint_list(Config) when is_list(Config) ->
    #{session := Session, view := ViewPid, reader := Reader, vmon := VMon, rmon := RMon} =
        serve_fixture(),
    self() ! {term_input, [8364]},
    self() ! {term_input, eof},
    ok = arizona_terminal_tty:serve(Session, Reader),
    ?assert(down_within(VMon, ViewPid)),
    ?assert(down_within(RMon, Reader)).

%% --------------------------------------------------------------------
%% Helpers
%% --------------------------------------------------------------------

%% Start a demo session (no-op Out) plus a blocking stand-in reader, both monitored, for
%% driving arizona_terminal_tty:serve/2 to an exit path.
serve_fixture() ->
    {ok, Session} = arizona_terminal_session:start(
        arizona_term_demo, #{}, arizona_term_demo_driver, [], fun(_Io) -> ok end
    ),
    ViewPid = arizona_terminal_session:pid(Session),
    Reader = spawn(fun() -> timer:sleep(infinity) end),
    #{
        session => Session,
        view => ViewPid,
        reader => Reader,
        vmon => monitor(process, ViewPid),
        rmon => monitor(process, Reader)
    }.

%% Wait for a monitored process to go DOWN.
down_within(Mon, Pid) ->
    receive
        {'DOWN', Mon, process, Pid, _Reason} -> true
    after 2000 -> false
    end.

%% The next frame the capturing Out wrote.
next_out() ->
    receive
        {out, Bin} -> Bin
    after 2000 ->
        ct:fail(no_output)
    end.

%% Every frame the capturing Out has written so far, in order.
collect_frames() ->
    receive
        {out, Bin} -> [Bin | collect_frames()]
    after 0 -> []
    end.

contains(Frame, Sub) ->
    binary:match(Frame, Sub) =/= nomatch.

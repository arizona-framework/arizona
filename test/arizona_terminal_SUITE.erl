-module(arizona_terminal_SUITE).
-include_lib("stdlib/include/assert.hrl").

-export([all/0]).
-export([init_per_suite/1]).
-export([end_per_suite/1]).
-export([renders_status_block/1]).
-export([key_moves_selection/1]).
-export([key_changes_count/1]).
-export([tick_advances_clock/1]).
-export([broadcast_pushes_log_effect/1]).
-export([enter_quits_on_quit_item/1]).
-export([enter_logs_selection/1]).
-export([tty_log_effect/1]).
-export([tty_quit_effect/1]).
-export([count_lines_counts_rows/1]).
-export([log_lines_filters_effects/1]).
-export([has_quit_detects_quit/1]).
-export([normalize_key_mapping/1]).
-export([crlf_conversion/1]).
-export([session_drives_frames/1]).

%% Drives the ?terminal demo view through arizona_live with no transport and no
%% HTTP server -- the path the terminal runtime drives, minus the TTY.
%% render_current/1 returns the status block; broadcasts surface as
%% arizona_tty:log/1 effects.

all() ->
    [
        renders_status_block,
        key_moves_selection,
        key_changes_count,
        tick_advances_clock,
        broadcast_pushes_log_effect,
        enter_quits_on_quit_item,
        enter_logs_selection,
        tty_log_effect,
        tty_quit_effect,
        count_lines_counts_rows,
        log_lines_filters_effects,
        has_quit_detects_quit,
        normalize_key_mapping,
        crlf_conversion,
        session_drives_frames
    ].

init_per_suite(Config) ->
    %% The demo view subscribes to a pubsub channel at mount, so the pg scope
    %% must be running (the arizona app is not started in this suite).
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

renders_status_block(Config) when is_list(Config) ->
    {Pid, _ViewId} = start_demo(),
    Frame = frame(Pid),
    ?assert(contains(Frame, ~"== Arizona Terminal Demo ==")),
    %% all three menu rows render (via ?each), the first marked as selected
    ?assert(contains(Frame, ~"> New Game")),
    ?assert(contains(Frame, ~"  Options")),
    ?assert(contains(Frame, ~"  Quit")),
    ?assert(contains(Frame, ~"Count: 0")),
    ?assert(contains(Frame, ~"Server ticks: 0")),
    ?assert(contains(Frame, ~"[j/k] move")),
    %% the title carries a green SGR escape
    ?assert(contains(Frame, ~"\e[32m")).

key_moves_selection(Config) when is_list(Config) ->
    {Pid, ViewId} = start_demo(),
    ?assert(contains(frame(Pid), ~"> New Game")),
    {ok, _Ops, _Effects} = arizona_live:handle_event(Pid, ViewId, ~"key", #{~"key" => ~"j"}),
    Frame = frame(Pid),
    ?assert(contains(Frame, ~"> Options")),
    ?assert(contains(Frame, ~"  New Game")).

key_changes_count(Config) when is_list(Config) ->
    {Pid, ViewId} = start_demo(),
    {ok, _, _} = arizona_live:handle_event(Pid, ViewId, ~"key", #{~"key" => ~"+"}),
    {ok, _, _} = arizona_live:handle_event(Pid, ViewId, ~"key", #{~"key" => ~"+"}),
    ?assert(contains(frame(Pid), ~"Count: 2")).

tick_advances_clock(Config) when is_list(Config) ->
    {Pid, ViewId} = start_demo(),
    ?assert(contains(frame(Pid), ~"Server ticks: 0")),
    %% Stand in for the self-scheduled timer message reaching the live view.
    Pid ! {arizona_view, ViewId, tick},
    ?assert(contains(frame(Pid), ~"Server ticks: 1")).

broadcast_pushes_log_effect(Config) when is_list(Config) ->
    %% A pubsub broadcast reaches the subscribed view, which emits a log effect;
    %% the live process pushes it to the transport (here, the test process).
    {ok, Pid} = arizona_live:start_link(
        arizona_term_demo, #{}, self(), [], undefined
    ),
    {ok, _ViewId} = arizona_live:mount(Pid),
    ok = arizona_pubsub:broadcast(demo, {chat, ~"hello there"}),
    receive
        {arizona_push, _Ops, Effects} ->
            ?assertEqual([~"hello there"], arizona_terminal_session:log_lines(Effects))
    after 2000 ->
        ct:fail(no_push_received)
    end.

enter_quits_on_quit_item(Config) when is_list(Config) ->
    %% Move the selection to "Quit" (index 2) and press enter -> a quit effect.
    {Pid, ViewId} = start_demo(),
    {ok, _, _} = arizona_live:handle_event(Pid, ViewId, ~"key", #{~"key" => ~"j"}),
    {ok, _, _} = arizona_live:handle_event(Pid, ViewId, ~"key", #{~"key" => ~"j"}),
    {ok, _Ops, Effects} = arizona_live:handle_event(Pid, ViewId, ~"key", #{~"key" => ~"enter"}),
    ?assert(arizona_terminal_session:has_quit(Effects)).

enter_logs_selection(Config) when is_list(Config) ->
    %% Enter on a non-quit item ("New Game", index 0) logs the selection.
    {Pid, ViewId} = start_demo(),
    {ok, _Ops, Effects} = arizona_live:handle_event(Pid, ViewId, ~"key", #{~"key" => ~"enter"}),
    ?assertEqual([~"selected New Game"], arizona_terminal_session:log_lines(Effects)),
    ?assertNot(arizona_terminal_session:has_quit(Effects)).

tty_log_effect(Config) when is_list(Config) ->
    ?assertEqual({arizona_effect, [log, ~"hi"]}, arizona_tty:log(~"hi")),
    %% iodata is normalized to a binary
    ?assertEqual({arizona_effect, [log, ~"ab"]}, arizona_tty:log([~"a", ~"b"])).

tty_quit_effect(Config) when is_list(Config) ->
    ?assertEqual({arizona_effect, [quit]}, arizona_tty:quit()).

count_lines_counts_rows(Config) when is_list(Config) ->
    ?assertEqual(3, arizona_terminal_session:count_lines(~"a\nb\nc\n")),
    ?assertEqual(0, arizona_terminal_session:count_lines(~"no newline")).

log_lines_filters_effects(Config) when is_list(Config) ->
    Effects = [
        {arizona_effect, [log, ~"one"]},
        {arizona_effect, [9, ~"some-event"]},
        {arizona_effect, [log, ~"two"]}
    ],
    ?assertEqual([~"one", ~"two"], arizona_terminal_session:log_lines(Effects)).

has_quit_detects_quit(Config) when is_list(Config) ->
    ?assert(arizona_terminal_session:has_quit([arizona_tty:quit()])),
    ?assert(arizona_terminal_session:has_quit([arizona_tty:log(~"x"), arizona_tty:quit()])),
    ?assertNot(arizona_terminal_session:has_quit([arizona_tty:log(~"x")])),
    ?assertNot(arizona_terminal_session:has_quit([])).

normalize_key_mapping(Config) when is_list(Config) ->
    %% The driver turns raw key reads into quit / a ~"key" payload / ignore.
    ?assertEqual(quit, arizona_terminal_session:normalize_key("q")),
    ?assertEqual(quit, arizona_terminal_session:normalize_key([3])),
    ?assertEqual(quit, arizona_terminal_session:normalize_key([4])),
    ?assertEqual(~"up", arizona_terminal_session:normalize_key("\e[A")),
    ?assertEqual(~"down", arizona_terminal_session:normalize_key("\e[B")),
    ?assertEqual(~"enter", arizona_terminal_session:normalize_key("\r")),
    ?assertEqual(~"enter", arizona_terminal_session:normalize_key("\n")),
    ?assertEqual(~"j", arizona_terminal_session:normalize_key("j")),
    ?assertEqual(~"+", arizona_terminal_session:normalize_key("+")),
    %% an unrecognized escape sequence and a bare ESC are dropped
    ?assertEqual(ignore, arizona_terminal_session:normalize_key("\e[Z")),
    ?assertEqual(ignore, arizona_terminal_session:normalize_key([27])).

crlf_conversion(Config) when is_list(Config) ->
    %% Raw mode needs CRLF; the driver adds the \r to the target's logical \n.
    ?assertEqual(~"a\r\nb\r\n", arizona_terminal_session:to_crlf(~"a\nb\n")),
    ?assertEqual(~"no breaks", arizona_terminal_session:to_crlf(~"no breaks")).

session_drives_frames(Config) when is_list(Config) ->
    %% Drive the transport-agnostic session over a capturing Out fun: the same
    %% path arizona_terminal_app and arizona_ssh take, minus any real terminal.
    Self = self(),
    Out = fun(Io) ->
        Self ! {out, iolist_to_binary(Io)},
        ok
    end,
    {ok, Session} = arizona_terminal_session:start(arizona_term_demo, #{}, Out),
    %% start/3 paints the initial status block.
    Frame0 = next_out(),
    ?assert(contains(Frame0, ~"== Arizona Terminal Demo ==")),
    ?assert(contains(Frame0, ~"> New Game")),
    %% A key event moves the selection and repaints. Keys arrive as a char list
    %% from the local TTY or as a byte binary from a network transport (SSH);
    %% both reach the same normalization.
    {cont, Session1} = arizona_terminal_session:handle_key(Session, "j"),
    ?assert(contains(next_out(), ~"> Options")),
    {cont, Session2} = arizona_terminal_session:handle_key(Session1, ~"j"),
    ?assert(contains(next_out(), ~"> Quit")),
    %% A push carrying a log effect streams the line above the block and repaints.
    {cont, Session3} = arizona_terminal_session:handle_push(
        Session2, [arizona_tty:log(~"streamed")]
    ),
    ?assert(contains(next_out(), ~"streamed")),
    %% A quit effect (or the quit key) stops the session.
    ?assertEqual(quit, arizona_terminal_session:handle_push(Session3, [arizona_tty:quit()])),
    ?assertEqual(quit, arizona_terminal_session:handle_key(Session3, "q")).

%% --------------------------------------------------------------------
%% Helpers
%% --------------------------------------------------------------------

next_out() ->
    receive
        {out, Bin} -> Bin
    after 2000 ->
        ct:fail(no_output)
    end.

start_demo() ->
    {ok, Pid} = arizona_live:start_link(
        arizona_term_demo, #{}, undefined, [], undefined
    ),
    {ok, ViewId} = arizona_live:mount(Pid),
    {Pid, ViewId}.

frame(Pid) ->
    {ok, Frame} = arizona_live:render_current(Pid),
    Frame.

contains(Frame, Sub) ->
    binary:match(Frame, Sub) =/= nomatch.

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
-export([input_broadcasts_message/1]).
-export([client_count_reflects_subscribers/1]).
-export([tty_serve_reaps_view_and_reader/1]).
-export([render_attr_rejected/1]).
-export([bell_effect_emits_bel/1]).
-export([set_title_effect_bel_terminated/1]).

%% Drives the ?terminal demo view through arizona_live with no transport and no
%% HTTP server -- the path the terminal runtime drives, minus the TTY.
%% render_current/1 returns the status block; broadcasts surface as
%% arizona_term_demo_effects:log/1 effects.

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
        session_drives_frames,
        input_broadcasts_message,
        client_count_reflects_subscribers,
        tty_serve_reaps_view_and_reader,
        render_attr_rejected,
        bell_effect_emits_bel,
        set_title_effect_bel_terminated
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

%% The local TTY transport must reap its session's live view and input reader on
%% every loop exit -- otherwise the view outlives the transport (keeps ticking,
%% stays subscribed to pubsub) and the reader stays blocked in io:get_chars,
%% stealing subsequent stdin. Drive serve/2 (the loop + teardown) directly with a
%% pre-loaded eof so it exits at once, then assert both processes are reaped.
tty_serve_reaps_view_and_reader(Config) when is_list(Config) ->
    {ok, Session} = arizona_terminal_session:start(
        arizona_term_demo, #{}, arizona_term_demo_driver, [], fun(_Io) -> ok end
    ),
    ViewPid = arizona_terminal_session:pid(Session),
    %% A stand-in for the input reader: a process that just blocks, so the test
    %% can assert serve/2 kills it.
    Reader = spawn(fun() -> timer:sleep(infinity) end),
    VMon = monitor(process, ViewPid),
    RMon = monitor(process, Reader),
    self() ! {term_input, eof},
    ok = arizona_terminal_tty:serve(Session, Reader),
    ?assert(down_within(VMon, ViewPid)),
    ?assert(down_within(RMon, Reader)).

down_within(Mon, Pid) ->
    receive
        {'DOWN', Mon, process, Pid, _Reason} -> true
    after 2000 -> false
    end.

renders_status_block(Config) when is_list(Config) ->
    {Pid, _ViewId} = start_demo(),
    Frame = frame(Pid),
    ?assert(contains(Frame, ~"== Arizona Terminal Demo ==")),
    %% all four menu rows render (via ?each), the first marked as selected
    ?assert(contains(Frame, ~"> New Game")),
    ?assert(contains(Frame, ~"  Options")),
    ?assert(contains(Frame, ~"  Send message")),
    ?assert(contains(Frame, ~"  Quit")),
    ?assert(contains(Frame, ~"Count: 0")),
    ?assert(contains(Frame, ~"Server ticks: 0")),
    ?assert(contains(Frame, ~"Clients: ")),
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
        arizona_term_demo, #{}, self(), []
    ),
    {ok, _ViewId} = arizona_live:mount(Pid),
    ok = arizona_pubsub:broadcast(demo, {chat, ~"hello there"}),
    receive
        {arizona_push, _Ops, Effects} ->
            ?assertEqual([~"hello there"], arizona_term_demo_driver:log_lines(Effects))
    after 2000 ->
        ct:fail(no_push_received)
    end.

enter_quits_on_quit_item(Config) when is_list(Config) ->
    %% Move the selection to "Quit" (index 3) and press enter -> a quit effect.
    {Pid, ViewId} = start_demo(),
    {ok, _, _} = arizona_live:handle_event(Pid, ViewId, ~"key", #{~"key" => ~"j"}),
    {ok, _, _} = arizona_live:handle_event(Pid, ViewId, ~"key", #{~"key" => ~"j"}),
    {ok, _, _} = arizona_live:handle_event(Pid, ViewId, ~"key", #{~"key" => ~"j"}),
    {ok, _Ops, Effects} = arizona_live:handle_event(Pid, ViewId, ~"key", #{~"key" => ~"enter"}),
    ?assert(arizona_term_demo_driver:has_quit(Effects)).

enter_logs_selection(Config) when is_list(Config) ->
    %% Enter on a non-quit item ("New Game", index 0) logs the selection.
    {Pid, ViewId} = start_demo(),
    {ok, _Ops, Effects} = arizona_live:handle_event(Pid, ViewId, ~"key", #{~"key" => ~"enter"}),
    ?assertEqual([~"selected New Game"], arizona_term_demo_driver:log_lines(Effects)),
    ?assertNot(arizona_term_demo_driver:has_quit(Effects)).

tty_log_effect(Config) when is_list(Config) ->
    ?assertEqual({arizona_effect, [log, ~"hi"]}, arizona_term_demo_effects:log(~"hi")),
    %% iodata is normalized to a binary
    ?assertEqual({arizona_effect, [log, ~"ab"]}, arizona_term_demo_effects:log([~"a", ~"b"])).

tty_quit_effect(Config) when is_list(Config) ->
    ?assertEqual({arizona_effect, [quit]}, arizona_term_demo_effects:quit()).

count_lines_counts_rows(Config) when is_list(Config) ->
    ?assertEqual(3, arizona_term_demo_driver:count_lines(~"a\nb\nc\n")),
    ?assertEqual(0, arizona_term_demo_driver:count_lines(~"no newline")).

%% The terminal has no dynamic attributes (attr_dyn_name/1 rejects them at compile
%% time), so the render_attr/2 backend callback is unreachable at runtime and
%% rejects like its sibling -- asserting the invariant rather than a real path.
render_attr_rejected(Config) when is_list(Config) ->
    ?assertError(
        {arizona_render_reject, _},
        arizona_terminal:render_attr(~"data-x", ~"v")
    ).

%% Erlang has no `\a` escape (unlike C), so BEL must be written as control-G.
%% The bell effect must emit the actual BEL byte (0x07), never the literal `a`
%% that `~"\a"` produced.
bell_effect_emits_bel(Config) when is_list(Config) ->
    {Output, continue, _State} = arizona_terminal_default_driver:paint(
        ~"", [arizona_terminal_effect:bell()], #{}
    ),
    Bin = iolist_to_binary(Output),
    ?assertNotEqual(nomatch, binary:match(Bin, <<7>>)),
    ?assertEqual(nomatch, binary:match(Bin, ~"a")).

%% The set_title OSC string must be terminated by BEL (0x07), not the literal `a`.
%% Asserts the exact `ESC ]0;<title> BEL` sequence.
set_title_effect_bel_terminated(Config) when is_list(Config) ->
    {Output, continue, _State} = arizona_terminal_default_driver:paint(
        ~"", [arizona_terminal_effect:set_title(~"Doc")], #{}
    ),
    Bin = iolist_to_binary(Output),
    ?assertNotEqual(nomatch, binary:match(Bin, <<"\e]0;Doc", 7>>)).

log_lines_filters_effects(Config) when is_list(Config) ->
    Effects = [
        {arizona_effect, [log, ~"one"]},
        {arizona_effect, [9, ~"some-event"]},
        {arizona_effect, [log, ~"two"]}
    ],
    ?assertEqual([~"one", ~"two"], arizona_term_demo_driver:log_lines(Effects)).

has_quit_detects_quit(Config) when is_list(Config) ->
    ?assert(arizona_term_demo_driver:has_quit([arizona_term_demo_effects:quit()])),
    ?assert(
        arizona_term_demo_driver:has_quit([
            arizona_term_demo_effects:log(~"x"), arizona_term_demo_effects:quit()
        ])
    ),
    ?assertNot(arizona_term_demo_driver:has_quit([arizona_term_demo_effects:log(~"x")])),
    ?assertNot(arizona_term_demo_driver:has_quit([])).

normalize_key_mapping(Config) when is_list(Config) ->
    %% The demo driver maps raw key reads to commands: [stop] for the hard
    %% interrupts Ctrl-C/Ctrl-D, a ~"key" event for the rest, [] for ignored.
    %% `q` is an ordinary character (the view decides whether it quits).
    ?assertEqual([key(~"q")], cmds(~"q")),
    ?assertEqual([stop], cmds(<<3>>)),
    ?assertEqual([stop], cmds(<<4>>)),
    ?assertEqual([key(~"up")], cmds(~"\e[A")),
    ?assertEqual([key(~"down")], cmds(~"\e[B")),
    ?assertEqual([key(~"enter")], cmds(~"\r")),
    ?assertEqual([key(~"enter")], cmds(~"\n")),
    %% backspace (DEL and BS) and bare ESC reach the view for text editing
    ?assertEqual([key(~"backspace")], cmds(<<127>>)),
    ?assertEqual([key(~"backspace")], cmds(<<8>>)),
    ?assertEqual([key(~"esc")], cmds(<<27>>)),
    ?assertEqual([key(~"j")], cmds(~"j")),
    ?assertEqual([key(~"+")], cmds(~"+")),
    %% an unrecognized escape sequence is dropped
    ?assertEqual([], cmds(~"\e[Z")).

crlf_conversion(Config) when is_list(Config) ->
    %% Raw mode needs CRLF; the driver adds the \r to the target's logical \n.
    ?assertEqual(~"a\r\nb\r\n", arizona_term_demo_driver:to_crlf(~"a\nb\n")),
    ?assertEqual(~"no breaks", arizona_term_demo_driver:to_crlf(~"no breaks")).

session_drives_frames(Config) when is_list(Config) ->
    %% Drive the transport-agnostic session over a capturing Out fun: the same
    %% path arizona_terminal_tty and arizona_terminal_ssh take, minus any real terminal.
    Self = self(),
    Out = fun(Io) ->
        Self ! {out, iolist_to_binary(Io)},
        ok
    end,
    {ok, Session} = arizona_terminal_session:start(
        arizona_term_demo, #{}, arizona_term_demo_driver, [], Out
    ),
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
    ?assert(contains(next_out(), ~"> Send message")),
    %% A push carrying a log effect streams the line above the block and repaints.
    {cont, Session3} = arizona_terminal_session:handle_push(
        Session2, [arizona_term_demo_effects:log(~"streamed")]
    ),
    ?assert(contains(next_out(), ~"streamed")),
    %% A quit effect (or the quit key) stops the session.
    ?assertEqual(
        quit, arizona_terminal_session:handle_push(Session3, [arizona_term_demo_effects:quit()])
    ),
    ?assertEqual(quit, arizona_terminal_session:handle_key(Session3, "q")).

input_broadcasts_message(Config) when is_list(Config) ->
    %% Subscribe a second pid (this process) as a stand-in for another terminal.
    ok = arizona_pubsub:subscribe(demo, self()),
    Self = self(),
    Out = fun(Io) ->
        Self ! {out, iolist_to_binary(Io)},
        ok
    end,
    {ok, S0} = arizona_terminal_session:start(
        arizona_term_demo, #{}, arizona_term_demo_driver, [], Out
    ),
    _ = next_out(),
    %% Navigate to "Send message" (index 2) and open the input.
    {cont, S1} = arizona_terminal_session:handle_key(S0, ~"j"),
    _ = next_out(),
    {cont, S2} = arizona_terminal_session:handle_key(S1, ~"j"),
    _ = next_out(),
    {cont, S3} = arizona_terminal_session:handle_key(S2, ~"\r"),
    InputFrame = next_out(),
    ?assert(contains(InputFrame, ~"Message: ")),
    ?assert(contains(InputFrame, ~"[enter] send")),
    %% Type "hi" -- the input line reflects the draft.
    {cont, S4} = arizona_terminal_session:handle_key(S3, ~"h"),
    _ = next_out(),
    {cont, S5} = arizona_terminal_session:handle_key(S4, ~"i"),
    ?assert(contains(next_out(), ~"Message: hi")),
    %% Enter broadcasts to every subscriber on the demo channel.
    {cont, _S6} = arizona_terminal_session:handle_key(S5, ~"\r"),
    receive
        {chat, Msg} -> ?assertEqual(~"hi", Msg)
    after 2000 ->
        ct:fail(no_broadcast)
    end.

client_count_reflects_subscribers(Config) when is_list(Config) ->
    %% Relative assertions -- the demo channel is shared across the suite, so
    %% don't assume an absolute base count. The view monitors demo membership, so
    %% join/leave drive the count (the notification is async -> poll with wait_until).
    {Pid, _ViewId} = start_demo(),
    Before = client_count(frame(Pid)),
    %% A new subscriber bumps the count.
    ok = arizona_pubsub:subscribe(demo, self()),
    ok = wait_until(fun() -> client_count(frame(Pid)) =:= Before + 1 end),
    %% Leaving drops it again.
    ok = arizona_pubsub:unsubscribe(demo, self()),
    ok = wait_until(fun() -> client_count(frame(Pid)) =:= Before end).

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
        arizona_term_demo, #{}, undefined, []
    ),
    {ok, ViewId} = arizona_live:mount(Pid),
    {Pid, ViewId}.

frame(Pid) ->
    {ok, Frame} = arizona_live:render_current(Pid),
    Frame.

contains(Frame, Sub) ->
    binary:match(Frame, Sub) =/= nomatch.

%% Commands the demo driver maps a raw key read to.
cmds(Bytes) ->
    {Commands, _State} = arizona_term_demo_driver:keys(Bytes, #{}),
    Commands.

%% The command for a ~"key" event with payload key Value.
key(Value) ->
    {event, ~"key", #{~"key" => Value}}.

%% Parse the integer after "Clients: " in a rendered frame (the count line ends
%% the number with the line's SGR reset, so split on the leading ESC).
client_count(Frame) ->
    [_, Rest] = binary:split(Frame, ~"Clients: "),
    [Digits, _] = binary:split(Rest, ~"\e"),
    binary_to_integer(Digits).

%% Poll Pred until it returns true or a 3s deadline passes.
wait_until(Pred) ->
    wait_until(Pred, erlang:monotonic_time(millisecond) + 3000).

wait_until(Pred, Deadline) ->
    case Pred() of
        true ->
            ok;
        false ->
            case erlang:monotonic_time(millisecond) < Deadline of
                true ->
                    timer:sleep(50),
                    wait_until(Pred, Deadline);
                false ->
                    ct:fail(wait_until_timeout)
            end
    end.

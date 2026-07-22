-module(arizona_terminal_ssh_SUITE).
-include_lib("stdlib/include/assert.hrl").
-include_lib("common_test/include/ct.hrl").

-export([all/0]).
-export([init_per_suite/1]).
-export([end_per_suite/1]).
-export([serves_initial_frame/1]).
-export([key_moves_selection/1]).
-export([window_change_resizes/1]).
-export([restores_cursor_on_quit/1]).
-export([chat_broadcasts_to_all_terminals/1]).
-export([disconnect_removes_subscriber/1]).
-export([live_view_crash_tears_down/1]).

%% End-to-end: arizona_terminal_ssh serves the ?terminal demo view over a real SSH daemon,
%% driven by the OTP ssh client. A throwaway host key + accept-all password auth
%% make the daemon self-contained; the demo view is the same one the local TTY
%% runtime uses, proving the render target + live core are transport-agnostic.

-define(USER, "arizona").
-define(PASSWORD, "secret").

all() ->
    [
        serves_initial_frame,
        key_moves_selection,
        window_change_resizes,
        restores_cursor_on_quit,
        chat_broadcasts_to_all_terminals,
        disconnect_removes_subscriber,
        live_view_crash_tears_down
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
    {ok, _Started} = application:ensure_all_started(ssh),
    SystemDir = filename:join(proplists:get_value(priv_dir, Config), "host_keys"),
    ok = filelib:ensure_path(SystemDir),
    ok = write_host_key(SystemDir),
    {ok, Daemon} = arizona_terminal_ssh:start(#{
        port => 0,
        handler => arizona_term_demo,
        driver => arizona_term_demo_driver,
        system_dir => SystemDir,
        daemon_opts => [
            {auth_methods, "password"},
            {user_passwords, [{?USER, ?PASSWORD}]}
        ]
    }),
    {ok, Info} = ssh:daemon_info(Daemon),
    Port = proplists:get_value(port, Info),
    [{daemon, Daemon}, {port, Port} | Config].

end_per_suite(Config) ->
    ok = ssh:stop_daemon(proplists:get_value(daemon, Config)),
    Config.

serves_initial_frame(Config) when is_list(Config) ->
    {Conn, _Ch} = connect(?config(port, Config)),
    Frame = recv_until(Conn, ~"== Arizona Terminal Demo ==", 3000),
    ?assert(contains(Frame, ~"> New Game")),
    ?assert(contains(Frame, ~"  Quit")),
    ?assert(contains(Frame, ~"[j/k] move")),
    %% The cursor is hidden for the duration of the session (no visible block).
    ?assert(contains(Frame, ~"\e[?25l")),
    ok = ssh:close(Conn).

restores_cursor_on_quit(Config) when is_list(Config) ->
    %% Quitting must restore the cursor hidden at session start, otherwise the
    %% client's terminal is left with an invisible cursor after the session ends.
    {Conn, Ch} = connect(?config(port, Config)),
    _ = recv_until(Conn, ~"== Arizona Terminal Demo ==", 3000),
    ok = ssh_connection:send(Conn, Ch, ~"q"),
    _ = recv_until(Conn, iolist_to_binary(io_ansi:cursor_show()), 3000),
    ok = ssh:close(Conn).

key_moves_selection(Config) when is_list(Config) ->
    {Conn, Ch} = connect(?config(port, Config)),
    _ = recv_until(Conn, ~"> New Game", 3000),
    ok = ssh_connection:send(Conn, Ch, ~"j"),
    Frame = recv_until(Conn, ~"> Options", 3000),
    ?assert(contains(Frame, ~"  New Game")),
    ok = ssh:close(Conn).

window_change_resizes(Config) when is_list(Config) ->
    {Conn, Ch} = connect(?config(port, Config)),
    %% The pty-req allocated 80x24; the view reflects it.
    _ = recv_until(Conn, ~"Terminal: 80x24", 3000),
    %% A window resize reaches the view's handle_info and updates the size.
    ok = ssh_connection:window_change(Conn, Ch, 100, 40),
    _ = recv_until(Conn, ~"Terminal: 100x40", 3000),
    ok = ssh:close(Conn).

chat_broadcasts_to_all_terminals(Config) when is_list(Config) ->
    %% A message typed in one terminal appears in every connected terminal's log.
    Port = ?config(port, Config),
    {ConnA, ChA} = connect(Port),
    {ConnB, _ChB} = connect(Port),
    _ = recv_until(ConnA, ~"== Arizona Terminal Demo ==", 3000),
    _ = recv_until(ConnB, ~"== Arizona Terminal Demo ==", 3000),
    %% A: navigate to "Send message" (index 2), open the input, type, send.
    ok = ssh_connection:send(ConnA, ChA, ~"j"),
    ok = ssh_connection:send(ConnA, ChA, ~"j"),
    ok = ssh_connection:send(ConnA, ChA, ~"\r"),
    %% The input line renders (exercises the conditional ?each over SSH).
    _ = recv_until(ConnA, ~"Message: ", 3000),
    %% One byte per send so each keystroke is its own key event.
    lists:foreach(fun(Char) -> ok = ssh_connection:send(ConnA, ChA, <<Char>>) end, "hello"),
    ok = ssh_connection:send(ConnA, ChA, ~"\r"),
    %% Both terminals' scrolling logs show the broadcast.
    _ = recv_until(ConnA, ~"hello", 3000),
    _ = recv_until(ConnB, ~"hello", 3000),
    ok = ssh:close(ConnA),
    ok = ssh:close(ConnB).

disconnect_removes_subscriber(Config) when is_list(Config) ->
    %% A connected terminal subscribes to demo; closing it must drop the
    %% subscription, otherwise the connected-client count never decreases. The
    %% live view is linked to its channel, but a normal channel stop doesn't kill
    %% it via the link -- the channel stops it explicitly in terminate/2.
    Port = ?config(port, Config),
    Before = length(arizona_pubsub:subscribers(demo)),
    {Conn, _Ch} = connect(Port),
    ok = wait_until(fun() -> length(arizona_pubsub:subscribers(demo)) =:= Before + 1 end),
    ok = ssh:close(Conn),
    ok = wait_until(fun() -> length(arizona_pubsub:subscribers(demo)) =:= Before end).

live_view_crash_tears_down(Config) when is_list(Config) ->
    %% An abnormal live-view crash must not leave the client with a frozen screen.
    %% ssh channels trap exits, so the linked live view's death arrives as an
    %% {'EXIT', _, _} message the channel must turn into a teardown + stop, rather
    %% than silently swallowing it (the pre-fix behaviour).
    Port = ?config(port, Config),
    Before = arizona_pubsub:subscribers(demo),
    {Conn, _Ch} = connect(Port),
    _ = recv_until(Conn, ~"== Arizona Terminal Demo ==", 3000),
    %% The live view backing this channel is the new demo subscriber.
    LivePid = wait_new_subscriber(Before),
    exit(LivePid, kill),
    %% The client receives the cursor-restore teardown (hidden at session start).
    _ = recv_until(Conn, iolist_to_binary(io_ansi:cursor_show()), 3000),
    ok = ssh:close(Conn).

%% --------------------------------------------------------------------
%% Helpers
%% --------------------------------------------------------------------

%% Wait until exactly one subscriber has been added to the demo channel since
%% `Before`, returning its pid (the live view the just-connected channel mounted).
wait_new_subscriber(Before) ->
    wait_new_subscriber(Before, erlang:monotonic_time(millisecond) + 3000).

wait_new_subscriber(Before, Deadline) ->
    case arizona_pubsub:subscribers(demo) -- Before of
        [Pid] ->
            Pid;
        _ ->
            case erlang:monotonic_time(millisecond) < Deadline of
                true ->
                    timer:sleep(50),
                    wait_new_subscriber(Before, Deadline);
                false ->
                    ct:fail(no_new_subscriber)
            end
    end.

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

%% A throwaway RSA host key so the daemon has a system_dir key without shelling
%% out to ssh-keygen.
write_host_key(Dir) ->
    Key = public_key:generate_key({rsa, 2048, 65537}),
    Pem = public_key:pem_encode([public_key:pem_entry_encode('RSAPrivateKey', Key)]),
    file:write_file(filename:join(Dir, "ssh_host_rsa_key"), Pem).

%% Connect as the OTP ssh client, allocate an 80x24 pty, and request a shell --
%% the same sequence `ssh -t` performs.
connect(Port) ->
    {ok, Conn} = ssh:connect("localhost", Port, [
        {user, ?USER},
        {password, ?PASSWORD},
        {silently_accept_hosts, true},
        {user_interaction, false}
    ]),
    {ok, Ch} = ssh_connection:session_channel(Conn, 5000),
    success = ssh_connection:ptty_alloc(Conn, Ch, [
        {term, "xterm"}, {width, 80}, {height, 24}
    ]),
    ok = ssh_connection:shell(Conn, Ch),
    {Conn, Ch}.

%% Accumulate channel data until `Pattern` appears or the deadline passes. The
%% demo ticks every second, so frames keep arriving -- match on content, not idle.
recv_until(Conn, Pattern, Timeout) ->
    Deadline = erlang:monotonic_time(millisecond) + Timeout,
    recv_until(Conn, Pattern, Deadline, <<>>).

recv_until(Conn, Pattern, Deadline, Acc) ->
    case binary:match(Acc, Pattern) of
        nomatch ->
            Remaining = Deadline - erlang:monotonic_time(millisecond),
            case Remaining =< 0 of
                true ->
                    ct:fail({not_received, Pattern, Acc});
                false ->
                    receive
                        {ssh_cm, Conn, {data, _Ch, 0, Bin}} ->
                            recv_until(Conn, Pattern, Deadline, <<Acc/binary, Bin/binary>>);
                        {ssh_cm, Conn, _Other} ->
                            recv_until(Conn, Pattern, Deadline, Acc)
                    after Remaining ->
                        ct:fail({not_received, Pattern, Acc})
                    end
            end;
        _Found ->
            Acc
    end.

contains(Frame, Sub) ->
    binary:match(Frame, Sub) =/= nomatch.

#!/usr/bin/env escript
%%% Interactive terminal demo for Arizona -- a live view rendered in an ANSI
%%% terminal with NO HTTP server. Run via `make term-demo` (needs a real TTY).
%%%
%%% Boots the arizona application WITHOUT the `server` env, so only the
%%% supervisor and pubsub start -- no roadrunner listener, no open port. Three
%%% real-time paths are on screen at once:
%%%   * keys (j/k move, +/- count) -> event-driven repaints;
%%%   * the "server ticks" counter -> timer-driven repaints (?send_after);
%%%   * a background emitter broadcasting to the `demo` channel every few
%%%     seconds -> pubsub-driven repaints.
%%% Press q (or Ctrl-C) to quit.

-mode(compile).

main(_Args) ->
    ProjectDir = project_dir(),
    ok = setup_code_paths(ProjectDir),
    {ok, _Started} = application:ensure_all_started(arizona),
    _Emitter = spawn(fun() -> emit_loop(1) end),
    ok = arizona_terminal_tty:start(arizona_term_demo, #{}, arizona_term_demo_driver).

emit_loop(N) ->
    timer:sleep(3000),
    Msg = iolist_to_binary(io_lib:format("server broadcast #~b", [N])),
    ok = arizona_pubsub:broadcast(demo, {chat, Msg}),
    emit_loop(N + 1).

setup_code_paths(BaseDir) ->
    %% Prefer the test profile's lib dir so test/support/ modules (the demo view
    %% arizona_term_demo) are available.
    Candidates = [
        filename:join([BaseDir, "_build", "test", "lib"]),
        filename:join([BaseDir, "_build", "default", "lib"])
    ],
    LibDir =
        case lists:filter(fun filelib:is_dir/1, Candidates) of
            [Found | _] ->
                Found;
            [] ->
                io:format("error: no compiled libs found; run 'rebar3 as test compile' first~n"),
                halt(1)
        end,
    {ok, Libs} = file:list_dir(LibDir),
    lists:foreach(
        fun(Lib) ->
            EbinDir = filename:join([LibDir, Lib, "ebin"]),
            case filelib:is_dir(EbinDir) of
                true -> code:add_pathz(EbinDir);
                false -> ok
            end,
            TestDir = filename:join([LibDir, Lib, "test"]),
            case filelib:is_dir(TestDir) of
                true -> code:add_pathz(TestDir);
                false -> ok
            end
        end,
        Libs
    ),
    ok.

project_dir() ->
    filename:dirname(filename:absname(filename:dirname(escript:script_name()))).

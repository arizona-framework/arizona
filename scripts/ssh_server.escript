#!/usr/bin/env escript
%%% SSH terminal demo for Arizona -- the same live ?terminal view as the local
%%% `term-demo`, but served over SSH to a remote terminal client (browser+server
%%% model for terminals). Run via `make ssh-server`; connect from another shell
%%% with `make ssh-client`.
%%%
%%% Boots the arizona application WITHOUT the `server` env (no roadrunner HTTP
%%% listener) and starts an SSH daemon whose interactive shell is the live view.
%%% The SSH protocol carries keystrokes and the terminal size (pty-req +
%%% window-change), so the same three real-time paths as term-demo are on screen
%%% -- keys, server ticks, pubsub broadcasts -- plus live window resizing.
%%%
%%% Host key + auth are throwaway dev values (a generated RSA host key, and a
%%% pwdfun that accepts any password); production supplies real ones via
%%% arizona_ssh:start/1.

-mode(compile).

-define(PORT, 2222).
-define(USER, "arizona").

main(_Args) ->
    ProjectDir = project_dir(),
    ok = setup_code_paths(ProjectDir),
    {ok, _Started} = application:ensure_all_started(arizona),
    SystemDir = ensure_host_key(ProjectDir),
    {ok, _Daemon} = arizona_ssh:start(#{
        port => ?PORT,
        handler => arizona_term_demo,
        system_dir => SystemDir,
        daemon_opts => [
            {auth_methods, "password"},
            %% Dev demo: accept any password so `make ssh-client` just works.
            {pwdfun, fun(_User, _Password) -> true end}
        ]
    }),
    _Emitter = spawn(fun() -> emit_loop(1) end),
    io:format(
        "Arizona SSH terminal demo listening on port ~b.~n"
        "Connect from another shell:  make ssh-client~n"
        "  (or: ssh -p ~b ~s@localhost)~n"
        "At the password prompt just press Enter -- any password is accepted.~n"
        "Press Ctrl-C twice here to stop the server.~n",
        [?PORT, ?PORT, ?USER]
    ),
    %% Block forever so the daemon keeps serving until the escript is killed.
    receive
        stop -> ok
    end.

emit_loop(N) ->
    timer:sleep(3000),
    Msg = iolist_to_binary(io_lib:format("server broadcast #~b", [N])),
    ok = arizona_pubsub:broadcast(demo, {chat, Msg}),
    emit_loop(N + 1).

%% A throwaway RSA host key under _build, regenerated only if missing. Clients
%% connect with StrictHostKeyChecking=no, so a changing key is fine.
ensure_host_key(ProjectDir) ->
    Dir = filename:join([ProjectDir, "_build", "ssh_host_keys"]),
    ok = filelib:ensure_path(Dir),
    KeyFile = filename:join(Dir, "ssh_host_rsa_key"),
    case filelib:is_regular(KeyFile) of
        true ->
            ok;
        false ->
            Key = public_key:generate_key({rsa, 2048, 65537}),
            Pem = public_key:pem_encode([public_key:pem_entry_encode('RSAPrivateKey', Key)]),
            ok = file:write_file(KeyFile, Pem)
    end,
    Dir.

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

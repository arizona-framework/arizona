-module(arizona_fake_watcher_bare).
-moduledoc """
An `arizona_watcher` without the optional `terminate/1`, so the rule suite can
check that an orderly stop does not require it. Its `init/2` reports its
arguments to the process registered as `arizona_fake_watcher_bare_probe`, when
there is one, since a bare module gets no opts to carry a pid in.
""".
-behaviour(arizona_watcher).

-export([init/2]).
-export([handle_info/2]).

init(Dir, Opts) ->
    ok =
        case erlang:whereis(arizona_fake_watcher_bare_probe) of
            undefined ->
                ok;
            Probe ->
                _ = Probe ! {arizona_fake_watcher_bare, init, Dir, Opts},
                ok
        end,
    {ok, Opts}.

handle_info({arizona_fake_watcher, Paths}, Opts) ->
    {ok, Paths, Opts};
handle_info(_Info, Opts) ->
    {ok, [], Opts}.

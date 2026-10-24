-module(arizona_fake_watcher).
-moduledoc """
In-memory `arizona_watcher` for the rule-process suites: a test drives it by
sending the rule process `{arizona_fake_watcher, Paths}`, which `handle_info/2`
reports as changed, and `{arizona_fake_watcher, stop, Reason}`, which it
reports as a dead watch. With a `notify` pid in its opts it reports its `init/2`
arguments and its `terminate/1`; with `fail => Reason` its `init/2` fails; with
`link => true` it links a helper process to the rule process and reports
`{arizona_fake_watcher, linked, Pid}`, so a test can make it exit
(`Pid ! {exit, Reason}`).
""".
-behaviour(arizona_watcher).

-export([init/2]).
-export([handle_info/2]).
-export([terminate/1]).

init(_Dir, #{fail := Reason}) ->
    {error, Reason};
init(Dir, Opts) ->
    ok = notify(Opts, {arizona_fake_watcher, init, Dir, Opts}),
    ok = maybe_link(Opts),
    {ok, Opts}.

handle_info({arizona_fake_watcher, stop, Reason}, Opts) ->
    {stop, Reason, Opts};
handle_info({arizona_fake_watcher, Paths}, Opts) ->
    {ok, Paths, Opts};
handle_info(_Info, Opts) ->
    {ok, [], Opts}.

terminate(Opts) ->
    notify(Opts, {arizona_fake_watcher, terminate}).

maybe_link(#{link := true} = Opts) ->
    Helper = spawn_link(fun() ->
        receive
            {exit, Reason} -> exit(Reason)
        after 60000 -> ok
        end
    end),
    notify(Opts, {arizona_fake_watcher, linked, Helper});
maybe_link(#{}) ->
    ok.

notify(#{notify := Pid}, Msg) ->
    _ = Pid ! Msg,
    ok;
notify(#{}, _Msg) ->
    ok.

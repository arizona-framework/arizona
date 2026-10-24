-module(arizona_reloader_sup).
-moduledoc """
Supervises the dev reloader's rule processes (`arizona_reloader_rule`), one per
rule of the `reloader` application env.

It keeps dev tooling from taking the application down. A rule restarts like
any worker, but a watch that keeps failing (its OS backend missing, the inotify
watch limit hit) would otherwise exhaust `arizona_sup`'s restart intensity and
stop the whole application, HTTP listener included. Here it exhausts this
supervisor's instead, which `arizona_sup` runs as a `temporary` child: hot
reload stops, with a supervisor report saying why, and everything else keeps
serving.

A config error still fails the boot: `arizona_sup` builds the rule specs
(`arizona_reloader_rule:child_specs/1`) before starting this supervisor, and a
rule that cannot start at boot fails it too.
""".
-behaviour(supervisor).

%% --------------------------------------------------------------------
%% API function exports
%% --------------------------------------------------------------------

-export([start_link/1]).
-export([child_spec/1]).

%% --------------------------------------------------------------------
%% supervisor callback exports
%% --------------------------------------------------------------------

-export([init/1]).

%% --------------------------------------------------------------------
%% Ignore xref warnings
%% --------------------------------------------------------------------

-ignore_xref([start_link/1]).

%% --------------------------------------------------------------------
%% API Functions
%% --------------------------------------------------------------------

-doc "Starts the supervisor under the well-known name `arizona_reloader_sup`.".
-spec start_link(RuleSpecs) -> supervisor:startlink_ret() when
    RuleSpecs :: [supervisor:child_spec()].
start_link(RuleSpecs) ->
    supervisor:start_link({local, ?MODULE}, ?MODULE, RuleSpecs).

-doc """
This supervisor's spec under `arizona_sup`: `temporary`, so giving up stops hot
reload without counting against `arizona_sup`'s own restart intensity.
""".
-spec child_spec(RuleSpecs) -> supervisor:child_spec() when
    RuleSpecs :: [supervisor:child_spec()].
child_spec(RuleSpecs) ->
    #{
        id => ?MODULE,
        start => {?MODULE, start_link, [RuleSpecs]},
        restart => temporary,
        type => supervisor
    }.

%% --------------------------------------------------------------------
%% supervisor Callbacks
%% --------------------------------------------------------------------

-spec init(RuleSpecs) -> {ok, {SupFlags, RuleSpecs}} when
    RuleSpecs :: [supervisor:child_spec()],
    SupFlags :: supervisor:sup_flags().
init(RuleSpecs) ->
    %% A few restarts in a short window cover a watch that drops once (an
    %% editor briefly removing the directory); a loop gives up within seconds.
    {ok, {#{strategy => one_for_one, intensity => 5, period => 10}, RuleSpecs}}.

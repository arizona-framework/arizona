-module(arizona_sup).
-moduledoc false.
-behaviour(supervisor).

%% --------------------------------------------------------------------
%% API function exports
%% --------------------------------------------------------------------

-export([start_link/1]).

%% --------------------------------------------------------------------
%% Behaviour (supervisor) exports
%% --------------------------------------------------------------------

-export([init/1]).

%% --------------------------------------------------------------------
%% API function definitions
%% --------------------------------------------------------------------

-spec start_link(Config) -> supervisor:startlink_ret() when
    Config :: arizona:config().
start_link(Config) ->
    supervisor:start_link({local, ?MODULE}, ?MODULE, Config).

%% --------------------------------------------------------------------
%% Behaviour (supervisor) callbacks
%% --------------------------------------------------------------------

-spec init(Config) -> {ok, {SupFlags, [ChildSpec]}} when
    Config :: arizona:config(),
    SupFlags :: supervisor:sup_flags(),
    ChildSpec :: supervisor:child_spec().
init(Config) ->
    SupFlags = #{
        strategy => one_for_all,
        intensity => 0,
        period => 1
    },
    ChildSpecs = build_child_specs(Config),
    {ok, {SupFlags, ChildSpecs}}.

%% --------------------------------------------------------------------
%% Internal Functions
%% --------------------------------------------------------------------

build_child_specs(Config) ->
    BaseSpecs = [
        #{
            id => arizona_live,
            start => {pg, start_link, [arizona_live]}
        },
        #{
            id => arizona_pubsub,
            start => {pg, start_link, [arizona_pubsub]}
        }
    ],
    maybe_add_watcher_sup(BaseSpecs, Config).

maybe_add_watcher_sup(BaseSpecs, Config) ->
    case maps:get(watcher, Config, #{}) of
        #{enabled := true} ->
            WatcherSupSpec = #{
                id => arizona_watcher_sup,
                start => {arizona_watcher_sup, start_link, []},
                type => supervisor
            },
            [WatcherSupSpec | BaseSpecs];
        _ ->
            BaseSpecs
    end.

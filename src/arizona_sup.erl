-module(arizona_sup).
-behaviour(supervisor).
-moduledoc false.

%% API functions.
-export([start_link/0]).

%% Supervisor callbacks.
-export([init/1]).

%% --------------------------------------------------------------------
%% API functions.
%% --------------------------------------------------------------------

-spec start_link() -> supervisor:startlink_ret().
start_link() ->
    supervisor:start_link({local, ?MODULE}, ?MODULE, []).

%% --------------------------------------------------------------------
%% Supervisor callbacks.
%% --------------------------------------------------------------------

-spec init(Args) -> {ok, {SupFlags, [ChildSpec]}}
    when Args :: term(),
         SupFlags :: supervisor:sup_flags(),
         ChildSpec :: supervisor:child_spec().
init(_Args) ->
    SupFlags = #{
        strategy => one_for_all,
        intensity => 0,
        period => 1
    },
    ChildSpecs = case arizona_cfg:endpoint() of
        #{live_reload := true} ->
            [#{id => arizona_live_reload, start =>
              {arizona_live_reload, start_link, []}}];
        #{} ->
            []
    end,
    {ok, {SupFlags, ChildSpecs}}.

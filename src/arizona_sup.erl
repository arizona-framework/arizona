-module(arizona_sup).
-moduledoc false.
-behaviour(supervisor).

%% --------------------------------------------------------------------
%% API function exports
%% --------------------------------------------------------------------

-export([start_link/0]).

%% --------------------------------------------------------------------
%% Behaviour (supervisor) exports
%% --------------------------------------------------------------------

-export([init/1]).

%% --------------------------------------------------------------------
%% API function definitions
%% --------------------------------------------------------------------

-spec start_link() -> supervisor:startlink_ret().
start_link() ->
    supervisor:start_link({local, ?MODULE}, ?MODULE, []).

%% --------------------------------------------------------------------
%% Behaviour (supervisor) callbacks
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

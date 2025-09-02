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

-spec init(Args) -> {ok, {SupFlags, [ChildSpec]}} when
    Args :: term(),
    SupFlags :: supervisor:sup_flags(),
    ChildSpec :: supervisor:child_spec().
init(_Args) ->
    SupFlags = #{
        strategy => one_for_all,
        intensity => 0,
        period => 1
    },
    ChildSpecs = [
        #{
            id => arizona_live,
            start => {pg, start_link, [arizona_live]}
        },
        #{
            id => arizona_pubsub,
            start => {pg, start_link, [arizona_pubsub]}
        }
    ],
    {ok, {SupFlags, ChildSpecs}}.

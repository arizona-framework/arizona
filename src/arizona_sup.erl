-module(arizona_sup).
-moduledoc """
Top-level supervisor for the Arizona application.

Currently supervises only `arizona_pubsub` (the `pg`-based pubsub
scope). Live processes are not managed here -- they're started ad hoc
by the transport layer (`arizona_socket:init/3`) and linked to the
calling WebSocket process so they share its lifetime.
""".
-behaviour(supervisor).

%% --------------------------------------------------------------------
%% API function exports
%% --------------------------------------------------------------------

-export([start_link/0]).

%% --------------------------------------------------------------------
%% supervisor callback exports
%% --------------------------------------------------------------------

-export([init/1]).

%% --------------------------------------------------------------------
%% API Functions
%% --------------------------------------------------------------------

-doc """
Starts the supervisor under the well-known name `arizona_sup`.
""".
-spec start_link() -> supervisor:startlink_ret().
start_link() ->
    supervisor:start_link({local, ?MODULE}, ?MODULE, []).

%% --------------------------------------------------------------------
%% supervisor Callbacks
%% --------------------------------------------------------------------

-spec init(Args) -> {ok, {SupFlags, [ChildSpec]}} when
    Args :: list(),
    SupFlags :: supervisor:sup_flags(),
    ChildSpec :: supervisor:child_spec().
init([]) ->
    {ok,
        {#{strategy => one_for_one}, [
            #{
                id => arizona_pubsub,
                start => {arizona_pubsub, start_link, []},
                type => worker
            }
        ]}}.

-module(arizona_sup).
-moduledoc """
Top-level supervisor for the Arizona application.

Always supervises `arizona_pubsub` (the `pg`-based pubsub scope). Also
supervises one `arizona_watcher` per rule when the dev-mode reloader is
enabled via the `reloader` application env. Live processes are not
managed here -- they're started ad hoc by the transport layer
(`arizona_socket:init/3`) and linked to the calling WebSocket process
so they share its lifetime.

## Reloader config

```erlang
{arizona, [
    {reloader, #{
        enabled => true,
        rules => [
            #{directory => "src",
              patterns => [".*\\\\.erl$"],
              callback => fun arizona_reloader:reload_erl/1}
        ]
    }}
]}
```

Each rule map is passed to `arizona_watcher:start_link/2` with
`directory` stripped out. Malformed config (missing `directory`,
non-list `rules`) crashes the supervisor init so boot errors are
obvious.
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
    supervisor:start_link({local, ?MODULE}, ?MODULE, #{}).

%% --------------------------------------------------------------------
%% supervisor Callbacks
%% --------------------------------------------------------------------

-spec init(Args) -> {ok, {SupFlags, [ChildSpec]}} when
    Args :: map(),
    SupFlags :: supervisor:sup_flags(),
    ChildSpec :: supervisor:child_spec().
init(#{}) ->
    Reloader = application:get_env(arizona, reloader, #{}),
    Children = [pubsub_spec() | watcher_specs(Reloader)],
    {ok, {#{strategy => one_for_one}, Children}}.

%% --------------------------------------------------------------------
%% Internal functions
%% --------------------------------------------------------------------

pubsub_spec() ->
    #{
        id => arizona_pubsub,
        start => {arizona_pubsub, start_link, []},
        type => worker
    }.

watcher_specs(#{enabled := true, rules := Rules}) when is_list(Rules) ->
    [watcher_spec(I, R) || {I, R} <- lists:enumerate(Rules)];
watcher_specs(_) ->
    [].

watcher_spec(I, #{directory := Dir} = Rule) ->
    Opts = maps:without([directory], Rule),
    #{
        id => {arizona_watcher, I},
        start => {arizona_watcher, start_link, [Dir, Opts]},
        type => worker
    }.

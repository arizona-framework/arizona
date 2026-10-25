-module(arizona_cowboy_server).
-moduledoc """
The Cowboy `arizona_server` adapter: boots a Cowboy listener serving Arizona
routes.

Cowboy's router sees one catch-all route to `arizona_cowboy_handler`; the
Arizona routes are compiled by `arizona_cowboy_router` into a table kept per
listener in a `persistent_term`, which the handler and
`arizona_cowboy_req:resolve_route/3` read.

## Options

- `routes` -- the Arizona route list (required). `mcp` and `reload` routes are
  refused: this adapter does not serve them.
- `transport_opts` -- `[{port, Port}]` (default `4040`); other entries go to
  Ranch's socket options.
- `compress` -- `true` (default) compresses pages and assets
  (`arizona_route:transforms/2`).
- `error_page` -- `{Module, Function}` rendered on a page crash.
- `proto_opts` -- merged into Cowboy's protocol options.

Not supported yet: TLS (`scheme => https`) and Cowboy's HTTP/2 WebSocket.
""".

-behaviour(arizona_server).

%% --------------------------------------------------------------------
%% API function exports
%% --------------------------------------------------------------------

-export([start/2]).
-export([stop/1]).
-export([drain/2]).
-export([recompile_routes/0]).
-export([table/1]).
-export([sessions/1]).

%% --------------------------------------------------------------------
%% Macros
%% --------------------------------------------------------------------

-define(KEY(Name), {?MODULE, Name}).

%% --------------------------------------------------------------------
%% API Functions
%% --------------------------------------------------------------------

-doc "Starts the Cowboy listener `Name` serving the Arizona `routes` in `Opts`.".
-spec start(Name, Opts) -> {ok, pid()} | {error, term()} when
    Name :: atom(),
    Opts :: map().
start(Name, Opts0) ->
    #{routes := Routes} = Opts = arizona_config:resolve(Opts0),
    BuildOpts = #{
        compress => maps:get(compress, Opts, true),
        error_page => maps:get(error_page, Opts, {arizona_error_page, render})
    },
    Table = arizona_cowboy_router:compile(Routes, BuildOpts),
    {ok, _} = application:ensure_all_started(cowboy),
    persistent_term:put(?KEY(Name), {Routes, BuildOpts, Table}),
    Dispatch = cowboy_router:compile([{'_', [{"/[...]", arizona_cowboy_handler, Name}]}]),
    ProtoOpts = maps:merge(#{env => #{dispatch => Dispatch}}, maps:get(proto_opts, Opts, #{})),
    case cowboy:start_clear(Name, transport_opts(Opts), ProtoOpts) of
        {ok, _Pid} = Ok ->
            ok = arizona_server_registry:register(?MODULE),
            Ok;
        {error, _Reason} = Error ->
            persistent_term:erase(?KEY(Name)),
            Error
    end.

-doc "Stops the listener `Name` and forgets its routes.".
-spec stop(Name) -> ok | {error, not_found} when
    Name :: atom().
stop(Name) ->
    persistent_term:erase(?KEY(Name)),
    cowboy:stop_listener(Name).

-doc """
Drains the listener `Name`: stops accepting connections, tells every live
session `{arizona_drain, Deadline}` (through `arizona_socket:drain/2`), waits up
to `Timeout` milliseconds for them to close, then stops it.
""".
-spec drain(Name, Timeout) -> ok | {error, not_found} when
    Name :: atom(),
    Timeout :: non_neg_integer().
drain(Name, Timeout) ->
    case persistent_term:get(?KEY(Name), undefined) of
        undefined ->
            {error, not_found};
        _Stash ->
            ok = ranch:suspend_listener(Name),
            Deadline = erlang:monotonic_time(millisecond) + Timeout,
            ok = arizona_pubsub:broadcast(sessions(Name), {arizona_cowboy_drain, Deadline}),
            ok = await_sessions(Name, Deadline),
            stop(Name)
    end.

-doc "Recompiles every running listener's routes from the list it started with.".
-spec recompile_routes() -> ok.
recompile_routes() ->
    lists:foreach(
        fun
            ({?KEY(Name), {Routes, BuildOpts, _Table}}) ->
                Table = arizona_cowboy_router:compile(Routes, BuildOpts),
                persistent_term:put(?KEY(Name), {Routes, BuildOpts, Table});
            (_Other) ->
                ok
        end,
        persistent_term:get()
    ).

-doc "The compiled route table of the listener `Name`.".
-spec table(Name) -> arizona_cowboy_router:table() when
    Name :: atom().
table(Name) ->
    {_Routes, _BuildOpts, Table} = persistent_term:get(?KEY(Name)),
    Table.

-doc "The pubsub group of the listener's live WebSocket sessions.".
-spec sessions(Name) -> {?MODULE, sessions, atom()} when
    Name :: atom().
sessions(Name) ->
    {?MODULE, sessions, Name}.

%% --------------------------------------------------------------------
%% Internal functions
%% --------------------------------------------------------------------

transport_opts(Opts) ->
    TransportOpts = maps:get(transport_opts, Opts, []),
    case lists:keymember(port, 1, TransportOpts) of
        true -> TransportOpts;
        false -> [{port, 4040} | TransportOpts]
    end.

%% A session leaves the group when its process exits.
await_sessions(Name, Deadline) ->
    case
        arizona_pubsub:subscribers(sessions(Name)) =:= [] orelse
            erlang:monotonic_time(millisecond) >= Deadline
    of
        true ->
            ok;
        false ->
            receive
            after 20 -> await_sessions(Name, Deadline)
            end
    end.

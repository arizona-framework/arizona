-module(arizona_pubsub).
-moduledoc """
Thin wrapper over `pg` for channel-based pub/sub.

Subscribers join a channel (any term) by pid and receive whatever is
broadcast to that channel as a plain mailbox message. There's no
serialization or queueing -- it's just `pg:join/3` plus a fan-out send.

Used by `arizona_watcher` (file change events), `arizona_reloader`
(dev hot reload), and any handler that wants cross-process messaging
(e.g. multi-tab chat in `arizona_chat`).

## Example

```erlang
1> arizona_pubsub:start_link().
2> arizona_pubsub:subscribe(my_topic, self()).
ok
3> arizona_pubsub:broadcast(my_topic, {hello, world}).
ok
4> receive Msg -> Msg end.
{hello, world}
```
""".

%% --------------------------------------------------------------------
%% API function exports
%% --------------------------------------------------------------------

-export([start_link/0]).
-export([subscribe/2]).
-export([unsubscribe/2]).
-export([broadcast/2]).
-export([broadcast_from/3]).
-export([subscribers/1]).

%% --------------------------------------------------------------------
%% Ignore xref warnings
%% --------------------------------------------------------------------

-ignore_xref([start_link/0, unsubscribe/2, broadcast_from/3, subscribers/1]).

%% --------------------------------------------------------------------
%% Types exports
%% --------------------------------------------------------------------

-export_type([channel/0]).

%% --------------------------------------------------------------------
%% Types definitions
%% --------------------------------------------------------------------

-nominal channel() :: term().

%% --------------------------------------------------------------------
%% API Functions
%% --------------------------------------------------------------------

-doc """
Starts the pubsub `pg` scope. Called from the application supervisor.
""".
-spec start_link() -> gen_server:start_ret().
start_link() ->
    pg:start_link(?MODULE).

-doc """
Subscribes `Pid` to `Channel`. Returns `{error, already_joined}` if
the pid is already subscribed (idempotent at the API level).
""".
-spec subscribe(Channel, Pid) -> ok | {error, already_joined} when
    Channel :: channel(),
    Pid :: pid().
subscribe(Channel, Pid) ->
    case lists:member(Pid, subscribers(Channel)) of
        true -> {error, already_joined};
        false -> pg:join(?MODULE, Channel, [Pid])
    end.

-doc """
Unsubscribes `Pid` from `Channel`. Returns `{error, not_joined}` if
the pid was not a subscriber.
""".
-spec unsubscribe(Channel, Pid) -> ok | {error, not_joined} when
    Channel :: channel(),
    Pid :: pid().
unsubscribe(Channel, Pid) ->
    case pg:leave(?MODULE, Channel, [Pid]) of
        ok -> ok;
        not_joined -> {error, not_joined}
    end.

-doc """
Sends `Data` as a mailbox message to every subscriber of `Channel`.
""".
-spec broadcast(Channel, Data) -> ok when
    Channel :: channel(),
    Data :: term().
broadcast(Channel, Data) ->
    send_each(subscribers(Channel), Data).

-doc """
Like `broadcast/2` but skips `From` -- useful when the publisher is
also a subscriber and shouldn't echo to itself.
""".
-spec broadcast_from(From, Channel, Data) -> ok when
    From :: pid(),
    Channel :: channel(),
    Data :: term().
broadcast_from(From, Channel, Data) ->
    send_each_skip(subscribers(Channel), Data, From).

%% Tail-recursive send loop -- the previous `[Pid ! Data || Pid <- Subs]`
%% form allocated a result list (one cons cell per subscriber) just to
%% discard it via `_ = ...`. For high-fanout broadcasts (chat, presence,
%% etc.) that's pure heap pressure.
send_each([], _Data) ->
    ok;
send_each([Pid | Rest], Data) ->
    Pid ! Data,
    send_each(Rest, Data).

send_each_skip([], _Data, _From) ->
    ok;
send_each_skip([From | Rest], Data, From) ->
    send_each_skip(Rest, Data, From);
send_each_skip([Pid | Rest], Data, From) ->
    Pid ! Data,
    send_each_skip(Rest, Data, From).

-doc """
Returns the list of pids currently subscribed to `Channel`.
""".
-spec subscribers(Channel) -> [pid()] when
    Channel :: channel().
subscribers(Channel) ->
    pg:get_members(?MODULE, Channel).

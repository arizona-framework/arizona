-module(arizona_pubsub).
-moduledoc """
Thin wrapper over `pg` for channel-based pub/sub.

Subscribers join a channel (any term) by pid and receive whatever is
broadcast to that channel as a plain mailbox message. There's no
serialization or queueing -- it's just `pg:join/3` plus a fan-out send.

The broadcast paths iterate `pg`'s raw membership list without sorting or
deduplicating -- a per-message `usort` over the whole member list would tax
every broadcast (and every subscribe) to paper over a state only two
*concurrent* subscribes of the SAME pid can create (`subscribe/2`'s
check-then-join is not atomic). In that essentially-unreachable state a
subscriber receives one copy per membership until `unsubscribe/2` clears
them all; `subscribers/1` still reports each pid once.

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
-export([monitor/1]).

%% --------------------------------------------------------------------
%% Ignore xref warnings
%% --------------------------------------------------------------------

-ignore_xref([start_link/0, unsubscribe/2, broadcast_from/3, subscribers/1, monitor/1]).

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
the pid is already subscribed.

The membership check is a read followed by a join, so two concurrent
subscribes of the same pid can both return `ok` and both reach `pg`
(which has no atomic join-if-absent). `subscribers/1` reports each pid
once regardless, and `unsubscribe/2` drops every `pg` membership the pid
holds rather than leaving a stray one behind; until then a broadcast
delivers one copy per membership (see the moduledoc).
""".
-spec subscribe(Channel, Pid) -> ok | {error, already_joined} when
    Channel :: channel(),
    Pid :: pid().
subscribe(Channel, Pid) ->
    %% Raw membership read -- no usort; lists:member only needs to find the pid.
    case lists:member(Pid, pg:get_members(?MODULE, Channel)) of
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
    %% Leave every membership the pid holds, not just one: concurrent
    %% subscribes race past the `subscribe/2` check and can join the same
    %% pid twice, and `pg:leave/3` drops exactly as many occurrences as it
    %% is given -- a single-pid leave would leave the pid still subscribed.
    case [P || P <- pg:get_members(?MODULE, Channel), P =:= Pid] of
        [] ->
            {error, not_joined};
        Memberships ->
            ok = pg:leave(?MODULE, Channel, Memberships)
    end.

-doc """
Sends `Data` as a mailbox message to every subscriber of `Channel`
(one copy per `pg` membership -- see the moduledoc).
""".
-spec broadcast(Channel, Data) -> ok when
    Channel :: channel(),
    Data :: term().
broadcast(Channel, Data) ->
    send_each(pg:get_members(?MODULE, Channel), Data).

-doc """
Like `broadcast/2` but skips `From` -- useful when the publisher is
also a subscriber and shouldn't echo to itself.
""".
-spec broadcast_from(From, Channel, Data) -> ok when
    From :: pid(),
    Channel :: channel(),
    Data :: term().
broadcast_from(From, Channel, Data) ->
    send_each_skip(pg:get_members(?MODULE, Channel), Data, From).

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
Returns the pids currently subscribed to `Channel`, each once.

`pg` memberships are a multiset and `subscribe/2`'s check-then-join is
not atomic, so a pid can hold more than one membership; this diagnostic
view deduplicates. The broadcast paths deliberately do not (see the
moduledoc), so they never pay this sort.
""".
-spec subscribers(Channel) -> [pid()] when
    Channel :: channel().
subscribers(Channel) ->
    lists:usort(pg:get_members(?MODULE, Channel)).

-doc """
Subscribes the caller to membership changes on `Channel`. It begins receiving
`{Ref, join, Channel, Pids}` and `{Ref, leave, Channel, Pids}` messages as pids
join or leave -- a leave fires on a subscriber's process death too -- and returns
the monitor reference plus the current members. The monitor is removed
automatically when the caller exits.

Useful for presence, e.g. a view that shows a live count of connected peers.
""".
-spec monitor(Channel) -> {reference(), [pid()]} when
    Channel :: channel().
monitor(Channel) ->
    pg:monitor(?MODULE, Channel).

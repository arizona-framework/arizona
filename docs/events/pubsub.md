# PubSub

- [join/broadcast/leave](#joinbroadcastleave)
- [PubSub Messages as Events](#pubsub-messages-as-events)

## join/broadcast/leave

Arizona's PubSub system is built on Erlang's `pg` (process groups) module. It
provides a simple publish-subscribe mechanism for broadcasting messages between
processes. Any Erlang process can subscribe to a topic and receive messages
broadcast to that topic.

The API consists of five functions:

```erlang
%% Subscribe the current process to a topic
arizona_pubsub:join(~"notifications", self()).

%% Broadcast a message to all subscribers of a topic
arizona_pubsub:broadcast(~"notifications", #{message => ~"Hello!"}).

%% Broadcast to all subscribers except the sender
arizona_pubsub:broadcast_from(self(), ~"notifications", #{message => ~"Update"}).

%% Unsubscribe from a topic
arizona_pubsub:leave(~"notifications", self()).

%% List all processes currently subscribed to a topic
Members = arizona_pubsub:get_members(~"notifications").
```

**Topics** are binaries. You can use any naming convention, but a common
pattern is to use a descriptive prefix followed by an identifier:

```erlang
%% General topic
arizona_pubsub:join(~"chat:lobby", self()).

%% Entity-specific topic
arizona_pubsub:join(~"order:42", self()).

%% User-specific topic
arizona_pubsub:join(<<"user:", UserId/binary>>, self()).
```

**When to subscribe:** Views typically subscribe in their `mount/2` callback.
Since `mount/2` is called twice -- once during the initial HTTP render and
once when the WebSocket connects -- you should check if the view is connected
before subscribing to avoid subscribing during the static render:

```erlang
mount(_MountArg, _Req) ->
    View = arizona_view:new(?MODULE, #{count => 0}, none),
    case arizona_live:is_connected(self()) of
        true ->
            arizona_pubsub:join(~"counter", self());
        false ->
            ok
    end,
    View.
```

**`broadcast` vs `broadcast_from`:** Use `broadcast/2` when every subscriber
should receive the message, including the sender. Use `broadcast_from/3` when
the sender already knows about the change (because it just made the change)
and only other subscribers need to be notified:

```erlang
handle_event(~"increment", _Params, State) ->
    Count = arizona_stateful:get_binding(count, State) + 1,
    %% Notify others, but not ourselves (we already updated)
    arizona_pubsub:broadcast_from(self(), ~"counter", #{count => Count}),
    {[], arizona_stateful:put_binding(count, Count, State)}.
```

Because PubSub is built on `pg`, it works across nodes in a distributed
Erlang cluster. A broadcast on one node reaches subscribers on all connected
nodes.

## PubSub Messages as Events

When a process subscribed to a topic receives a broadcast, Arizona's
`arizona_live` GenServer intercepts the `{pubsub_message, Topic, Data}` tuple
and routes it to the view's `handle_event/3` callback. The topic becomes the
event name and the broadcast data becomes the params:

```erlang
mount(_MountArg, _Req) ->
    arizona_pubsub:join(~"counter", self()),
    arizona_view:new(?MODULE, #{count => 0}, none).

handle_event(~"counter", #{count := Count}, View) ->
    State = arizona_view:get_state(View),
    State1 = arizona_stateful:put_binding(count, Count, State),
    {[], arizona_view:update_state(State1, View)}.
```

The return value follows the same `{Actions, View}` convention as any other
`handle_event/3` call. After updating the state, Arizona computes a diff of the
template bindings and sends only the changed parts to the client. This means
PubSub-driven updates go through the same efficient differential rendering
pipeline as user-initiated events.

> **Note:** Non-PubSub Erlang messages (e.g., from `erlang:send_after/3` or
> direct `pid ! Msg` sends) are handled by `handle_info/2` instead. See
> [Views handle_info](../components/views.md#handle_info).

**Pattern matching on topics:** If a view subscribes to multiple topics, use
pattern matching to handle each one:

```erlang
handle_event(~"chat:lobby", #{msg := Msg}, View) ->
    State = arizona_view:get_state(View),
    Messages = [Msg | arizona_stateful:get_binding(messages, State)],
    State1 = arizona_stateful:put_binding(messages, Messages, State),
    {[], arizona_view:update_state(State1, View)};

handle_event(~"presence", #{users := Users}, View) ->
    State = arizona_view:get_state(View),
    State1 = arizona_stateful:put_binding(online_users, Users, State),
    {[], arizona_view:update_state(State1, View)}.
```

**Real-time workflow example:** Combining PubSub with events enables real-time
features. Consider a shared counter:

1. User A clicks the increment button, triggering a `handle_event/3` callback.
2. The handler updates the count and broadcasts via `broadcast_from/3`.
3. User B's view receives the broadcast in `handle_event/3` with the topic as
   the event name.
4. User B's state is updated and the template diff is sent to their client.
5. User B sees the counter update without refreshing the page.

This pattern works for chat applications, live dashboards, collaborative
editing, notifications, and any feature that requires real-time synchronization
across multiple clients.

See also:

- [Views handle_event](../components/views.md#handle_event) -- the `handle_event/3` callback
- [Realtime Example](../ecosystem/examples.md#realtime) -- a complete real-time example

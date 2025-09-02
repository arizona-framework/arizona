-module(arizona_pubsub).
-moduledoc ~"""
Simple publish-subscribe messaging system using Erlang's process groups.

Provides topic-based message broadcasting to subscribed processes using
Erlang's built-in `pg` module. Enables decoupled communication between
processes through named topics with automatic process monitoring.

## Message Protocol

Subscribers receive messages in the format:
```erlang
{pubsub_message, Topic, Data}
```

## Usage Pattern

1. **Subscribe**: Processes join topics to receive messages
2. **Publish**: Send data to all subscribers of a topic
3. **Receive**: Handle `pubsub_message` in process message loop
4. **Unsubscribe**: Leave topics when no longer interested

## Example Usage

```erlang
%% Subscriber process
init() ->
    ok = arizona_pubsub:join(~"live_reload", self()),
    {ok, State}.

handle_info({pubsub_message, ~"live_reload", Data}, State) ->
    handle_reload(Data),
    {noreply, State}.

%% Publisher process
notify_reload() ->
    arizona_pubsub:broadcast(~"live_reload", {file_changed, reload}).
```

## Common Topics

- `~"live_reload"` - Development-time file change notifications
- Custom application topics for real-time features

## Process Management

Built on Erlang's `pg` module which automatically handles:
- Process monitoring and cleanup
- Node distribution (if needed)
- Efficient group membership management
""".

%% --------------------------------------------------------------------
%% API function exports
%% --------------------------------------------------------------------

-export([broadcast/2]).
-export([broadcast_from/3]).
-export([join/2]).
-export([leave/2]).
-export([get_members/1]).

%% --------------------------------------------------------------------
%% Ignore xref warnings
%% --------------------------------------------------------------------

-ignore_xref([broadcast/2]).
-ignore_xref([broadcast_from/3]).
-ignore_xref([join/2]).
-ignore_xref([leave/2]).
-ignore_xref([get_members/1]).

%% --------------------------------------------------------------------
%% Types exports
%% --------------------------------------------------------------------

-export_type([topic/0]).

%% --------------------------------------------------------------------
%% Types definitions
%% --------------------------------------------------------------------

-nominal topic() :: binary().

%% --------------------------------------------------------------------
%% API function definitions
%% --------------------------------------------------------------------

-doc ~"""
Broadcasts a message to all subscribers of a topic.

Sends `{pubsub_message, Topic, Data}` to every process subscribed
to the specified topic, including the sender if subscribed.
""".
-spec broadcast(Topic, Data) -> ok when
    Topic :: topic(),
    Data :: dynamic().
broadcast(Topic, Data) when is_binary(Topic) ->
    lists:foreach(
        fun(Pid) ->
            send_message(Pid, Topic, Data)
        end,
        get_members(Topic)
    ).

-doc ~"""
Broadcasts a message to all subscribers except the sender.

Sends `{pubsub_message, Topic, Data}` to every process subscribed
to the topic except the specified sender process.
""".
-spec broadcast_from(From, Topic, Data) -> ok when
    From :: pid(),
    Topic :: topic(),
    Data :: dynamic().
broadcast_from(From, Topic, Data) when is_binary(Topic) ->
    lists:foreach(
        fun
            (Pid) when Pid =:= From ->
                ok;
            (Pid) ->
                send_message(Pid, Topic, Data)
        end,
        get_members(Topic)
    ).

-doc ~"""
Subscribes a process to a topic.

Adds the process to the topic's subscriber list. The process will
receive all future broadcasts to this topic until it leaves or terminates.
""".
-spec join(Topic, Pid) -> ok when
    Topic :: topic(),
    Pid :: pid().
join(Topic, Pid) when is_binary(Topic), is_pid(Pid) ->
    pg:join(?MODULE, Topic, Pid).

-doc ~"""
Unsubscribes a process from a topic.

Removes the process from the topic's subscriber list. Returns `ok`
if successfully removed, `not_joined` if the process wasn't subscribed.
""".
-spec leave(Topic, Pid) -> ok | not_joined when
    Topic :: topic(),
    Pid :: pid().
leave(Topic, Pid) when is_binary(Topic), is_pid(Pid) ->
    pg:leave(?MODULE, Topic, Pid).

-doc ~"""
Returns all processes subscribed to a topic.

Gets the current list of subscriber processes for the specified topic.
Useful for debugging or metrics collection.
""".
-spec get_members(Topic) -> [pid()] when
    Topic :: topic().
get_members(Topic) when is_binary(Topic) ->
    pg:get_members(?MODULE, Topic).

%% --------------------------------------------------------------------
%% Internal functions
%% --------------------------------------------------------------------

send_message(Pid, Topic, Data) ->
    Pid ! {pubsub_message, Topic, Data}.

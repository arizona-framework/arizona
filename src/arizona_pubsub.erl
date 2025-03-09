-module(arizona_pubsub).

%% --------------------------------------------------------------------
%% API function exports
%% --------------------------------------------------------------------

-export([start_link/0]).
-export([subscribe/2]).
-export([publish/4]).

%

-ignore_xref([start_link/0]).

%% --------------------------------------------------------------------
%% API function definitions
%% --------------------------------------------------------------------

-spec start_link() -> {ok, pid()} | {error, dynamic()}.
start_link() ->
    pg:start_link(?MODULE).

-spec subscribe(EventName, Subscriber) -> ok when
    EventName :: arizona:event_name(),
    Subscriber :: pid().
subscribe(EventName, Subscriber) when is_binary(EventName), is_pid(Subscriber) ->
    pg:join(?MODULE, EventName, Subscriber).

-spec publish(ViewId, EventName, Payload, Sender) -> ok when
    ViewId :: arizona_view:id(),
    EventName :: arizona:event_name(),
    Payload :: arizona:event_payload(),
    Sender :: pid().
publish(ViewId, EventName, Payload, Sender) when
    is_binary(ViewId),
    is_binary(EventName),
    is_pid(Sender)
->
    _ = [
        Pid ! {broadcast, Sender, ViewId, EventName, Payload}
     || Pid <- members(EventName)
    ],
    ok.

%% --------------------------------------------------------------------
%% Private functions
%% --------------------------------------------------------------------

-spec members(EventName) -> [Member] when
    EventName :: arizona:event_name(),
    Member :: pid().
members(EventName) ->
    pg:get_members(?MODULE, EventName).

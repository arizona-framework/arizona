-module(arizona_pubsub).
-export([start_link/0]).
-export([subscribe/2, unsubscribe/2, broadcast/2, broadcast_from/3, subscribers/1]).
-ignore_xref([start_link/0, unsubscribe/2, broadcast_from/3, subscribers/1]).

-type channel() :: term().

-export_type([channel/0]).

-spec start_link() -> {ok, pid()} | {error, term()}.
start_link() ->
    pg:start_link(?MODULE).

-spec subscribe(channel(), pid()) -> ok | {error, already_joined}.
subscribe(Channel, Pid) ->
    case lists:member(Pid, subscribers(Channel)) of
        true -> {error, already_joined};
        false -> pg:join(?MODULE, Channel, [Pid])
    end.

-spec unsubscribe(channel(), pid()) -> ok | {error, not_joined}.
unsubscribe(Channel, Pid) ->
    case pg:leave(?MODULE, Channel, [Pid]) of
        ok -> ok;
        not_joined -> {error, not_joined}
    end.

-spec broadcast(channel(), term()) -> ok.
broadcast(Channel, Data) ->
    _ = [Pid ! Data || Pid <:- subscribers(Channel)],
    ok.

-spec broadcast_from(pid(), channel(), term()) -> ok.
broadcast_from(From, Channel, Data) ->
    _ = [Pid ! Data || Pid <:- subscribers(Channel), Pid =/= From],
    ok.

-spec subscribers(channel()) -> [pid()].
subscribers(Channel) ->
    pg:get_members(?MODULE, Channel).

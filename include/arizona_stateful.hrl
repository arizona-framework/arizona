-behaviour(arizona_stateful).
-compile({parse_transform, arizona_parse_transform}).
-include("arizona_common.hrl").

%% Per-view messaging -- short forms (?send/1, ?send_after/2) require `Bindings` in scope
-define(send(ViewId, Msg), arizona_live:send(ViewId, Msg)).
-define(send(Msg), ?send(map_get(id, Bindings), Msg)).
-define(send_after(ViewId, Time, Msg), arizona_live:send_after(ViewId, Time, Msg)).
-define(send_after(Time, Msg), ?send_after(map_get(id, Bindings), Time, Msg)).

%% PubSub -- subscribes the live process, so all its views share one membership:
%% `ok`, or `{error, already_joined}` when it is already subscribed (e.g. by another
%% view), and one view's ?unsubscribe ends it for all of them
-define(subscribe(Topic), arizona_pubsub:subscribe(Topic, self())).
-define(unsubscribe(Topic), arizona_pubsub:unsubscribe(Topic, self())).

-behaviour(arizona_stateful).
-compile({parse_transform, arizona_parse_transform}).
-include("arizona_template.hrl").

%% Per-view messaging -- short forms (?send/1, ?send_after/2) require `Bindings` in scope
-define(send(ViewId, Msg), arizona_live:send(ViewId, Msg)).
-define(send(Msg), ?send(map_get(id, Bindings), Msg)).
-define(send_after(ViewId, Time, Msg), arizona_live:send_after(ViewId, Time, Msg)).
-define(send_after(Time, Msg), ?send_after(map_get(id, Bindings), Time, Msg)).

%% PubSub
-define(subscribe(Topic), arizona_pubsub:subscribe(Topic, self())).
-define(unsubscribe(Topic), arizona_pubsub:unsubscribe(Topic, self())).

-module(arizona_js).

-export([send/3]).

-spec send(ViewId, Event, Payload) -> Js when
    ViewId :: arizona_view:id(),
    Event :: binary(),
    Payload :: dynamic(),
    Js :: binary().
send(ViewId, Event, Payload) when is_binary(ViewId), is_binary(Event) ->
    iolist_to_binary([
        "'arizona.send(\"", ViewId, "\", \"", Event, "\", ", json:encode(Payload), ")'"
    ]).

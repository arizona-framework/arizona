-module(arizona_js).

%% --------------------------------------------------------------------
%% API function exports
%% --------------------------------------------------------------------

-export([send/3]).

%

-ignore_xref([send/3]).

%% --------------------------------------------------------------------
%% API function definitions
%% --------------------------------------------------------------------

-spec send(ViewId, Event, Payload) -> Js when
    ViewId :: arizona_view:id(),
    Event :: binary(),
    Payload :: dynamic(),
    Js :: binary().
send(ViewId, Event, Payload) when is_binary(ViewId), is_binary(Event) ->
    iolist_to_binary([
        "'arizona.send(\"", ViewId, "\", \"", Event, "\", ", json:encode(Payload), ")'"
    ]).

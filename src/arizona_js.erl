-module(arizona_js).

%% --------------------------------------------------------------------
%% API function exports
%% --------------------------------------------------------------------

-export([send_event/3]).

%% --------------------------------------------------------------------
%% API function definitions
%% --------------------------------------------------------------------

-spec send_event(ViewId, EventName, Payload) -> Js when
    ViewId :: arizona_view:id(),
    EventName :: binary(),
    Payload :: dynamic(),
    Js :: binary().
send_event(ViewId, EventName, Payload) when is_binary(ViewId), is_binary(EventName) ->
    <<"arizona.send(&quot;", EventName/binary, "&quot;, &quot;", ViewId/binary, "&quot;, ",
        (iolist_to_binary(json:encode(Payload)))/binary, ")">>.

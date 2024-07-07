-module(arizona_js).
-moduledoc """
Javascript support.
""".

%% API functions.
-export([send/1]).
-export([send/2]).
-ignore_xref([send/2]).

%% --------------------------------------------------------------------
%% API funtions.
%% --------------------------------------------------------------------

-spec send(EventName) -> Sent
    when EventName :: binary(),
         Sent :: binary().
send(EventName) ->
    <<"arizona.send.bind(this)('", EventName/binary, "')"/utf8>>.

-spec send(EventName, Payload) -> Sent
    when EventName :: binary(),
         Payload :: json:encode_value(),
         Sent :: binary().
send(EventName, Payload) ->
    <<"arizona.send.bind(this)('", EventName/binary, "', ", (safe(Payload))/binary, ")"/utf8>>.

%% --------------------------------------------------------------------
%% Internal funtions.
%% --------------------------------------------------------------------

safe(Term) ->
    iolist_to_binary(json:encode(Term)).

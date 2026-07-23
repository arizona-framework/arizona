-module(arizona_roadrunner_resp_SUITE).
-include_lib("stdlib/include/assert.hrl").

-export([all/0]).
-export([groups/0]).

-export([flush_adds_header_and_cookie_to_buffered/1]).
-export([flush_adds_header_to_stream_loop_and_sendfile/1]).
-export([flush_flattens_iodata_header_value/1]).
-export([flush_empty_stash_leaves_response_untouched/1]).
-export([flush_onto_websocket_upgrade_errors/1]).

all() ->
    [{group, flush}].

groups() ->
    [
        {flush, [parallel], [
            flush_adds_header_and_cookie_to_buffered,
            flush_adds_header_to_stream_loop_and_sendfile,
            flush_flattens_iodata_header_value,
            flush_empty_stash_leaves_response_untouched,
            flush_onto_websocket_upgrade_errors
        ]}
    ].

%% --------------------------------------------------------------------
%% flush/2
%% --------------------------------------------------------------------

flush_adds_header_and_cookie_to_buffered(Config) when is_list(Config) ->
    {200, Headers, Body} = arizona_roadrunner_resp:flush(
        stashed_req(), roadrunner_resp:text(200, ~"ok")
    ),
    ?assertEqual(~"1", proplists:get_value(~"x-a", Headers)),
    ?assertMatch({_, _}, binary:match(proplists:get_value(~"set-cookie", Headers), ~"probe=set")),
    ?assertEqual(~"ok", Body).

flush_adds_header_to_stream_loop_and_sendfile(Config) when is_list(Config) ->
    %% The non-buffered shapes carry their headers in the third position; a
    %% middleware stash must reach them too, not crash the dispatcher.
    Fun = fun(Send) -> Send(~"chunk", fin) end,
    {stream, 200, StreamHeaders, Fun} = arizona_roadrunner_resp:flush(
        stashed_req(), {stream, 200, [], Fun}
    ),
    ?assertEqual(~"1", proplists:get_value(~"x-a", StreamHeaders)),
    {loop, 200, LoopHeaders, state} = arizona_roadrunner_resp:flush(
        stashed_req(), {loop, 200, [], state}
    ),
    ?assertEqual(~"1", proplists:get_value(~"x-a", LoopHeaders)),
    Spec = {"/etc/hostname", 0, 1},
    {sendfile, 200, FileHeaders, Spec} = arizona_roadrunner_resp:flush(
        stashed_req(), {sendfile, 200, [], Spec}
    ),
    ?assertEqual(~"1", proplists:get_value(~"x-a", FileHeaders)),
    ?assertMatch(
        {_, _}, binary:match(proplists:get_value(~"set-cookie", FileHeaders), ~"probe=set")
    ).

flush_flattens_iodata_header_value(Config) when is_list(Config) ->
    %% `put_resp_header/3` accepts iodata, so every shape must flatten it -- the
    %% wire encoder rejects a non-binary value.
    Req = arizona_req:put_resp_header(arizona_req_test_adapter:new(#{}), ~"x-io", [~"a", ~"b"]),
    {stream, 200, Headers, _Fun} = arizona_roadrunner_resp:flush(
        Req, {stream, 200, [], fun(Send) -> Send(~"", fin) end}
    ),
    ?assertEqual(~"ab", proplists:get_value(~"x-io", Headers)).

flush_empty_stash_leaves_response_untouched(Config) when is_list(Config) ->
    %% Nothing stashed means nothing to fold -- including onto a websocket
    %% upgrade, which has no header section to fold into.
    Req = arizona_req_test_adapter:new(#{}),
    Resp = {websocket, some_ws_handler, state},
    ?assertEqual(Resp, arizona_roadrunner_resp:flush(Req, Resp)).

flush_onto_websocket_upgrade_errors(Config) when is_list(Config) ->
    %% A websocket upgrade has nowhere to put a Set-Cookie. Dropping it silently
    %% would lose a cleared flash or a rotated session, so it fails loudly.
    ?assertError(
        {unflushable_response, websocket},
        arizona_roadrunner_resp:flush(stashed_req(), {websocket, some_ws_handler, state})
    ).

%% --------------------------------------------------------------------
%% Helpers
%% --------------------------------------------------------------------

%% A request carrying one stashed response header and one stashed cookie, the
%% shape a middleware leaves behind for the dispatcher to flush.
stashed_req() ->
    Req = arizona_req:put_resp_header(arizona_req_test_adapter:new(#{}), ~"x-a", ~"1"),
    arizona_req:put_resp_cookie(Req, ~"probe", ~"set", #{path => ~"/"}).

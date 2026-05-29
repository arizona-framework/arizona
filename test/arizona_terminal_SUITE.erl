-module(arizona_terminal_SUITE).
-include_lib("stdlib/include/assert.hrl").

-export([all/0]).
-export([init_per_suite/1]).
-export([end_per_suite/1]).
-export([renders_initial_frame/1]).
-export([key_moves_selection/1]).
-export([key_changes_count/1]).
-export([tick_advances_clock/1]).
-export([broadcast_appends_message/1]).
-export([normalize_key_mapping/1]).
-export([req_adapter_blank/1]).

%% Drives the ?terminal demo view through arizona_live with NO transport and no
%% HTTP server -- exactly how the terminal runtime drives it, minus the TTY.
%% render_current/1 materializes the full ANSI frame each time.

all() ->
    [
        renders_initial_frame,
        key_moves_selection,
        key_changes_count,
        tick_advances_clock,
        broadcast_appends_message,
        normalize_key_mapping,
        req_adapter_blank
    ].

init_per_suite(Config) ->
    %% The demo view subscribes to a pubsub channel at mount, so the pg scope
    %% must be running (the arizona app is not started in this suite).
    case erlang:whereis(arizona_pubsub) of
        undefined ->
            {ok, Pid} = pg:start_link(arizona_pubsub),
            unlink(Pid);
        _ ->
            ok
    end,
    Config.

end_per_suite(Config) when is_list(Config) ->
    Config.

renders_initial_frame(Config) when is_list(Config) ->
    {Pid, _ViewId} = start_demo(),
    Frame = frame(Pid),
    ?assert(contains(Frame, ~"== Arizona Terminal Demo ==")),
    %% selection marker on the first item, plain markers on the rest
    ?assert(contains(Frame, ~"> New Game")),
    ?assert(contains(Frame, ~"  Options")),
    ?assert(contains(Frame, ~"Count: 0")),
    ?assert(contains(Frame, ~"Server ticks: 0")),
    %% the title carries a green SGR escape
    ?assert(contains(Frame, ~"\e[32m")).

key_moves_selection(Config) when is_list(Config) ->
    {Pid, ViewId} = start_demo(),
    ?assert(contains(frame(Pid), ~"> New Game")),
    {ok, _Ops, _Effects} = arizona_live:handle_event(Pid, ViewId, ~"key", #{~"key" => ~"j"}),
    Frame = frame(Pid),
    ?assert(contains(Frame, ~"> Options")),
    ?assert(contains(Frame, ~"  New Game")).

key_changes_count(Config) when is_list(Config) ->
    {Pid, ViewId} = start_demo(),
    {ok, _, _} = arizona_live:handle_event(Pid, ViewId, ~"key", #{~"key" => ~"+"}),
    {ok, _, _} = arizona_live:handle_event(Pid, ViewId, ~"key", #{~"key" => ~"+"}),
    ?assert(contains(frame(Pid), ~"Count: 2")).

tick_advances_clock(Config) when is_list(Config) ->
    {Pid, ViewId} = start_demo(),
    ?assert(contains(frame(Pid), ~"Server ticks: 0")),
    %% Stand in for the self-scheduled timer message reaching the live view.
    Pid ! {arizona_view, ViewId, tick},
    ?assert(contains(frame(Pid), ~"Server ticks: 1")).

broadcast_appends_message(Config) when is_list(Config) ->
    {Pid, _ViewId} = start_demo(),
    %% mount/2 subscribed the live process to the `demo` channel, so a broadcast
    %% from any process repaints it.
    ok = arizona_pubsub:broadcast(demo, {chat, ~"hello there"}),
    ?assert(contains(frame(Pid), ~"hello there")).

normalize_key_mapping(Config) when is_list(Config) ->
    %% The driver turns raw key reads into quit / a ~"key" payload / ignore.
    ?assertEqual(quit, arizona_terminal_app:normalize_key("q")),
    ?assertEqual(quit, arizona_terminal_app:normalize_key([3])),
    ?assertEqual(quit, arizona_terminal_app:normalize_key([4])),
    ?assertEqual(~"up", arizona_terminal_app:normalize_key("\e[A")),
    ?assertEqual(~"down", arizona_terminal_app:normalize_key("\e[B")),
    ?assertEqual(~"j", arizona_terminal_app:normalize_key("j")),
    ?assertEqual(~"+", arizona_terminal_app:normalize_key("+")),
    %% an unrecognized escape sequence and a bare ESC are dropped
    ?assertEqual(ignore, arizona_terminal_app:normalize_key("\e[Z")),
    ?assertEqual(ignore, arizona_terminal_app:normalize_key([27])).

req_adapter_blank(Config) when is_list(Config) ->
    %% The synthetic request mounts a view with no HTTP server.
    Req = arizona_terminal_req:new(),
    ?assertEqual(arizona_terminal_req, arizona_req:adapter(Req)),
    ?assertEqual(~"GET", arizona_req:method(Req)),
    ?assertEqual(~"/", arizona_req:path(Req)),
    {Bindings, _Req1} = arizona_req:bindings(Req),
    ?assertEqual(#{}, Bindings).

%% --------------------------------------------------------------------
%% Helpers
%% --------------------------------------------------------------------

start_demo() ->
    {ok, Pid} = arizona_live:start_link(
        arizona_term_demo, #{}, undefined, [], arizona_req_test_adapter:new()
    ),
    {ok, ViewId} = arizona_live:mount(Pid),
    {Pid, ViewId}.

frame(Pid) ->
    {ok, Frame} = arizona_live:render_current(Pid),
    Frame.

contains(Frame, Sub) ->
    binary:match(Frame, Sub) =/= nomatch.

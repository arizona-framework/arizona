-module(arizona_socket_SUITE).
-include_lib("stdlib/include/assert.hrl").
-include("arizona.hrl").

%% Drives arizona_socket directly (init/handle_in/handle_info) with the stub
%% request adapter -- the calling test process IS the socket process, so live
%% pushes land in its own mailbox exactly as they do in the real transport.

-export([all/0]).
-export([push_racing_navigate_dropped/1]).
-export([queued_push_prepended_to_event_reply/1]).
-export([queued_push_prepended_to_patch_reply/1]).
-export([full_navigate_drops_pending_flash/1]).

all() ->
    [
        push_racing_navigate_dropped,
        queued_push_prepended_to_event_reply,
        queued_push_prepended_to_patch_reply,
        full_navigate_drops_pending_flash
    ].

push_racing_navigate_dropped(Config) when is_list(Config) ->
    %% A push the live process emits just before serving a navigate is
    %% processed by the socket AFTER the navigate reply. It belongs to the OLD
    %% page, so it must be dropped -- shipping it tagged with the new root id
    %% delivers stale ops into the fresh page (the client's resolveEl fallback
    %% can innerHTML-overwrite the just-rendered view).
    Req = arizona_req_test_adapter:new(#{
        routes => #{~"/next" => {arizona_root_counter, #{}}}
    }),
    {ok, Socket0} = arizona_socket:init(arizona_timer, #{}, Req, #{}),
    Pid = arizona_socket:live_pid(Socket0),
    %% Queue an info for the current page, then navigate: the live process
    %% handles the info first (FIFO), pushing old-page ops into our mailbox,
    %% and only then serves the navigate call.
    Pid ! {set_message, ~"stale"},
    NavFrame = iolist_to_binary(
        json:encode([~"navigate", #{~"path" => ~"/next", ~"qs" => <<>>}])
    ),
    {reply, _ReplaceFrame, Socket1} = arizona_socket:handle_in(NavFrame, Socket0),
    receive
        Push ->
            %% Dropped, not shipped: before the fix this replied with the stale
            %% ops retagged as the NEW view's.
            ?assertEqual({ok, Socket1}, arizona_socket:handle_info(Push, Socket1)),
            %% The push names its owning root view so the socket can tell.
            ?assertMatch({arizona_push, ~"timer", _, _}, Push)
    after 1000 ->
        error(timeout_waiting_for_stale_push)
    end.

queued_push_prepended_to_event_reply(Config) when is_list(Config) ->
    %% Causal order: an info (count -> 5) precedes the event (inc -> 6). The
    %% live process pushes the "5" op into the socket mailbox, then serves the
    %% event call with the "6" reply. The reply frame must carry BOTH ops in
    %% causal order -- before the fix the reply carried only "6" and the queued
    %% "5" shipped in a LATER frame, so the stale value won client-side.
    Req = arizona_req_test_adapter:new(),
    {ok, Socket0} = arizona_socket:init(arizona_root_counter, #{}, Req, #{}),
    Pid = arizona_socket:live_pid(Socket0),
    Pid ! {set_count, 5},
    EventFrame = iolist_to_binary(json:encode([~"counter", ~"inc", #{}])),
    {reply, Frame, _Socket1} = arizona_socket:handle_in(EventFrame, Socket0),
    #{~"o" := Ops} = json:decode(iolist_to_binary(Frame)),
    ?assertMatch([[?OP_TEXT, _, ~"5"], [?OP_TEXT, _, ~"6"]], Ops),
    %% The push was folded into the reply -- nothing left to double-ship.
    receive
        LeftOver -> error({push_left_in_mailbox, LeftOver})
    after 0 -> ok
    end.

queued_push_prepended_to_patch_reply(Config) when is_list(Config) ->
    %% Same causal-order guarantee on the in-place patch path: the queued info
    %% push ("5") precedes the patch's own diff ("7") in one frame.
    Req = arizona_req_test_adapter:new(#{
        routes => #{~"/rc" => {arizona_root_counter, #{bindings => #{count => 7}}}}
    }),
    {ok, Socket0} = arizona_socket:init(arizona_root_counter, #{}, Req, #{}),
    Pid = arizona_socket:live_pid(Socket0),
    Pid ! {set_count, 5},
    PatchFrame = iolist_to_binary(
        json:encode([~"patch", #{~"path" => ~"/rc", ~"qs" => <<>>}])
    ),
    {reply, Frame, _Socket1} = arizona_socket:handle_in(PatchFrame, Socket0),
    #{~"o" := Ops} = json:decode(iolist_to_binary(Frame)),
    ?assertMatch([[?OP_TEXT, _, ~"5"], [?OP_TEXT, _, ~"7"]], Ops),
    receive
        LeftOver -> error({push_left_in_mailbox, LeftOver})
    after 0 -> ok
    end.

full_navigate_drops_pending_flash(Config) when is_list(Config) ->
    %% A flash stashed for a WS-carried navigate needs a LIVE destination:
    %% do_navigate/do_patch inject it into the resolved request, but a target
    %% resolving to no live route degrades to a full-page navigation, which
    %% destroys the socket -- and a WS frame has no Set-Cookie leg, so the
    %% flash cannot follow. The socket must warn (the app-visible symptom is a
    %% silently missing flash) and clear the stash so it cannot leak into a
    %% later, unrelated navigate.
    HandlerId = ?FUNCTION_NAME,
    ok = logger:add_handler(HandlerId, arizona_test_log_handler, #{
        level => warning, config => #{pid => self()}
    }),
    try
        Req = arizona_req_test_adapter:new(#{routes => #{}}),
        {ok, Socket0} = arizona_socket:init(arizona_crashable, #{}, Req, #{}),
        %% The handler's navigate effect carries a flash opt; encode_reply
        %% strips it from the client effect and stashes it on the socket.
        EventFrame = iolist_to_binary(json:encode([~"crashable", ~"flash_navigate", #{}])),
        {reply, _EffectFrame, Socket1} = arizona_socket:handle_in(EventFrame, Socket0),
        %% The follow-up navigate resolves to no live route -> full-page nav.
        NavFrame = iolist_to_binary(
            json:encode([~"navigate", #{~"path" => ~"/show_flash", ~"qs" => <<>>}])
        ),
        {reply, _FullNavFrame, Socket2} = arizona_socket:handle_in(NavFrame, Socket1),
        receive
            {arizona_test_log_handler, #{level := warning, msg := {Fmt, Args}}} ->
                Msg = iolist_to_binary(io_lib:format(Fmt, Args)),
                ?assertMatch({_, _}, binary:match(Msg, ~"/show_flash"))
        after 1000 ->
            error(no_flash_drop_warning)
        end,
        %% The stash was cleared: a second unresolvable navigate has nothing
        %% left to drop, so it does not warn again.
        NavFrame2 = iolist_to_binary(
            json:encode([~"navigate", #{~"path" => ~"/elsewhere", ~"qs" => <<>>}])
        ),
        {reply, _FullNavFrame2, _Socket3} = arizona_socket:handle_in(NavFrame2, Socket2),
        receive
            {arizona_test_log_handler, Unexpected} -> error({unexpected_warning, Unexpected})
        after 200 -> ok
        end
    after
        ok = logger:remove_handler(HandlerId)
    end.

-module(arizona_socket_SUITE).
-include_lib("stdlib/include/assert.hrl").
-include("arizona.hrl").
-include("arizona_effect.hrl").

%% Drives arizona_socket directly (init/handle_in/handle_info) with the stub
%% request adapter -- the calling test process IS the socket process, so live
%% pushes land in its own mailbox exactly as they do in the real transport.

-export([all/0]).
-export([push_racing_navigate_dropped/1]).
-export([item_patch_carries_child_view_wrapper/1]).
-export([queued_push_prepended_to_event_reply/1]).
-export([queued_push_prepended_to_patch_reply/1]).
-export([full_navigate_drops_pending_flash/1]).
-export([navigate_keeping_layouts_stays_on_the_socket/1]).
-export([navigate_changing_layouts_forces_full_load/1]).
-export([patch_changing_layouts_forces_full_load/1]).
-export([patch_to_other_handler_changing_layouts_forces_full_load/1]).
-export([middleware_flash_replays_requested_path_on_full_load/1]).
-export([flash_replay_does_not_hijack_an_unrelated_navigation/1]).
-export([unflagged_reconnect_replies_immediately/1]).
-export([flagged_reconnect_defers_and_dedups_resync/1]).
-export([deferred_resync_flushed_by_event_frame/1]).
-export([deferred_resync_flushed_by_ping/1]).
-export([deferred_resync_timeout_flushes_undeduped/1]).
-export([unmount_skipped_when_never_mounted/1]).
-export([unmount_runs_after_mount/1]).
-export([resync_mount_crash_on_fps_frame_closes_crash/1]).
-export([resync_mount_crash_on_other_frame_closes_crash/1]).
-export([resync_mount_crash_on_timeout_closes_crash/1]).
-export([resync_on_dead_live_process_closes_going_away/1]).
-export([drain_before_mount_closes_going_away/1]).
-export([push_emitted_after_reply_not_folded/1]).
-export([child_push_scoped_to_emitting_view/1]).
-export([foreign_caller_does_not_desync_drain/1]).

all() ->
    [
        push_racing_navigate_dropped,
        item_patch_carries_child_view_wrapper,
        queued_push_prepended_to_event_reply,
        queued_push_prepended_to_patch_reply,
        full_navigate_drops_pending_flash,
        navigate_keeping_layouts_stays_on_the_socket,
        navigate_changing_layouts_forces_full_load,
        patch_changing_layouts_forces_full_load,
        patch_to_other_handler_changing_layouts_forces_full_load,
        middleware_flash_replays_requested_path_on_full_load,
        flash_replay_does_not_hijack_an_unrelated_navigation,
        unflagged_reconnect_replies_immediately,
        flagged_reconnect_defers_and_dedups_resync,
        deferred_resync_flushed_by_event_frame,
        deferred_resync_flushed_by_ping,
        deferred_resync_timeout_flushes_undeduped,
        unmount_skipped_when_never_mounted,
        unmount_runs_after_mount,
        resync_mount_crash_on_fps_frame_closes_crash,
        resync_mount_crash_on_other_frame_closes_crash,
        resync_mount_crash_on_timeout_closes_crash,
        resync_on_dead_live_process_closes_going_away,
        drain_before_mount_closes_going_away,
        push_emitted_after_reply_not_folded,
        child_push_scoped_to_emitting_view,
        foreign_caller_does_not_desync_drain
    ].

%% A `?stateful` inside a stream item re-renders when the item updates, so the item's
%% inner ops carry a `[ChildViewId, ChildOps]` wrapper -- a head that is a VIEW ID, not
%% an op code. The client's worker resolves op payloads by switching on that head, so it
%% needs a case for this shape; pin the shape here, because if the diff ever stops
%% emitting it the client's handling becomes dead code nothing would notice.
%%
%% The wrapper only appears when the child's own dynamics differ: a child fed a constant
%% re-renders identically and `make_ops/5` suppresses the empty `[VId, []]`.
item_patch_carries_child_view_wrapper(Config) when is_list(Config) ->
    Req = arizona_req_test_adapter:new(),
    {ok, Socket0} = arizona_socket:init(arizona_stream_item_child, #{}, Req, #{}),
    %% Empty the label, then refill it: the child's slot holds a conditional, so its
    %% statics change and the patch carries the whole nested template rather than a
    %% bare scalar -- the payload shape that has to be resolved before it can be applied.
    {reply, _Cleared, Socket1} = arizona_socket:handle_in(relabel(1, <<>>), Socket0),
    {reply, Frame, _Socket2} = arizona_socket:handle_in(relabel(1, ~"TWO"), Socket1),
    ItemPatch = [Op || [?OP_ITEM_PATCH | _] = Op <- ops(Frame)],
    ?assertMatch([[?OP_ITEM_PATCH, _Az, ~"1", [[~"badge-1", [_ | _]]]]], ItemPatch),
    [[?OP_ITEM_PATCH, _, _, [[_ChildId, ChildOps]]]] = ItemPatch,
    %% The child's own op, and a payload that is a template map rather than a scalar.
    ?assertMatch([[?OP_TEXT, _, #{~"f" := _}]], ChildOps).

relabel(Id, Label) ->
    iolist_to_binary(
        json:encode([~"sic", ~"relabel", #{~"id" => Id, ~"label" => Label}])
    ).

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

%% The layouts around the root view render once, at SSR, and no frame can
%% re-render them -- `?OP_REPLACE` swaps only the view INSIDE them. So a navigate
%% is servable on the socket exactly when the target keeps the same layouts, and
%% must degrade to a full page load when it doesn't. The four cases below pin
%% both sides of that line across `navigate` and `patch`.

navigate_keeping_layouts_stays_on_the_socket(Config) when is_list(Config) ->
    Layouts = [{arizona_layout, render}],
    Req = arizona_req_test_adapter:new(#{
        routes => #{~"/next" => {arizona_root_counter, #{layouts => Layouts}}}
    }),
    {ok, Socket0} = arizona_socket:init(arizona_timer, #{}, Req, #{layouts => Layouts}),
    {reply, Frame, _Socket1} = arizona_socket:handle_in(navigate_frame(~"/next"), Socket0),
    %% Same shell: the root view is replaced in place, no page load.
    ?assertMatch([[?OP_REPLACE, _ViewId, _HTML]], ops(Frame)).

navigate_changing_layouts_forces_full_load(Config) when is_list(Config) ->
    Req = arizona_req_test_adapter:new(#{
        routes => #{
            ~"/next" => {arizona_root_counter, #{layouts => [{arizona_outer_layout, render}]}}
        }
    }),
    {ok, Socket0} = arizona_socket:init(arizona_timer, #{}, Req, #{
        layouts => [{arizona_layout, render}]
    }),
    {reply, Frame, _Socket1} = arizona_socket:handle_in(navigate_frame(~"/next"), Socket0),
    %% Different shell: replacing the view in place would drop the new page into
    %% the old page's layout, so the client is told to load the URL for real.
    ?assertMatch(
        [[?EFFECT_NAVIGATE, ~"/next", #{~"full" := true}]],
        effects(Frame)
    ),
    %% ...and emphatically NOT an in-place replace.
    ?assertEqual(error, maps:find(~"o", decode(Frame))).

patch_changing_layouts_forces_full_load(Config) when is_list(Config) ->
    %% A patch keeps the view, so it keeps the shell too -- which makes a
    %% layout-changing patch even less servable than a layout-changing navigate.
    Req = arizona_req_test_adapter:new(#{
        routes => #{
            ~"/rc" => {arizona_root_counter, #{layouts => [{arizona_outer_layout, render}]}}
        }
    }),
    {ok, Socket0} = arizona_socket:init(arizona_root_counter, #{}, Req, #{
        layouts => [{arizona_layout, render}]
    }),
    {reply, Frame, _Socket1} = arizona_socket:handle_in(patch_frame(~"/rc"), Socket0),
    ?assertMatch([[?EFFECT_NAVIGATE, ~"/rc", #{~"full" := true}]], effects(Frame)).

patch_to_other_handler_changing_layouts_forces_full_load(Config) when is_list(Config) ->
    %% A patch to a DIFFERENT handler already degrades to a navigate. That
    %% navigate is a real root replace, so it has to clear the layout bar too --
    %% checking only the `navigate` frame would leave this path serving the wrong
    %% shell.
    Req = arizona_req_test_adapter:new(#{
        routes => #{~"/other" => {arizona_timer, #{layouts => [{arizona_outer_layout, render}]}}}
    }),
    {ok, Socket0} = arizona_socket:init(arizona_root_counter, #{}, Req, #{
        layouts => [{arizona_layout, render}]
    }),
    {reply, Frame, _Socket1} = arizona_socket:handle_in(patch_frame(~"/other"), Socket0),
    ?assertMatch([[?EFFECT_NAVIGATE, ~"/other", #{~"full" := true}]], effects(Frame)).

middleware_flash_replays_requested_path_on_full_load(Config) when is_list(Config) ->
    %% A halting middleware's flash rides the socket to the redirect target. When
    %% that target needs a full page load, the in-process carry dies with the
    %% socket -- but this flash is REPRODUCIBLE: re-requesting the gated path over
    %% HTTP runs the same middleware, which redirects again with a real
    %% `Set-Cookie`. So the full navigation goes to the gated path, not the
    %% target, and the message survives instead of being dropped.
    HandlerId = ?FUNCTION_NAME,
    ok = logger:add_handler(HandlerId, arizona_test_log_handler, #{
        level => warning, config => #{pid => self()}
    }),
    %% put_flash/3 signs, so the middleware needs a key even though nothing here
    %% ever reads the cookie back.
    ok = application:set_env(arizona, secret_key, ~"socket-suite-secret-key-32-bytes"),
    try
        Gate = fun(Req0, _B) ->
            Req1 = arizona_req:put_flash(Req0, error, ~"Please sign in first."),
            {halt, arizona_req:redirect(Req1, ~"/target")}
        end,
        Req = arizona_req_test_adapter:new(#{
            routes => #{
                ~"/gate" =>
                    {arizona_root_counter, #{
                        layouts => [{arizona_layout, render}],
                        middlewares => [Gate]
                    }},
                ~"/target" =>
                    {arizona_root_counter, #{layouts => [{arizona_outer_layout, render}]}}
            }
        }),
        {ok, Socket0} = arizona_socket:init(arizona_root_counter, #{}, Req, #{
            layouts => [{arizona_layout, render}]
        }),
        %% The gate shares the current shell, so the navigate is served here and
        %% the middleware halts. The client gets a bare navigate -- the flash is
        %% stashed server-side and never reaches the browser.
        {reply, HaltFrame, Socket1} =
            arizona_socket:handle_in(navigate_frame(~"/gate"), Socket0),
        ?assertMatch([[?EFFECT_NAVIGATE, ~"/target"]], effects(HaltFrame)),
        %% The follow-up frame for the redirect target crosses shells. Rather
        %% than drop the flash, the socket sends the browser back through the
        %% gate over HTTP, where the redirect can carry the flash cookie.
        {reply, FullFrame, _Socket2} =
            arizona_socket:handle_in(navigate_frame(~"/target"), Socket1),
        ?assertMatch([[?EFFECT_NAVIGATE, ~"/gate", #{~"full" := true}]], effects(FullFrame)),
        %% Nothing was dropped, so nothing was warned about.
        receive
            {arizona_test_log_handler, Unexpected} -> error({unexpected_warning, Unexpected})
        after 200 -> ok
        end
    after
        ok = application:unset_env(arizona, secret_key),
        ok = logger:remove_handler(HandlerId)
    end.

flash_replay_does_not_hijack_an_unrelated_navigation(Config) when is_list(Config) ->
    %% The replay is good for exactly one destination: the redirect the halt
    %% issued. If a second navigate beats the follow-up frame, the user is going
    %% somewhere else -- routing them back through the gate to save a flash would
    %% send them to a page they did not ask for, which is worse than losing the
    %% message. So an unrelated full navigation goes where it was asked to go and
    %% the flash drops loudly.
    HandlerId = ?FUNCTION_NAME,
    ok = logger:add_handler(HandlerId, arizona_test_log_handler, #{
        level => warning, config => #{pid => self()}
    }),
    ok = application:set_env(arizona, secret_key, ~"socket-suite-secret-key-32-bytes"),
    try
        Gate = fun(Req0, _B) ->
            Req1 = arizona_req:put_flash(Req0, error, ~"Please sign in first."),
            {halt, arizona_req:redirect(Req1, ~"/target")}
        end,
        Req = arizona_req_test_adapter:new(#{
            routes => #{
                ~"/gate" =>
                    {arizona_root_counter, #{
                        layouts => [{arizona_layout, render}],
                        middlewares => [Gate]
                    }},
                ~"/elsewhere" =>
                    {arizona_root_counter, #{layouts => [{arizona_outer_layout, render}]}}
            }
        }),
        {ok, Socket0} = arizona_socket:init(arizona_root_counter, #{}, Req, #{
            layouts => [{arizona_layout, render}]
        }),
        {reply, _HaltFrame, Socket1} =
            arizona_socket:handle_in(navigate_frame(~"/gate"), Socket0),
        %% Not the redirect target: the user asked for /elsewhere, so that is
        %% where they go -- NOT back through /gate.
        {reply, FullFrame, _Socket2} =
            arizona_socket:handle_in(navigate_frame(~"/elsewhere"), Socket1),
        ?assertMatch([[?EFFECT_NAVIGATE, ~"/elsewhere", #{~"full" := true}]], effects(FullFrame)),
        %% The undeliverable flash is dropped loudly rather than silently.
        receive
            {arizona_test_log_handler, #{level := warning, msg := {Fmt, Args}}} ->
                Msg = iolist_to_binary(io_lib:format(Fmt, Args)),
                ?assertMatch({_, _}, binary:match(Msg, ~"/elsewhere"))
        after 1000 ->
            error(no_flash_drop_warning)
        end
    after
        ok = application:unset_env(arizona, secret_key),
        ok = logger:remove_handler(HandlerId)
    end.

unflagged_reconnect_replies_immediately(Config) when is_list(Config) ->
    %% Regression pin: a reconnect WITHOUT `fps_follow` (a native client, any
    %% non-announcing one) keeps today's immediate full-page resync, statics
    %% attached -- zero added latency.
    Req = arizona_req_test_adapter:new(),
    {reply, Frame, _Socket} =
        arizona_socket:init(arizona_root_counter, #{}, Req, #{reconnect => true}),
    #{~"o" := [[?OP_REPLACE, _ViewId, Payload]]} = json:decode(iolist_to_binary(Frame)),
    ?assertMatch(#{~"f" := _, ~"s" := [_ | _]}, Payload).

flagged_reconnect_defers_and_dedups_resync(Config) when is_list(Config) ->
    %% The core of the deferred resync: a flagged reconnect's init replies
    %% NOTHING (pre-fix it replied the full page immediately, fps_follow
    %% ignored); the client's `cached_fps` frame then triggers the resync,
    %% whose payload ELIDES the statics of every announced fingerprint --
    %% the whole point of deferring.
    Req0 = arizona_req_test_adapter:new(),
    %% Reference run (unflagged) to learn the page's root fingerprint.
    {reply, RefFrame, _RefSocket} =
        arizona_socket:init(arizona_root_counter, #{}, Req0, #{reconnect => true}),
    #{~"o" := [[?OP_REPLACE, _, #{~"f" := Fp, ~"s" := [_ | _]}]]} =
        json:decode(iolist_to_binary(RefFrame)),
    %% Flagged run: no reply at init.
    Req = arizona_req_test_adapter:new(),
    {ok, Socket0} =
        arizona_socket:init(
            arizona_root_counter, #{}, Req, #{reconnect => true, fps_follow => true}
        ),
    %% The announcement flushes the resync, deduped for the announced fp.
    FpsFrame = iolist_to_binary(json:encode([~"cached_fps", [Fp]])),
    {reply, Frame, _Socket1} = arizona_socket:handle_in(FpsFrame, Socket0),
    #{~"o" := [[?OP_REPLACE, _ViewId, Payload]]} = json:decode(iolist_to_binary(Frame)),
    ?assertMatch(#{~"f" := Fp, ~"d" := _}, Payload),
    ?assertNot(is_map_key(~"s", Payload)).

deferred_resync_flushed_by_event_frame(Config) when is_list(Config) ->
    %% Protocol-violation robustness: a flagged client's first frame SHOULD be
    %% `cached_fps`, but an event racing the announcement must not reach the
    %% still-unmounted live process. The socket flushes the resync first
    %% (undeduped) and the event's own reply follows it in one `reply_many` --
    %% resync applies before the event ops on the client, in order.
    Req = arizona_req_test_adapter:new(),
    {ok, Socket0} =
        arizona_socket:init(
            arizona_root_counter, #{}, Req, #{reconnect => true, fps_follow => true}
        ),
    EventFrame = iolist_to_binary(json:encode([~"counter", ~"inc", #{}])),
    {reply_many, [ResyncFrame, ReplyFrame], _Socket1} =
        arizona_socket:handle_in(EventFrame, Socket0),
    #{~"o" := [[?OP_REPLACE, _, ResyncPayload]]} =
        json:decode(iolist_to_binary(ResyncFrame)),
    %% Undeduped: nothing was announced, so the statics ship.
    ?assertMatch(#{~"f" := _, ~"s" := [_ | _]}, ResyncPayload),
    %% The event was processed against the (just-mounted) live process.
    #{~"o" := EventOps} = json:decode(iolist_to_binary(ReplyFrame)),
    ?assertMatch([[?OP_TEXT, _, ~"1"]], EventOps).

deferred_resync_flushed_by_ping(Config) when is_list(Config) ->
    %% A heartbeat ping while the resync is pending flushes it too -- the pong
    %% follows the resync frame. (Any frame at all settles the deferral.)
    Req = arizona_req_test_adapter:new(),
    {ok, Socket0} =
        arizona_socket:init(
            arizona_root_counter, #{}, Req, #{reconnect => true, fps_follow => true}
        ),
    {reply_many, [ResyncFrame, ~"1"], _Socket1} = arizona_socket:handle_in(~"0", Socket0),
    ?assertMatch(
        #{~"o" := [[?OP_REPLACE, _, #{~"s" := [_ | _]}]]},
        json:decode(iolist_to_binary(ResyncFrame))
    ).

unmount_skipped_when_never_mounted(Config) when is_list(Config) ->
    %% A flagged reconnect leaves the live process UNMOUNTED for the whole
    %% deferral window. If the transport goes away in that window (a
    %% deploy-drain reconnect storm), the live process stops -- and its
    %% terminate/2 must not run unmount/1 for a view that never mounted: the
    %% bindings are still the raw route bindings (middleware-derived session
    %% data), so the handler's unmount head does not match, the raised
    %% `{unhandled_unmount, ...}` embeds those bindings in the crash report, and
    %% the cleanup the handler actually wanted never runs anyway.
    %%
    %% A never-mounted view never calls its handler, so nothing else in this
    %% case loads the module -- and `call_unmount/2` short-circuits on
    %% `function_exported/3`, which answers `false` for an unloaded module. A
    %% live server always has the handler loaded (its route resolved and its
    %% page rendered), so load it here rather than let the assertion depend on
    %% which case ran first.
    {module, arizona_unmount_parent} = code:ensure_loaded(arizona_unmount_parent),
    Self = self(),
    Transport = spawn(fun() ->
        Req = arizona_req_test_adapter:new(),
        {ok, Socket} = arizona_socket:init(
            arizona_unmount_parent,
            #{notify => Self},
            Req,
            #{reconnect => true, fps_follow => true}
        ),
        Self ! {live_pid, arizona_socket:live_pid(Socket)},
        await_stop()
    end),
    LivePid = await_live_pid(),
    Ref = erlang:monitor(process, LivePid),
    %% Transport exits normally mid-deferral -- the live process's transport
    %% monitor reaps it, so terminate/2 runs on a never-mounted state.
    Transport ! stop,
    receive
        {'DOWN', Ref, process, LivePid, Reason} ->
            ?assertEqual(normal, Reason)
    after 2000 ->
        error(live_process_did_not_stop)
    end,
    %% Nothing was mounted, so nothing may be unmounted.
    receive
        {root_unmounted, _} = Msg -> error({spurious_unmount, Msg})
    after 0 -> ok
    end.

unmount_runs_after_mount(Config) when is_list(Config) ->
    %% Control for unmount_skipped_when_never_mounted: an unflagged reconnect
    %% mounts at init, so the same transport exit MUST unmount -- children
    %% first, then the root.
    Self = self(),
    Transport = spawn(fun() ->
        Req = arizona_req_test_adapter:new(),
        {reply, _Frame, Socket} = arizona_socket:init(
            arizona_unmount_parent, #{notify => Self}, Req, #{reconnect => true}
        ),
        Self ! {live_pid, arizona_socket:live_pid(Socket)},
        await_stop()
    end),
    LivePid = await_live_pid(),
    Ref = erlang:monitor(process, LivePid),
    Transport ! stop,
    receive
        {'DOWN', Ref, process, LivePid, Reason} -> ?assertEqual(normal, Reason)
    after 2000 -> error(live_process_did_not_stop)
    end,
    ?assertEqual({child_unmounted, ~"uchild"}, await_unmount()),
    ?assertEqual({root_unmounted, ~"uparent"}, await_unmount()).

await_live_pid() ->
    receive
        {live_pid, Pid} -> Pid
    after 2000 -> error(no_live_pid)
    end.

%% Keeps a stand-in transport process alive until the case tells it to exit
%% (normally, so the live process's transport monitor is what reaps the view).
%% The timeout only bounds a case that fails before signalling.
await_stop() ->
    receive
        stop -> ok
    after 30000 -> ok
    end.

await_unmount() ->
    receive
        {child_unmounted, _} = Msg -> Msg;
        {root_unmounted, _} = Msg -> Msg
    after 2000 -> error(timeout_waiting_for_unmount)
    end.

deferred_resync_timeout_flushes_undeduped(Config) when is_list(Config) ->
    %% Backstop: a flagged client that never sends its promised announcement.
    %% The armed timer targets the socket process (this test process), so the
    %% real `arizona_resync_timeout` message arrives here; feeding it to
    %% handle_info flushes the resync undeduped.
    Req = arizona_req_test_adapter:new(),
    {ok, Socket0} =
        arizona_socket:init(
            arizona_root_counter, #{}, Req, #{reconnect => true, fps_follow => true}
        ),
    receive
        arizona_resync_timeout = Msg ->
            {reply, Frame, Socket1} = arizona_socket:handle_info(Msg, Socket0),
            ?assertMatch(
                #{~"o" := [[?OP_REPLACE, _, #{~"f" := _, ~"s" := [_ | _]}]]},
                json:decode(iolist_to_binary(Frame))
            ),
            %% A stale timeout after the flush is ignored (already resynced).
            ?assertEqual({ok, Socket1}, arizona_socket:handle_info(Msg, Socket1))
    after 2000 ->
        error(resync_timeout_never_fired)
    end.

resync_mount_crash_on_fps_frame_closes_crash(Config) when is_list(Config) ->
    %% The deferred resync is the one mount path that ran outside a crash guard.
    %% A raise in mount/1, on_mount, or render/1 escaped handle_in/2 and killed
    %% the ws_session with no close frame, so the client saw a bare 1006 and
    %% backed off forever instead of the 4500 close its purpose-built
    %% `crashReload()` guard exists for. The conforming leg -- the promised
    %% `cached_fps` frame -- must close 4500 like every sibling mount path.
    Socket = crashing_flagged_socket(),
    FpsFrame = iolist_to_binary(json:encode([~"cached_fps", []])),
    ?assertMatch({close, 4500, ~"server crash", _}, arizona_socket:handle_in(FpsFrame, Socket)).

resync_mount_crash_on_other_frame_closes_crash(Config) when is_list(Config) ->
    %% Protocol-violation leg: a non-`cached_fps` first frame flushes the resync
    %% undeduped before its own reply. A crash there must close 4500 too -- and
    %% the close outranks the frame's reply, so nothing is shipped alongside it.
    Socket = crashing_flagged_socket(),
    EventFrame = iolist_to_binary(
        json:encode([~"crashable", ~"set_status", #{~"value" => ~"x"}])
    ),
    ?assertMatch({close, 4500, ~"server crash", _}, arizona_socket:handle_in(EventFrame, Socket)).

resync_mount_crash_on_timeout_closes_crash(Config) when is_list(Config) ->
    %% Backstop leg: the flagged client never announced, so the timer flushes
    %% the resync from handle_info/2 -- equally unguarded before the fix.
    Socket = crashing_flagged_socket(),
    ?assertMatch(
        {close, 4500, ~"server crash", _},
        arizona_socket:handle_info(arizona_resync_timeout, Socket)
    ).

resync_on_dead_live_process_closes_going_away(Config) when is_list(Config) ->
    %% Drain/exit race: the resync flush reaches a live process that already
    %% exited (a listener drain landing inside the deferral window). Its sibling
    %% mount paths (do_navigate/do_patch) translate that into a 1001 going-away
    %% close so the client's form-state-preserving reconnect runs; the flush
    %% raised a bare `{noproc, ...}` out of handle_in/2 instead.
    Req = arizona_req_test_adapter:new(),
    {ok, Socket} = arizona_socket:init(
        arizona_root_counter, #{}, Req, #{reconnect => true, fps_follow => true}
    ),
    Pid = arizona_socket:live_pid(Socket),
    Ref = erlang:monitor(process, Pid),
    ok = gen_server:stop(Pid),
    receive
        {'DOWN', Ref, process, Pid, _Reason} -> ok
    after 2000 -> error(live_process_did_not_stop)
    end,
    FpsFrame = iolist_to_binary(json:encode([~"cached_fps", []])),
    ?assertMatch({close, 1001, <<>>, _}, arizona_socket:handle_in(FpsFrame, Socket)).

drain_before_mount_closes_going_away(Config) when is_list(Config) ->
    %% A listener drain broadcast landing inside the deferred reconnect window
    %% reaches a live process that has not mounted yet, where the catch-all
    %% pre-mount drop swallowed it: `handle_drain/2` never ran, no
    %% `{shutdown, drain}` exit reached the socket, so the client never got the
    %% 1001 that runs its form-state-preserving reconnect -- while the transport
    %% had already acknowledged the drain, leaving the listener to count it
    %% handled and hard-kill the connection at the deadline.
    Req = arizona_req_test_adapter:new(),
    {ok, Socket} = arizona_socket:init(
        arizona_root_counter, #{}, Req, #{reconnect => true, fps_follow => true}
    ),
    Pid = arizona_socket:live_pid(Socket),
    Pid ! {arizona_drain, erlang:monotonic_time(millisecond) + 5000},
    receive
        {'EXIT', Pid, Reason} = Exit ->
            ?assertEqual({shutdown, drain}, Reason),
            ?assertMatch({close, 1001, <<>>, _}, arizona_socket:handle_info(Exit, Socket))
    after 1000 ->
        error(drain_swallowed_before_mount)
    end.

push_emitted_after_reply_not_folded(Config) when is_list(Config) ->
    %% The drain folds queued pushes in FRONT of a synchronous reply, which is
    %% only sound for pushes the live process emitted BEFORE that reply. The
    %% `?send` self-message idiom breaks that assumption: the handler enqueues
    %% to the live process's own mailbox inside handle_event/3, the live process
    %% replies, and only then dequeues and pushes -- all before the socket
    %% process is rescheduled to its `after 0`. Prepending that push inverts an
    %% order-dependent op pair: the wire carried MOVE-then-INSERT where the
    %% server meant INSERT-then-MOVE, and the client drops a move whose key it
    %% has not seen, leaving the server's snapshot wrong for good.
    Req = arizona_req_test_adapter:new(),
    {ok, Socket0} = arizona_socket:init(arizona_stream_self_send, #{}, Req, #{}),
    %% Repeat the interleave: the race is what the fix removes, so one trial
    %% only proves the fix when it happens to lose.
    interleave_add_then_move(Socket0, lists:seq(1, 20)).

interleave_add_then_move(_Socket, []) ->
    ok;
interleave_add_then_move(Socket0, [N | Rest]) ->
    Id = <<"k", (integer_to_binary(N))/binary>>,
    Frame = iolist_to_binary(
        json:encode([~"self_send", ~"add_then_move", #{~"id" => Id}])
    ),
    {reply, ReplyFrame, Socket} = arizona_socket:handle_in(Frame, Socket0),
    #{~"o" := ReplyOps} = json:decode(iolist_to_binary(ReplyFrame)),
    %% The reply carries the INSERT alone -- the MOVE was emitted after it.
    ?assertMatch([[?OP_INSERT, _, Id, _Pos, _HTML]], ReplyOps),
    %% ...and is not lost: it follows in its own frame, after the insert, with a
    %% `null` after-ref (move to the front).
    receive
        {arizona_push, _, _, _} = Push ->
            {reply, PushFrame, _Socket1} = arizona_socket:handle_info(Push, Socket),
            #{~"o" := PushOps} = json:decode(iolist_to_binary(PushFrame)),
            ?assertMatch([[?OP_MOVE, _, Id, null]], PushOps)
    after 2000 ->
        error({move_push_never_arrived, Id})
    end,
    interleave_add_then_move(Socket, Rest).

child_push_scoped_to_emitting_view(Config) when is_list(Config) ->
    %% A `?send`/`?send_after`-driven update to an embedded child pushed the
    %% child's own (child-relative) ops tagged with the ROOT view id, so the
    %% socket scoped them `<root>:<childAz>`. `az` is fingerprint-derived, so
    %% two instances of the same handler carry identical `az` values and only
    %% the view id separates them -- a tick meant for one twin patched the
    %% other. (The event path is already correct: it scopes with the child id
    %% the frame named.)
    Req = arizona_req_test_adapter:new(),
    {ok, Socket} = arizona_socket:init(arizona_twin_parent, #{}, Req, #{}),
    Pid = arizona_socket:live_pid(Socket),
    TargetA = child_push_target(Pid, Socket, ~"twin_a"),
    TargetB = child_push_target(Pid, Socket, ~"twin_b"),
    [ViewIdA, AzA] = binary:split(TargetA, ~":"),
    [ViewIdB, AzB] = binary:split(TargetB, ~":"),
    %% The premise: identical handler, identical slot address.
    ?assertEqual(AzA, AzB),
    %% ...so the view id is the only thing that can route the patch.
    ?assertEqual(~"twin_a", ViewIdA),
    ?assertEqual(~"twin_b", ViewIdB).

%% Drive one embedded child's handle_info/2 and return the scoped target its
%% push produced on the wire.
child_push_target(Pid, Socket, ViewId) ->
    Pid ! {arizona_view, ViewId, close},
    receive
        {arizona_push, _, _, _} = Push ->
            {reply, Frame, _Socket} = arizona_socket:handle_info(Push, Socket),
            #{~"o" := [[?OP_TEXT, Target, _Value]]} = json:decode(iolist_to_binary(Frame)),
            Target
    after 2000 ->
        error({no_child_push, ViewId})
    end.

foreign_caller_does_not_desync_drain(Config) when is_list(Config) ->
    %% `arizona_live:handle_event/4` and `patch/2` are exported, so a process
    %% other than the socket can call them on a live process whose transport
    %% folds queued pushes. A marker emitted for such a call is one nobody
    %% drains, and the offset would persist FOREVER: every later drain eats the
    %% previous cycle's marker, folds nothing, and lets a queued push ship in a
    %% later frame over a newer value. The marker is emitted only for a call made
    %% by the transport itself, so the socket's mailbox never holds a stray one.
    Req = arizona_req_test_adapter:new(),
    {ok, Socket0} = arizona_socket:init(arizona_root_counter, #{}, Req, #{}),
    Pid = arizona_socket:live_pid(Socket0),
    Self = self(),
    %% A foreign process drives the same live process.
    Foreign = spawn(fun() ->
        {ok, _Ops, _Effects} = arizona_live:handle_event(Pid, ~"counter", ~"inc", #{}),
        Self ! foreign_done
    end),
    receive
        foreign_done -> ok
    after 2000 -> error({foreign_caller_stuck, Foreign})
    end,
    %% The socket's own cycle still folds correctly: an info push emitted before
    %% the reply is prepended to it, in causal order, in ONE frame.
    Pid ! {set_count, 5},
    EventFrame = iolist_to_binary(json:encode([~"counter", ~"inc", #{}])),
    {reply, Frame, _Socket1} = arizona_socket:handle_in(EventFrame, Socket0),
    #{~"o" := Ops} = json:decode(iolist_to_binary(Frame)),
    ?assertMatch([[?OP_TEXT, _, ~"5"], [?OP_TEXT, _, ~"6"]], Ops),
    %% Nothing stray left behind for the next cycle to trip over.
    receive
        LeftOver -> error({unexpected_mailbox_message, LeftOver})
    after 0 -> ok
    end.

navigate_frame(Path) ->
    iolist_to_binary(json:encode([~"navigate", #{~"path" => Path, ~"qs" => <<>>}])).

patch_frame(Path) ->
    iolist_to_binary(json:encode([~"patch", #{~"path" => Path, ~"qs" => <<>>}])).

decode(Frame) ->
    json:decode(iolist_to_binary(Frame)).

ops(Frame) ->
    #{~"o" := Ops} = decode(Frame),
    Ops.

effects(Frame) ->
    #{~"e" := Effects} = decode(Frame),
    Effects.

%% A flagged-reconnect socket whose live process crashes the moment the deferred
%% resync mounts it. `init/4` defers the mount, so the crash lands in the flush,
%% never at init.
crashing_flagged_socket() ->
    Req = arizona_req_test_adapter:new(),
    {ok, Socket} = arizona_socket:init(
        arizona_crashable,
        #{crash_on_mount => true},
        Req,
        #{reconnect => true, fps_follow => true}
    ),
    Socket.

-module(arizona_socket).
-moduledoc """
Bridges a WebSocket frame stream with the live process.

The transport layer (`arizona_roadrunner_ws`) creates a socket
via `init/4`, then forwards inbound text frames to `handle_in/2` and
inbox messages to `handle_info/2`. Each call returns a result tuple
that the transport translates into WebSocket frames or close codes.

## Wire protocol

Inbound text frames are JSON arrays:

```
[~"cached_fps", FpList]                           %% client tells us which fingerprints it has
[~"navigate", #{~"path" := Path, ~"qs" := Qs}]    %% SPA navigation request
[ViewId, Event, Payload]                          %% UI event
~"0"                                              %% ping (replied with ~"1")
```

Outbound text frames are JSON maps with keys `~"o"` (ops), `~"e"`
(effects), and/or `~"a"` (newly proven `az-*` attribute names -- the connect
frame carries the compile-time walk of the route's handler and layouts, any
later frame only the per-socket delta; see `arizona_event_attrs`). `~"o"` and
`~"e"` are arrays produced by `arizona_diff` and `arizona_js` respectively.

## Exit handling

The live process is linked. Exits map to WebSocket close codes:

- `normal`, `shutdown`, `{shutdown, _other_}` -- graceful close
  `1000`. Client does NOT auto-reconnect (treats it as a deliberate
  end of session).
- `{shutdown, drain}` -- graceful close `1001` ("going away"). Client
  auto-reconnects via the form-state-preserving path; new live
  process mounts against the new server version. A frame whose call
  into the live process finds it gone (a drain landing while the frame
  is in flight) is dropped, and the close comes from the queued exit,
  after any push the process sent before exiting -- a drain's effects
  among them.
- Anything else (including raises in `handle_in/2` and `handle_info/2`,
  among them a frame that cannot be encoded) -- crash close `4500`: the
  view crashed after it mounted. Client remounts it in place by reconnecting.

A failure to mount the view closes `4501` instead, whatever the exit: the
connect's mount (`init/4`), a reconnect's deferred resync mount, and a
navigate -- each up to the frame that delivers the view.
The page itself may be stale (a deploy changed the route or the assets), so
the client reloads it after a jittered delay.
""".

-include("arizona.hrl").
-include("arizona_effect.hrl").

-ifdef(TEST).
-include_lib("eunit/include/eunit.hrl").
-endif.

%% --------------------------------------------------------------------
%% API function exports
%% --------------------------------------------------------------------

-export([init/4]).
-export([handle_in/2]).
-export([handle_info/2]).
-export([live_pid/1]).

%% --------------------------------------------------------------------
%% Ignore elvis warnings
%% --------------------------------------------------------------------

-ifdef(TEST).
%% Inline EUnit tests intentionally repeat op tuples (input vs expected
%% scoped output) for readability of the assertions.
-elvis([{elvis_style, dont_repeat_yourself, disable}]).
-endif.

%% --------------------------------------------------------------------
%% Macros
%% --------------------------------------------------------------------

-define(OPS, ~"o").
%% The `az-*` attribute names newly proven for this socket: the compile-time
%% walk on the connect frame, per-socket deltas after (see arizona_event_attrs).
-define(AZ_ATTRS, ~"a").
-define(EFFECTS, ~"e").
-define(SYS_PING, ~"0").
-define(SYS_PONG, ~"1").
-define(CLOSE_GOING_AWAY, 1001).
-define(CLOSE_CRASH, 4500).
-define(CLOSE_MOUNT_FAILED, 4501).
%% Backstop for a deferred reconnect resync (`_az_fps_follow`): a client that
%% set the flag promises its `cached_fps` frame as the first frame after open,
%% typically in flight already -- the timeout only covers one that then sends
%% nothing at all, so the resync still goes out (undeduped) instead of leaving
%% the page stale forever. Generous next to a frame's real arrival time (one
%% network round trip plus the client's IndexedDB cache hydration).
-define(RESYNC_TIMEOUT_MS, 1000).

%% --------------------------------------------------------------------
%% Records
%% --------------------------------------------------------------------

-record(socket, {
    pid :: pid() | undefined,
    view_id :: binary() | undefined,
    handler :: module() | undefined,
    req :: az:request(),
    %% The layouts that wrapped the page this socket is attached to. Only the
    %% root VIEW is replaceable over the wire (`?OP_REPLACE`); the layouts around
    %% it rendered once at SSR and no frame can re-render them. So a navigate to
    %% a route whose layouts differ cannot be served in place at all -- it
    %% degrades to a full-page navigation (`layout_changed/2`).
    %%
    %% Set once at connect and never refreshed, which is exact rather than
    %% convenient: the only navigate that could change it is the one that forces
    %% a full page load, and that destroys this socket.
    layouts = [] :: [arizona_render:layout()],
    %% Delivery bookkeeping for the declared az-* set (see arizona_event_attrs):
    %% modules already walked, names already sent, and names proven since the
    %% last outgoing frame. `az_pending` is attached to the next outgoing frame
    %% (encode_reply_1/3, the connect/resync encodes) and cleared, so a delta
    %% always rides the same frame as the render that proved it.
    az_walked = #{} :: #{module() => true},
    az_sent = #{} :: #{binary() => true},
    az_pending = [] :: [binary()],
    %% One-shot flash carried in-process across an SPA navigate/patch. A WebSocket
    %% frame has no `Set-Cookie` leg, so a flash set by a halting middleware (or an
    %% `arizona_js:navigate`/`patch` `flash` opt) rides the socket to the follow-up
    %% frame, where `take_pending_flash/2` injects it into the resolved request.
    %% Delivery therefore requires the follow-up target to stay on this socket: a
    %% target that degrades to a full-page navigation destroys it, and the flash
    %% then either replays (see `flash_replay`) or is dropped with a warning
    %% (`drop_pending_flash/2`).
    pending_flash = #{} :: arizona_req:flash(),
    %% `{RedirectTarget, RequestedPath}` for a `pending_flash` a halting
    %% middleware produced. Such a flash is reproducible: re-requesting
    %% `RequestedPath` over HTTP runs the same middlewares, and the HTTP leg has
    %% the `Set-Cookie` the socket lacks. So a full-page navigation heading for
    %% `RedirectTarget` that would otherwise drop the flash navigates to
    %% `RequestedPath` instead and lets the halt replay properly
    %% (`full_navigate/3`). The target is held so the replay fires ONLY for the
    %% redirect it belongs to -- a full navigation elsewhere is the user going
    %% elsewhere, and rerouting it would hijack the navigation.
    %%
    %% `undefined` for a flash an in-view handler set via an
    %% `arizona_js:navigate`/`patch` `flash` opt -- that one is live-process
    %% state with no request-side generator, so it cannot be replayed.
    flash_replay = undefined :: undefined | {binary(), binary()},
    %% Deferred reconnect resync (`_az_fps_follow`): the backstop timer ref
    %% while the socket waits for the client's `cached_fps` frame before
    %% mounting + rendering the full-page replace, so the payload dedups
    %% against the announced fingerprints. `undefined` once flushed (or when
    %% the connection never deferred).
    pending_resync = undefined :: undefined | reference(),
    %% A mount call (a deferred resync, a navigate) failed with the live process
    %% exiting under it. The frame was dropped so the exit closes in order behind
    %% the process's last pushes (crash_close/4); a crash exit then means the
    %% view could not mount, so it closes 4501 rather than 4500.
    mount_failed = false :: boolean()
}).

%% --------------------------------------------------------------------
%% Types exports
%% --------------------------------------------------------------------

-export_type([socket/0]).
-export_type([result/0]).

%% --------------------------------------------------------------------
%% Types definitions
%% --------------------------------------------------------------------

-opaque socket() :: #socket{}.

-nominal result() ::
    {ok, socket()}
    | {reply, iodata(), socket()}
    | {reply_many, [iodata()], socket()}
    | {close, pos_integer(), binary(), socket()}.

%% --------------------------------------------------------------------
%% API Functions
%% --------------------------------------------------------------------

-doc """
Creates a socket for `Handler` with the given `Bindings` and `Req`,
and starts its live process.

`Opts` may include:
- `reconnect` -- if `true`, renders the page and replies with an
  `?OP_REPLACE` op (used when the client is reconnecting after a
  network drop)
- `fps_follow` -- if `true` (with `reconnect`), the client promised its
  `cached_fps` frame as the first frame after open, so the resync render
  is DEFERRED until that frame arrives and its payload dedups statics
  against the announced fingerprints -- the difference between re-shipping
  a full page's statics to every client of a deploy-drain reconnect storm
  and shipping roughly the dynamics. A backstop timer
  (`?RESYNC_TIMEOUT_MS`) flushes the resync undeduped for a flagged
  client that never sends the frame; a client without the flag (a native
  client, any non-announcing one) keeps the immediate resync -- zero
  penalty.
- `on_mount` -- list of `t:arizona_live:on_mount/0` hooks
- `layouts` -- the route's `t:arizona_render:layout/0` list, i.e. what
  wrapped this page at SSR. A later navigate/patch to a route whose
  layouts differ cannot be served over the wire and degrades to a
  full-page navigation

The route adapter (used by SPA navigate to resolve new routes) is
recovered from `Req` itself via `arizona_req:adapter/1`.
""".
-spec init(Handler, Bindings, Req, Opts) -> result() when
    Handler :: module(),
    Bindings :: map(),
    Req :: az:request(),
    Opts :: map().
init(Handler, Bindings, Req, Opts) ->
    Reconnect = maps:get(reconnect, Opts, false),
    FpsFollow = maps:get(fps_follow, Opts, false),
    OnMount = maps:get(on_mount, Opts, []),
    Capabilities = maps:get(capabilities, Opts, #{}),
    Layouts = maps:get(layouts, Opts, []),
    %% Track the root handler so a `patch` frame can decide same-view (patch in
    %% place) vs different-view (fall back to a full navigate/replace), and the
    %% layouts so either can tell an in-place swap from one that needs the whole
    %% page rebuilt.
    Socket = #socket{req = Req, handler = Handler, layouts = Layouts},
    safe_init(Handler, Socket, fun() ->
        %% `push_barrier` opts this transport into the ordering marker the
        %% event/patch/navigate drain below relies on -- see drain_pending_pushes/1.
        ConnInfo = #{
            capabilities => Capabilities, reconnect => Reconnect, push_barrier => true
        },
        {ok, Pid} = arizona_live:start_link(Handler, Bindings, self(), OnMount, ConnInfo),
        init_view(Reconnect, FpsFollow, Handler, Pid, Socket)
    end).

%% The three connect shapes, by reconnect and the client's fingerprint-follow
%% promise.
%%
%% A flagged reconnect defers the whole mount+render to the resync flush: the
%% mount must not run twice, and no frame can reach the unmounted live process
%% -- handle_in flushes before processing any frame, and an unmounted process
%% has no subscriptions or timers to push from.
%% Every clause ships `?AZ_ATTRS`, including the two that otherwise have nothing to
%% say. The client delegates a DOM event type only for a name in this set, so a
%% connect that sent no frame would leave the page unable to answer its own first
%% event -- and a first connect (the third clause) is exactly the case that used to
%% reply with nothing at all. The walk roots are the handler AND the layout
%% modules: a layout renders az-* markup too, once at SSR, and no later frame can
%% carry a layout's names.
init_view(true, true, Handler, Pid, #socket{layouts = Layouts} = Socket0) ->
    TRef = erlang:send_after(?RESYNC_TIMEOUT_MS, self(), arizona_resync_timeout),
    {Attrs, Socket} = az_connect(az_roots(Handler, Layouts), {[], []}, Socket0),
    {reply, encode(#{?AZ_ATTRS => Attrs}), Socket#socket{
        pid = Pid, pending_resync = TRef
    }};
init_view(true, false, Handler, Pid, #socket{layouts = Layouts} = Socket0) ->
    {ok, ViewId, PageHTML, Observed} = arizona_live:mount_and_render(Pid),
    Ops = replace_ops(ViewId, PageHTML),
    {Attrs, Socket} = az_connect(az_roots(Handler, Layouts), Observed, Socket0),
    {reply, encode(#{?OPS => Ops, ?AZ_ATTRS => Attrs}), Socket#socket{
        pid = Pid, view_id = ViewId
    }};
init_view(false, _FpsFollow, Handler, Pid, #socket{layouts = Layouts} = Socket0) ->
    {ok, ViewId, Observed} = arizona_live:mount(Pid),
    {Attrs, Socket} = az_connect(az_roots(Handler, Layouts), Observed, Socket0),
    {reply, encode(#{?AZ_ATTRS => Attrs}), Socket#socket{
        pid = Pid, view_id = ViewId
    }}.

-doc """
Handles an inbound text frame.

While a deferred reconnect resync is pending (`fps_follow`, see `init/4`),
the FIRST frame settles it: the promised `[~"cached_fps", FpList]` seeds
the fingerprints and the resync replies deduped; any other frame flushes
the resync undeduped first, its own reply following in the same result
(`reply_many`), so no frame ever reaches the still-unmounted live process.

Recognized payloads:
- `~"0"` -- ping, replied with `~"1"`
- `[~"cached_fps", FpList]` -- seeds fingerprints into the live process
- `[~"navigate", #{~"path" := Path, ~"qs" := Qs}]` -- SPA navigation (replace)
- `[~"patch", #{~"path" := Path, ~"qs" := Qs}]` -- in-place SPA navigation
- `[ViewId, Event, Payload]` -- UI event dispatch (`Event` a binary,
  `Payload` a map)

Unrecognized payloads, and frames whose body is not valid JSON, are
silently dropped (a single bad frame must not crash the socket). An event
frame whose `Payload` is not a map is dropped the same way, so a crafted
non-map payload can't reach a `#{...}`-matching handler and crash it.
""".
-spec handle_in(Frame, Socket) -> result() when
    Frame :: binary(),
    Socket :: socket().
handle_in(Frame, Socket) ->
    try
        do_handle_in(Frame, Socket)
    catch
        Class:Reason:Stacktrace -> crash_close(Class, Reason, Stacktrace, Socket)
    end.

do_handle_in(Frame, #socket{pending_resync = TRef, pid = Pid} = Socket0) when
    is_reference(TRef)
->
    %% Deferred reconnect resync: the first inbound frame settles it. The
    %% conforming case is the promised `cached_fps` -- seed the announced
    %% fingerprints (a cast; processed before the mount_and_render call below,
    %% same-sender ordering) so the resync payload elides their statics. ANY
    %% other frame (an event racing the announcement, a ping) flushes the
    %% resync first, undeduped -- the live process is not mounted until the
    %% flush, so nothing may reach it earlier -- and the frame's own reply
    %% follows the resync frame on the wire (`reply_many`).
    ok = erlang:cancel_timer(TRef, [{info, false}]),
    Socket1 = Socket0#socket{pending_resync = undefined},
    case decode_cached_fps(Frame) of
        {ok, FpList} ->
            ok = arizona_live:seed_fps(Pid, FpList),
            resync_reply(flush_resync(Socket1));
        error ->
            resync_before_frame(flush_resync(Socket1), Frame)
    end;
do_handle_in(?SYS_PING, Socket) ->
    {reply, ?SYS_PONG, Socket};
do_handle_in(JSON, #socket{pid = Pid, view_id = RootViewId} = Socket) ->
    %% `try ... of` so the catch fires ONLY on a malformed decode -- exceptions
    %% raised inside the matched clause bodies are not caught here, so a genuine
    %% handler crash reaches handle_in/2's guard and still closes 4500.
    try json:decode(JSON) of
        [~"cached_fps", FpList] when is_list(FpList) ->
            arizona_live:seed_fps(Pid, FpList),
            {ok, Socket};
        [~"navigate", #{~"path" := Path, ~"qs" := Qs}] when
            is_binary(Path), is_binary(Qs)
        ->
            handle_navigate(Path, Qs, Socket);
        [~"patch", #{~"path" := Path, ~"qs" := Qs}] when
            is_binary(Path), is_binary(Qs)
        ->
            handle_patch(Path, Qs, Socket);
        [Target, Event, Payload] when is_binary(Event), is_map(Payload) ->
            ViewId = event_target(Target, RootViewId),
            {AllOps, AllEffects, Observed} = dispatch_event(Pid, ViewId, Event, Payload),
            reply_after_pending(AllOps, AllEffects, Observed, Socket);
        _ ->
            {ok, Socket}
    catch
        %% Malformed JSON (e.g. a scanner/probe, a corrupted or fragmented
        %% frame, a stale client). `json:decode/1` raises `error:{invalid_byte,
        %% _}` / `error:unexpected_end` / `error:{unexpected_sequence, _}`. Drop
        %% the frame like an unrecognized payload rather than crashing the
        %% socket -- one bad frame must not tear down a live session.
        error:_ ->
            {ok, Socket}
    end.

-doc """
Returns the live process pid backing this socket, or `undefined` if
the socket was constructed without one (test fixtures).
""".
-spec live_pid(socket()) -> pid() | undefined.
live_pid(#socket{pid = Pid}) -> Pid.

-doc """
Handles inbox messages forwarded by the transport.

Routes `{arizona_push, ViewId, Ops, Effects}` from the live process into
a reply frame, its ops tagged with the view id the push names. A push the old
page emitted before a navigate this socket drives never gets here: its ops are
folded into the navigate's frame ahead of the replace and its effects dropped
(`drain_pending_pushes/1`). Handles
`'EXIT'` from the linked live process per the mapping in this module's
"Exit handling" section.
""".
-spec handle_info(Info, Socket) -> result() when
    Info :: term(),
    Socket :: socket().
handle_info(Info, Socket) ->
    try
        do_handle_info(Info, Socket)
    catch
        Class:Reason:Stacktrace -> crash_close(Class, Reason, Stacktrace, Socket)
    end.

do_handle_info(arizona_resync_timeout, #socket{pending_resync = TRef} = Socket0) when
    is_reference(TRef)
->
    %% Backstop: the flagged client never sent its promised `cached_fps`.
    %% Resync undeduped rather than leaving the page stale forever. (A stale
    %% timeout message -- the resync already flushed by a frame that raced the
    %% firing timer -- has `pending_resync = undefined` and falls through to
    %% the catch-all below.)
    resync_reply(flush_resync(Socket0#socket{pending_resync = undefined}));
do_handle_info({arizona_push, ViewId, Ops, Effects, Observed}, Socket) ->
    encode_reply(flatten_ops(ViewId, Ops), Effects, note_az([], Observed, Socket));
do_handle_info({'EXIT', Pid, {shutdown, drain}}, #socket{pid = Pid} = Socket) ->
    %% Drain-initiated graceful exit. Close with 1001 (going away) so the
    %% JS client's auto-reconnect path runs (Worker treats any non-1000
    %% code as reconnectable; main thread preserves form state). Matches
    %% RFC 6455 §7.4 semantics for "server going away".
    {close, ?CLOSE_GOING_AWAY, <<>>, Socket};
do_handle_info({'EXIT', Pid, normal}, #socket{pid = Pid} = Socket) ->
    {close, 1000, <<>>, Socket};
do_handle_info({'EXIT', Pid, shutdown}, #socket{pid = Pid} = Socket) ->
    %% OTP graceful shutdown (atom form, e.g. supervisor-initiated).
    %% Close 1000 — same as `normal`, no reconnect. User code that
    %% wants reconnect on a custom shutdown should use the
    %% `{shutdown, drain}` reason explicitly.
    {close, 1000, <<>>, Socket};
do_handle_info({'EXIT', Pid, {shutdown, _}}, #socket{pid = Pid} = Socket) ->
    %% OTP graceful shutdown (tuple form, custom reason). Same
    %% close-1000 semantics as `shutdown` atom; only `{shutdown, drain}`
    %% (matched above) opts into the reconnect path.
    {close, 1000, <<>>, Socket};
do_handle_info({'EXIT', Pid, Reason}, #socket{pid = Pid, mount_failed = true} = Socket) ->
    logger:error("Live process ~p crashed while mounting: ~p", [Pid, Reason]),
    close_mount_failed(Socket);
do_handle_info({'EXIT', Pid, Reason}, #socket{pid = Pid} = Socket) ->
    logger:error("Live process ~p crashed: ~p", [Pid, Reason]),
    close_crash(Socket);
do_handle_info(_Info, Socket) ->
    {ok, Socket}.

%% --------------------------------------------------------------------
%% Internal functions
%% --------------------------------------------------------------------

safe_init(Handler, Socket, Fun) ->
    process_flag(trap_exit, true),
    try
        Fun()
    catch
        error:{non_utf8_render, _, _} = Reason:Stacktrace ->
            ok = log_crash(error, Reason, Stacktrace, Socket),
            close_mount_failed(Socket);
        Class:Reason:Stacktrace ->
            logger:error("~s in ~p:~p~n~p", [Class, Handler, Reason, Stacktrace]),
            close_mount_failed(Socket)
    end.

close_crash(Socket) ->
    {close, ?CLOSE_CRASH, ~"server crash", Socket}.

close_mount_failed(Socket) ->
    {close, ?CLOSE_MOUNT_FAILED, ~"mount failed", Socket}.

%% The catch body of handle_in/2 and handle_info/2. The transport has no catch
%% of its own, so a raise there -- a handler crash, or a frame that cannot be
%% JSON-encoded -- would end the session with no close frame (a bare 1006).
%% Close 4500 like every other crash instead. The trys sit inline in both entry
%% points so no closure is built per frame.
%%
%% A call into the socket's own live process that raised with the process no
%% longer alive (gone before the call, or exited under it) is not a socket
%% crash: the process's own exit decides. The socket is linked and traps exits,
%% so that `{'EXIT', Pid, _}` reaches this mailbox behind every push the process
%% sent before exiting (signals from one process arrive in order) -- a drain's
%% `handle_drain/2` effects push in particular. So the frame is dropped and the
%% transport left to deliver both in order: the push ships, then the exit closes
%% per the mapping (1000, 1001 for `{shutdown, drain}`, or a logged 4500 -- 4501
%% when the failed call was a mount, see mount_call/1). A
%% failed call to a process still alive gets no exit to wait for, so it is a
%% crash, as is a `noproc` from any other process a middleware or route adapter
%% calls.
%%
%% The socket returned is the one the frame entered with, so a deferred resync
%% the same frame flushed before its own call failed is discarded: pushes that
%% resync's mount queued may ship against the stale page just before the close.
%% Brief -- the close follows them.
crash_close(Class, Reason, Stacktrace, Socket) ->
    case live_call_failed(Class, Reason, Socket) of
        true ->
            {ok, Socket#socket{mount_failed = mount_call(Reason)}};
        false ->
            ok = log_crash(Class, Reason, Stacktrace, Socket),
            close_crash(Socket)
    end.

%% A frame refused for bytes that are not UTF-8 (encode/1) names the value but
%% not the template line: the frame has left the values behind. In dev mode
%% arizona_utf8:locate_live/3 renders the live views again to find it; either way
%% the log carries the formatted reason, which names the fix.
log_crash(error, {non_utf8_render, _, _} = Reason0, Stacktrace0, #socket{pid = Pid}) ->
    {Reason, Stacktrace} = arizona_utf8:locate_live(Reason0, Stacktrace0, Pid),
    logger:error("error: ~p~n~ts", [
        Reason, erl_error:format_exception(error, Reason, Stacktrace)
    ]);
log_crash(Class, Reason, Stacktrace, _Socket) ->
    logger:error("~s: ~p~n~p", [Class, Reason, Stacktrace]).

live_call_failed(exit, {_, {gen_server, call, [Pid | _]}}, #socket{pid = Pid}) when is_pid(Pid) ->
    not is_process_alive(Pid);
live_call_failed(_Class, _Reason, _Socket) ->
    false.

%% Whether the failed call was a mount -- a reconnect's deferred resync or a
%% navigate -- that the live process exited under. Its crash exit then closes
%% 4501 (see `mount_failed`). A process already gone before the call (`noproc`)
%% exited for some other reason, which decides alone.
mount_call({noproc, _}) -> false;
mount_call({_, {gen_server, call, [_Pid, mount_and_render | _]}}) -> true;
mount_call({_, {gen_server, call, [_Pid, {navigate, _, _, _} | _]}}) -> true;
mount_call(_Reason) -> false.

%% Is this frame the `cached_fps` announcement? Decoded here only to route the
%% deferred-resync leg; the announcement leg never re-decodes, and the rare
%% violation leg (a flagged client's non-announcement first frame) pays one
%% duplicate decode in handle_in.
decode_cached_fps(Frame) ->
    try json:decode(Frame) of
        [~"cached_fps", FpList] when is_list(FpList) -> {ok, FpList};
        _ -> error
    catch
        error:_ -> error
    end.

%% Mount the live process and encode the reconnect full-page replace. The
%% dedup against `sent_fps` happens inside `mount_and_render` -- any
%% fingerprints seeded before this call elide their statics from the payload.
%%
%% A raise in `mount/1`, `on_mount` or `render/1` kills the live process under
%% the call, so it reaches the entry catch like the drain/exit race does: the
%% frame is dropped and the queued exit closes (crash_close/4) -- 4501, as the
%% failed call was a mount. Every caller runs inside handle_in/2 or
%% handle_info/2, and the raise happens while this is still an argument, so
%% resync_before_frame/2 never runs and the triggering frame never reaches a
%% live process the flush left unmounted.
flush_resync(#socket{pid = Pid} = Socket) ->
    {ok, ViewId, PageHTML, Observed} = arizona_live:mount_and_render(Pid),
    {ok, replace_ops(ViewId, PageHTML), note_az([], Observed, Socket#socket{view_id = ViewId})}.

resync_reply({ok, Ops, Socket0}) ->
    {Attrs, Socket} = take_az_pending(Socket0),
    deliver_view(fun() -> {reply, encode(resync_frame(Ops, Attrs)), Socket} end, Socket).

%% Ship the resync frame BEFORE the triggering frame's own result: the client
%% must apply the full-page replace first, then the frame's reply against the
%% fresh DOM.
resync_before_frame({ok, Ops, Socket0}, Frame) ->
    {Attrs, Socket} = take_az_pending(Socket0),
    case deliver_view(fun() -> {ok, encode(resync_frame(Ops, Attrs))} end, Socket) of
        {ok, ResyncFrame} -> resync_then(ResyncFrame, do_handle_in(Frame, Socket));
        {close, _Code, _Reason, _Socket} = Close -> Close
    end.

%% The frame that delivers a freshly mounted view is part of the mount: until
%% it reaches the client there is no mounted view there, so failing to encode
%% it is a mount failure (4501), not a crash after the mount.
deliver_view(Encode, Socket) ->
    try
        Encode()
    catch
        Class:Reason:Stacktrace ->
            ok = log_crash(Class, Reason, Stacktrace, Socket),
            close_mount_failed(Socket)
    end.

%% A close outranks the resync -- the socket is going away.
resync_then(ResyncFrame, {ok, Socket}) ->
    {reply, ResyncFrame, Socket};
resync_then(ResyncFrame, {reply, Frame, Socket}) ->
    {reply_many, [ResyncFrame, Frame], Socket};
resync_then(_ResyncFrame, {close, _Code, _Reason, _Socket} = Close) ->
    Close.

handle_navigate(Path, Qs, #socket{req = Req} = Socket) ->
    case resolve_route(Path, Qs, Req) of
        {ok, H, RouteOpts, NewReq} ->
            case layout_changed(RouteOpts, Socket) of
                true ->
                    full_navigate(Path, Qs, Socket);
                false ->
                    do_navigate(H, RouteOpts, NewReq, url(Path, Qs), Socket)
            end;
        error ->
            full_navigate(Path, Qs, Socket)
    end.

%% A `patch` keeps the current view IFF the patched path resolves to the same
%% root handler; otherwise it can't (a different view needs a real mount), so it
%% degrades to a full navigate/replace. Resolves once and reuses the result for
%% either branch. A path that resolves to no live route degrades further to a
%% full-page navigation -- as does one whose layouts differ, which neither
%% branch can serve (a patch keeps the shell, and a replace only swaps what is
%% inside it).
handle_patch(Path, Qs, #socket{req = Req} = Socket) ->
    case resolve_route(Path, Qs, Req) of
        {ok, H, RouteOpts, NewReq} ->
            case layout_changed(RouteOpts, Socket) of
                true ->
                    full_navigate(Path, Qs, Socket);
                false ->
                    patch_or_navigate(H, RouteOpts, NewReq, url(Path, Qs), Socket)
            end;
        error ->
            full_navigate(Path, Qs, Socket)
    end.

patch_or_navigate(H, RouteOpts, NewReq, Requested, #socket{handler = H} = Socket) ->
    do_patch(RouteOpts, NewReq, Requested, Socket);
patch_or_navigate(H, RouteOpts, NewReq, Requested, Socket) ->
    do_navigate(H, RouteOpts, NewReq, Requested, Socket).

%% Does the target route wrap its page in different layouts than the ones
%% already on screen? Term equality on the whole list, deliberately: a
%% difference at ANY depth is disqualifying, since `arizona_render:apply_layouts/4`
%% nests them (`[Root, Section]` renders `Root(Section(Page))`), so an inner
%% layer wraps the replaced view exactly as the outer one does.
layout_changed(RouteOpts, #socket{layouts = Layouts}) ->
    case maps:get(layouts, RouteOpts, []) of
        Layouts -> false;
        _Different -> true
    end.

resolve_route(Path, Qs, Req) ->
    Adapter = arizona_req:adapter(Req),
    arizona_req:call_resolve_route(Adapter, Path, Qs, arizona_req:raw(Req)).

%% A navigate/patch target this socket cannot serve in place -- one that doesn't
%% resolve to a live route (a typo, a controller/asset path, a 404), or one whose
%% layouts differ from the ones already rendered. Neither can be SPA-navigated:
%% the first would have the client re-request it over the socket in a loop, and
%% the second would drop the new page into the old page's shell. Instead of
%% crashing the live session, tell the client to do a real full-page navigation
%% -- the browser loads it normally, layouts and all.
%%
%% A pending flash cannot cross that navigation on the socket (the WS frame has
%% no `Set-Cookie` leg), so it goes one of two ways: replayed if it can be, and
%% otherwise dropped loudly.
%%
%% A native client has no page to load, so it takes a `full` navigate as the
%% next best thing: it drops this socket and connects fresh to the target path,
%% which runs the upgrade's middlewares (and mount) anew.
full_navigate(Path, Qs, Socket0) ->
    Url = url(Path, Qs),
    case flash_replay(Url, Socket0) of
        {ok, Replay} ->
            %% The flash came from a halting middleware, so the HTTP request that
            %% produced it reproduces it: navigate to THAT path instead of the
            %% target and let the halt replay over a channel that has a
            %% `Set-Cookie`. The middleware redirects again from there, this time
            %% as a real 3xx carrying the signed flash cookie, and the browser
            %% lands on the target with both its own layouts and the message.
            %% Costs a re-run of that path's middlewares -- acceptable because any
            %% GET route's middlewares already have to tolerate repetition (a
            %% refresh, a prefetch, back/forward all re-run them).
            encode_reply(
                [],
                [arizona_js:navigate(Replay, #{full => true})],
                Socket0#socket{pending_flash = #{}, flash_replay = undefined}
            );
        error ->
            Socket = drop_pending_flash(Path, Socket0),
            encode_reply([], [arizona_js:navigate(Url, #{full => true})], Socket)
    end.

%% The stashed replay is good for exactly ONE destination: the redirect the halt
%% issued. A full navigation anywhere else is the user going somewhere else --
%% sending them back through the gate would hijack it, which is worse than any
%% flash outcome -- so only the matching target replays and everything else falls
%% through to the drop. Reachable whenever a second navigate beats the follow-up
%% frame for the redirect.
flash_replay(Target, #socket{pending_flash = Flash, flash_replay = {Target, Replay}}) when
    map_size(Flash) > 0
->
    {ok, Replay};
flash_replay(_Url, _Socket) ->
    error.

url(Path, <<>>) -> Path;
url(Path, Qs) -> <<Path/binary, "?", Qs/binary>>.

%% A flash stashed for a WS-carried navigate can only reach a LIVE destination
%% (do_navigate/do_patch inject it into the resolved request); a full-page
%% navigation destroys this socket, and a WebSocket frame has no Set-Cookie leg
%% for the signed flash cookie, so the flash cannot follow. Reached only when the
%% flash is NOT replayable (an in-view handler set it, so no request reproduces
%% it -- see `flash_replay`). Warn, since the app-visible symptom is a silently
%% missing flash, and clear the stash so it cannot leak into a later, unrelated
%% navigate.
drop_pending_flash(_Path, #socket{pending_flash = Flash} = Socket) when map_size(Flash) =:= 0 ->
    Socket;
drop_pending_flash(Path, Socket) ->
    logger:warning(
        "flash dropped on full-page navigation to ~s: a flash set by a live "
        "handler cannot cross a full page load (a WS frame has no Set-Cookie "
        "leg, and nothing on the new request reproduces it). Set it from a "
        "middleware on the requested path, or store it in the session",
        [Path]
    ),
    Socket#socket{pending_flash = #{}}.

do_navigate(H, RouteOpts, NewReq0, Requested, Socket0) ->
    {NewReq, #socket{pid = Pid, view_id = OldVId} = Socket} =
        take_pending_flash(NewReq0, Socket0),
    IB = maps:get(bindings, RouteOpts, #{}),
    OnMount = maps:get(on_mount, RouteOpts, []),
    Middlewares = maps:get(middlewares, RouteOpts, []),
    case arizona_middleware:apply_middlewares(Middlewares, NewReq, IB) of
        {halt, HaltReq} ->
            halt_navigate(Requested, HaltReq, Socket);
        {cont, _NewReq1, Bindings1} ->
            %% A live process gone under this call is handled at the entry catch
            %% (crash_close/4): the frame is dropped and its queued exit closes.
            %% A crash exit under this call -- the new page's mount, or the old
            %% page's unmount -- closes 4501: the new page could not be served.
            %%
            %% The socket is the only caller of arizona_live:navigate/4 (it is
            %% transport-facing): the drain below relies on every push before the
            %% barrier belonging to a page this socket put on screen.
            {ok, NewVId, PageHTML, Observed} =
                arizona_live:navigate(Pid, H, Bindings1, OnMount),
            %% The old page's pushes queued ahead of the reply ship in the
            %% same frame, their ops ahead of the replace: one may carry the
            %% first statics of a fingerprint the replace then references
            %% bare. Their effects are dropped: a client applies a frame's
            %% effects after its ops, so they would run against the NEW
            %% page (a relayed push_event reaches the new root, a navigate
            %% hijacks the user's own). Old-page ops may mount `az-hook`
            %% elements the replace destroys in the same frame -- churn,
            %% not a fault.
            %%
            %% The navigate itself adds no effects: mount/1 has no effects
            %% channel (it also runs at SSR, where no client exists to
            %% receive one). A view that wants one self-casts from mount/1
            %% and answers in handle_info/2, which pushes its own frame --
            %% behind the barrier, so after this replace.
            {PendOps, PendEffects, Socket1} = drain_pending_pushes(Socket),
            ok = log_dropped_effects(PendEffects),
            Socket2 = (note_az([H], Observed, Socket1))#socket{view_id = NewVId, handler = H},
            deliver_view(
                fun() -> encode_reply(PendOps ++ replace_ops(OldVId, PageHTML), [], Socket2) end,
                Socket2
            )
    end.

%% Same root handler: keep the view, deliver the route's static bindings threaded
%% through its middlewares to handle_update/3, and ship the diff ops + effects on
%% the same `view_id` (no replace, no remount). Path captures are not in there on
%% their own -- they live on the request until an extract([path_bindings]) step
%% copies them over. Runs the route middlewares first, exactly like navigate
%% -- but deliberately does NOT read `on_mount` (contrast do_navigate): on_mount
%% is a mount-phase hook and a patch does not remount (see arizona_live:patch/2).
do_patch(RouteOpts, NewReq0, Requested, Socket0) ->
    {NewReq, #socket{pid = Pid, view_id = ViewId} = Socket} =
        take_pending_flash(NewReq0, Socket0),
    IB = maps:get(bindings, RouteOpts, #{}),
    Middlewares = maps:get(middlewares, RouteOpts, []),
    case arizona_middleware:apply_middlewares(Middlewares, NewReq, IB) of
        {halt, HaltReq} ->
            halt_navigate(Requested, HaltReq, Socket);
        {cont, _NewReq1, Bindings1} ->
            {ok, Ops, Effects, Observed} = arizona_live:patch(Pid, Bindings1),
            reply_after_pending(flatten_ops(ViewId, Ops), Effects, Observed, Socket)
    end.

%% Middleware halt during WS navigate -- there is no HTTP response channel
%% mid-session, so we translate an `arizona_req:redirect/2` halt into an
%% `arizona_js:navigate` client effect. A flash the middleware set via `put_flash/3`
%% before halting has no `Set-Cookie` leg to ride here, so it is stashed on the
%% socket for the follow-up frame (`take_pending_flash/2`), delivered exactly once
%% with no cookie. A halt without a stashed redirect has a response only HTTP
%% can deliver, so the client is told to load the requested URL for real -- a
%% reconnect would not get there, since the WebSocket upgrade runs the same
%% halting middleware and is refused.
halt_navigate(Requested, HaltReq, Socket) ->
    case arizona_req:halted_redirect(HaltReq) of
        {_Status, Location} ->
            encode_reply(
                [],
                [arizona_js:navigate(Location)],
                stash_halt_flash(Requested, Location, HaltReq, Socket)
            );
        undefined ->
            encode_reply([], [arizona_js:navigate(Requested, #{full => true})], Socket)
    end.

%% Stash the halting middleware's flash BESIDE the path that produced it. Unlike
%% an in-view handler's flash, this one is reproducible: any later request to
%% `Requested` runs the same middlewares and sets it again. That is what lets a
%% follow-up frame which must go full-page replay the halt over HTTP -- where the
%% signed flash cookie can actually be set -- instead of dropping the message
%% (see `full_navigate/3`). Stashing it here rather than as a `flash` opt on the
%% outgoing effect is what keeps the two origins distinguishable: everything
%% reaching `capture_nav_flash/5` is by construction handler-set.
stash_halt_flash(Requested, Location, HaltReq, Socket) ->
    case arizona_req:flash_out(HaltReq) of
        Flash when map_size(Flash) =:= 0 ->
            Socket;
        Flash ->
            #socket{pending_flash = Pending} = Socket,
            Socket#socket{
                pending_flash = maps:merge(Pending, Flash),
                flash_replay = {strip_fragment(Location), Requested}
            }
    end.

%% A navigate frame carries path + query only (the client parses the URL and
%% sends `u.pathname`/`u.search`), so a fragment on the redirect Location would
%% never match the follow-up and would silently disable the replay. Two other
%% Location forms miss for the same reason and are deliberately left to: an
%% absolute URL (a cross-origin redirect cannot carry our flash cookie anyway),
%% and a relative one with no leading slash, which the client resolves against
%% the origin so `target` arrives as `/target`. Both degrade to the
%% drop-with-warning, never to a wrong destination.
strip_fragment(Location) ->
    [Base | _Fragment] = binary:split(Location, ~"#"),
    Base.

%% Inject a one-shot in-process flash into the resolved navigate/patch request and
%% clear it from the socket (consumed once). Empty is a no-op so a real incoming
%% cookie flash on `NewReq` is never masked.
take_pending_flash(Req, #socket{pending_flash = Flash} = Socket) when map_size(Flash) > 0 ->
    {arizona_req:seed_flash(Req, Flash), Socket#socket{
        pending_flash = #{}, flash_replay = undefined
    }};
take_pending_flash(Req, Socket) ->
    {Req, Socket}.

replace_ops(ViewId, PageHTML) ->
    [[?OP_REPLACE, ViewId, PageHTML]].

%% Resolve the client-supplied event target to a view id for dispatch and op
%% tagging. A `push_event` handler effect has no enclosing element, so the client
%% relays it with a `null` target; a hook or element outside any `[az-view]` can
%% too. Route those to the root view: `handle_event` already dispatches a
%% non-child id to the root, but the id must be the root's binary id (not `null`)
%% because `flatten_ops`/`op_encoder` tag every diff op with it, and a
%% `null`-tagged op is unencodable, and an unencodable frame closes the session.
event_target(Target, _RootViewId) when is_binary(Target) -> Target;
event_target(_Target, RootViewId) -> RootViewId.

dispatch_event(Pid, ViewId, Event, Payload) ->
    {ok, Ops, Effects, Observed} = arizona_live:handle_event(Pid, ViewId, Event, Payload),
    {flatten_ops(ViewId, Ops), Effects, Observed}.

%% Selectively receive the `{arizona_push, ...}` messages the live process
%% emitted BEFORE the reply to a synchronous event/patch/navigate call, and fold them in
%% front of that reply's own ops. Such a push is causally earlier, so it must be
%% applied first: without this the reply ships first and the earlier push lands
%% in a LATER frame, letting a stale value overwrite the reply's client-side
%% (and keyed stream ops with relative refs can fail to apply outright).
%%
%% "Before the reply" is decided by the live process, not guessed here: it sends
%% `arizona_push_barrier` from inside the call (`arizona_live:push_barrier/1`),
%% so the marker sits behind every push that preceded the reply and ahead of
%% every push emitted after it. Draining up to the marker is therefore exact --
%% an `after 0` drain instead swept up whatever happened to have landed by the
%% time this process was rescheduled, and the `?send` self-message idiom makes
%% that a push emitted AFTER the reply (the handler enqueues to the live
%% process's own mailbox during handle_event/3, so the live process replies,
%% then dequeues and pushes). Prepending it inverted an order-dependent pair: a
%% stream MOVE ahead of the INSERT that created its key, which the client drops
%% for good. Anything behind the marker stays queued for handle_info/2, which
%% ships it in the next frame -- in order.
%%
%% The marker was sent before the reply the caller has already received, so it
%% is in this mailbox and the `after 0` below is unreachable -- a `receive`
%% cannot time out while a matching message is queued. It is there so that a
%% broken invariant degrades to the old (merely mis-ordered) behaviour instead
%% of wedging the connection on a receive that never returns.
%%
%% Every push up to the marker is taken, whatever view id it names, each tagged
%% with its own: "everything before the marker" is the whole invariant. Navigate
%% drains too, so the old page's pushes all ship ahead of the replace instead of
%% trailing it into the new page.
drain_pending_pushes(Socket) ->
    drain_pending_pushes([], [], Socket).

drain_pending_pushes(OpsAcc, EffectsAcc, Socket) ->
    receive
        {arizona_push, PushVId, Ops, Effects, Observed} ->
            drain_pending_pushes(
                [flatten_ops(PushVId, Ops) | OpsAcc],
                [Effects | EffectsAcc],
                note_az([], Observed, Socket)
            );
        arizona_push_barrier ->
            drained_pushes(OpsAcc, EffectsAcc, Socket)
    after 0 ->
        drained_pushes(OpsAcc, EffectsAcc, Socket)
    end.

drained_pushes(OpsAcc, EffectsAcc, Socket) ->
    {lists:append(lists:reverse(OpsAcc)), lists:append(lists:reverse(EffectsAcc)), Socket}.

%% An event/patch reply: the pushes queued ahead of it, then its own ops and
%% effects, in one frame -- all of them belong to the page still on screen.
reply_after_pending(Ops, Effects, Observed, Socket0) ->
    {PendOps, PendEffects, Socket} = drain_pending_pushes(Socket0),
    encode_reply(PendOps ++ Ops, PendEffects ++ Effects, note_az([], Observed, Socket)).

%% The old page's effects a navigate drops (see do_navigate/5): logged once, by
%% op code only -- their payloads (push_event data, flash contents) stay out of
%% the log.
log_dropped_effects([]) ->
    ok;
log_dropped_effects(Dropped) ->
    Cmds = unwrap_effects(Dropped),
    ok = lists:foreach(fun warn_dropped_flash/1, nav_flash_ops(Cmds)),
    logger:notice(
        "dropped ~b old-page effect(s) on navigate, op codes ~w",
        [length(Cmds), [Op || [Op | _] <:- Cmds]]
    ).

%% A dropped navigate/patch may carry a flash, which then never reaches anything
%% (the same silent loss `drop_pending_flash/2` warns about).
warn_dropped_flash(Op) ->
    logger:warning(
        "flash dropped with an old-page ~s effect on navigate: the page that "
        "queued it is being replaced, so its navigation does not run",
        [effect_name(Op)]
    ).

effect_name(?EFFECT_NAVIGATE) -> "navigate";
effect_name(?EFFECT_PATCH) -> "patch".

%% The navigate/patch op codes that carry a `flash` opt anywhere in a command
%% list, including inside the commands a `transition`, `on_key` or a `fetch`
%% `on_error` wraps.
nav_flash_ops(Cmds) ->
    lists:append([nav_flash_cmd(Cmd) || Cmd <- Cmds]).

nav_flash_cmd([Op, _Path, #{flash := _}]) when Op =:= ?EFFECT_NAVIGATE; Op =:= ?EFFECT_PATCH ->
    [Op];
nav_flash_cmd(Cmd) ->
    case wrapped(Cmd) of
        {_When, Inner, _Rewrap} -> nav_flash_ops(Inner);
        none -> []
    end.

%% The commands a `transition`, `on_key` or a `fetch` `on_error` wraps, as a
%% list, with when they run (`now` when the frame lands, `deferred` later, if
%% ever) and a fun that rebuilds the wrapper around a replacement list.
wrapped([?EFFECT_TRANSITION, Opts, Cmds]) ->
    unwrap(now, Cmds, fun(New) -> [?EFFECT_TRANSITION, Opts, New] end);
wrapped([?EFFECT_ON_KEY, Key, Cmds]) ->
    unwrap(deferred, Cmds, fun(New) -> [?EFFECT_ON_KEY, Key, New] end);
wrapped([?EFFECT_FETCH, Url, #{on_error := Cmds} = Opts]) ->
    unwrap(deferred, Cmds, fun(New) -> [?EFFECT_FETCH, Url, Opts#{on_error => New}] end);
wrapped(_Cmd) ->
    none.

%% A wrapper holds one command (its head is an op code) or a list: the walks get
%% a list either way, and the rebuild restores the shape it was given.
unwrap(When, [Op | _] = Cmd, Wrap) when is_integer(Op) ->
    {When, [Cmd], fun([New]) -> Wrap(New) end};
unwrap(When, Cmds, Wrap) ->
    {When, Cmds, Wrap}.

%% The compile-time walk roots for a page: the route handler plus its layout
%% modules. Layouts render az-* markup too -- once, at SSR -- and no later frame
%% can re-render one, so their names must ride the connect (or navigate) walk.
az_roots(Handler, Layouts) ->
    [Handler | [Mod || {Mod, _Fun} <- Layouts]].

%% Connect-time seeding, run once per socket: everything is new, so the delta
%% bookkeeping note_az/3 does per frame (filter against walked, dedup against
%% sent, the pending round trip) would only compare against empty sets. Walk
%% the roots plus the mount's observations, seed both sets whole, and hand the
%% names straight to the connect frame the caller builds.
az_connect(Roots, {ObsNames, ObsMods}, Socket) ->
    Mods = Roots ++ ObsMods,
    Names = lists:usort(arizona_event_attrs:all(Mods) ++ ObsNames),
    {Names, Socket#socket{
        az_walked = maps:from_keys(Mods, true),
        az_sent = maps:from_keys(Names, true)
    }}.

%% Fold newly proven az-* names into the socket's pending delta: walk modules not
%% yet walked (extra roots plus render-observed ones), union render-observed
%% names, and dedup against everything already sent. The next outgoing frame
%% carries the result (and marking sent here is safe: every note_az is followed
%% by that frame's construction in the same handle_in/handle_info result).
note_az([], {[], []}, Socket) ->
    Socket;
note_az(Roots, {ObsNames, ObsMods}, Socket) ->
    #socket{az_walked = Walked0, az_sent = Sent0, az_pending = Pending} = Socket,
    NewMods = [Mod || Mod <- Roots ++ ObsMods, not is_map_key(Mod, Walked0)],
    Walked = maps:merge(Walked0, #{Mod => true || Mod <- NewMods}),
    Names = arizona_event_attrs:all(NewMods) ++ ObsNames,
    Fresh = [Name || Name <- lists:usort(Names), not is_map_key(Name, Sent0)],
    Sent = maps:merge(Sent0, #{Name => true || Name <- Fresh}),
    Socket#socket{az_walked = Walked, az_sent = Sent, az_pending = Pending ++ Fresh}.

take_az_pending(#socket{az_pending = Pending} = Socket) ->
    {Pending, Socket#socket{az_pending = []}}.

resync_frame(Ops, []) -> #{?OPS => Ops};
resync_frame(Ops, Attrs) -> #{?OPS => Ops, ?AZ_ATTRS => Attrs}.

%% Single chokepoint for every reply that carries effects. Before encoding, an
%% in-view flash a handler set (an `arizona_js:navigate`/`patch` `flash` opt) is
%% moved onto the socket's one-shot pending flash and stripped from the outgoing
%% effect, so delivery is identical whether the flash came from a halting
%% middleware (`stash_halt_flash/3`) or a live handler -- the follow-up
%% navigate frame injects it via `take_pending_flash/2`. The two differ only in
%% whether a full-page navigation can recover them (see `flash_replay`).
encode_reply(Ops, Effects0, Socket0) ->
    {Effects, Socket} = capture_pending_flash(Effects0, Socket0),
    encode_reply_1(Ops, Effects, Socket).

%% The first four clauses are the no-delta fast path -- almost every frame.
%% They must not touch `az_pending`: reading it costs nothing, but clearing it
%% would copy the socket record per event for a field that is already empty.
encode_reply_1([], [], #socket{az_pending = []} = Socket) ->
    {ok, Socket};
encode_reply_1(Ops, [], #socket{az_pending = []} = Socket) ->
    {reply, encode(#{?OPS => Ops}), Socket};
encode_reply_1([], Effects, #socket{az_pending = []} = Socket) ->
    {reply, encode(#{?EFFECTS => unwrap_effects(Effects)}), Socket};
encode_reply_1(Ops, Effects, #socket{az_pending = []} = Socket) ->
    {reply, encode(#{?OPS => Ops, ?EFFECTS => unwrap_effects(Effects)}), Socket};
encode_reply_1(Ops, Effects, Socket0) ->
    {Attrs, Socket} = take_az_pending(Socket0),
    Fields =
        [{?OPS, Ops} || Ops =/= []] ++
            [{?EFFECTS, unwrap_effects(Effects)} || Effects =/= []] ++
            [{?AZ_ATTRS, Attrs}],
    {reply, encode(maps:from_list(Fields)), Socket}.

unwrap_effects(Effects) ->
    [Cmd || {arizona_effect, Cmd} <:- Effects].

%% Move any `flash` opt off a navigate/patch effect onto the socket's one-shot
%% pending flash (merged) and strip it from the outgoing effect. The flash is
%% delivered purely in-process to the follow-up navigate/patch frame
%% (`take_pending_flash/2`), exactly once -- a live navigate has no cookie leg; the
%% signed flash cookie is the HTTP full-page redirect mechanism only. The browser
%% never sees the flash at all.
capture_pending_flash([], Socket) ->
    {[], Socket};
capture_pending_flash(Effects, #socket{pending_flash = Pending0} = Socket) ->
    {Effects1, Pending} = lists:mapfoldl(fun capture_flash_effect/2, Pending0, Effects),
    {Effects1, captured_flash(Pending0, Pending, Socket)}.

%% Nothing captured -- the stash, and any halt replay armed for it, stand.
captured_flash(Pending, Pending, Socket) ->
    Socket;
%% A handler-set flash joined the stash. No request reproduces that one, so a
%% replay armed by `stash_halt_flash/3` can no longer regenerate the whole of it.
%% Disarm: a full-page navigation should drop the flash loudly rather than replay
%% a silently partial one.
captured_flash(_Pending0, Pending, Socket) ->
    Socket#socket{pending_flash = Pending, flash_replay = undefined}.

capture_flash_effect({arizona_effect, Cmd0}, Pending0) ->
    {Cmd, Pending} = capture_flash_cmd(Cmd0, Pending0),
    {{arizona_effect, Cmd}, Pending}.

%% A navigate/patch runs when the reply lands, so its flash is captured; so does
%% one a `transition` wraps, which runs right away too. One an `on_key` or a
%% `fetch`'s `on_error` wraps runs later, if ever, so its flash cannot be paired
%% with the frame that follows it: it is stripped with a warning instead of
%% riding into the browser or onto whatever navigation comes next.
capture_flash_cmd([Op, Path, #{flash := Flash} = Opts], Pending) when
    Op =:= ?EFFECT_NAVIGATE; Op =:= ?EFFECT_PATCH
->
    capture_nav_flash(Op, Path, Flash, Opts, Pending);
capture_flash_cmd(Cmd, Pending0) ->
    case wrapped(Cmd) of
        {now, Inner0, Rewrap} ->
            {Inner, Pending} = lists:mapfoldl(fun capture_flash_cmd/2, Pending0, Inner0),
            {Rewrap(Inner), Pending};
        {deferred, Inner, Rewrap} ->
            {Rewrap([strip_flash_cmd(C) || C <- Inner]), Pending0};
        none ->
            {Cmd, Pending0}
    end.

strip_flash_cmd([Op, Path, #{flash := _} = Opts]) when
    Op =:= ?EFFECT_NAVIGATE; Op =:= ?EFFECT_PATCH
->
    ok = logger:warning(
        "flash dropped from a deferred ~s effect: one an on_key or a fetch's "
        "on_error wraps runs later, if ever, so no frame can carry its flash",
        [effect_name(Op)]
    ),
    [Op, Path, maps:remove(flash, Opts)];
strip_flash_cmd(Cmd) ->
    case wrapped(Cmd) of
        {_When, Inner, Rewrap} -> Rewrap([strip_flash_cmd(C) || C <- Inner]);
        none -> Cmd
    end.

%% `flash` is always stripped from the client effect (the browser never sees it),
%% regardless of its shape -- so no non-map `flash` opt can leak. A non-map value is
%% a caller error and crashes here at `maps:merge` (fail-closed, like the strict
%% generator in `unwrap_effects/1`); there is deliberately no defensive `is_map` guard
%% that would let it fall through to the client instead.
capture_nav_flash(Op, Path, Flash, Opts, Pending) ->
    {[Op, Path, maps:remove(flash, Opts)], maps:merge(Pending, Flash)}.

%% A frame the encoder refuses for a string that is not UTF-8 is renamed
%% `non_utf8_render`, naming the value (arizona_utf8:refuse/3); the encoder's own
%% `{invalid_byte, _}`/`unexpected_end` names neither it nor the cause. The
%% encoder validates as it escapes, so the check itself costs nothing.
encode(Frame) ->
    try
        encode_frame(Frame)
    catch
        error:Reason:Stacktrace -> arizona_utf8:refuse(Frame, Reason, Stacktrace)
    end.

%% Fast path for the reply shapes produced by encode_reply/3 and the connect
%% frames. Hand writes the outer `{"o":...}` / `{"e":...}` / combined wrapper,
%% skipping OTP json's per-key map walk and the per-call escape on the constant
%% `<<"o">>`/`<<"e">>`/`<<"a">>` keys. The Ops list goes through the
%% specialized `encode_ops/1` walk rather than generic `json:encode/2`.
%% Effects keep the default encoder -- they're plain JSON values.
%%
%% Clause order carries a trap: map patterns are SUBSET matches, so the
%% widest key set must match first -- an `o`+`e`+`a` frame reaching the
%% `o`+`a` clause would re-encode without `e`, silently dropping the
%% effects on exactly the frames that carry a delta.
encode_frame(#{?OPS := Ops, ?EFFECTS := Effects, ?AZ_ATTRS := Attrs}) ->
    [
        <<"{\"o\":">>,
        encode_ops(Ops),
        <<",\"e\":">>,
        json:encode(Effects),
        <<",\"a\":">>,
        json:encode(Attrs),
        $}
    ];
encode_frame(#{?EFFECTS := Effects, ?AZ_ATTRS := Attrs}) ->
    [<<"{\"e\":">>, json:encode(Effects), <<",\"a\":">>, json:encode(Attrs), $}];
encode_frame(#{?OPS := Ops, ?AZ_ATTRS := Attrs}) ->
    [
        <<"{\"o\":">>,
        encode_ops(Ops),
        <<",\"a\":">>,
        json:encode(Attrs),
        $}
    ];
encode_frame(#{?AZ_ATTRS := Attrs}) ->
    [<<"{\"a\":">>, json:encode(Attrs), $}];
encode_frame(#{?OPS := Ops, ?EFFECTS := Effects}) ->
    [
        <<"{\"o\":">>,
        encode_ops(Ops),
        <<",\"e\":">>,
        json:encode(Effects),
        $}
    ];
encode_frame(#{?OPS := Ops}) ->
    [<<"{\"o\":">>, encode_ops(Ops), $}];
encode_frame(#{?EFFECTS := Effects}) ->
    [<<"{\"e\":">>, json:encode(Effects), $}];
encode_frame(Map) ->
    json:encode(Map).

%% Pre-flatten parent + child-view ops into tagged tuples ready for
%% `op_encoder/2`. Each tuple's ViewId is the owning view; the encoder
%% emits the scoped target inline at JSON write time -- no binary
%% concat, no per-target escape_binary call. `replace_ops/2` produces
%% UNTAGGED ops (the target IS the ViewId), so they bypass the encoder
%% special case and go through default JSON encoding.
flatten_ops(_ViewId, []) ->
    [];
flatten_ops(ParentViewId, [[ChildViewId, ChildOps] | Rest]) when is_binary(ChildViewId) ->
    flatten_ops(ChildViewId, ChildOps) ++ flatten_ops(ParentViewId, Rest);
flatten_ops(ViewId, [Op | Rest]) ->
    [{ViewId, Op} | flatten_ops(ViewId, Rest)].

%% Specialized JSON encoder for the ops list. Ops are framework-shaped --
%% arrays of binaries, integers and nested op arrays, with a rare map
%% payload -- so encoding them through generic `json:encode/2` pays a fun
%% dispatch (`op_encoder/2` -> `json:encode_value/2` -> type switch) per
%% value, which on an ops-heavy frame (a 100-item stream reset) costs more
%% than the diff that produced it. This walk switches on the type directly
%% and emits through json's exported per-type functions, so the bytes stay
%% identical to what `json:encode(Ops, fun op_encoder/2)` produces --
%% `encode_ops_matches_generic_encoder_test` holds the two together.
%%
%% The scoped target `<ViewId>:<Az>` is also memoized across consecutive
%% ops: `flatten_ops/2` groups ops by view, and every per-item op of one
%% stream drain shares the container's az, so a run of ops re-encoded (and
%% re-escaped) the same target once per op. `ViewId` is the app-supplied
%% `id` binding and is NOT validated -- an id containing `"` would break
%% the ops frame, and a crafted value could inject ops (an injected
%% `OP_REPLACE`, or an `OP_TEXT` carrying an HTML payload, reaches
%% `innerHTML`: stored XSS via the diff channel) -- so the scoped target
%% goes through `json:encode_binary/1`, which escapes the JSON-breaking
%% bytes (`"`/`\`). The SSR path already escapes the same id in HTML; this
%% closes the ops channel.
encode_ops([]) ->
    <<"[]">>;
encode_ops([Op | Ops]) ->
    {Enc, Cache} = encode_op(Op, none),
    [$[, Enc | encode_ops_rest(Ops, Cache)].

encode_ops_rest([], _Cache) ->
    [$]];
encode_ops_rest([Op | Ops], Cache) ->
    {Enc, Cache1} = encode_op(Op, Cache),
    [$,, Enc | encode_ops_rest(Ops, Cache1)].

%% A tagged op emits the memoized scoped target; anything else (an untagged
%% replace op, whose target IS the ViewId) is a plain value.
encode_op({ViewId, [OpCode, Target | Args]}, Cache) when
    is_integer(OpCode),
    OpCode >= 0,
    is_binary(ViewId),
    is_binary(Target)
->
    {TargetJson, Cache1} =
        case Cache of
            {ViewId, Target, Hit} ->
                {Hit, Cache};
            _ ->
                Enc = json:encode_binary(<<ViewId/binary, $:, Target/binary>>),
                {Enc, {ViewId, Target, Enc}}
        end,
    {[$[, op_code_iodata(OpCode), $,, TargetJson, encode_op_args(Args), $]], Cache1};
encode_op(Op, Cache) ->
    {encode_op_value(Op), Cache}.

encode_op_args([]) -> [];
encode_op_args([A | As]) -> [$,, encode_op_value(A) | encode_op_args(As)].

%% The op-arg vocabulary, most frequent first. The fallback keeps generic
%% behavior (with `op_encoder/2` as the recursion point, matching what the
%% generic path did) for the shapes the fast clauses don't name -- the
%% `#{~"raw" => ...}` marker and fingerprinted `zip_item/2` payload maps.
encode_op_value(B) when is_binary(B) -> json:encode_binary(B);
encode_op_value(I) when is_integer(I) -> integer_to_binary(I);
encode_op_value([]) -> <<"[]">>;
encode_op_value([H | T]) -> [$[, encode_op_value(H) | encode_op_list_rest(T)];
encode_op_value(V) -> json:encode_value(V, fun op_encoder/2).

encode_op_list_rest([]) -> [$]];
encode_op_list_rest([H | T]) -> [$,, encode_op_value(H) | encode_op_list_rest(T)].

%% The generic-path encoder `encode_ops/1` specializes: `json:encode/2`
%% callback emitting a tagged op's scoped target inline. Reached today only
%% through `encode_op_value/1`'s map fallback (map values re-enter here) and
%% as the differential test's reference implementation -- kept whole, not
%% pruned to the map case, so the reference stays an independent oracle.
op_encoder({ViewId, [OpCode, Target | RestArgs]}, E) when
    is_integer(OpCode),
    OpCode >= 0,
    is_binary(ViewId),
    is_binary(Target)
->
    [
        $[,
        op_code_iodata(OpCode),
        $,,
        json:encode(<<ViewId/binary, $:, Target/binary>>),
        encode_rest(RestArgs, E),
        $]
    ];
op_encoder(V, E) ->
    json:encode_value(V, E).

%% Op codes 0..9 emit as a single ASCII digit (`OpCode + $0`, skipping an
%% `integer_to_binary/1` per op); codes 10+ (e.g. `?OP_LIST_PATCH`) use
%% `integer_to_binary/1`.
op_code_iodata(OpCode) when OpCode =< 9 -> OpCode + $0;
op_code_iodata(OpCode) -> integer_to_binary(OpCode).

encode_rest([], _E) -> [];
encode_rest([H | T], E) -> [$,, E(H, E) | encode_rest(T, E)].

-ifdef(TEST).

flatten_ops_tags_test() ->
    %% flatten_ops emits {ViewId, RawOp} tuples; the per-target scoping
    %% happens in op_encoder/2 at JSON write time, not here.
    Op = [5, ~"0", ~"1", -1, ~"<li>A</li>"],
    ?assertEqual([{~"page", Op}], flatten_ops(~"page", [Op])).

flatten_ops_child_diff_test() ->
    %% Child-view diff: [ChildViewId, ChildOps] flattens with the child's
    %% ViewId tag, parent ops keep the parent's tag.
    ChildOp = [0, ~"f7-0", ~"99"],
    ParentOp = [0, ~"f12-0", ~"42"],
    ?assertEqual(
        [{~"counter", ChildOp}, {~"page", ParentOp}],
        flatten_ops(~"page", [[~"counter", [ChildOp]], ParentOp])
    ).

encode_replace_op_test() ->
    %% Replace ops are UNTAGGED -- target IS the ViewId. Default JSON
    %% encoding, no scoping.
    Bytes = iolist_to_binary(encode(#{?OPS => [[8, ~"page", ~"<main>new</main>"]]})),
    ?assertEqual(~"{\"o\":[[8,\"page\",\"<main>new</main>\"]]}", Bytes).

encode_stream_ops_test() ->
    %% Tagged ops go through op_encoder/2 -- inline `<ViewId>:<Az>` emit.
    %% INSERT
    InsOp = [5, ~"0", ~"1", -1, ~"<li>A</li>"],
    InsBytes = iolist_to_binary(encode(#{?OPS => flatten_ops(~"page", [InsOp])})),
    ?assertEqual(~"{\"o\":[[5,\"page:0\",\"1\",-1,\"<li>A</li>\"]]}", InsBytes),
    %% REMOVE
    RemOp = [6, ~"0", ~"1"],
    RemBytes = iolist_to_binary(encode(#{?OPS => flatten_ops(~"page", [RemOp])})),
    ?assertEqual(~"{\"o\":[[6,\"page:0\",\"1\"]]}", RemBytes),
    %% ITEM_PATCH -- inner ops are item-relative (NOT scoped by op_encoder
    %% because they're not tagged tuples).
    PatchOp = [7, ~"0", ~"1", [[0, ~"0", ~"New"]]],
    PatchBytes = iolist_to_binary(encode(#{?OPS => flatten_ops(~"page", [PatchOp])})),
    ?assertEqual(~"{\"o\":[[7,\"page:0\",\"1\",[[0,\"0\",\"New\"]]]]}", PatchBytes),
    %% MOVE
    MoveOp = [9, ~"0", ~"1", 0],
    MoveBytes = iolist_to_binary(encode(#{?OPS => flatten_ops(~"page", [MoveOp])})),
    ?assertEqual(~"{\"o\":[[9,\"page:0\",\"1\",0]]}", MoveBytes).

%% LIST_PATCH (op code 10 -- the first TWO-digit op code) with positional sub-ops:
%% ITEM_PATCH (idx + inner ops), INSERT (idx + item payload map), REMOVE (idx).
%% Regression for op_encoder assuming single-digit codes: the old `=< 9` guard +
%% `OpCode + $0` dropped op 10 to the `json:encode_value` fallback, which crashed
%% on the tagged tuple (`unsupported_type`). The container op is scoped
%% (`page:4`); sub-ops are positional and go through default JSON encoding.
encode_list_patch_op_test() ->
    ListPatchOp = [
        10,
        ~"4",
        [
            [7, 0, [[0, ~"0", ~"X"]]],
            [5, 1, #{~"f" => ~"fp"}],
            [6, 2]
        ]
    ],
    Bytes = iolist_to_binary(encode(#{?OPS => flatten_ops(~"page", [ListPatchOp])})),
    ?assertEqual(
        ~"{\"o\":[[10,\"page:4\",[[7,0,[[0,\"0\",\"X\"]]],[5,1,{\"f\":\"fp\"}],[6,2]]]]}",
        Bytes
    ).

%% encode_ops/1 must emit byte-for-byte what the generic
%% `json:encode(Ops, fun op_encoder/2)` path emits -- the specialization is
%% dispatch removal plus the consecutive-target memo, never a format change.
%% The ops list deliberately walks every branch: a run of tagged ops sharing
%% one target (memo hits), a target flip and a view flip mid-run (memo
%% misses), an untagged replace op (memo survives across it), every arg
%% shape (escapes, unicode, control bytes, negative/zero integers, nested
%% and empty lists, the `#{~"raw" => ...}` marker and a fingerprint payload
%% map), and the two-digit op code 10.
encode_ops_matches_generic_encoder_test() ->
    Ops = [
        {~"page", [7, ~"az-0", ~"1", [[0, ~"in-1", ~"same target run"]]]},
        {~"page", [7, ~"az-0", ~"2", [[0, ~"in-1", ~"a\"b\\c\nd\td"]]]},
        {~"page", [7, ~"az-0", ~"3", [[1, ~"in-2", ~"cls", ~"wide"], [2, ~"in-2", ~"x"]]]},
        {~"page", [5, ~"az-1", ~"4", -1, ~"<li class=\"x\">unicode: αβ→</li>"]},
        [8, ~"page", ~"<main>untagged replace</main>"],
        {~"page", [9, ~"az-1", ~"5", 0]},
        {~"child", [0, ~"az-0", ~"view flip"]},
        {~"child", [0, ~"az-0", #{~"raw" => ~"<script>1</script>"}]},
        {~"child", [10, ~"az-2", [[7, 0, [[0, ~"0", ~"X"]]], [5, 1, #{~"f" => ~"fp"}], [6, 2]]]},
        {~"child", [0, ~"az-3", #{~"f" => ~"fp", ~"s" => [~"<b>", ~"</b>"], ~"d" => [[~"v"]]}]},
        {~"ev\"il", [0, ~"az-0", ~"escaped view id"]},
        {~"ev\"il", [4, ~"az-0"]},
        {~"page", [0, <<0, 1, 31>>, <<"control bytes", 0, 31>>]}
    ],
    ?assertEqual(
        iolist_to_binary(json:encode(Ops, fun op_encoder/2)),
        iolist_to_binary(encode_ops(Ops))
    ),
    ?assertEqual(~"[]", iolist_to_binary(encode_ops([]))).

%% A view id is the app-supplied `id` binding and is NOT validated, so a value
%% carrying a JSON metacharacter (`"`) must be escaped in the ops frame. An
%% unescaped quote would terminate the target string early and inject ops
%% (OP_REPLACE, or an HTML-payload OP_TEXT -> innerHTML: XSS via the diff
%% channel). op_encoder runs the scoped target through json:encode, so the quote
%% is escaped rather than closing the string, and the frame stays valid JSON with
%% the id intact.
op_encoder_escapes_view_id_test() ->
    Op = [0, ~"0", ~"New"],
    Bytes = iolist_to_binary(encode(#{?OPS => flatten_ops(~"ev\"il", [Op])})),
    ?assertEqual(~"{\"o\":[[0,\"ev\\\"il:0\",\"New\"]]}", Bytes),
    %% Round-trips as valid JSON (the raw, unescaped emit would not parse).
    ?assertEqual(#{~"o" => [[0, ~"ev\"il:0", ~"New"]]}, json:decode(Bytes)).

-endif.

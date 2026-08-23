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

Outbound text frames are JSON maps with keys `~"o"` (ops) and/or
`~"e"` (effects). Both are arrays produced by `arizona_diff` and
`arizona_js` respectively.

## Exit handling

The live process is linked. Exits map to WebSocket close codes:

- `normal`, `shutdown`, `{shutdown, _other_}` -- graceful close
  `1000`. Client does NOT auto-reconnect (treats it as a deliberate
  end of session).
- `{shutdown, drain}` -- graceful close `1001` ("going away"). Client
  auto-reconnects via the form-state-preserving path; new live
  process mounts against the new server version.
- Anything else (including raises in `handle_in/2`) -- crash close
  `4500`. Client triggers `location.reload()` for a fresh start.
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
-define(EFFECTS, ~"e").
-define(SYS_PING, ~"0").
-define(SYS_PONG, ~"1").
-define(CLOSE_GOING_AWAY, 1001).
-define(CLOSE_CRASH, 4500).
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
    pending_resync = undefined :: undefined | reference()
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
        %% event/patch drain below relies on -- see drain_pending_pushes/1.
        ConnInfo = #{
            capabilities => Capabilities, reconnect => Reconnect, push_barrier => true
        },
        {ok, Pid} = arizona_live:start_link(Handler, Bindings, self(), OnMount, ConnInfo),
        init_view(Reconnect, FpsFollow, Pid, Socket)
    end).

%% The three connect shapes, by reconnect and the client's fingerprint-follow
%% promise.
%%
%% A flagged reconnect defers the whole mount+render to the resync flush: the
%% mount must not run twice, and no frame can reach the unmounted live process
%% -- handle_in flushes before processing any frame, and an unmounted process
%% has no subscriptions or timers to push from.
init_view(true, true, Pid, Socket) ->
    TRef = erlang:send_after(?RESYNC_TIMEOUT_MS, self(), arizona_resync_timeout),
    {ok, Socket#socket{pid = Pid, pending_resync = TRef}};
init_view(true, false, Pid, Socket) ->
    {ok, ViewId, PageHTML} = arizona_live:mount_and_render(Pid),
    Ops = replace_ops(ViewId, PageHTML),
    {reply, encode(#{?OPS => Ops}), Socket#socket{pid = Pid, view_id = ViewId}};
init_view(false, _FpsFollow, Pid, Socket) ->
    {ok, ViewId} = arizona_live:mount(Pid),
    {ok, Socket#socket{pid = Pid, view_id = ViewId}}.

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
handle_in(Frame, #socket{pending_resync = TRef, pid = Pid} = Socket0) when
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
handle_in(?SYS_PING, Socket) ->
    {reply, ?SYS_PONG, Socket};
handle_in(JSON, #socket{pid = Pid, view_id = RootViewId} = Socket) ->
    %% `try ... of` so the catch fires ONLY on a malformed decode -- exceptions
    %% raised inside the matched clause bodies (the inner navigate/dispatch
    %% trys) are not caught here, so a genuine handler crash still closes 4500.
    try json:decode(JSON) of
        [~"cached_fps", FpList] when is_list(FpList) ->
            arizona_live:seed_fps(Pid, FpList),
            {ok, Socket};
        [~"navigate", #{~"path" := Path, ~"qs" := Qs}] when
            is_binary(Path), is_binary(Qs)
        ->
            try
                handle_navigate(Path, Qs, Socket)
            catch
                Class:Reason:Stacktrace ->
                    logger:error("~s: ~p~n~p", [Class, Reason, Stacktrace]),
                    close_crash(Socket)
            end;
        [~"patch", #{~"path" := Path, ~"qs" := Qs}] when
            is_binary(Path), is_binary(Qs)
        ->
            try
                handle_patch(Path, Qs, Socket)
            catch
                Class:Reason:Stacktrace ->
                    logger:error("~s: ~p~n~p", [Class, Reason, Stacktrace]),
                    close_crash(Socket)
            end;
        [Target, Event, Payload] when is_binary(Event), is_map(Payload) ->
            ViewId = event_target(Target, RootViewId),
            try dispatch_event(Pid, ViewId, Event, Payload) of
                {AllOps, AllEffects} ->
                    {PendOps, PendEffects} = drain_pending_pushes(Socket),
                    encode_reply(PendOps ++ AllOps, PendEffects ++ AllEffects, Socket)
            catch
                Class:Reason:Stacktrace ->
                    logger:error("~s: ~p~n~p", [Class, Reason, Stacktrace]),
                    close_crash(Socket)
            end;
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
a reply frame -- but only when `ViewId` (the emitting page's root view id)
is still the socket's current one. A push emitted just before a navigate
is processed after it; retagging it with the new page's id would deliver
stale ops into the fresh view, so it is dropped instead. Handles `'EXIT'`
from the linked live process per the mapping in this module's
"Exit handling" section.
""".
-spec handle_info(Info, Socket) -> result() when
    Info :: term(),
    Socket :: socket().
handle_info(arizona_resync_timeout, #socket{pending_resync = TRef} = Socket0) when
    is_reference(TRef)
->
    %% Backstop: the flagged client never sent its promised `cached_fps`.
    %% Resync undeduped rather than leaving the page stale forever. (A stale
    %% timeout message -- the resync already flushed by a frame that raced the
    %% firing timer -- has `pending_resync = undefined` and falls through to
    %% the catch-all below.)
    resync_reply(flush_resync(Socket0#socket{pending_resync = undefined}));
handle_info({arizona_push, ViewId, Ops, Effects}, #socket{view_id = ViewId} = Socket) ->
    encode_reply(flatten_ops(ViewId, Ops), Effects, Socket);
handle_info({arizona_push, _StaleViewId, _Ops, _Effects}, Socket) ->
    %% Emitted by a page this socket already navigated away from -- drop.
    {ok, Socket};
handle_info({'EXIT', Pid, {shutdown, drain}}, #socket{pid = Pid} = Socket) ->
    %% Drain-initiated graceful exit. Close with 1001 (going away) so the
    %% JS client's auto-reconnect path runs (Worker treats any non-1000
    %% code as reconnectable; main thread preserves form state). Matches
    %% RFC 6455 §7.4 semantics for "server going away".
    {close, ?CLOSE_GOING_AWAY, <<>>, Socket};
handle_info({'EXIT', Pid, normal}, #socket{pid = Pid} = Socket) ->
    {close, 1000, <<>>, Socket};
handle_info({'EXIT', Pid, shutdown}, #socket{pid = Pid} = Socket) ->
    %% OTP graceful shutdown (atom form, e.g. supervisor-initiated).
    %% Close 1000 — same as `normal`, no reconnect. User code that
    %% wants reconnect on a custom shutdown should use the
    %% `{shutdown, drain}` reason explicitly.
    {close, 1000, <<>>, Socket};
handle_info({'EXIT', Pid, {shutdown, _}}, #socket{pid = Pid} = Socket) ->
    %% OTP graceful shutdown (tuple form, custom reason). Same
    %% close-1000 semantics as `shutdown` atom; only `{shutdown, drain}`
    %% (matched above) opts into the reconnect path.
    {close, 1000, <<>>, Socket};
handle_info({'EXIT', Pid, Reason}, #socket{pid = Pid} = Socket) ->
    logger:error("Live process ~p crashed: ~p", [Pid, Reason]),
    close_crash(Socket);
handle_info(_Info, Socket) ->
    {ok, Socket}.

%% --------------------------------------------------------------------
%% Internal functions
%% --------------------------------------------------------------------

safe_init(Handler, Socket, Fun) ->
    process_flag(trap_exit, true),
    try
        Fun()
    catch
        Class:Reason:Stacktrace ->
            logger:error("~s in ~p:~p~n~p", [Class, Handler, Reason, Stacktrace]),
            close_crash(Socket)
    end.

close_crash(Socket) ->
    {close, ?CLOSE_CRASH, ~"server crash", Socket}.

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

%% Mount the live process and render the reconnect full-page replace. The
%% dedup against `sent_fps` happens inside `mount_and_render` -- any
%% fingerprints seeded before this call elide their statics from the payload.
%%
%% This is a mount, so it carries the same guards as every other mount path
%% (`safe_init/3` at connect, the trys in do_navigate/do_patch/the event leg):
%% a raise anywhere in `mount/1`, `on_mount`, or `render/1` becomes the 4500
%% crash close the client's `crashReload()` guard is built for -- unguarded it
%% escaped both `handle_in/2` and `handle_info/2`, killed the session with no
%% close frame, and left the client backing off on a bare 1006 forever. The
%% drain/exit race gets the same 1001 going-away close its siblings give it.
%% Returns `{ok, Ops, Socket}` or a ready-made close result.
flush_resync(#socket{pid = Pid} = Socket) ->
    try arizona_live:mount_and_render(Pid) of
        {ok, ViewId, PageHTML} ->
            {ok, replace_ops(ViewId, PageHTML), Socket#socket{view_id = ViewId}}
    catch
        %% Same drain/exit race as do_navigate (see there).
        exit:{noproc, _} ->
            {close, ?CLOSE_GOING_AWAY, <<>>, Socket};
        exit:{{shutdown, drain}, _} ->
            {close, ?CLOSE_GOING_AWAY, <<>>, Socket};
        Class:Reason:Stacktrace ->
            logger:error("~s: ~p~n~p", [Class, Reason, Stacktrace]),
            close_crash(Socket)
    end.

resync_reply({ok, Ops, Socket}) ->
    {reply, encode(#{?OPS => Ops}), Socket};
resync_reply({close, _Code, _Reason, _Socket} = Close) ->
    Close.

%% Ship the resync frame BEFORE the triggering frame's own result: the client
%% must apply the full-page replace first, then the frame's reply against the
%% fresh DOM. A failed flush closes instead -- the frame must never reach a
%% live process the flush left unmounted.
resync_before_frame({ok, Ops, Socket}, Frame) ->
    resync_then(encode(#{?OPS => Ops}), handle_in(Frame, Socket));
resync_before_frame({close, _Code, _Reason, _Socket} = Close, _Frame) ->
    Close.

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
%% difference at ANY depth is disqualifying, since `arizona_render:apply_layouts/3`
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
%% The `full` opt is browser-only. The Android and iOS clients read only the
%% path off a navigate effect and send the frame straight back, so a `full`
%% reply to one would resolve, differ again, and re-emit -- a loop, not a page
%% load. Held off today only by native routes declaring no `layouts` (they wrap
%% no HTTP page), a convention nothing enforces; the unresolvable-route case
%% above has carried the same latent loop all along.
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
            try arizona_live:navigate(Pid, H, Bindings1, OnMount) of
                {ok, NewVId, PageHTML} ->
                    Ops = replace_ops(OldVId, PageHTML),
                    %% No effects: a navigate remounts, and mount/1 has no
                    %% effects channel (it also runs at SSR, where no client
                    %% exists to receive one). A view that wants one self-casts
                    %% from mount/1 and answers in handle_info/2, which pushes
                    %% its own frame rather than riding this reply.
                    encode_reply(Ops, [], Socket#socket{view_id = NewVId, handler = H})
            catch
                %% Live process exited between the navigate frame arriving and
                %% this gen_server:call landing — typical during a drain where
                %% handle_drain returned `{stop, _, _}`. Two sub-cases of the
                %% same race: the call finds the process already gone
                %% (`{noproc, _}`), or the process exits with `{shutdown, drain}`
                %% while the call is in flight (gen_server:call re-raises the
                %% server's exit reason as `{{shutdown, drain}, _}`). Both
                %% translate to a going-away close so the client's auto-reconnect
                %% path runs (1001 routes through the reconnect-with-form-state
                %% flow, not the crash reload). A genuine navigate crash exits
                %% with a different reason and still propagates to a 4500 close.
                exit:{noproc, _} ->
                    {close, ?CLOSE_GOING_AWAY, ~"", Socket};
                exit:{{shutdown, drain}, _} ->
                    {close, ?CLOSE_GOING_AWAY, ~"", Socket}
            end
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
            try arizona_live:patch(Pid, Bindings1) of
                {ok, Ops, Effects} ->
                    {PendOps, PendEffects} = drain_pending_pushes(Socket),
                    encode_reply(
                        PendOps ++ flatten_ops(ViewId, Ops), PendEffects ++ Effects, Socket
                    )
            catch
                %% Same drain/exit race as do_navigate (see there).
                exit:{noproc, _} ->
                    {close, ?CLOSE_GOING_AWAY, ~"", Socket};
                exit:{{shutdown, drain}, _} ->
                    {close, ?CLOSE_GOING_AWAY, ~"", Socket}
            end
    end.

%% Middleware halt during WS navigate -- there is no HTTP response channel
%% mid-session, so we translate an `arizona_req:redirect/2` halt into an
%% `arizona_js:navigate` client effect. A flash the middleware set via `put_flash/3`
%% before halting has no `Set-Cookie` leg to ride here, so it is stashed on the
%% socket for the follow-up frame (`take_pending_flash/2`), delivered exactly once
%% with no cookie. Halts without a stashed redirect close the socket so the client
%% reconnects and the next HTTP handshake receives the full middleware response.
halt_navigate(Requested, HaltReq, Socket) ->
    case arizona_req:halted_redirect(HaltReq) of
        {_Status, Location} ->
            encode_reply(
                [],
                [arizona_js:navigate(Location)],
                stash_halt_flash(Requested, Location, HaltReq, Socket)
            );
        undefined ->
            close_crash(Socket)
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
%% `null`-tagged op is unencodable -- it would crash the transport uncaught (the
%% encode runs in the `try ... of` body, outside handle_in's catch).
event_target(Target, _RootViewId) when is_binary(Target) -> Target;
event_target(_Target, RootViewId) -> RootViewId.

dispatch_event(Pid, ViewId, Event, Payload) ->
    {ok, Ops, Effects} = arizona_live:handle_event(Pid, ViewId, Event, Payload),
    {flatten_ops(ViewId, Ops), Effects}.

%% Selectively receive the `{arizona_push, ...}` messages the live process
%% emitted BEFORE the reply to a synchronous event/patch call, and fold them in
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
%% A push from a page this socket already navigated away from is dropped,
%% mirroring handle_info/2. The navigate path needs no drain: its OP_REPLACE
%% supersedes old-page ops, and the stale-view-id drop disposes of them once
%% processed after the reply.
drain_pending_pushes(#socket{view_id = ViewId}) ->
    drain_pending_pushes(ViewId, [], []).

drain_pending_pushes(ViewId, OpsAcc, EffectsAcc) ->
    receive
        {arizona_push, ViewId, Ops, Effects} ->
            drain_pending_pushes(
                ViewId, [flatten_ops(ViewId, Ops) | OpsAcc], [Effects | EffectsAcc]
            );
        {arizona_push, _StaleViewId, _Ops, _Effects} ->
            drain_pending_pushes(ViewId, OpsAcc, EffectsAcc);
        arizona_push_barrier ->
            drained_pushes(OpsAcc, EffectsAcc)
    after 0 ->
        drained_pushes(OpsAcc, EffectsAcc)
    end.

drained_pushes(OpsAcc, EffectsAcc) ->
    {lists:append(lists:reverse(OpsAcc)), lists:append(lists:reverse(EffectsAcc))}.

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

encode_reply_1([], [], Socket) ->
    {ok, Socket};
encode_reply_1(Ops, [], Socket) ->
    {reply, encode(#{?OPS => Ops}), Socket};
encode_reply_1([], Effects, Socket) ->
    {reply, encode(#{?EFFECTS => unwrap_effects(Effects)}), Socket};
encode_reply_1(Ops, Effects, Socket) ->
    {reply, encode(#{?OPS => Ops, ?EFFECTS => unwrap_effects(Effects)}), Socket}.

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

capture_flash_effect(
    {arizona_effect, [?EFFECT_NAVIGATE, Path, #{flash := Flash} = Opts]}, Pending
) ->
    capture_nav_flash(?EFFECT_NAVIGATE, Path, Flash, Opts, Pending);
capture_flash_effect(
    {arizona_effect, [?EFFECT_PATCH, Path, #{flash := Flash} = Opts]}, Pending
) ->
    capture_nav_flash(?EFFECT_PATCH, Path, Flash, Opts, Pending);
capture_flash_effect(Effect, Pending) ->
    {Effect, Pending}.

%% `flash` is always stripped from the client effect (the browser never sees it),
%% regardless of its shape -- so no non-map `flash` opt can leak. A non-map value is
%% a caller error and crashes here at `maps:merge` (fail-closed, like the strict
%% generator in `unwrap_effects/1`); there is deliberately no defensive `is_map` guard
%% that would let it fall through to the client instead.
capture_nav_flash(Op, Path, Flash, Opts, Pending) ->
    {{arizona_effect, [Op, Path, maps:remove(flash, Opts)]}, maps:merge(Pending, Flash)}.

%% Fast path for the three reply shapes produced by encode_reply/3. Hand
%% writes the outer `{"o":...}` / `{"e":...}` / both wrapper, skipping
%% OTP json's per-key map walk and the per-call escape on the constant
%% `<<"o">>`/`<<"e">>` keys. The Ops list goes through `json:encode/2`
%% with `op_encoder/2` -- the custom encoder emits `"<ViewId>:<Az>"`
%% inline as iodata, skipping the per-op binary concat (and per-target
%% `escape_binary/5` walk) that the previous `scope_ops` did. Effects
%% keep the default encoder -- they're plain JSON values.
encode(#{?OPS := Ops, ?EFFECTS := Effects}) ->
    [
        <<"{\"o\":">>,
        json:encode(Ops, fun op_encoder/2),
        <<",\"e\":">>,
        json:encode(Effects),
        $}
    ];
encode(#{?OPS := Ops}) ->
    [<<"{\"o\":">>, json:encode(Ops, fun op_encoder/2), $}];
encode(#{?EFFECTS := Effects}) ->
    [<<"{\"e\":">>, json:encode(Effects), $}];
encode(Map) ->
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

%% Custom JSON encoder. Pattern-matches the `{ViewId, RawOp}` tag
%% produced by `flatten_ops/2` and emits the JSON array with the scoped
%% target `<ViewId>:<Az>` as a JSON string. `Az` (Target) is
%% framework-generated (fingerprint-scoped, alphanumeric + dash) and safe,
%% but `ViewId` is the app-supplied `id` binding (root and `?stateful`
%% props) and is NOT validated -- an id from user data containing `"`
%% would break the ops frame, and a crafted value could inject ops
%% (an injected `OP_REPLACE`, or an `OP_TEXT` carrying an HTML payload,
%% reaches `innerHTML`: stored XSS via the diff channel). So the scoped
%% target is run through `json:encode/1`, which escapes the JSON-breaking
%% bytes (`"`/`\`); on safe ids this is byte-identical to the previous raw
%% emit. The SSR path already escapes the same id in HTML; this closes the
%% ops channel.
%% Op codes 0..9 emit as a single digit byte (`OpCode + $0`, skipping an
%% `integer_to_binary/1` per op); codes 10+ use `integer_to_binary/1` (see
%% `op_code_iodata/1`). Falls back to `json:encode_value/2` for everything
%% else (untagged replace ops, effects, payload values).
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

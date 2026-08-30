-module(arizona_live).
-moduledoc """
The live process: one `gen_server` per connected client.

Holds the root handler's bindings and snapshot, plus a `views` map of
nested stateful children. Bridges the transport with the render and
diff pipeline.

## Lifecycle

1. **Mount** -- `mount/1` or `mount_and_render/1` calls the root handler's
   `mount/1`, runs `on_mount` hooks, renders the first template, and
   stores the resulting snapshot.
2. **Events** -- `handle_event/4` dispatches a client event to either
   the root handler or a nested child view (located via `ViewId`),
   then diffs the resulting template against the prior snapshot and
   pushes ops back over the transport.
3. **Info messages** -- `handle_info/2` invokes the handler's optional
   `handle_info/2` callback, diffs, and pushes the resulting ops.
4. **Navigate** -- `navigate/3,4` unmounts the old page (embedded child
   views first, then the root -- the same removal semantics as a diff
   removal; `terminate/2` unmounts in the same order), cancels pending
   timers, and mounts the new handler. The previous root's final
   bindings are carried forward as the floor for the new mount's input
   -- `InitBindings` (route static config + middleware enrichments)
   overrides on key overlap. Keys the new handler omits from its mount
   return are dropped on the next navigate, so handlers control what
   persists by what they return. Stateful children's state (in `views`)
   is wiped.

## Process dictionary keys

- `$arizona_connected` -- set to `true` while a transport is attached;
  consulted by `connected/0` so render code can branch on SSR vs live.
- `$arizona_capabilities` -- the native-shell capabilities the client
  advertised at connect (`_az_caps`); set while a transport is attached and
  read by `capabilities/0` / `capability/1`. A UI/effect hint only.
- `$arizona_reconnected` -- `true` when this connection is a reconnection (vs the
  first connect); set while a transport is attached and read by `reconnected/0`.
- `$arizona_timers` -- `#{ViewId => [Ref]}` from `send_after/3`. A fired
  ref is pruned when its message is delivered; a removed child view's
  timers are cancelled synchronously (and its queued view messages
  flushed) on unmount; `navigate/3,4` cancels them all so stale timers
  don't fire after a page change.
- `$arizona_deps` -- per-dynamic dependency capture set, used by
  `arizona_eval` and `arizona_template:track/1`.

## Fingerprint deduplication

Templates carry a base-36 `f` fingerprint of their statics. Once a
fingerprint has been sent to the client, the live process strips the
statics from subsequent payloads sharing the same `f`, sending only
the dynamics. `seed_fps/2` is used by SSR to pre-populate the set with
fingerprints already shipped in the initial HTML.
""".
-behaviour(gen_server).

-include("arizona.hrl").

%% --------------------------------------------------------------------
%% API function exports
%% --------------------------------------------------------------------

-export([start_link/4]).
-export([start_link/5]).
-export([connected/0]).
-export([capabilities/0]).
-export([capability/1]).
-export([reconnected/0]).
-export([send/2]).
-export([send_after/3]).
-export([mount/1]).
-export([mount_and_render/1]).
-export([render_current/1]).
-export([stop/1]).
-export([navigate/3]).
-export([navigate/4]).
-export([patch/2]).
-export([handle_event/4]).
-export([seed_fps/2]).
-export([merge_seed_fps/2]).
-export([dedup_fps/2]).
-export([apply_on_mount/2]).
-export([format_error/2]).
-export([view_state/1]).

%% --------------------------------------------------------------------
%% gen_server callback exports
%% --------------------------------------------------------------------

-export([init/1]).
-export([handle_call/3]).
-export([handle_cast/2]).
-export([handle_info/2]).
-export([terminate/2]).

%% --------------------------------------------------------------------
%% Ignore xref warnings
%% --------------------------------------------------------------------

-ignore_xref([
    connected/0,
    capabilities/0,
    capability/1,
    reconnected/0,
    send/2,
    send_after/3,
    navigate/3,
    merge_seed_fps/2,
    dedup_fps/2,
    format_error/2
]).

%% --------------------------------------------------------------------
%% Ignore elvis warnings
%% --------------------------------------------------------------------

%% Several handlers (mount/event/info; root vs child) call the same helper
%% functions with the same destructure shape. That call-site shape is
%% intentional -- it's the point of having shared helpers.
-elvis([{elvis_style, dont_repeat_yourself, disable}]).

%% --------------------------------------------------------------------
%% Types exports
%% --------------------------------------------------------------------

-export_type([on_mount/0]).
-export_type([on_mount_hook/0]).
-export_type([route_opts/0]).

%% --------------------------------------------------------------------
%% Types definitions
%% --------------------------------------------------------------------

-nominal on_mount_hook() ::
    fun((map()) -> map()) | {module(), atom()}.
-nominal on_mount() :: [on_mount_hook()].

%% Route's static config: the map a router associates with each route
%% declaration. Consumed by `arizona_http`, `arizona_render`, and the
%% live runtime; produced by routers and the `arizona_req:resolve_route/3`
%% callback. All keys are optional and defaulted at use-site.
-nominal route_opts() :: #{
    bindings => arizona_template:bindings(),
    on_mount => on_mount(),
    layouts => [arizona_render:layout()],
    middlewares => [arizona_middleware:middleware()],
    %% CSRF Origin check is on by default; set false to opt this route out.
    check_origin => boolean(),
    _ => term()
}.

%% --------------------------------------------------------------------
%% Macros
%% --------------------------------------------------------------------

%% Upper bound on the per-connection `sent_fps` set. Client-reported cached
%% fingerprints (`cached_fps` frames) merge into it, so an unbounded merge lets a
%% crafted client grow it without limit (per-connection memory exhaustion). A
%% fingerprint is a base-36 phash2 of a template's statics, so the number a
%% legitimate connection accumulates is bounded by its distinct rendered
%% templates -- well under this cap. Overflowing merely disables static dedup for
%% the extra fingerprints (their statics re-ship), never a crash.
-define(MAX_SENT_FPS, 10000).

%% --------------------------------------------------------------------
%% Records
%% --------------------------------------------------------------------

-record(state, {
    handler :: module(),
    bindings :: map(),
    snapshot :: map() | undefined,
    %% #{ViewId => #{handler, bindings, snapshot}}
    views :: map(),
    on_mount :: on_mount(),
    transport_pid :: pid() | undefined,
    %% #{fingerprint_binary() => true}
    sent_fps :: map(),
    %% Does this transport fold queued pushes into synchronous replies? See
    %% push_barrier/1.
    push_barrier :: boolean(),
    %% Child views whose own diff has moved past the copy held in `snapshot`.
    %% Settled lazily, at the next root diff. See apply_pending_refresh/3.
    pending_refresh = #{} :: #{binary() => true}
}).

-type state() :: #state{}.

%% --------------------------------------------------------------------
%% API Functions
%% --------------------------------------------------------------------

-doc """
Returns `true` if the calling process is a live process attached to a
transport, `false` otherwise (e.g. during SSR).
""".
-spec connected() -> boolean().
connected() ->
    case erlang:get('$arizona_connected') of
        true -> true;
        _ -> false
    end.

-doc """
Returns the native-shell capabilities the embedding shell advertised at connect
(`#{}` in a plain browser or during SSR). See `arizona_os` and `capability/1`.
""".
-spec capabilities() -> map().
capabilities() ->
    case erlang:get('$arizona_capabilities') of
        Caps when is_map(Caps) -> Caps;
        _ -> #{}
    end.

-doc """
Returns `true` if the embedding native shell advertised capability `Key` at
connect, `false` otherwise (including a plain browser and during SSR).

This reflects an unauthenticated, client-advertised claim, so it is a UI/effect
hint only -- NEVER branch a server-side authorization decision on it.
""".
-spec capability(Key) -> boolean() when
    Key :: binary().
capability(Key) ->
    case erlang:get('$arizona_capabilities') of
        #{Key := true} -> true;
        _ -> false
    end.

-doc """
Returns `true` if this connection is a **reconnection** (the client dropped and
re-opened the WebSocket), `false` on the first connect and during SSR.

Mirrors `connected/0`. The live process re-mounts on every reconnect, so
`connected/0` alone cannot tell first-connect from reconnection. Use this to
suppress one-shot OS commands: `connected() andalso not reconnected()` fires a
one-shot (`notify`, `focus`) on the first connect only, while a declarative
re-assert (`set_title`, `fullscreen`) can fire on every connect.
""".
-spec reconnected() -> boolean().
reconnected() ->
    case erlang:get('$arizona_reconnected') of
        true -> true;
        _ -> false
    end.

-doc """
Sends a message to a specific view by id. The message is delivered to
the live process's mailbox and routed to the matching child view.
""".
-spec send(ViewId, Msg) -> term() when
    ViewId :: binary(),
    Msg :: term().
send(ViewId, Msg) ->
    self() ! {arizona_view, ViewId, Msg}.

-doc """
Like `send/2` but delivers after `Time` milliseconds. Returns the timer
ref, which is also tracked in the process dictionary (keyed by `ViewId`)
so a removed child view's pending timers can be cancelled on unmount and
`navigate/3,4` can cancel them all on page change. A fired ref is pruned
from the tracked set when its message is delivered.
""".
-spec send_after(ViewId, Time, Msg) -> reference() when
    ViewId :: binary(),
    Time :: non_neg_integer(),
    Msg :: term().
send_after(ViewId, Time, Msg) ->
    Ref = erlang:send_after(Time, self(), {arizona_view, ViewId, Msg}),
    Timers =
        case erlang:get('$arizona_timers') of
            undefined -> #{};
            T -> T
        end,
    ViewRefs = maps:get(ViewId, Timers, []),
    _ = erlang:put('$arizona_timers', Timers#{ViewId => [Ref | ViewRefs]}),
    Ref.

-doc """
Starts a live process for a route-level view `Handler`.

The transport pid receives `{arizona_push, RootViewId, Ops, Effects, Observed}`
messages when the live process diffs and emits updates; `RootViewId` is
the emitting page's root view id, so a transport can drop a push that
raced a navigate. `OnMount` is the route's hook
chain. Any request data the view needs is supplied as bindings by the
transport layer (e.g. via `arizona_middleware:extract/1` middlewares); the live
process is transport-agnostic and never sees a request.
""".
-spec start_link(Handler, InitBindings, TransportPid, OnMount) ->
    gen_server:start_ret()
when
    Handler :: module(),
    InitBindings :: map(),
    TransportPid :: pid() | undefined,
    OnMount :: on_mount().
start_link(Handler, InitBindings, TransportPid, OnMount) ->
    start_link(Handler, InitBindings, TransportPid, OnMount, #{}).

-doc """
Like `start_link/4` but also threads the connection context the transport knows --
`#{capabilities => map(), reconnect => boolean(), push_barrier => boolean()}` --
into the live process, where `capability/1` and `reconnected/0` read the first
two. Browser/SSR callers use `start_link/4` (empty context).

`push_barrier` is the transport's own contract, not client data: set it when the
transport folds queued `{arizona_push, ...}` messages into a synchronous
event/patch reply, and the live process marks the boundary between the pushes
that preceded that reply and anything emitted after it (see `push_barrier/1`).
Transports that ship every push in its own frame leave it unset.
""".
-spec start_link(Handler, InitBindings, TransportPid, OnMount, ConnInfo) ->
    gen_server:start_ret()
when
    Handler :: module(),
    InitBindings :: map(),
    TransportPid :: pid() | undefined,
    OnMount :: on_mount(),
    ConnInfo :: #{
        capabilities => map(), reconnect => boolean(), push_barrier => boolean()
    }.
start_link(Handler, InitBindings, TransportPid, OnMount, ConnInfo) ->
    %% Capture caller-side logger metadata (typically set by roadrunner
    %% with the per-conn request_id) so any ?LOG_* from inside the
    %% view's mount/handle_event/handle_info gets correlated to the
    %% originating request.
    ParentMetadata = logger:get_process_metadata(),
    gen_server:start_link(
        ?MODULE,
        {Handler, InitBindings, TransportPid, OnMount, ParentMetadata, ConnInfo},
        []
    ).

-doc """
Mounts the handler without rendering. Returns `{ok, ViewId}`.
""".
-spec mount(Pid) -> {ok, binary(), arizona_event_attrs:observed()} when
    Pid :: pid().
mount(Pid) ->
    gen_server:call(Pid, mount, infinity).

-doc """
Mounts and renders the handler. Returns `{ok, ViewId, PageContent, Observed}`
where `PageContent` is either a fingerprint payload (if the template
has `f`) or an HTML binary, and `Observed` the render-time az-* observations
(see `arizona_event_attrs`).
""".
-spec mount_and_render(Pid) ->
    {ok, binary(), binary() | map(), arizona_event_attrs:observed()}
when
    Pid :: pid().
mount_and_render(Pid) ->
    gen_server:call(Pid, mount_and_render, infinity).

-doc """
Re-renders the current view tree to a complete output binary from the
live process's current bindings and child views.

Unlike `mount_and_render/1` (which returns a fingerprint payload meant for
client-side diff application), this always materializes the full output --
intended for transports that repaint the whole view each frame (e.g. a
terminal renderer) instead of applying diff ops. Because it re-renders
through `arizona_render:render/2`, it threads the live `views` map and so
reflects current root *and* nested child state; the freshly produced
snapshot/views are discarded (read-only render).
""".
-spec render_current(Pid) -> {ok, binary(), arizona_event_attrs:observed()} when
    Pid :: pid().
render_current(Pid) ->
    gen_server:call(Pid, render_current, infinity).

-doc """
Stops a live process, running its `terminate/2` (and thus `unmount/1`) cleanup.

For transports that manage view lifecycles directly -- e.g. an SSH channel
closing one terminal session among many in a long-running daemon. The live
process is linked to its transport, but a transport that stops with reason
`normal` would not bring the view down via the link, so the transport stops it
explicitly.
""".
-spec stop(Pid) -> ok when Pid :: pid().
stop(Pid) ->
    gen_server:stop(Pid).

-doc """
Dispatches a client event to a view. If `ViewId` matches a nested
child, the event goes to that view; otherwise it goes to the root
handler. Returns `{ok, Ops, Effects, Observed}`.
""".
-spec handle_event(Pid, ViewId, Event, Payload) -> {ok, Ops, Effects, Observed} when
    Pid :: pid(),
    ViewId :: binary(),
    Event :: binary(),
    Payload :: map(),
    Ops :: [list()],
    Effects :: [term()],
    Observed :: arizona_event_attrs:observed().
handle_event(Pid, ViewId, Event, Payload) ->
    gen_server:call(Pid, {event, ViewId, Event, Payload}, infinity).

-doc """
SPA navigation: unmounts the current root handler, mounts a new one,
and returns fresh page content. Equivalent to
`navigate(Pid, NewHandler, InitBindings, [])`.
""".
-spec navigate(Pid, NewHandler, InitBindings) ->
    {ok, binary(), binary() | map(), arizona_event_attrs:observed()}
when
    Pid :: pid(),
    NewHandler :: module(),
    InitBindings :: map().
navigate(Pid, NewHandler, InitBindings) ->
    navigate(Pid, NewHandler, InitBindings, []).

-doc """
SPA navigation with `on_mount` hooks for the new handler.
""".
-spec navigate(Pid, NewHandler, InitBindings, OnMount) ->
    {ok, binary(), binary() | map(), arizona_event_attrs:observed()}
when
    Pid :: pid(),
    NewHandler :: module(),
    InitBindings :: map(),
    OnMount :: on_mount().
navigate(Pid, NewHandler, InitBindings, OnMount) ->
    gen_server:call(Pid, {navigate, NewHandler, InitBindings, OnMount}, infinity).

-doc """
In-place SPA navigation (`patch`): keeps the current root view mounted and
delivers `Params` to its `handle_update/3`, re-rendering through the diff
instead of remounting. The root handler, view id, process, and child views
all survive; only the changed slots produce ops. Returns the diff ops plus
any effects the reaction emitted (`handle_update`'s, folded with children's).

The caller (the socket) only invokes this when the patched route resolves to
the *same* root handler; a different handler must go through `navigate/4`.

`mount/1` and `on_mount` do **not** re-run on a patch -- they belong to the
mount phase, and a patch by definition does not remount (it would otherwise
clobber the live state the patch exists to preserve: `on_mount`'s output is a
*mount input* fed into `mount/1`, with no `mount/1` here it would land
unmediated on the live bindings). The route's **middlewares do run** (in the
socket's `do_patch`), so per-arrival, request-shaped derivation (session,
path params, ...) flows in as `Params`; handler-specific per-navigation logic
goes in `handle_update/3`, which sees both the new `Params` and the live state.
""".
-spec patch(Pid, Params) ->
    {ok, [arizona_diff:op()], [arizona_stateful:effect()], arizona_event_attrs:observed()}
when
    Pid :: pid(),
    Params :: map().
patch(Pid, Params) ->
    gen_server:call(Pid, {patch, Params}, infinity).

-doc """
Seeds the live process's `sent_fps` set with fingerprints already
shipped to the client (typically by SSR). Subsequent diffs will strip
statics for matching fingerprints.
""".
-spec seed_fps(Pid, FpList) -> ok when
    Pid :: pid(),
    FpList :: [binary()].
seed_fps(Pid, FpList) ->
    gen_server:cast(Pid, {seed_fps, FpList}).

-doc """
The state of the views this process holds: the root's handler and bindings, and
the same for each child view under `views`, keyed the way the process keys them.

`snapshot` is deliberately not included. It is the diff engine's bookkeeping --
the last rendered structure, kept to diff the next render against -- not part of
what a view holds, and it is large.
""".
-spec view_state(Pid) ->
    #{
        handler := module(),
        bindings := arizona_template:bindings(),
        views := #{
            binary() => #{handler := module(), bindings := arizona_template:bindings()}
        }
    }
when
    Pid :: pid().
view_state(Pid) ->
    %% Finite, unlike the `infinity` the render calls use: this is an
    %% introspection read, and a view busy in a long callback should time the
    %% reader out rather than block it for as long as the callback runs.
    gen_server:call(Pid, view_state, 5000).

-doc """
Folds an `on_mount` hook chain over `Bindings`. Each hook is either a
1-arity fun or a `{Module, Function}` tuple whose target has arity 1.
Used both internally and exposed for SSR-style rendering paths in
`arizona_render`.
""".
-spec apply_on_mount(OnMount, Bindings) -> Bindings1 when
    OnMount :: on_mount(),
    Bindings :: map(),
    Bindings1 :: map().
apply_on_mount([], Bindings) ->
    Bindings;
apply_on_mount([{Mod, Fun} | Rest], Bindings) ->
    apply_on_mount(Rest, Mod:Fun(Bindings));
apply_on_mount([Fun | Rest], Bindings) ->
    apply_on_mount(Rest, Fun(Bindings)).

%% --------------------------------------------------------------------
%% gen_server Callbacks
%% --------------------------------------------------------------------

-spec init({Handler, InitBindings, TransportPid, OnMount, ParentMetadata, ConnInfo}) ->
    {ok, state()}
when
    Handler :: module(),
    InitBindings :: map(),
    TransportPid :: pid() | undefined,
    OnMount :: on_mount(),
    ParentMetadata :: logger:metadata() | undefined,
    ConnInfo :: #{
        capabilities => map(), reconnect => boolean(), push_barrier => boolean()
    }.
init({Handler, InitBindings, TransportPid, OnMount, ParentMetadata, ConnInfo}) ->
    proc_lib:set_label({arizona_live, Handler}),
    inherit_logger_metadata(ParentMetadata),
    Capabilities = maps:get(capabilities, ConnInfo, #{}),
    Reconnect = maps:get(reconnect, ConnInfo, false),
    Barrier = maps:get(push_barrier, ConnInfo, false),
    TransportPid =/= undefined andalso erlang:put('$arizona_connected', true),
    TransportPid =/= undefined andalso erlang:put('$arizona_capabilities', Capabilities),
    TransportPid =/= undefined andalso erlang:put('$arizona_reconnected', Reconnect),
    %% Arm the render-time observation collector (see arizona_event_attrs).
    %% Peerless renders (SSR/static, TransportPid = undefined) never arm it, so
    %% observation stays a no-op outside a transported live process.
    TransportPid =/= undefined andalso arizona_event_attrs:arm(),
    %% Monitor the transport so a *normal* transport exit tears the view down.
    %% The start_link link only brings the view down on an *abnormal* transport
    %% exit; a `normal` exit (the common case -- a clean client disconnect) is
    %% silently ignored by this non-trapping process, which would otherwise leave
    %% the live process (bindings, snapshot, child views, pending timers, pubsub
    %% subscriptions) running forever after the socket is gone. The monitor fires
    %% for every exit reason, so a `{'DOWN', _, process, TransportPid, _}` in
    %% handle_info/2 reaps the view. A peerless render (SSR/static) passes
    %% `undefined` and sets up no monitor; the terminal transport passes a real
    %% pid, so it is also reaped if its session process dies without first
    %% calling `stop/1` (the normal terminal path stops the view synchronously,
    %% so the monitor never fires there).
    TransportPid =/= undefined andalso erlang:monitor(process, TransportPid),
    {ok, #state{
        handler = Handler,
        bindings = InitBindings,
        views = #{},
        on_mount = OnMount,
        transport_pid = TransportPid,
        sent_fps = #{},
        push_barrier = Barrier
    }}.

%% Mirror the parent process's logger metadata (request_id, peer, etc.)
%% so view-side ?LOG_* calls correlate to the originating request.
inherit_logger_metadata(undefined) ->
    ok;
inherit_logger_metadata(Metadata) when is_map(Metadata) ->
    logger:set_process_metadata(Metadata).

handle_call(
    mount,
    _From,
    #state{handler = H, bindings = B0, views = V0, on_mount = OM} = State
) ->
    {ViewId, _HTML, Snap, B2, V1} = do_mount(H, B0, V0, OM),
    %% A full render already reflects every child, so nothing is pending on it.
    {reply, {ok, ViewId, arizona_event_attrs:drain()}, State#state{
        bindings = B2, snapshot = Snap, views = V1, pending_refresh = #{}
    }};
handle_call(
    mount_and_render,
    _From,
    #state{
        handler = H,
        bindings = B0,
        views = V0,
        on_mount = OM,
        sent_fps = Fps0
    } = State
) ->
    {ViewId, HTML, Snap, B2, V1} = do_mount(H, B0, V0, OM),
    {PageContent1, Fps1, _Changed} = dedup_fp_val(page_content(Snap, HTML), Fps0),
    {reply, {ok, ViewId, PageContent1, arizona_event_attrs:drain()}, State#state{
        bindings = B2, snapshot = Snap, views = V1, sent_fps = Fps1, pending_refresh = #{}
    }};
handle_call(render_current, _From, #state{handler = H, bindings = B, views = V} = State) ->
    Tmpl = arizona_stateful:call_render(H, B),
    {HTML, _Snap, _Views1} = arizona_render:render(Tmpl, V),
    {reply, {ok, iolist_to_binary(HTML), arizona_event_attrs:drain()}, State};
handle_call({event, ViewId, Event, Payload}, From, #state{views = V0, bindings = B0} = State) ->
    ok = push_barrier(From, State),
    case V0 of
        #{ViewId := _} ->
            handle_child_event(ViewId, Event, Payload, From, State);
        #{} ->
            case maps:get(id, B0) of
                ViewId ->
                    handle_root_event(Event, Payload, State);
                RootId ->
                    %% Unknown view id -- neither the root nor a known child. Drop
                    %% it (no ops, no effects) instead of dispatching to the root,
                    %% so a crafted frame can't route an arbitrary event to the
                    %% root via a bogus id. `arizona_socket:event_target/2` already
                    %% maps a null/non-binary target to the root's real id, so a
                    %% legitimate push_event with no enclosing element still matches
                    %% the root here.
                    %%
                    %% Warn, because dropping is invisible from both ends: the
                    %% client gets no frame back and the view never runs. Without
                    %% this the only symptom is an event that silently does
                    %% nothing, forever.
                    logger:warning(
                        "event ~ts dropped: target view ~ts is neither the root "
                        "view (~ts) nor a child view. Usually an in-flight frame "
                        "for a view a diff has just removed, which is harmless; "
                        "otherwise an az-target naming a view that does not "
                        "exist, or a ?stateful rendered outside the live tree so "
                        "its az-view marker names a view that was never "
                        "registered",
                        [Event, ViewId, RootId]
                    ),
                    {reply, {ok, [], [], {[], []}}, State}
            end
    end;
handle_call({navigate, NewHandler, NewIB, NewOnMount}, _From, State) ->
    do_navigate_call(NewHandler, NewIB, NewOnMount, State);
handle_call({patch, Params}, From, #state{handler = H, bindings = B0} = State) ->
    ok = push_barrier(From, State),
    %% In-place navigation: the root view stays mounted. Deliver Params to its
    %% handle_update/3 (navigation as the root's prop source), then re-render
    %% and diff against the live snapshot -- no unmount, no remount, no timer
    %% cancel, no OP_REPLACE. The handler, view id, and child views all survive.
    %% Mirrors handle_root_event, but the reaction is handle_update, and the
    %% effect accumulator seeds empty (the patch is the originating action).
    {B1, Resets, Effects} = arizona_stateful:call_handle_update(H, Params, B0, []),
    {Ops1, Snap1, V1, B3, Fps1, NewState, Effects1} = process_root_change(
        H, B1, Resets, Effects, State
    ),
    {reply, {ok, Ops1, Effects1, arizona_event_attrs:drain()}, NewState#state{
        bindings = B3, snapshot = Snap1, views = V1, sent_fps = Fps1
    }};
handle_call(view_state, _From, #state{handler = H, bindings = B, views = V} = State) ->
    Views =
        #{
            ViewId => #{handler => ChildH, bindings => ChildB}
         || ViewId := #{handler := ChildH, bindings := ChildB} <- V
        },
    {reply, #{handler => H, bindings => B, views => Views}, State}.

handle_cast({seed_fps, FpList}, #state{sent_fps = Fps0} = State) ->
    {noreply, State#state{sent_fps = merge_seed_fps(Fps0, FpList)}};
handle_cast(_Msg, State) ->
    {noreply, State}.

handle_info(
    {'DOWN', _MonRef, process, TPid, _Reason},
    #state{transport_pid = TPid} = State
) ->
    %% The transport (WebSocket session) went away -- typically a clean client
    %% disconnect, which exits `normal` and so is not propagated by the link.
    %% Stop normally so terminate/2 runs the handler's unmount. Matching on the
    %% state's transport_pid means a DOWN for any other monitor the view set up
    %% falls through to the handler's handle_info below.
    {stop, normal, State};
handle_info({arizona_drain, Deadline}, #state{snapshot = Snap} = State) when
    Snap =/= undefined
->
    handle_drain_info(Deadline, State);
handle_info({arizona_drain, _Deadline}, State) ->
    %% Drain arriving before the view mounted -- the deferred reconnect resync
    %% window, exactly when a deploy-drain reconnect storm makes it likely.
    %% Falling through to the pre-mount drop below swallowed it: the transport
    %% had already acknowledged the drain, so the listener counted it handled
    %% and hard-killed the connection at the deadline, with no `{shutdown,
    %% drain}` exit and therefore no 1001 close to run the client's
    %% form-state-preserving reconnect.
    %%
    %% Stop with the drain reason rather than force a mount to run the
    %% callback: `handle_drain/2` takes the handler's own bindings, and
    %% pre-mount those are still the raw route bindings (route config plus
    %% middleware enrichments) the handler never produced -- a callback head
    %% that destructures its mount keys would raise `unhandled_drain` and close
    %% 4500 instead of 1001. Mounting first to avoid that would run the
    %% handler's mount side effects for a view that renders nothing and is
    %% unmounted microseconds later (and a mount crash would close 4500 too).
    %% There is also nothing for the callback to coordinate: nothing has been
    %% rendered for this connection, and the client is mid-reconnect already.
    %% So take the framework's documented default for a drain -- the same
    %% `{stop, Bindings, []}` `call_handle_drain/3` returns for a handler that
    %% does not export the callback.
    {stop, {shutdown, drain}, State};
handle_info(_Info, #state{snapshot = undefined} = State) ->
    {noreply, State};
handle_info({arizona_view, ViewId, Msg}, #state{bindings = B0, views = V0} = State) ->
    ok = prune_fired_timers(ViewId),
    case maps:get(id, B0) of
        ViewId ->
            handle_root_info(Msg, State);
        _ ->
            case V0 of
                #{ViewId := _} ->
                    handle_child_info(ViewId, Msg, State);
                #{} ->
                    erlang:error(
                        {unknown_view, ViewId, Msg},
                        [{arizona_view, ViewId, Msg}, B0, V0],
                        [{error_info, #{module => ?MODULE}}]
                    )
            end
    end;
handle_info(Info, State) ->
    handle_root_info(Info, State).

terminate(_Reason, #state{snapshot = Snap, handler = H, bindings = B, views = V}) when
    Snap =/= undefined
->
    %% Unmount every child view, then the root -- the same children-first
    %% removal semantics as navigate, for any exit reason terminate sees.
    ok = unmount_removed_views(V),
    ok = arizona_stateful:call_unmount(H, B);
terminate(_Reason, _State) ->
    %% Never mounted: `mount/1` has not run, so `bindings` are still the raw
    %% route bindings the handler never produced (route static config plus
    %% middleware enrichments -- session data included) and there is nothing
    %% mounted to tear down. Unmounting here would hand `unmount/1` a foreign
    %% map -- a handler head that destructures its own mount keys raises, and
    %% the `{unhandled_unmount, ...}` term embeds those bindings in the crash
    %% report -- while the cleanup the handler meant to do (paired with a mount
    %% that never ran) is a no-op anyway. The window is the deferred reconnect
    %% resync (`fps_follow`), where the view stays unmounted until the client's
    %% `cached_fps` frame or the socket's backstop timer, so a transport that
    %% goes away in it (a deploy-drain reconnect storm) lands right here.
    ok.

-doc """
Formats `arizona_live` runtime errors raised with an `error_info`
annotation pointing at this module. Picked up by
`erl_error:format_exception/3`.
""".
-spec format_error(Reason, Stacktrace) -> ErrorInfo when
    Reason :: term(),
    Stacktrace :: [tuple()],
    ErrorInfo :: #{general := iolist()}.
format_error({unknown_view, ViewId, Msg}, [{_M, _F, [_, _B, V0], _Info} | _]) ->
    #{
        general => io_lib:format(
            "no view matches id ~0tp; the message ~0tp can't be routed. "
            "Known child views in this live process: ~0tp. The id either "
            "belongs to an unmounted view or was sent before the child "
            "was embedded.",
            [ViewId, Msg, lists:sort(maps:keys(V0))]
        )
    }.

%% --------------------------------------------------------------------
%% Internal functions
%% --------------------------------------------------------------------

page_content(#{f := _} = Snap, _HTML) ->
    arizona_render:fingerprint_payload(Snap);
page_content(_Snap, HTML) ->
    iolist_to_binary(HTML).

do_navigate_call(NewHandler, NewIB, NewOnMount, State) ->
    #state{
        handler = OldH,
        bindings = OldB,
        views = OldV,
        transport_pid = TPid,
        sent_fps = Fps0,
        push_barrier = Barrier
    } = State,
    ok = cancel_pending_timers(),
    %% The outgoing page's children are discarded wholesale (the views map is
    %% wiped by do_mount below), which is a removal -- so unmount them exactly
    %% like a diff removal would: children first, then the root, mirroring
    %% removal semantics.
    ok = unmount_removed_views(OldV),
    ok = arizona_stateful:call_unmount(OldH, OldB),
    %% Carry the previous root handler's final bindings forward as the floor;
    %% NewIB (route static config + middleware enrichments) overrides on
    %% overlap. The new handler's `mount/1` receives `OldB ⊕ NewIB`, picks
    %% what it cares about, and returns its own bindings — values it does
    %% not include in the return are dropped. Handlers that want to keep
    %% session-level state (current_user, theme, locale) just include those
    %% keys in their mount return; everything else is page-local and
    %% naturally evaporates on the next navigate.
    %%
    %% Framework-restricted keys (currently `id`) are stripped from the
    %% carry: they're route-bound, and `do_mount` enforces that the new
    %% handler's mount must keep `Props` restricted keys verbatim --
    %% letting them carry would force the new route to pretend it's the
    %% old one.
    OldB1 = maps:without(arizona_eval:restricted_keys(), OldB),
    Merged = maps:merge(OldB1, NewIB),
    {NewViewId, HTML, Snap, B2, V1} = do_mount(NewHandler, Merged, #{}, NewOnMount),
    {PageContent1, Fps1, _Changed} = dedup_fp_val(page_content(Snap, HTML), Fps0),
    {reply, {ok, NewViewId, PageContent1, arizona_event_attrs:drain()}, #state{
        handler = NewHandler,
        bindings = B2,
        snapshot = Snap,
        views = V1,
        on_mount = NewOnMount,
        transport_pid = TPid,
        sent_fps = Fps1,
        push_barrier = Barrier
    }}.

do_mount(H, B0, V0, OnMount) ->
    B1 = apply_on_mount(OnMount, B0),
    {B2, Resets} = arizona_stateful:call_mount(H, B1),
    ok = arizona_eval:check_restricted_keys(B2, B1, H),
    ViewId = maps:get(id, B2),
    Tmpl = arizona_stateful:call_render(H, B2),
    {HTML, Snap, V1} = arizona_render:render(Tmpl, V0),
    B3 = arizona_stream:clear_stream_pending(B2, arizona_stream:stream_keys(B2)),
    B4 = maps:merge(B3, Resets),
    {ViewId, HTML, Snap, B4, V1}.

handle_root_event(Event, Payload, #state{handler = H, bindings = B0} = State) ->
    {B1, Resets, Effects} = arizona_stateful:call_handle_event(H, Event, Payload, B0),
    {Ops1, Snap1, V1, B3, Fps1, NewState, Effects1} = process_root_change(
        H, B1, Resets, Effects, State
    ),
    {reply, {ok, Ops1, Effects1, arizona_event_attrs:drain()}, NewState#state{
        bindings = B3, snapshot = Snap1, views = V1, sent_fps = Fps1
    }}.

handle_child_event(ViewId, Event, Payload, From, #state{views = V0} = State) ->
    #{ViewId := #{handler := H, bindings := B0} = View} = V0,
    {B1, Resets, Effects} = arizona_stateful:call_handle_event(H, Event, Payload, B0),
    {Ops1, V1, Fps1, Effects1} = process_child_change(H, B1, Resets, Effects, ViewId, View, State),
    NewState = mark_pending_refresh(ViewId, From, State#state{views = V1, sent_fps = Fps1}),
    {reply, {ok, Ops1, Effects1, arizona_event_attrs:drain()}, NewState}.

handle_root_info(Info, #state{handler = H, bindings = B0, transport_pid = TPid} = State) ->
    case arizona_stateful:call_handle_info(H, Info, B0) of
        ok ->
            {noreply, State};
        {B1, Resets, Effects} ->
            {Ops1, Snap1, V1, B3, Fps1, NewState, Effects1} = process_root_change(
                H, B1, Resets, Effects, State
            ),
            push(TPid, root_view_id(State), Ops1, Effects1),
            {noreply, NewState#state{
                bindings = B3, snapshot = Snap1, views = V1, sent_fps = Fps1
            }}
    end.

handle_child_info(ViewId, Msg, #state{views = V0, transport_pid = TPid} = State) ->
    #{ViewId := #{handler := H, bindings := B0} = View} = V0,
    case arizona_stateful:call_handle_info(H, Msg, B0) of
        ok ->
            {noreply, State};
        {B1, Resets, Effects} ->
            {Ops1, V1, Fps1, Effects1} = process_child_change(
                H, B1, Resets, Effects, ViewId, View, State
            ),
            push(TPid, root_view_id(State), scope_child_ops(ViewId, Ops1), Effects1),
            NewState = mark_pending_refresh(ViewId, State#state{views = V1, sent_fps = Fps1}),
            {noreply, NewState}
    end.

handle_drain_info(Deadline, #state{handler = H, bindings = B0, transport_pid = TPid} = State) ->
    case arizona_stateful:call_handle_drain(H, Deadline, B0) of
        ok ->
            {noreply, State};
        {stop, B1, Effects} ->
            %% Push effects (e.g. a "reconnecting" client indicator) before
            %% exiting so the WS session forwards them before it observes
            %% the {'EXIT', _, {shutdown, drain}} that closes the socket.
            %% The `{shutdown, drain}` reason routes through
            %% `arizona_socket`'s 1001 close path so the JS client
            %% auto-reconnects (vs `normal` which closes 1000 and stays
            %% disconnected). `terminate/2` still runs `unmount/1` as for
            %% any other graceful exit.
            push(TPid, root_view_id(State), [], Effects),
            {stop, {shutdown, drain}, State#state{bindings = B1}};
        {B1, Resets, Effects} ->
            {Ops1, Snap1, V1, B3, Fps1, NewState, Effects1} = process_root_change(
                H, B1, Resets, Effects, State
            ),
            push(TPid, root_view_id(State), Ops1, Effects1),
            {noreply, NewState#state{
                bindings = B3, snapshot = Snap1, views = V1, sent_fps = Fps1
            }}
    end.

%% Render the new template, diff against the root snapshot, dedup fingerprints,
%% unmount removed child views, and merge resets back into bindings.
%%
%% `Effects0` seeds the update-effects accumulator with the originating
%% callback's effects (the event/info/drain/patch that triggered this change).
%% Any child whose props changed runs its handle_update/3 during the diff and
%% folds its own effects onto the accumulator; the drained result is the full
%% list to ship -- no caller-side concatenation.
process_root_change(
    H,
    B1,
    Resets,
    Effects0,
    #state{
        bindings = B0, snapshot = Snap0, views = V0, sent_fps = Fps0, pending_refresh = Pending
    } = State
) ->
    Snap = apply_pending_refresh(Pending, V0, Snap0),
    Tmpl = arizona_stateful:call_render(H, B1),
    Changed = compute_changed(B0, B1),
    ok = arizona_eval:set_update_effects(Effects0),
    {Ops, Snap1, V1} = arizona_diff:diff(Tmpl, Snap, V0, Changed),
    Effects1 = arizona_eval:drain_update_effects(),
    RemovedViews = #{K => V || K := V <- V0, not is_map_key(K, V1)},
    ok = unmount_removed_views(RemovedViews),
    {Ops1, Fps1} = dedup_fps_if_noted(Ops, Fps0),
    B3 = clear_streams_and_apply_resets(B1, Resets),
    {Ops1, Snap1, V1, B3, Fps1, State#state{pending_refresh = #{}}, Effects1}.

%% Same idea as process_root_change/5 but for a nested child view. Diffs through
%% the dep-gated view-tracking path (diff/4, mirroring the root): only the
%% dynamics whose deps intersect the child's changed bindings re-evaluate, so a
%% grandchild stateful descriptor with untouched props is skipped (no spurious
%% handle_update) and resolves to a child snapshot instead of crashing the bare
%% diff (`bad_template_value`). NewViews is this child's freshly rendered
%% descendant subtree; reconcile it against the old subtree (recorded on Snap0 as
%% child_views): grandchildren the child no longer renders are unmounted, the rest
%% merged back, and the child's own snapshot records the new transitive set.
%%
%% `Effects0` seeds the update-effects accumulator exactly like the root path:
%% a grandchild whose props changed runs its handle_update/3 during this diff
%% and folds its effects onto the accumulator; the drained result is the full
%% list the caller ships (reply or push).
process_child_change(
    H, B1, Resets, Effects0, ViewId, #{bindings := B0, snapshot := Snap0} = View, #state{
        views = V0, sent_fps = Fps0
    }
) ->
    Tmpl = arizona_stateful:call_render(H, B1),
    Changed = compute_changed(B0, B1),
    ok = arizona_eval:set_update_effects(Effects0),
    {Ops, Snap1, NewViews} = arizona_diff:diff(Tmpl, Snap0, V0, Changed),
    Effects1 = arizona_eval:drain_update_effects(),
    {Ops1, Fps1} = dedup_fps_if_noted(Ops, Fps0),
    B3 = clear_streams_and_apply_resets(B1, Resets),
    NewDescendants = arizona_eval:child_view_set(NewViews),
    OldDescendants = maps:get(child_views, Snap0, #{}),
    Removed = [K || K := _ <- OldDescendants, not is_map_key(K, NewViews)],
    ok = unmount_removed_views(maps:with(Removed, V0)),
    Snap2 = Snap1#{child_views => NewDescendants},
    V1 = maps:merge(maps:without(Removed, V0), NewViews),
    V2 = V1#{ViewId => View#{bindings => B3, snapshot => Snap2}},
    {Ops1, V2, Fps1, Effects1}.

%% Note that child `ViewId`'s own diff has moved it past the copy the ROOT
%% snapshot holds. O(1) -- the copy is settled later, by apply_pending_refresh/3.
%%
%% The root snapshot is the diff baseline for the child's slot, and a child that
%% changes on its own (its own event, or a `?send`-driven `handle_info`) updates
%% only `views`. Left on the pre-event copy, the next root diff re-emits
%% everything the child had already patched: a redundant op for a plain slot,
%% and for a stream container a wholesale re-render of the list the child had
%% just patched item-by-item -- innerHTML, so focus, scroll, uncontrolled input
%% state and every `?local` inside the items are destroyed. Only the FIRST root
%% diff after the child change did it (the one after re-seeded the baseline from
%% its own fresh evaluation), which is exactly the diff a user is most likely to
%% be interacting through.
%%
%% Settling it here, on the child event itself, would rebuild every enclosing
%% container -- for a child inside a stream, every item's dynamics list plus the
%% container map, on every child event. That is linear in the list length on the
%% one shape this fix exists for, so a list whose rows each tick would make the
%% update cycle quadratic. Nothing reads the root snapshot between a child change
%% and the next root diff (`process_root_change/5` is its only reader), so the
%% work belongs there, where an O(snapshot) diff is already being paid.
mark_pending_refresh(ViewId, #state{pending_refresh = Pending} = State) ->
    State#state{pending_refresh = Pending#{ViewId => true}}.

%% Event-path variant: mark only when the TRANSPORT drove the event. A foreign
%% caller of the exported `handle_event/4` takes the resulting ops itself, so the
%% client never sees them -- settling the root's copy would then tell the diff the
%% client holds a value it was never sent, and that slot would stay wrong for
%% good. Left unmarked, the stale copy makes the next root diff re-emit it and the
%% client catches up. Mirrors push_barrier/2's caller check; the `handle_info`
%% path needs no such gate, since its ops always go to the transport.
mark_pending_refresh(ViewId, {Pid, _Tag}, #state{transport_pid = Pid} = State) ->
    mark_pending_refresh(ViewId, State);
mark_pending_refresh(_ViewId, _From, State) ->
    State.

%% Settle every child marked since the last root diff, in ONE walk -- so N
%% children ticking before a root diff costs one traversal, not N.
%%
%% SCOPE: ROOT diffs only. An intermediate stateful view's OWN diff still runs
%% against its own stored copy of a grandchild, so in a three-level tree where the
%% middle view takes the events, a grandchild that patched itself still gets the
%% wholesale re-render from that middle diff. Unchanged from before this settle
%% existed -- not a regression, and not a class this closes.
apply_pending_refresh(Pending, _Views, Snap) when map_size(Pending) =:= 0 ->
    Snap;
apply_pending_refresh(Pending, Views, Snap) ->
    %% Every view on the path down to a changed one is refreshed, not just the
    %% changed one itself: an ancestor's stored copy can predate the descendant's
    %% very EXISTENCE (the root's copy of a child that later rendered a
    %% grandchild holds the slot as it was before -- no grandchild in it at all),
    %% so there is nothing there to replace and only taking the ancestor's own
    %% live snapshot recovers the structure.
    HolderIds = refresh_holders(Pending, Views),
    refresh_into(fresh_child_snaps(HolderIds, Views), HolderIds, Snap).

%% The ids on a path to a changed view: the changed ids themselves, plus --
%% transitively -- every live view that records one of them among its
%% descendants. Drives both the descent prune and the set of copies refreshed.
%%
%% It cannot test `child_views` against the changed ids directly, because that
%% set is accurate only as of the container's LAST EVALUATION: a view created
%% afterwards (a grandchild first rendered by a nested child's own event, with no
%% enclosing container re-evaluated) is named only by its own parent. Chaining
%% upward through the live views map recovers the rest of the path -- the parent
%% names the grandchild, the container names the parent -- so the prune sees a
%% changed view at any depth. Iterated to a fixpoint because that chain can be
%% several levels long.
%%
%% Returned as a LIST, because that is how the walk consumes it: `holds_any_view/2`
%% scans these ids (a handful) against a container's `child_views` set rather than
%% the other way round, so the prune check is O(holders) instead of O(child_views).
%%
%% Runs once per settle, which is once per root diff -- a walk the diff is about
%% to do anyway. The child-event path never reaches here.
refresh_holders(Pending, Views) ->
    maps:keys(grow_holders(#{Id => true || Id := _ <- Pending}, descendant_index(Views))).

%% Only a view that records descendants can put another view on a path, so index
%% those once and let the fixpoint iterate the index instead of re-scanning every
%% live view per round. On a page whose views are mostly leaves (a long list of
%% simple children) the index is near-empty and the fixpoint settles immediately.
descendant_index(Views) ->
    #{
        Id => Ids
     || Id := #{snapshot := #{child_views := Ids}} <:- Views, map_size(Ids) =/= 0
    }.

grow_holders(Holders, Index) ->
    HolderIds = maps:keys(Holders),
    Grown = maps:merge(
        Holders,
        #{Id => true || Id := Ids <- Index, holds_any(HolderIds, Ids)}
    ),
    Settled = map_size(Holders),
    case map_size(Grown) of
        Settled -> Holders;
        _ -> grow_holders(Grown, Index)
    end.

%% The live snapshot of every id on a path to a change. An id can have been
%% removed since it was marked (a child's own diff can drop a grandchild), so
%% resolve each against `views` -- one no longer mounted simply isn't in the
%% result, and the walk then never matches it. Driven off the id set (a handful)
%% rather than off `views` (every live view on the page), so a page with
%% thousands of stateful children pays nothing per settle for the ones that did
%% not change; the single-element generator is the filter-and-bind.
fresh_child_snaps(HolderIds, Views) ->
    #{
        Id => ChildSnap
     || Id <- HolderIds, #{Id := #{snapshot := ChildSnap}} <- [Views]
    }.

%% Replace every copy of a changed view's snapshot held anywhere inside `Snap`.
%% `HolderIds` is the list of ids on a path to one (see refresh_holders/2), so a
%% container naming none of them is skipped whole and the walk follows only the
%% paths that matter; a container carrying no `child_views` at all (the root
%% snapshot itself) is descended into.
%%
%% Every step answers `unchanged` when nothing beneath it refers to a changed
%% view, so the caller keeps its existing term and only the paths actually
%% leading to one are rebuilt. Without that, settling a single child inside a
%% list rebuilt every item's dynamics list and the container map -- linear
%% allocation and garbage on every root diff, for a list the diff itself may well
%% be dep-skipping. `refresh_into/3` is the entry point that turns the answer
%% back into a plain snapshot.
refresh_into(Fresh, HolderIds, Snap) ->
    case refresh_container(Fresh, HolderIds, Snap) of
        unchanged -> Snap;
        {changed, Snap1} -> Snap1
    end.

refresh_view_snap(Fresh, HolderIds, #{view_id := Id} = Snap) ->
    case Fresh of
        #{Id := ChildSnap} ->
            %% Take this view's live snapshot -- then keep going INTO it. A
            %% view's snapshot is only rebuilt when that view itself diffs, so
            %% its own copies of deeper views can be stale in turn; and having
            %% just swapped in the live one, the annotation guiding the descent
            %% is now current too.
            {changed, refresh_into(Fresh, HolderIds, ChildSnap)};
        #{} ->
            refresh_container(Fresh, HolderIds, Snap)
    end;
refresh_view_snap(Fresh, HolderIds, Snap) ->
    refresh_container(Fresh, HolderIds, Snap).

refresh_container(Fresh, HolderIds, #{t := ?EACH, items := Items} = Snap) ->
    case holds_any_view(HolderIds, Snap) of
        true -> rewrap(refresh_each_items(Fresh, HolderIds, Items), items, Snap);
        false -> unchanged
    end;
refresh_container(Fresh, HolderIds, #{d := D} = Snap) when is_list(D) ->
    case holds_any_view(HolderIds, Snap) of
        true -> rewrap(refresh_dyns(Fresh, HolderIds, D), d, Snap);
        false -> unchanged
    end;
refresh_container(_Fresh, _HolderIds, _Value) ->
    unchanged.

rewrap(unchanged, _Key, _Snap) -> unchanged;
rewrap({changed, Value}, Key, Snap) -> {changed, Snap#{Key => Value}}.

%% Tested against `HolderIds`, not against the changed ids: a container's
%% `child_views` is only accurate as of its last evaluation, so it can name an
%% ancestor of the changed view without naming the view itself. `HolderIds` carries
%% those ancestors, which is what makes this exact at any depth. A container with
%% no annotation is descended into rather than skipped.
%%
%% The scan runs over `HolderIds` (the handful of views on a path to a change) and
%% probes the container's `child_views` SET, never the reverse. `child_views` is
%% page-sized -- a list container names every child it rendered -- so scanning it
%% made each prune check O(rows), and the walk does one per container; probing a
%% set makes it O(holders), flat in the page size. That is the whole reason
%% `child_views` is a set rather than a list: nothing reads it in order, and
%% everything asks it for membership.
holds_any_view(HolderIds, #{child_views := Ids}) ->
    holds_any(HolderIds, Ids);
holds_any_view(_HolderIds, #{}) ->
    true.

%% Empty first, so a leaf view (which records no descendants but is still asked)
%% answers on the guard instead of walking the holder ids.
holds_any(_HolderIds, Ids) when map_size(Ids) =:= 0 ->
    false;
holds_any(HolderIds, Ids) ->
    lists:any(fun(Id) -> is_map_key(Id, Ids) end, HolderIds).

%% A stream/map-keyed `?each` holds its items in a map, a plain-list one in a
%% list; either way an item is a list of `{Az, Value, Deps}` triples. The map
%% form updates in place, so only the keys actually holding a changed view cost
%% anything.
refresh_each_items(Fresh, HolderIds, Items) when is_map(Items) ->
    refresh_item_map(maps:keys(Items), Fresh, HolderIds, Items, unchanged);
refresh_each_items(Fresh, HolderIds, Items) when is_list(Items) ->
    refresh_item_list(Fresh, HolderIds, Items).

refresh_item_map([], _Fresh, _HolderIds, _Items, unchanged) ->
    unchanged;
refresh_item_map([], _Fresh, _HolderIds, Items, changed) ->
    {changed, Items};
refresh_item_map([Key | Rest], Fresh, HolderIds, Items, Status) ->
    #{Key := ItemD} = Items,
    case refresh_dyns(Fresh, HolderIds, ItemD) of
        unchanged ->
            refresh_item_map(Rest, Fresh, HolderIds, Items, Status);
        {changed, ItemD1} ->
            refresh_item_map(Rest, Fresh, HolderIds, Items#{Key => ItemD1}, changed)
    end.

refresh_item_list(_Fresh, _HolderIds, []) ->
    unchanged;
refresh_item_list(Fresh, HolderIds, [ItemD | Rest]) ->
    combine(
        refresh_dyns(Fresh, HolderIds, ItemD),
        ItemD,
        refresh_item_list(Fresh, HolderIds, Rest),
        Rest
    ).

refresh_dyns(_Fresh, _HolderIds, []) ->
    unchanged;
refresh_dyns(Fresh, HolderIds, [Dyn | Rest]) ->
    combine(
        refresh_dyn(Fresh, HolderIds, Dyn),
        Dyn,
        refresh_dyns(Fresh, HolderIds, Rest),
        Rest
    ).

%% Rebuild a cons cell only when this element or the tail actually changed.
combine(unchanged, _Head, unchanged, _Tail) -> unchanged;
combine(unchanged, Head, {changed, Tail1}, _Tail) -> {changed, [Head | Tail1]};
combine({changed, Head1}, _Head, unchanged, Tail) -> {changed, [Head1 | Tail]};
combine({changed, Head1}, _Head, {changed, Tail1}, _Tail) -> {changed, [Head1 | Tail1]}.

refresh_dyn(Fresh, HolderIds, {Az, Value}) ->
    retag(refresh_view_snap(Fresh, HolderIds, Value), fun(V) -> {Az, V} end);
refresh_dyn(Fresh, HolderIds, {Az, Value, Deps}) ->
    retag(refresh_view_snap(Fresh, HolderIds, Value), fun(V) -> {Az, V, Deps} end).

retag(unchanged, _Rebuild) -> unchanged;
retag({changed, Value}, Rebuild) -> {changed, Rebuild(Value)}.

clear_streams_and_apply_resets(B1, Resets) ->
    B2 = arizona_stream:clear_stream_pending(B1, arizona_stream:stream_keys(B1)),
    maps:merge(B2, Resets).

%% A removed view's in-flight ?send_after messages must die with it: cancel its
%% timers synchronously and flush any of its already-queued view messages BEFORE
%% unmounting, so a late `close`-style tick can never route to the pruned view
%% and crash the session with unknown_view.
unmount_removed_views(RemovedViews) ->
    maps:foreach(
        fun(Id, #{handler := H, bindings := B}) ->
            ok = cancel_view_timers(Id),
            ok = flush_view_messages(Id),
            ok = arizona_stateful:call_unmount(H, B)
        end,
        RemovedViews
    ).

%% Synchronous cancel: after this returns, each timer's message either was
%% already delivered (and is flushed right after) or never will be. An async
%% cancel could complete after the flush and deliver a stale message later.
cancel_view_timers(ViewId) ->
    case erlang:get('$arizona_timers') of
        undefined ->
            ok;
        #{ViewId := Refs} = Timers ->
            ok = cancel_timer_refs(Refs),
            _ = erlang:put('$arizona_timers', maps:remove(ViewId, Timers)),
            ok;
        #{} ->
            ok
    end.

cancel_timer_refs(Refs) ->
    lists:foreach(fun(Ref) -> ok = erlang:cancel_timer(Ref, [{info, false}]) end, Refs).

cancel_pending_timers() ->
    case erlang:erase('$arizona_timers') of
        undefined ->
            ok;
        Timers ->
            maps:foreach(fun(_ViewId, Refs) -> cancel_timer_refs(Refs) end, Timers)
    end,
    flush_view_messages().

%% Drop the refs whose timer already fired for this view, called when one of
%% its view-routed messages is delivered -- so the standard re-arming tick
%% idiom stays bounded instead of accumulating a dead ref per fire.
prune_fired_timers(ViewId) ->
    case erlang:get('$arizona_timers') of
        undefined ->
            ok;
        #{ViewId := Refs} = Timers ->
            case [Ref || Ref <- Refs, is_integer(erlang:read_timer(Ref))] of
                [] -> _ = erlang:put('$arizona_timers', maps:remove(ViewId, Timers));
                Live -> _ = erlang:put('$arizona_timers', Timers#{ViewId => Live})
            end,
            ok;
        #{} ->
            ok
    end.

flush_view_messages() ->
    receive
        {arizona_view, _, _} -> flush_view_messages()
    after 0 -> ok
    end.

flush_view_messages(ViewId) ->
    receive
        {arizona_view, ViewId, _} -> flush_view_messages(ViewId)
    after 0 -> ok
    end.

compute_changed(OldBindings, NewBindings) ->
    #{K => V || K := V <- NewBindings, key_changed(K, V, OldBindings)}.

%% True iff `K` is missing from `OldBindings`, or `OldBindings`
%% holds a different value for it. Pattern-bind `V` from `NewBindings`
%% and reuse it as the literal in the `OldBindings` match to confirm
%% equality.
key_changed(K, V, OldBindings) ->
    case OldBindings of
        #{K := V} -> false;
        #{} -> true
    end.

%% A child view's diff ops are addressed relative to the CHILD, so they must be
%% scoped by the child's view id -- while the push itself must keep naming the
%% ROOT view, which is what the transport compares against its current page to
%% drop a push that raced a navigate (see push/4). Both fit in the message
%% unchanged: hand the ops over in the same `[ChildViewId, ChildOps]` nesting a
%% root diff already uses for its embedded children, and the transport's
%% flattening re-tags them with the child id.
%%
%% Pushed bare, they were scoped with the root's id instead. `az` is
%% fingerprint-derived, so two instances of the same handler share it and only
%% the view id separates them: a `?send`/`?send_after` tick meant for one
%% instance patched whichever one the client resolved first. (The event path
%% never had the bug -- the transport scopes an event's ops with the child id
%% the frame named.)
%%
%% Empty stays empty so push/4 can still skip a no-op push.
scope_child_ops(_ViewId, []) ->
    [];
scope_child_ops(ViewId, Ops) ->
    [[ViewId, Ops]].

%% A push names the root view that owns it (the page's id at emit time), so a
%% transport can drop a push that raced a navigate -- processed after the
%% navigate it would otherwise be tagged with the NEW page's id and deliver
%% stale ops into the fresh view.
push(undefined, _ViewId, _Ops, _Effects) ->
    ok;
push(_Pid, _ViewId, [], []) ->
    ok;
push(Pid, ViewId, Ops, Effects) ->
    Pid ! {arizona_push, ViewId, Ops, Effects, arizona_event_attrs:drain()},
    ok.

%% Marks, in the transport's mailbox, the boundary between the pushes emitted
%% BEFORE the reply this call is about to produce and anything emitted after it.
%%
%% A transport that folds queued pushes into a synchronous reply must fold only
%% the former -- a push emitted before the reply is causally earlier, so it has
%% to be applied first, but one emitted after it is later and prepending it
%% inverts an order-dependent op pair (a stream MOVE landing in front of the
%% INSERT that created its key; the client drops the move and the server's
%% snapshot is wrong from then on). A transport cannot tell the two apart on its
%% own: the documented `?send`/`?send_after` idiom has the handler enqueue to
%% this process's own mailbox during `handle_event/3`, so the live process
%% replies and only then dequeues that message and pushes for it -- typically
%% before the transport is rescheduled to look at its own mailbox.
%%
%% Sending the marker from inside the call settles it: this process cannot push
%% between here and the reply, so every push already in flight is ahead of the
%% marker and every later one is behind it. The transport drains up to the
%% marker (it is guaranteed present -- it was sent before the reply the
%% transport has by then received) and leaves the rest for its own inbox path,
%% which ships them in a later frame, in order.
%%
%% Emitted only when the CALLER is the folding transport itself. `handle_event/4`
%% and `patch/2` are exported, so any process can call them on a live process
%% whose transport folds -- and a marker sent for someone else's call is one
%% nobody drains, which would offset the transport's drain by one from then on
%% (every later drain eats the previous cycle's marker, folds nothing, and lets a
%% stale push ship in a later frame over a newer value). Matching the caller
%% against `transport_pid` means such a call emits no marker at all, so the
%% transport's mailbox can never hold one it did not cause.
push_barrier({Pid, _Tag}, #state{push_barrier = true, transport_pid = Pid}) ->
    Pid ! arizona_push_barrier,
    ok;
push_barrier(_From, #state{}) ->
    ok.

root_view_id(#state{bindings = Bindings}) ->
    maps:get(id, Bindings).

-doc false.
-spec merge_seed_fps(Fps, FpList) -> Fps1 when
    Fps :: #{binary() => true},
    FpList :: [term()],
    Fps1 :: #{binary() => true}.
merge_seed_fps(Fps, FpList) ->
    %% Threads a growing accumulator whose cap check depends on its current size,
    %% so a fold (not a comprehension) is required.
    lists:foldl(fun seed_one_fp/2, Fps, FpList).

%% A fingerprint is a base-36 phash2 binary. Drop any non-binary entry (a crafted
%% `cached_fps` frame can carry arbitrary JSON, and `maps:from_keys/2` would
%% accept any term), and stop growing the set past ?MAX_SENT_FPS so repeated
%% frames of distinct keys cannot exhaust per-connection memory.
seed_one_fp(Fp, Acc) when is_binary(Fp) ->
    case Acc of
        #{Fp := _} -> Acc;
        #{} when map_size(Acc) < ?MAX_SENT_FPS -> Acc#{Fp => true};
        #{} -> Acc
    end;
seed_one_fp(_Fp, Acc) ->
    Acc.

%% Walk ops only when the diff actually built a fingerprint payload
%% (`arizona_render:drain_fp_note/0`): the dominant all-scalar frame has
%% nothing to strip and no fingerprint to record, so it skips the visit
%% entirely. A stale note walks one frame for nothing; a missed one cannot
%% happen (the producers set it unconditionally).
dedup_fps_if_noted(Ops, Fps) ->
    case arizona_render:drain_fp_note() of
        true -> dedup_fps(Ops, Fps);
        false -> {Ops, Fps}
    end.

%% Walk ops, stripping statics from fingerprinted payloads already sent.
%% Almost every frame strips nothing (its payloads are scalars), so each
%% walker also answers whether its sub-walk changed anything and hands back
%% the ORIGINAL term when it did not -- an untouched frame crosses without
%% one list cell or map being rebuilt. Only a stripped or rewritten payload
%% pays for reconstruction, and only along its own spine.
dedup_fps(Ops, Fps0) ->
    {Ops1, Fps1, _Changed} = dedup_fp_ops(Ops, Fps0),
    {Ops1, Fps1}.

dedup_fp_ops([], Fps) ->
    {[], Fps, false};
dedup_fp_ops([Op | Rest] = Ops, Fps0) ->
    {Op1, Fps1, OpChanged} = dedup_fp_op(Op, Fps0),
    {Rest1, Fps2, RestChanged} = dedup_fp_ops(Rest, Fps1),
    case OpChanged orelse RestChanged of
        true -> {[Op1 | Rest1], Fps2, true};
        false -> {Ops, Fps2, false}
    end.

dedup_fp_op([BinId, ChildOps] = Op, Fps0) when is_binary(BinId), is_list(ChildOps) ->
    %% Child view ops: [ViewId, InnerOps]
    {ChildOps1, Fps1, Changed} = dedup_fp_ops(ChildOps, Fps0),
    case Changed of
        true -> {[BinId, ChildOps1], Fps1, true};
        false -> {Op, Fps1, false}
    end;
dedup_fp_op([OpCode, Target | Rest] = Op, Fps0) when is_integer(OpCode) ->
    {Rest1, Fps1, Changed} = dedup_fp_vals(Rest, Fps0),
    case Changed of
        true -> {[OpCode, Target | Rest1], Fps1, true};
        false -> {Op, Fps1, false}
    end;
dedup_fp_op(Op, Fps) ->
    {Op, Fps, false}.

dedup_fp_val(#{~"f" := F, ~"s" := _, ~"d" := D} = Val, Fps) ->
    case Fps of
        #{F := _} ->
            {D1, Fps1, _Changed} = dedup_fp_vals(D, Fps),
            {maps:without([~"s"], Val#{~"d" => D1}), Fps1, true};
        #{} ->
            {D1, Fps1, Changed} = dedup_fp_vals(D, Fps#{F => true}),
            {rebuild_d(Val, D1, Changed), Fps1, Changed}
    end;
dedup_fp_val(#{~"f" := F, ~"d" := D} = Val, Fps) ->
    %% Already stripped (no ~"s"), still recurse into nested dynamics
    {D1, Fps1, Changed} = dedup_fp_vals(D, Fps),
    Fps2 =
        case Fps1 of
            #{F := _} -> Fps1;
            #{} -> Fps1#{F => true}
        end,
    {rebuild_d(Val, D1, Changed), Fps2, Changed};
dedup_fp_val(Items, Fps) when is_list(Items) ->
    %% List: stream items from ~"d" or inner ops from OP_ITEM_PATCH.
    %% Use dedup_fp_vals (not dedup_fp_ops) so fingerprinted maps in lists
    %% are properly matched and deduped -- dedup_fps/dedup_fp_op only
    %% recognizes op-shaped lists, not bare fingerprint maps.
    dedup_fp_vals(Items, Fps);
dedup_fp_val(Val, Fps) ->
    {Val, Fps, false}.

rebuild_d(Val, D1, true) -> Val#{~"d" => D1};
rebuild_d(Val, _D1, false) -> Val.

%% One walker for both an op's trailing args and a payload's d-list --
%% every element goes through dedup_fp_val/2 either way.
dedup_fp_vals([], Fps) ->
    {[], Fps, false};
dedup_fp_vals([H | T] = L, Fps0) ->
    {H1, Fps1, HChanged} = dedup_fp_val(H, Fps0),
    {T1, Fps2, TChanged} = dedup_fp_vals(T, Fps1),
    case HChanged orelse TChanged of
        true -> {[H1 | T1], Fps2, true};
        false -> {L, Fps2, false}
    end.

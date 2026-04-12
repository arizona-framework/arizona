-module(arizona_live).
-moduledoc """
The live process: one `gen_server` per connected client.

Holds the root handler's bindings and snapshot, plus a `views` map of
nested stateful children. Bridges the transport (typically a Cowboy
WebSocket handler) with the render and diff pipeline.

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
4. **Navigate** -- `navigate/3,4` unmounts the old root, cancels pending
   timers, mounts the new handler, and replies with fresh content.

## Process dictionary keys

- `$arizona_connected` -- set to `true` while a transport is attached;
  consulted by `connected/0` so render code can branch on SSR vs live.
- `$arizona_timers` -- list of refs from `send_after/3`, drained on
  `navigate/3,4` so stale timers don't fire after a page change.
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

%% --------------------------------------------------------------------
%% API function exports
%% --------------------------------------------------------------------

-export([start_link/1]).
-export([start_link/2]).
-export([start_link/3]).
-export([start_link/4]).
-export([connected/0]).
-export([send/2]).
-export([send_after/3]).
-export([mount/1]).
-export([mount_and_render/1]).
-export([navigate/3]).
-export([navigate/4]).
-export([handle_event/4]).
-export([seed_fps/2]).
-export([apply_on_mount/2]).

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

-ignore_xref([start_link/1, start_link/2]).

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

%% --------------------------------------------------------------------
%% Types definitions
%% --------------------------------------------------------------------

-nominal on_mount_hook() :: fun((map()) -> map()) | {module(), atom()}.
-nominal on_mount() :: [on_mount_hook()].

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
    sent_fps :: map()
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
ref, which is also tracked in the process dictionary so `navigate/3,4`
can cancel pending timers on page change.
""".
-spec send_after(ViewId, Time, Msg) -> reference() when
    ViewId :: binary(),
    Time :: non_neg_integer(),
    Msg :: term().
send_after(ViewId, Time, Msg) ->
    Ref = erlang:send_after(Time, self(), {arizona_view, ViewId, Msg}),
    Timers =
        case erlang:get('$arizona_timers') of
            undefined -> [];
            T -> T
        end,
    _ = erlang:put('$arizona_timers', [Ref | Timers]),
    Ref.

-doc """
Starts a live process for `Handler`. Equivalent to
`start_link(Handler, #{}, undefined, [])`.
""".
-spec start_link(Handler) -> {ok, pid()} | {error, term()} when
    Handler :: module().
start_link(Handler) ->
    start_link(Handler, #{}, undefined, []).

-doc """
Starts a live process with initial bindings.
""".
-spec start_link(Handler, InitBindings) -> {ok, pid()} | {error, term()} when
    Handler :: module(),
    InitBindings :: map().
start_link(Handler, InitBindings) ->
    start_link(Handler, InitBindings, undefined, []).

-doc """
Starts a live process with initial bindings and a transport pid.
The transport pid receives `{arizona_push, Ops, Effects}` messages
when the live process diffs and emits updates.
""".
-spec start_link(Handler, InitBindings, TransportPid) -> {ok, pid()} | {error, term()} when
    Handler :: module(),
    InitBindings :: map(),
    TransportPid :: pid() | undefined.
start_link(Handler, InitBindings, TransportPid) ->
    start_link(Handler, InitBindings, TransportPid, []).

-doc """
Starts a live process with initial bindings, a transport pid, and a
list of `on_mount` hooks applied before the handler's `mount/1`.
""".
-spec start_link(Handler, InitBindings, TransportPid, OnMount) -> {ok, pid()} | {error, term()} when
    Handler :: module(),
    InitBindings :: map(),
    TransportPid :: pid() | undefined,
    OnMount :: on_mount().
start_link(Handler, InitBindings, TransportPid, OnMount) ->
    gen_server:start_link(?MODULE, {Handler, InitBindings, TransportPid, OnMount}, []).

-doc """
Mounts the handler without rendering. Returns `{ok, ViewId}`.
""".
-spec mount(Pid) -> {ok, binary()} when
    Pid :: pid().
mount(Pid) ->
    gen_server:call(Pid, mount, infinity).

-doc """
Mounts and renders the handler. Returns `{ok, ViewId, PageContent}`
where `PageContent` is either a fingerprint payload (if the template
has `f`) or an HTML binary.
""".
-spec mount_and_render(Pid) -> {ok, binary(), binary() | map()} when
    Pid :: pid().
mount_and_render(Pid) ->
    gen_server:call(Pid, mount_and_render, infinity).

-doc """
Dispatches a client event to a view. If `ViewId` matches a nested
child, the event goes to that view; otherwise it goes to the root
handler. Returns `{ok, Ops, Effects}`.
""".
-spec handle_event(Pid, ViewId, Event, Payload) -> {ok, Ops, Effects} when
    Pid :: pid(),
    ViewId :: binary(),
    Event :: binary(),
    Payload :: map(),
    Ops :: [list()],
    Effects :: [term()].
handle_event(Pid, ViewId, Event, Payload) ->
    gen_server:call(Pid, {event, ViewId, Event, Payload}, infinity).

-doc """
SPA navigation: unmounts the current root handler, mounts a new one,
and returns fresh page content. Equivalent to
`navigate(Pid, NewHandler, InitBindings, [])`.
""".
-spec navigate(Pid, NewHandler, InitBindings) -> {ok, binary(), binary() | map()} when
    Pid :: pid(),
    NewHandler :: module(),
    InitBindings :: map().
navigate(Pid, NewHandler, InitBindings) ->
    navigate(Pid, NewHandler, InitBindings, []).

-doc """
SPA navigation with `on_mount` hooks for the new handler.
""".
-spec navigate(Pid, NewHandler, InitBindings, OnMount) ->
    {ok, binary(), binary() | map()}
when
    Pid :: pid(),
    NewHandler :: module(),
    InitBindings :: map(),
    OnMount :: on_mount().
navigate(Pid, NewHandler, InitBindings, OnMount) ->
    gen_server:call(Pid, {navigate, NewHandler, InitBindings, OnMount}, infinity).

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
Folds an `on_mount` hook chain over `Bindings`. Each hook is either a
1-arity fun or a `{Module, Function}` tuple. Used both internally and
exposed for SSR-style rendering paths in `arizona_render`.
""".
-spec apply_on_mount(OnMount, Bindings) -> Bindings1 when
    OnMount :: on_mount(),
    Bindings :: map(),
    Bindings1 :: map().
apply_on_mount([], Bindings) -> Bindings;
apply_on_mount([{Mod, Fun} | Rest], Bindings) -> apply_on_mount(Rest, apply(Mod, Fun, [Bindings]));
apply_on_mount([Fun | Rest], Bindings) -> apply_on_mount(Rest, apply(Fun, [Bindings])).

%% --------------------------------------------------------------------
%% gen_server Callbacks
%% --------------------------------------------------------------------

-spec init({Handler, InitBindings, TransportPid, OnMount}) -> {ok, state()} when
    Handler :: module(),
    InitBindings :: map(),
    TransportPid :: pid() | undefined,
    OnMount :: on_mount().
init({Handler, InitBindings, TransportPid, OnMount}) ->
    TransportPid =/= undefined andalso erlang:put('$arizona_connected', true),
    {ok, #state{
        handler = Handler,
        bindings = InitBindings,
        views = #{},
        on_mount = OnMount,
        transport_pid = TransportPid,
        sent_fps = #{}
    }}.

handle_call(mount, _From, #state{handler = H, bindings = B0, views = V0, on_mount = OM} = State) ->
    {ViewId, _HTML, Snap, B2, V1} = do_mount(H, B0, V0, OM),
    {reply, {ok, ViewId}, State#state{bindings = B2, snapshot = Snap, views = V1}};
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
    {PageContent1, Fps1} = dedup_fp_val(page_content(Snap, HTML), Fps0),
    {reply, {ok, ViewId, PageContent1}, State#state{
        bindings = B2, snapshot = Snap, views = V1, sent_fps = Fps1
    }};
handle_call({event, ViewId, Event, Payload}, _From, #state{views = V0} = State) ->
    case V0 of
        #{ViewId := _} ->
            handle_child_event(ViewId, Event, Payload, State);
        #{} ->
            handle_root_event(Event, Payload, State)
    end;
handle_call(
    {navigate, NewHandler, NewIB, NewOnMount},
    _From,
    #state{handler = OldH, bindings = OldB, transport_pid = TPid, sent_fps = Fps0} = _State
) ->
    ok = cancel_pending_timers(),
    ok = arizona_stateful:call_unmount(OldH, OldB),
    {NewViewId, HTML, Snap, B2, V1} = do_mount(NewHandler, NewIB, #{}, NewOnMount),
    {PageContent1, Fps1} = dedup_fp_val(page_content(Snap, HTML), Fps0),
    {reply, {ok, NewViewId, PageContent1}, #state{
        handler = NewHandler,
        bindings = B2,
        snapshot = Snap,
        views = V1,
        on_mount = NewOnMount,
        transport_pid = TPid,
        sent_fps = Fps1
    }}.

handle_cast({seed_fps, FpList}, #state{sent_fps = Fps0} = State) ->
    Fps1 = maps:merge(Fps0, maps:from_keys(FpList, true)),
    {noreply, State#state{sent_fps = Fps1}};
handle_cast(_Msg, State) ->
    {noreply, State}.

handle_info(_Info, #state{snapshot = undefined} = State) ->
    {noreply, State};
handle_info({arizona_view, ViewId, Msg}, #state{bindings = B0, views = V0} = State) ->
    case maps:get(id, B0) of
        ViewId ->
            handle_root_info(Msg, State);
        _ ->
            case V0 of
                #{ViewId := _} ->
                    handle_child_info(ViewId, Msg, State);
                #{} ->
                    error({unknown_view, ViewId, Msg})
            end
    end;
handle_info(Info, State) ->
    handle_root_info(Info, State).

terminate(_Reason, #state{handler = H, bindings = B}) ->
    ok = arizona_stateful:call_unmount(H, B);
terminate(_Reason, _State) ->
    ok.

%% --------------------------------------------------------------------
%% Internal functions
%% --------------------------------------------------------------------

page_content(#{f := _} = Snap, _HTML) ->
    arizona_render:fingerprint_payload(Snap);
page_content(_Snap, HTML) ->
    iolist_to_binary(HTML).

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
    {Ops1, Snap1, V1, B3, Fps1, NewState} = process_root_change(H, B1, Resets, State),
    {reply, {ok, Ops1, Effects}, NewState#state{
        bindings = B3, snapshot = Snap1, views = V1, sent_fps = Fps1
    }}.

handle_child_event(ViewId, Event, Payload, #state{views = V0} = State) ->
    #{ViewId := #{handler := H, bindings := B0} = View} = V0,
    {B1, Resets, Effects} = arizona_stateful:call_handle_event(H, Event, Payload, B0),
    {Ops1, V1, Fps1} = process_child_change(H, B1, Resets, ViewId, View, State),
    {reply, {ok, Ops1, Effects}, State#state{views = V1, sent_fps = Fps1}}.

handle_root_info(Info, #state{handler = H, bindings = B0, transport_pid = TPid} = State) ->
    case arizona_stateful:call_handle_info(H, Info, B0) of
        ok ->
            {noreply, State};
        {B1, Resets, Effects} ->
            {Ops1, Snap1, V1, B3, Fps1, NewState} = process_root_change(H, B1, Resets, State),
            push(TPid, Ops1, Effects),
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
            {Ops1, V1, Fps1} = process_child_change(H, B1, Resets, ViewId, View, State),
            push(TPid, Ops1, Effects),
            {noreply, State#state{views = V1, sent_fps = Fps1}}
    end.

%% Render the new template, diff against the root snapshot, dedup fingerprints,
%% unmount removed child views, and merge resets back into bindings.
process_root_change(
    H,
    B1,
    Resets,
    #state{
        bindings = B0, snapshot = Snap0, views = V0, sent_fps = Fps0
    } = State
) ->
    Tmpl = arizona_stateful:call_render(H, B1),
    Changed = compute_changed(B0, B1),
    {Ops, Snap1, V1} = arizona_diff:diff(Tmpl, Snap0, V0, Changed),
    RemovedViews = maps:without(maps:keys(V1), V0),
    ok = unmount_removed_views(RemovedViews),
    {Ops1, Fps1} = dedup_fps(Ops, Fps0),
    B3 = clear_streams_and_apply_resets(B1, Resets),
    {Ops1, Snap1, V1, B3, Fps1, State}.

%% Same idea as process_root_change/4 but for a nested child view.
process_child_change(H, B1, Resets, ViewId, #{snapshot := Snap0} = View, #state{
    views = V0, sent_fps = Fps0
}) ->
    Tmpl = arizona_stateful:call_render(H, B1),
    {Ops, Snap1} = arizona_diff:diff(Tmpl, Snap0),
    {Ops1, Fps1} = dedup_fps(Ops, Fps0),
    B3 = clear_streams_and_apply_resets(B1, Resets),
    V1 = V0#{ViewId => View#{bindings => B3, snapshot => maps:merge(Snap0, Snap1)}},
    {Ops1, V1, Fps1}.

clear_streams_and_apply_resets(B1, Resets) ->
    B2 = arizona_stream:clear_stream_pending(B1, arizona_stream:stream_keys(B1)),
    maps:merge(B2, Resets).

unmount_removed_views(RemovedViews) ->
    maps:foreach(
        fun(_Id, #{handler := H, bindings := B}) ->
            ok = arizona_stateful:call_unmount(H, B)
        end,
        RemovedViews
    ).

cancel_pending_timers() ->
    case erlang:erase('$arizona_timers') of
        undefined ->
            ok;
        Timers ->
            _ = [erlang:cancel_timer(Ref, [{async, true}, {info, false}]) || Ref <- Timers],
            ok
    end,
    flush_view_messages().

flush_view_messages() ->
    receive
        {arizona_view, _, _} -> flush_view_messages()
    after 0 -> ok
    end.

compute_changed(OldBindings, NewBindings) ->
    maps:filter(
        fun(K, V) ->
            case OldBindings of
                #{K := V} -> false;
                #{} -> true
            end
        end,
        NewBindings
    ).

push(undefined, _Ops, _Effects) ->
    ok;
push(_Pid, [], []) ->
    ok;
push(Pid, Ops, Effects) ->
    Pid ! {arizona_push, Ops, Effects},
    ok.

%% Walk ops, stripping statics from fingerprinted payloads already sent.
dedup_fps(Ops, Fps) ->
    lists:mapfoldl(fun dedup_fp_op/2, Fps, Ops).

dedup_fp_op([BinId, ChildOps], Fps0) when is_binary(BinId), is_list(ChildOps) ->
    %% Child view ops: [ViewId, InnerOps]
    {ChildOps1, Fps1} = dedup_fps(ChildOps, Fps0),
    {[BinId, ChildOps1], Fps1};
dedup_fp_op([OpCode, Target | Rest], Fps0) when is_integer(OpCode) ->
    {Rest1, Fps1} = dedup_fp_rest(Rest, Fps0),
    {[OpCode, Target | Rest1], Fps1};
dedup_fp_op(Op, Fps) ->
    {Op, Fps}.

dedup_fp_rest([], Fps) ->
    {[], Fps};
dedup_fp_rest([H | T], Fps0) ->
    {H1, Fps1} = dedup_fp_val(H, Fps0),
    {T1, Fps2} = dedup_fp_rest(T, Fps1),
    {[H1 | T1], Fps2}.

dedup_fp_val(#{~"f" := F, ~"s" := _, ~"d" := D} = Val, Fps) ->
    case Fps of
        #{F := _} ->
            {D1, Fps1} = dedup_fp_dlist(D, Fps),
            {maps:without([~"s"], Val#{~"d" => D1}), Fps1};
        #{} ->
            {D1, Fps1} = dedup_fp_dlist(D, Fps#{F => true}),
            {Val#{~"d" => D1}, Fps1}
    end;
dedup_fp_val(#{~"f" := F, ~"d" := D} = Val, Fps) ->
    %% Already stripped (no ~"s"), still recurse into nested dynamics
    {D1, Fps1} = dedup_fp_dlist(D, Fps),
    Fps2 =
        case Fps1 of
            #{F := _} -> Fps1;
            #{} -> Fps1#{F => true}
        end,
    {Val#{~"d" => D1}, Fps2};
dedup_fp_val(Items, Fps) when is_list(Items) ->
    %% List: stream items from ~"d" or inner ops from OP_ITEM_PATCH.
    %% Use dedup_fp_dlist (not dedup_fps) so fingerprinted maps in lists
    %% are properly matched and deduped -- dedup_fps/dedup_fp_op only
    %% recognizes op-shaped lists, not bare fingerprint maps.
    dedup_fp_dlist(Items, Fps);
dedup_fp_val(Val, Fps) ->
    {Val, Fps}.

dedup_fp_dlist([], Fps) ->
    {[], Fps};
dedup_fp_dlist([H | T], Fps0) ->
    {H1, Fps1} = dedup_fp_val(H, Fps0),
    {T1, Fps2} = dedup_fp_dlist(T, Fps1),
    {[H1 | T1], Fps2}.

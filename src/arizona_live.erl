-module(arizona_live).
-behaviour(gen_server).

-export([
    start_link/1, start_link/2, start_link/3, start_link/4,
    connected/0,
    send/2,
    send_after/3,
    mount/1,
    mount_and_render/1,
    navigate/3, navigate/4,
    handle_event/4,
    seed_fps/2,
    apply_on_mount/2
]).
-export([init/1, handle_call/3, handle_cast/2, handle_info/2, terminate/2]).
-ignore_xref([start_link/1, start_link/2]).

-type on_mount_hook() :: fun((map()) -> map()) | {module(), atom()}.
-type on_mount() :: [on_mount_hook()].
-export_type([on_mount/0, on_mount_hook/0]).

-record(state, {
    handler :: module(),
    bindings :: map(),
    snapshot :: map() | undefined,
    % #{ViewId => #{handler, bindings, snapshot}}
    views :: map(),
    on_mount :: on_mount(),
    transport_pid :: pid() | undefined,
    % #{fingerprint_binary() => true}
    sent_fps :: map()
}).

%% --- API --------------------------------------------------------------------

-spec connected() -> boolean().
connected() ->
    erlang:get('$arizona_connected') =:= true.

-spec send(ViewId :: binary(), Msg :: term()) -> term().
send(ViewId, Msg) ->
    self() ! {arizona_view, ViewId, Msg}.

-spec send_after(ViewId :: binary(), Time :: non_neg_integer(), Msg :: term()) -> reference().
send_after(ViewId, Time, Msg) ->
    Ref = erlang:send_after(Time, self(), {arizona_view, ViewId, Msg}),
    Timers =
        case erlang:get('$arizona_timers') of
            undefined -> [];
            T -> T
        end,
    _ = erlang:put('$arizona_timers', [Ref | Timers]),
    Ref.

start_link(Handler) ->
    start_link(Handler, #{}, undefined, []).

start_link(Handler, InitBindings) ->
    start_link(Handler, InitBindings, undefined, []).

start_link(Handler, InitBindings, TransportPid) ->
    start_link(Handler, InitBindings, TransportPid, []).

start_link(Handler, InitBindings, TransportPid, OnMount) ->
    gen_server:start_link(?MODULE, {Handler, InitBindings, TransportPid, OnMount}, []).

mount(Pid) ->
    gen_server:call(Pid, mount, infinity).

mount_and_render(Pid) ->
    gen_server:call(Pid, mount_and_render, infinity).

handle_event(Pid, ViewId, Event, Payload) ->
    gen_server:call(Pid, {event, ViewId, Event, Payload}, infinity).

navigate(Pid, NewHandler, InitBindings) ->
    navigate(Pid, NewHandler, InitBindings, []).

navigate(Pid, NewHandler, InitBindings, OnMount) ->
    gen_server:call(Pid, {navigate, NewHandler, InitBindings, OnMount}, infinity).

seed_fps(Pid, FpList) ->
    gen_server:cast(Pid, {seed_fps, FpList}).

%% --- gen_server callbacks ---------------------------------------------------

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
    PageContent =
        case Snap of
            #{f := _} ->
                arizona_render:fingerprint_payload(Snap);
            #{} ->
                iolist_to_binary(HTML)
        end,
    {PageContent1, Fps1} = dedup_fp_val(PageContent, Fps0),
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
    ok = maybe_unmount(OldH, OldB),
    {NewViewId, HTML, Snap, B2, V1} = do_mount(NewHandler, NewIB, #{}, NewOnMount),
    PageContent =
        case Snap of
            #{f := _} ->
                arizona_render:fingerprint_payload(Snap);
            #{} ->
                iolist_to_binary(HTML)
        end,
    {PageContent1, Fps1} = dedup_fp_val(PageContent, Fps0),
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

%% --- Internal ---------------------------------------------------------------

do_mount(H, B0, V0, OnMount) ->
    B1 = apply_on_mount(OnMount, B0),
    {B2, Resets} = H:mount(B1),
    ok = arizona_eval:check_restricted_keys(B2, B1, H),
    ViewId = maps:get(id, B2),
    Tmpl = H:render(B2),
    {HTML, Snap, V1} = arizona_render:render(Tmpl, V0),
    B3 = arizona_stream:clear_stream_pending(B2, arizona_stream:stream_keys(B2)),
    B4 = maps:merge(B3, Resets),
    {ViewId, HTML, Snap, B4, V1}.

handle_root_event(
    Event,
    Payload,
    #state{
        handler = H,
        bindings = B0,
        snapshot = Snap0,
        views = V0,
        sent_fps = Fps0
    } = State
) ->
    {B1, Resets, Effects} = H:handle_event(Event, Payload, B0),
    Tmpl = H:render(B1),
    Changed = compute_changed(B0, B1),
    {Ops, Snap1, V1} = arizona_diff:diff(Tmpl, Snap0, V0, Changed),
    RemovedViews = maps:without(maps:keys(V1), V0),
    ok = unmount_removed_views(RemovedViews),
    {Ops1, Fps1} = dedup_fps(Ops, Fps0),
    B2 = arizona_stream:clear_stream_pending(B1, arizona_stream:stream_keys(B1)),
    B3 = maps:merge(B2, Resets),
    {reply, {ok, Ops1, Effects}, State#state{
        bindings = B3, snapshot = Snap1, views = V1, sent_fps = Fps1
    }}.

handle_child_event(
    ViewId,
    Event,
    Payload,
    #state{views = V0, sent_fps = Fps0} = State
) ->
    #{ViewId := #{handler := H, bindings := B0, snapshot := Snap0} = View} = V0,
    {B1, Resets, Effects} = H:handle_event(Event, Payload, B0),
    Tmpl = H:render(B1),
    {Ops, Snap1} = arizona_diff:diff(Tmpl, Snap0),
    {Ops1, Fps1} = dedup_fps(Ops, Fps0),
    B2 = arizona_stream:clear_stream_pending(B1, arizona_stream:stream_keys(B1)),
    B3 = maps:merge(B2, Resets),
    V1 = V0#{ViewId => View#{bindings => B3, snapshot => maps:merge(Snap0, Snap1)}},
    {reply, {ok, Ops1, Effects}, State#state{views = V1, sent_fps = Fps1}}.

handle_root_info(
    Info,
    #state{
        handler = H,
        bindings = B0,
        snapshot = Snap0,
        views = V0,
        transport_pid = TPid,
        sent_fps = Fps0
    } = State
) ->
    case erlang:function_exported(H, handle_info, 2) of
        false ->
            {noreply, State};
        true ->
            {B1, Resets, Effects} = H:handle_info(Info, B0),
            Tmpl = H:render(B1),
            Changed = compute_changed(B0, B1),
            {Ops, Snap1, V1} = arizona_diff:diff(Tmpl, Snap0, V0, Changed),
            RemovedViews = maps:without(maps:keys(V1), V0),
            ok = unmount_removed_views(RemovedViews),
            {Ops1, Fps1} = dedup_fps(Ops, Fps0),
            B2 = arizona_stream:clear_stream_pending(B1, arizona_stream:stream_keys(B1)),
            B3 = maps:merge(B2, Resets),
            push(TPid, Ops1, Effects),
            {noreply, State#state{
                bindings = B3, snapshot = Snap1, views = V1, sent_fps = Fps1
            }}
    end.

handle_child_info(
    ViewId,
    Msg,
    #state{views = V0, transport_pid = TPid, sent_fps = Fps0} = State
) ->
    #{ViewId := #{handler := H, bindings := B0, snapshot := Snap0} = View} = V0,
    case erlang:function_exported(H, handle_info, 2) of
        false ->
            {noreply, State};
        true ->
            {B1, Resets, Effects} = H:handle_info(Msg, B0),
            Tmpl = H:render(B1),
            {Ops, Snap1} = arizona_diff:diff(Tmpl, Snap0),
            {Ops1, Fps1} = dedup_fps(Ops, Fps0),
            B2 = arizona_stream:clear_stream_pending(B1, arizona_stream:stream_keys(B1)),
            B3 = maps:merge(B2, Resets),
            V1 = V0#{ViewId => View#{bindings => B3, snapshot => maps:merge(Snap0, Snap1)}},
            push(TPid, Ops1, Effects),
            {noreply, State#state{views = V1, sent_fps = Fps1}}
    end.

apply_on_mount([], Bindings) -> Bindings;
apply_on_mount([{Mod, Fun} | Rest], Bindings) -> apply_on_mount(Rest, Mod:Fun(Bindings));
apply_on_mount([Fun | Rest], Bindings) -> apply_on_mount(Rest, Fun(Bindings)).

maybe_unmount(H, Bindings) ->
    case erlang:function_exported(H, unmount, 1) of
        true -> H:unmount(Bindings);
        false -> ok
    end.

unmount_removed_views(RemovedViews) ->
    maps:foreach(
        fun(_Id, #{handler := H, bindings := B}) ->
            ok = maybe_unmount(H, B)
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

terminate(_Reason, #state{handler = H, bindings = B}) ->
    ok = maybe_unmount(H, B);
terminate(_Reason, _State) ->
    ok.

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

%% --- Fingerprint dedup -------------------------------------------------------
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

dedup_fp_val(#{<<"f">> := F, <<"s">> := _, <<"d">> := D} = Val, Fps) ->
    case Fps of
        #{F := _} ->
            {D1, Fps1} = dedup_fp_dlist(D, Fps),
            {maps:without([<<"s">>], Val#{<<"d">> => D1}), Fps1};
        #{} ->
            {D1, Fps1} = dedup_fp_dlist(D, Fps#{F => true}),
            {Val#{<<"d">> => D1}, Fps1}
    end;
dedup_fp_val(#{<<"f">> := F, <<"d">> := D} = Val, Fps) ->
    %% Already stripped (no <<"s">>), still recurse into nested dynamics
    {D1, Fps1} = dedup_fp_dlist(D, Fps),
    Fps2 =
        case Fps1 of
            #{F := _} -> Fps1;
            #{} -> Fps1#{F => true}
        end,
    {Val#{<<"d">> => D1}, Fps2};
dedup_fp_val(Items, Fps) when is_list(Items) ->
    %% List: stream items from <<"d">> or inner ops from OP_ITEM_PATCH.
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

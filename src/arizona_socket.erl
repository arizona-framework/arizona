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

%% --------------------------------------------------------------------
%% Records
%% --------------------------------------------------------------------

-record(socket, {
    pid :: pid() | undefined,
    view_id :: binary() | undefined,
    handler :: module() | undefined,
    req :: az:request(),
    %% One-shot flash carried in-process across an SPA navigate/patch. A WebSocket
    %% frame has no `Set-Cookie` leg, so a flash set by a halting middleware (or an
    %% `arizona_js:navigate`/`patch` `flash` opt) rides the socket to the follow-up
    %% frame, where `take_pending_flash/2` injects it into the resolved request.
    pending_flash = #{} :: arizona_req:flash()
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
    | {close, pos_integer(), binary(), socket()}.

%% --------------------------------------------------------------------
%% API Functions
%% --------------------------------------------------------------------

-doc """
Creates a socket for `Handler` with the given `Bindings` and `Req`,
and starts its live process.

`Opts` may include:
- `reconnect` -- if `true`, immediately renders the page and replies
  with an `?OP_REPLACE` op (used when the client is reconnecting after
  a network drop)
- `on_mount` -- list of `t:arizona_live:on_mount/0` hooks

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
    OnMount = maps:get(on_mount, Opts, []),
    Capabilities = maps:get(capabilities, Opts, #{}),
    %% Track the root handler so a `patch` frame can decide same-view (patch in
    %% place) vs different-view (fall back to a full navigate/replace).
    Socket = #socket{req = Req, handler = Handler},
    safe_init(Handler, Socket, fun() ->
        ConnInfo = #{capabilities => Capabilities, reconnect => Reconnect},
        {ok, Pid} = arizona_live:start_link(Handler, Bindings, self(), OnMount, ConnInfo),
        case Reconnect of
            true ->
                {ok, ViewId, PageHTML} = arizona_live:mount_and_render(Pid),
                Ops = replace_ops(ViewId, PageHTML),
                {reply, encode(#{?OPS => Ops}), Socket#socket{pid = Pid, view_id = ViewId}};
            false ->
                {ok, ViewId} = arizona_live:mount(Pid),
                {ok, Socket#socket{pid = Pid, view_id = ViewId}}
        end
    end).

-doc """
Handles an inbound text frame.

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
                    encode_reply(AllOps, AllEffects, Socket)
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

Routes `{arizona_push, Ops, Effects}` from the live process into a
reply frame. Handles `'EXIT'` from the linked live process per the
mapping in this module's "Exit handling" section.
""".
-spec handle_info(Info, Socket) -> result() when
    Info :: term(),
    Socket :: socket().
handle_info({arizona_push, Ops, Effects}, #socket{view_id = ViewId} = Socket) ->
    encode_reply(flatten_ops(ViewId, Ops), Effects, Socket);
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

handle_navigate(Path, Qs, #socket{req = Req} = Socket) ->
    case resolve_route(Path, Qs, Req) of
        {ok, H, RouteOpts, NewReq} ->
            do_navigate(H, RouteOpts, NewReq, Socket);
        error ->
            full_navigate(Path, Qs, Socket)
    end.

%% A `patch` keeps the current view IFF the patched path resolves to the same
%% root handler; otherwise it can't (a different view needs a real mount), so it
%% degrades to a full navigate/replace. Resolves once and reuses the result for
%% either branch. A path that resolves to no live route degrades further to a
%% full-page navigation.
handle_patch(Path, Qs, #socket{handler = CurrentHandler, req = Req} = Socket) ->
    case resolve_route(Path, Qs, Req) of
        {ok, CurrentHandler, RouteOpts, NewReq} ->
            do_patch(RouteOpts, NewReq, Socket);
        {ok, H, RouteOpts, NewReq} ->
            do_navigate(H, RouteOpts, NewReq, Socket);
        error ->
            full_navigate(Path, Qs, Socket)
    end.

resolve_route(Path, Qs, Req) ->
    Adapter = arizona_req:adapter(Req),
    arizona_req:call_resolve_route(Adapter, Path, Qs, arizona_req:raw(Req)).

%% A navigate/patch target that doesn't resolve to a live route (a typo, a
%% controller/asset path, a 404) can't be SPA-navigated: the client would just
%% re-request it over the socket in a loop. Instead of crashing the live session,
%% tell the client to do a real full-page navigation to the path -- the browser
%% loads it normally (hitting the actual controller/asset or a 404 page).
full_navigate(Path, Qs, Socket) ->
    Url =
        case Qs of
            <<>> -> Path;
            _ -> <<Path/binary, "?", Qs/binary>>
        end,
    encode_reply([], [arizona_js:navigate(Url, #{full => true})], Socket).

do_navigate(H, RouteOpts, NewReq0, Socket0) ->
    {NewReq, #socket{pid = Pid, view_id = OldVId} = Socket} =
        take_pending_flash(NewReq0, Socket0),
    IB = maps:get(bindings, RouteOpts, #{}),
    OnMount = maps:get(on_mount, RouteOpts, []),
    Middlewares = maps:get(middlewares, RouteOpts, []),
    case arizona_middleware:apply_middlewares(Middlewares, NewReq, IB) of
        {halt, HaltReq} ->
            halt_navigate(HaltReq, Socket);
        {cont, _NewReq1, Bindings1} ->
            try arizona_live:navigate(Pid, H, Bindings1, OnMount) of
                {ok, NewVId, PageHTML} ->
                    Ops = replace_ops(OldVId, PageHTML),
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

%% Same root handler: keep the view, deliver the resolved route params to its
%% handle_update/3, and ship the diff ops + effects on the same `view_id` (no
%% replace, no remount). Runs the route middlewares first, exactly like navigate
%% -- but deliberately does NOT read `on_mount` (contrast do_navigate): on_mount
%% is a mount-phase hook and a patch does not remount (see arizona_live:patch/2).
do_patch(RouteOpts, NewReq0, Socket0) ->
    {NewReq, #socket{pid = Pid, view_id = ViewId} = Socket} =
        take_pending_flash(NewReq0, Socket0),
    IB = maps:get(bindings, RouteOpts, #{}),
    Middlewares = maps:get(middlewares, RouteOpts, []),
    case arizona_middleware:apply_middlewares(Middlewares, NewReq, IB) of
        {halt, HaltReq} ->
            halt_navigate(HaltReq, Socket);
        {cont, _NewReq1, Bindings1} ->
            try arizona_live:patch(Pid, Bindings1) of
                {ok, Ops, Effects} ->
                    encode_reply(flatten_ops(ViewId, Ops), Effects, Socket)
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
%% before halting has no `Set-Cookie` leg to ride here, so it rides the navigate
%% effect through the same `capture_pending_flash/2` path as an in-view handler
%% flash: stashed on the socket for the follow-up frame (`take_pending_flash/2`),
%% delivered exactly once with no cookie. Halts without a stashed redirect close the
%% socket so the client reconnects and the next HTTP handshake receives the full
%% middleware response.
halt_navigate(HaltReq, Socket) ->
    case arizona_req:halted_redirect(HaltReq) of
        {_Status, Location} ->
            Effect = navigate_effect(Location, arizona_req:flash_out(HaltReq)),
            encode_reply([], [Effect], Socket);
        undefined ->
            close_crash(Socket)
    end.

navigate_effect(Location, Flash) when map_size(Flash) > 0 ->
    arizona_js:navigate(Location, #{flash => Flash});
navigate_effect(Location, _Flash) ->
    arizona_js:navigate(Location).

%% Inject a one-shot in-process flash into the resolved navigate/patch request and
%% clear it from the socket (consumed once). Empty is a no-op so a real incoming
%% cookie flash on `NewReq` is never masked.
take_pending_flash(Req, #socket{pending_flash = Flash} = Socket) when map_size(Flash) > 0 ->
    {arizona_req:seed_flash(Req, Flash), Socket#socket{pending_flash = #{}}};
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

%% Single chokepoint for every reply that carries effects. Before encoding, an
%% in-view flash a handler set (an `arizona_js:navigate`/`patch` `flash` opt) is
%% moved onto the socket's one-shot pending flash and stripped from the outgoing
%% effect, so the flow is identical whether the flash came from a halting middleware
%% (`halt_navigate/2`) or a live handler -- the follow-up navigate frame injects it
%% via `take_pending_flash/2`.
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
    {Effects1, Socket#socket{pending_flash = Pending}}.

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
%% (an injected `OP_REPLACE`/`OP_UPDATE` reaches `innerHTML`: stored XSS
%% via the diff channel). So the scoped target is run through
%% `json:encode/1`, which escapes the JSON-breaking bytes (`"`/`\`); on
%% safe ids this is byte-identical to the previous raw emit. The SSR path
%% already escapes the same id in HTML; this closes the ops channel.
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
%% (OP_REPLACE/OP_UPDATE -> innerHTML: XSS via the diff channel). op_encoder runs
%% the scoped target through json:encode, so the quote is escaped rather than
%% closing the string, and the frame stays valid JSON with the id intact.
op_encoder_escapes_view_id_test() ->
    Op = [0, ~"0", ~"New"],
    Bytes = iolist_to_binary(encode(#{?OPS => flatten_ops(~"ev\"il", [Op])})),
    ?assertEqual(~"{\"o\":[[0,\"ev\\\"il:0\",\"New\"]]}", Bytes),
    %% Round-trips as valid JSON (the raw, unescaped emit would not parse).
    ?assertEqual(#{~"o" => [[0, ~"ev\"il:0", ~"New"]]}, json:decode(Bytes)).

-endif.

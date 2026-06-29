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
    req :: az:request()
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
- `[ViewId, Event, Payload]` -- UI event dispatch

Unrecognized payloads, and frames whose body is not valid JSON, are
silently dropped (a single bad frame must not crash the socket).
""".
-spec handle_in(Frame, Socket) -> result() when
    Frame :: binary(),
    Socket :: socket().
handle_in(?SYS_PING, Socket) ->
    {reply, ?SYS_PONG, Socket};
handle_in(JSON, #socket{pid = Pid} = Socket) ->
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
        [Target, Event, Payload] when is_binary(Event) ->
            try dispatch_event(Pid, Target, Event, Payload) of
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
    {H, RouteOpts, NewReq} = resolve_route(Path, Qs, Req),
    do_navigate(H, RouteOpts, NewReq, Socket).

%% A `patch` keeps the current view IFF the patched path resolves to the same
%% root handler; otherwise it can't (a different view needs a real mount), so it
%% degrades to a full navigate/replace. Resolves once and reuses the result for
%% either branch.
handle_patch(Path, Qs, #socket{handler = CurrentHandler, req = Req} = Socket) ->
    {H, RouteOpts, NewReq} = resolve_route(Path, Qs, Req),
    case H of
        CurrentHandler ->
            do_patch(RouteOpts, NewReq, Socket);
        _ ->
            do_navigate(H, RouteOpts, NewReq, Socket)
    end.

resolve_route(Path, Qs, Req) ->
    Adapter = arizona_req:adapter(Req),
    arizona_req:call_resolve_route(Adapter, Path, Qs, arizona_req:raw(Req)).

do_navigate(H, RouteOpts, NewReq, #socket{pid = Pid, view_id = OldVId} = Socket) ->
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
do_patch(RouteOpts, NewReq, #socket{pid = Pid, view_id = ViewId} = Socket) ->
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
%% `arizona_js:navigate/1` client effect. Halts without a stashed redirect
%% close the socket so the client reconnects and the next HTTP handshake
%% receives the full middleware response.
halt_navigate(HaltReq, Socket) ->
    case arizona_req:halted_redirect(HaltReq) of
        {_Status, Location} ->
            Effects = [arizona_js:navigate(Location)],
            encode_reply([], Effects, Socket);
        undefined ->
            close_crash(Socket)
    end.

replace_ops(ViewId, PageHTML) ->
    [[?OP_REPLACE, ViewId, PageHTML]].

dispatch_event(Pid, ViewId, Event, Payload) ->
    {ok, Ops, Effects} = arizona_live:handle_event(Pid, ViewId, Event, Payload),
    {flatten_ops(ViewId, Ops), Effects}.

encode_reply([], [], Socket) ->
    {ok, Socket};
encode_reply(Ops, [], Socket) ->
    {reply, encode(#{?OPS => Ops}), Socket};
encode_reply([], Effects, Socket) ->
    {reply, encode(#{?EFFECTS => unwrap_effects(Effects)}), Socket};
encode_reply(Ops, Effects, Socket) ->
    {reply, encode(#{?OPS => Ops, ?EFFECTS => unwrap_effects(Effects)}), Socket}.

unwrap_effects(Effects) ->
    [Cmd || {arizona_effect, Cmd} <:- Effects].

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
%% target inline as iodata. ViewId and Az are known safe (alphanumeric
%% + dash + colon), so we can skip `json:escape_binary/5` on them.
%% Op codes are bounded 0..9 (see `?OP_*` in `arizona.hrl`) so the
%% binary form is just `OpCode + $0` -- skips an `integer_to_binary/1`
%% BIF call per op. Falls back to `json:encode_value/2` for everything
%% else (untagged replace ops, effects, payload values).
op_encoder({ViewId, [OpCode, Target | RestArgs]}, E) when
    is_integer(OpCode),
    OpCode >= 0,
    OpCode =< 9,
    is_binary(ViewId),
    is_binary(Target)
->
    [
        $[,
        OpCode + $0,
        <<",\"">>,
        ViewId,
        $:,
        Target,
        <<"\"">>,
        encode_rest(RestArgs, E),
        $]
    ];
op_encoder(V, E) ->
    json:encode_value(V, E).

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

-endif.

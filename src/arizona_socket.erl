-module(arizona_socket).
-moduledoc """
Bridges a WebSocket frame stream with the live process.

The transport layer (typically `arizona_cowboy_ws`) creates a socket
via `init/3`, then forwards inbound text frames to `handle_in/2` and
inbox messages to `handle_info/2`. Each call returns a result tuple
that the transport translates into WebSocket frames or close codes.

## Wire protocol

Inbound text frames are JSON arrays:

```
[~"cached_fps", FpList]            %% client tells us which fingerprints it has
[~"navigate", #{~"path" := Path}]  %% SPA navigation request
[ViewId, Event, Payload]           %% UI event
~"0"                               %% ping (replied with ~"1")
```

Outbound text frames are JSON maps with keys `~"o"` (ops) and/or
`~"e"` (effects). Both are arrays produced by `arizona_diff` and
`arizona_js` respectively.

## Crash handling

The live process is linked. If it exits with a non-normal reason, the
socket attempts to remount the same handler; if that also fails, the
socket closes with code `4500` (server crash).
""".

-include("arizona.hrl").

-ifdef(TEST).
-include_lib("eunit/include/eunit.hrl").
-endif.

%% --------------------------------------------------------------------
%% API function exports
%% --------------------------------------------------------------------

-export([init/3]).
-export([handle_in/2]).
-export([handle_info/2]).

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
-define(CLOSE_CRASH, 4500).

%% --------------------------------------------------------------------
%% Records
%% --------------------------------------------------------------------

-record(socket, {
    pid :: pid() | undefined,
    view_id :: binary() | undefined,
    handler :: module(),
    bindings :: map(),
    on_mount :: arizona_live:on_mount(),
    adapter :: module(),
    adapter_state :: term()
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
Creates a socket for `Handler` with the given `Bindings` and starts
its live process.

`Opts` may include:
- `reconnect` -- if `true`, immediately renders the page and replies
  with an `?OP_REPLACE` op (used when the client is reconnecting after
  a network drop)
- `on_mount` -- list of `t:arizona_live:on_mount/0` hooks
- `adapter` -- transport adapter module (used by SPA navigate to
  resolve new routes)
- `adapter_state` -- opaque state passed back to the adapter
""".
-spec init(Handler, Bindings, Opts) -> result() when
    Handler :: module(),
    Bindings :: map(),
    Opts :: map().
init(Handler, Bindings, Opts) ->
    Reconnect = maps:get(reconnect, Opts, false),
    OnMount = maps:get(on_mount, Opts, []),
    Adapter = maps:get(adapter, Opts, undefined),
    AdapterState = maps:get(adapter_state, Opts, undefined),
    Socket = #socket{
        handler = Handler,
        bindings = Bindings,
        on_mount = OnMount,
        adapter = Adapter,
        adapter_state = AdapterState
    },
    safe_init(Socket, fun() ->
        {ok, Pid} = arizona_live:start_link(Handler, Bindings, self(), OnMount),
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
- `[~"navigate", #{~"path" := Path}]` -- SPA navigation
- `[ViewId, Event, Payload]` -- UI event dispatch

Unrecognized payloads are silently dropped.
""".
-spec handle_in(Frame, Socket) -> result() when
    Frame :: binary(),
    Socket :: socket().
handle_in(?SYS_PING, Socket) ->
    {reply, ?SYS_PONG, Socket};
handle_in(JSON, #socket{pid = Pid} = Socket) ->
    case json:decode(JSON) of
        [~"cached_fps", FpList] when is_list(FpList) ->
            arizona_live:seed_fps(Pid, FpList),
            {ok, Socket};
        [~"navigate", #{~"path" := Path}] ->
            try
                handle_navigate(Path, Socket)
            catch
                _:_ -> remount_or_close(Socket)
            end;
        [Target, Event, Payload] when is_binary(Event) ->
            try dispatch_event(Pid, Target, Event, Payload) of
                {AllOps, AllEffects} ->
                    encode_reply(AllOps, AllEffects, Socket)
            catch
                _:_ ->
                    remount_or_close(Socket)
            end;
        _ ->
            {ok, Socket}
    end.

-doc """
Handles inbox messages forwarded by the transport.

Routes `{arizona_push, Ops, Effects}` from the live process into a
reply frame. Handles `'EXIT'` from the linked live process by either
remounting (non-normal exit) or closing cleanly (normal exit).
""".
-spec handle_info(Info, Socket) -> result() when
    Info :: term(),
    Socket :: socket().
handle_info({arizona_push, Ops, Effects}, #socket{view_id = ViewId} = Socket) ->
    ScopedOps = scope_ops(ViewId, Ops),
    encode_reply(ScopedOps, Effects, Socket);
handle_info({'EXIT', Pid, Reason}, #socket{pid = Pid} = Socket) when Reason =/= normal ->
    logger:error("Live process ~p crashed: ~p", [Pid, Reason]),
    remount_or_close(Socket);
handle_info({'EXIT', Pid, normal}, #socket{pid = Pid} = Socket) ->
    {close, 1000, <<>>, Socket};
handle_info(_Info, Socket) ->
    {ok, Socket}.

%% --------------------------------------------------------------------
%% Internal functions
%% --------------------------------------------------------------------

safe_init(Socket, Fun) ->
    process_flag(trap_exit, true),
    try
        Fun()
    catch
        Class:Reason:Stacktrace ->
            logger:error("~s in ~p:~p~n~p", [Class, Socket#socket.handler, Reason, Stacktrace]),
            {close, ?CLOSE_CRASH, ~"server crash", Socket}
    end.

remount_or_close(
    #socket{handler = H, bindings = IB, on_mount = OnMount, view_id = OldVId} = Socket
) ->
    try
        {ok, Pid} = arizona_live:start_link(H, IB, self(), OnMount),
        {ok, NewVId, PageHTML} = arizona_live:mount_and_render(Pid),
        Ops = replace_ops(OldVId, PageHTML),
        encode_reply(Ops, [], Socket#socket{pid = Pid, view_id = NewVId})
    catch
        Class:Reason:Stacktrace ->
            logger:error("~s in ~p:~p~n~p", [Class, H, Reason, Stacktrace]),
            {close, ?CLOSE_CRASH, ~"server crash", Socket}
    end.

handle_navigate(
    Path, #socket{pid = Pid, view_id = OldVId, adapter = Adapter, adapter_state = AS} = Socket
) ->
    {H, RouteOpts} = arizona_adapter:call_resolve_route(Adapter, Path, AS),
    IB = maps:get(bindings, RouteOpts, #{}),
    OnMount = maps:get(on_mount, RouteOpts, []),
    {ok, NewVId, PageHTML} = arizona_live:navigate(Pid, H, IB, OnMount),
    Ops = replace_ops(OldVId, PageHTML),
    encode_reply(Ops, [], Socket#socket{view_id = NewVId, on_mount = OnMount}).

replace_ops(ViewId, PageHTML) ->
    [[?OP_REPLACE, ViewId, PageHTML]].

dispatch_event(Pid, ViewId, Event, Payload) ->
    {ok, Ops, Effects} = arizona_live:handle_event(Pid, ViewId, Event, Payload),
    {scope_ops(ViewId, Ops), Effects}.

encode_reply([], [], Socket) ->
    {ok, Socket};
encode_reply(Ops, [], Socket) ->
    {reply, encode(#{?OPS => Ops}), Socket};
encode_reply([], Effects, Socket) ->
    {reply, encode(#{?EFFECTS => unwrap_effects(Effects)}), Socket};
encode_reply(Ops, Effects, Socket) ->
    {reply, encode(#{?OPS => Ops, ?EFFECTS => unwrap_effects(Effects)}), Socket}.

unwrap_effects(Effects) ->
    [Cmd || {arizona_js, Cmd} <:- Effects].

encode(Map) ->
    json:encode(Map).

scope_ops(ViewId, Ops) ->
    lists:append([scope_op(ViewId, Op) || Op <:- Ops]).

scope_op(_ParentViewId, [ChildViewId, ChildOps]) when is_binary(ChildViewId) ->
    %% Recursive child diff -- flatten with child's own view id
    scope_ops(ChildViewId, ChildOps);
scope_op(ViewId, [?OP_ITEM_PATCH, Target, Key, InnerOps]) ->
    %% ITEM_PATCH: scope the target, inner ops are item-relative (not scoped)
    [[?OP_ITEM_PATCH, <<ViewId/binary, ":", Target/binary>>, Key, InnerOps]];
scope_op(ViewId, [OpCode, Target | Rest]) when is_integer(OpCode) ->
    [[OpCode, <<ViewId/binary, ":", Target/binary>> | Rest]].

-ifdef(TEST).

scope_op_replace_test() ->
    ReplOp = [8, ~"page", ~"<main>new</main>"],
    ?assertEqual(
        [[8, ~"root:page", ~"<main>new</main>"]],
        scope_op(~"root", ReplOp)
    ).

stream_scope_ops_test() ->
    %% INSERT
    InsOp = [5, ~"0", ~"1", -1, ~"<li>A</li>"],
    ?assertEqual(
        [[5, ~"page:0", ~"1", -1, ~"<li>A</li>"]],
        scope_op(~"page", InsOp)
    ),
    %% REMOVE
    RemOp = [6, ~"0", ~"1"],
    ?assertEqual(
        [[6, ~"page:0", ~"1"]],
        scope_op(~"page", RemOp)
    ),
    %% ITEM_PATCH
    PatchOp = [7, ~"0", ~"1", [[0, ~"0", ~"New"]]],
    ?assertEqual(
        [[7, ~"page:0", ~"1", [[0, ~"0", ~"New"]]]],
        scope_op(~"page", PatchOp)
    ),
    %% MOVE
    MoveOp = [9, ~"0", ~"1", 0],
    ?assertEqual(
        [[9, ~"page:0", ~"1", 0]],
        scope_op(~"page", MoveOp)
    ).

-endif.

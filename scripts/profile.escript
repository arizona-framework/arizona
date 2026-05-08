#!/usr/bin/env escript
%%% Performance profiler for Arizona.
%%%
%%% Companion to `scripts/bench.escript`. Whereas bench reports per-op
%%% wall-clock stats (good for catching regressions), profile runs each
%%% workload once under eprof or fprof and dumps a per-MFA breakdown
%%% (good for finding hot paths to optimize).
%%%
%%% Run via `make prof` (or `./scripts/profile.escript`). Like bench,
%%% never wired into ci/precommit -- this is a developer tool.
%%%
%%% Usage: ./profile.escript [--only LABEL ...] [--tool eprof|fprof]
%%%                          [--ops N] [--min-ms F] [--out-dir DIR]
%%%   --only LABEL    restrict to specific workload(s) by label; repeatable.
%%%                   Unknown labels exit 1. Default: all profilable workloads.
%%%   --tool          eprof (default) | fprof. fprof gives a richer call tree
%%%                   (OWN/ACC time, callers) at higher trace cost.
%%%   --ops N         ops per profiling pass (default 1000). One pass, no
%%%                   warmup, no multi-trial averaging -- profile fidelity is
%%%                   what matters, not statistical confidence in timing.
%%%   --min-ms F      hide MFAs whose total time < F ms in the eprof analysis
%%%                   (default 1.0). fprof analysis ignores this.
%%%   --out-dir DIR   directory for profile-<label>.log (default /tmp).

-mode(compile).

-define(DEFAULT_OPS, 1000).
-define(DEFAULT_MIN_MS, 1.0).
-define(DEFAULT_OUT_DIR, "/tmp").

main(Args) ->
    Opts = parse_args(Args),
    ProjectDir = project_dir(),
    ok = setup_code_paths(ProjectDir),
    {ok, _} = application:ensure_all_started(arizona),

    All = profilers(),
    Selected = filter_workloads(All, maps:get(only, Opts, [])),
    lists:foreach(
        fun({Label, Fun}) -> Fun(Label, Opts) end,
        Selected
    ).

profilers() ->
    [
        {~"render_view_page", fun prof_render_view_page/2},
        {~"render_each_100", fun prof_render_each_100/2},
        {~"diff_simple_event", fun prof_diff_simple_event/2},
        {~"stream_reorder_100", fun prof_stream_reorder_100/2},
        {~"http_get_e2e", fun prof_http_get_e2e/2},
        {~"ws_event_e2e", fun prof_ws_event_e2e/2},
        {~"mount_only", fun prof_mount_only/2},
        {~"stream_insert_1k", fun prof_stream_insert_1k/2},
        {~"pubsub_broadcast_100", fun prof_pubsub_broadcast_100/2},
        {~"render_nested_each", fun prof_render_nested_each/2},
        {~"render_stateful_chain", fun prof_render_stateful_chain/2},
        {~"render_view_page_dyn_js", fun prof_render_view_page_dyn_js/2},
        {~"mixed_todo_session", fun prof_mixed_todo_session/2},
        {~"stream_with_child_100", fun prof_stream_with_child_100/2},
        {~"stream_insert_at_100", fun prof_stream_insert_at_100/2},
        {~"navigate_e2e", fun prof_navigate_e2e/2}
    ].

filter_workloads(All, []) ->
    All;
filter_workloads(All, Labels) ->
    Known = [L || {L, _} <- All],
    Unknown = [L || L <- Labels, not lists:member(L, Known)],
    case Unknown of
        [] ->
            ok;
        _ ->
            io:format("error: unknown workload(s): ~p~n", [Unknown]),
            io:format("available: ~p~n", [Known]),
            halt(1)
    end,
    [{L, F} || {L, F} <- All, lists:member(L, Labels)].

%% ---------------------------------------------------------------------------
%% Workloads
%% ---------------------------------------------------------------------------

%% Each workload mirrors a `bench.escript` workload but inlines the loop
%% so the profiler captures only the unit of work, not the bench harness's
%% timing/stats machinery.

prof_render_view_page(Label, Opts) ->
    %% Mirrors bench_render_view_page (bench.escript:107). Renders
    %% arizona_page (route-level view with three embedded arizona_counter
    %% stateful children). Exercises the multi-child snapshot path:
    %% arizona_template:stateful, arizona_render:make_ssr_child_snap, and
    %% child-view fingerprint propagation.
    Req = arizona_req_test_adapter:new(),
    Sample = arizona_render:render_view_to_iolist(arizona_page, Req, #{}),
    sanity_render(arizona_page, Sample),
    Op = fun() ->
        arizona_render:render_view_to_iolist(arizona_page, Req, #{})
    end,
    profile_loop(Label, Op, Opts).

prof_render_each_100(Label, Opts) ->
    %% Mirrors bench_render_each_100 (bench.escript:115). Renders
    %% arizona_about with a 100-element `tags` binding -- exercises
    %% arizona_eval:render_list_items, arizona_render:zip_list_fp, and
    %% the per-item iteration path real apps rely on.
    Tags = [iolist_to_binary(io_lib:format("tag~b", [I])) || I <- lists:seq(1, 100)],
    Bindings = #{bindings => #{tags => Tags}},
    Req = arizona_req_test_adapter:new(),
    Sample = arizona_render:render_view_to_iolist(arizona_about, Req, Bindings),
    sanity_render(arizona_about, Sample),
    Op = fun() ->
        arizona_render:render_view_to_iolist(arizona_about, Req, Bindings)
    end,
    profile_loop(Label, Op, Opts).

prof_diff_simple_event(Label, Opts) ->
    %% Mirrors bench_diff_simple_event (bench.escript:489). Mount once,
    %% then loop `inc` events on arizona_root_counter. Each event mutates
    %% the `count` binding, so arizona_diff:diff/4 emits one OP_TEXT op.
    %% Exercises full WS event roundtrip: arizona_socket:handle_in +
    %% handler dispatch + diff + reply encode.
    Req = arizona_req_test_adapter:new(),
    {ok, Socket} = arizona_socket:init(arizona_root_counter, #{}, Req, #{}),
    Json = iolist_to_binary(json:encode([~"counter", ~"inc", #{}])),
    sanity_handle_in(Json, Socket),
    Op = fun() ->
        case arizona_socket:handle_in(Json, Socket) of
            {ok, _} -> ok;
            {reply, _, _} -> ok
        end
    end,
    profile_loop(Label, Op, Opts).

prof_stream_reorder_100(Label, Opts) ->
    %% Mirrors bench_stream_reorder_100 (bench.escript:313). Mount
    %% arizona_datatable seeded with a 100-row stream, then loop sort
    %% events. Each sort flips asc<->desc, producing a full reverse --
    %% the LIS reorder worst case (1 item stays put, 99 emit MOVE ops).
    Items = [
        #{
            id => I,
            name => iolist_to_binary(io_lib:format("name~b", [I])),
            age => 20 + I rem 50
        }
     || I <- lists:seq(1, 100)
    ],
    Stream = arizona_stream:new(fun(#{id := Id}) -> Id end, Items),
    Req = arizona_req_test_adapter:new(),
    {ok, Socket} = arizona_socket:init(
        arizona_datatable, #{rows => Stream}, Req, #{}
    ),
    Json = iolist_to_binary(
        json:encode([~"page", ~"sort", #{~"col" => ~"id"}])
    ),
    sanity_handle_in(Json, Socket),
    Op = fun() ->
        case arizona_socket:handle_in(Json, Socket) of
            {ok, _} -> ok;
            {reply, _, _} -> ok
        end
    end,
    profile_loop(Label, Op, Opts).

prof_mixed_todo_session(Label, Opts) ->
    %% Real-world-ish session: arizona_page (3 counters + todos stream
    %% + form), pre-mounted with 10 seed todos, then per op cycles
    %% through 4 representative event types in round-robin order:
    %%   1. counter `inc`   -- per-child text update + diff
    %%   2. page `add_todo` -- stream insert + diff with new ITEM_PATCH
    %%   3. page `update_todo` (id 1) -- stream update with per-item dep
    %%      tracking
    %%   4. page `add`      -- parent broadcast, all child counters
    %%      receive handle_update
    %%
    %% Surfaces hot paths the synthetic single-event workloads miss:
    %% mixed-mode dispatch, stream lifecycle interactions, multi-key
    %% changed maps, and the handle_call -> diff -> encode -> reply
    %% pipeline under varied input shapes.
    Req = arizona_req_test_adapter:new(),
    {ok, Socket} = arizona_socket:init(arizona_page, #{}, Req, #{}),
    %% Pre-populate 10 todos so update_todo always hits an existing id.
    Socket1 = lists:foldl(
        fun(_I, S) ->
            Json = iolist_to_binary(
                json:encode([~"page", ~"add_todo", #{}])
            ),
            socket_handle_in(Json, S)
        end,
        Socket,
        lists:seq(1, 10)
    ),
    Events = {
        iolist_to_binary(json:encode([~"counter", ~"inc", #{}])),
        iolist_to_binary(json:encode([~"page", ~"add_todo", #{}])),
        iolist_to_binary(
            json:encode([~"page", ~"update_todo", #{~"id" => 1, ~"value" => ~"updated"}])
        ),
        iolist_to_binary(json:encode([~"page", ~"add", #{}]))
    },
    Counter = counters:new(1, []),
    Op = fun() ->
        N = counters:get(Counter, 1),
        ok = counters:add(Counter, 1, 1),
        Json = element((N rem 4) + 1, Events),
        socket_handle_in(Json, Socket1),
        ok
    end,
    profile_loop(Label, Op, Opts).

socket_handle_in(Json, Socket) ->
    case arizona_socket:handle_in(Json, Socket) of
        {ok, S} -> S;
        {reply, _, S} -> S
    end.

prof_render_view_page_dyn_js(Label, Opts) ->
    %% Renders `arizona_page` with a pre-populated 100-item todos stream
    %% so the per-item dynamic `arizona_js:push_event(~"update_todo",
    %% #{~"id" => Id})` etc. fire on every render. The static-JS fold
    %% (commit 5fbe876) eliminates `escape_attr/1` for literal commands;
    %% this workload exists so we still profile (and guard regressions
    %% on) the dynamic-arg branch real apps with `?get`-driven JS
    %% payloads will hit. Each render produces ~300 escape_attr calls
    %% (100 todos x 3 dynamic JS attrs).
    Todos = arizona_stream:new(
        fun(#{id := Id}) -> Id end,
        [
            #{id => I, text => iolist_to_binary(io_lib:format("Todo ~b", [I]))}
         || I <- lists:seq(1, 100)
        ]
    ),
    RenderOpts = #{bindings => #{todos => Todos}},
    Req = arizona_req_test_adapter:new(),
    Sample = arizona_render:render_view_to_iolist(arizona_page, Req, RenderOpts),
    sanity_render(arizona_page, Sample),
    Op = fun() ->
        arizona_render:render_view_to_iolist(arizona_page, Req, RenderOpts)
    end,
    profile_loop(Label, Op, Opts).

prof_render_nested_each(Label, Opts) ->
    %% View renders 10 sections, each with a 10-item `?each` of leaves
    %% (`arizona_bench_nested_each`). Two levels of `?each` exercise the
    %% recursive `arizona_render:zip/2` + `arizona_eval:render_list_items_simple/2`
    %% path that real list-in-list shapes (categories x items, threads x replies)
    %% hit. Complements `render_each_100`'s flat 100-item case.
    %%
    %% Pre-generate the 10x10 dataset once and pass via bindings -- the
    %% fixture's lazy default would otherwise rebuild 100 binaries per
    %% render, dominating the profile.
    Sections = generate_nested_sections(),
    RenderOpts = #{bindings => #{sections => Sections}},
    Req = arizona_req_test_adapter:new(),
    Sample = arizona_render:render_view_to_iolist(arizona_bench_nested_each, Req, RenderOpts),
    sanity_render(arizona_bench_nested_each, Sample),
    Op = fun() ->
        arizona_render:render_view_to_iolist(arizona_bench_nested_each, Req, RenderOpts)
    end,
    profile_loop(Label, Op, Opts).

generate_nested_sections() ->
    [
        #{
            id => SectId,
            title => iolist_to_binary(io_lib:format("Section ~b", [SectId])),
            items => [
                #{
                    id => ItemId,
                    text => iolist_to_binary(
                        io_lib:format("Item ~b-~b", [SectId, ItemId])
                    )
                }
             || ItemId <- lists:seq(1, 10)
            ]
        }
     || SectId <- lists:seq(1, 10)
    ].

prof_render_stateful_chain(Label, Opts) ->
    %% 3-level stateful chain: view (chain_a) -> stateful (chain_b) ->
    %% stateful (chain_c) with a 10-item `?each` leaf. Exercises the
    %% recursive `arizona_render:render_ssr_val/1` propagation through
    %% nested `?stateful(...)` descriptors -- something the flat
    %% `render_view_page` (single-level stateful children) can't show.
    Req = arizona_req_test_adapter:new(),
    Sample = arizona_render:render_view_to_iolist(arizona_bench_chain_a, Req, #{}),
    sanity_render(arizona_bench_chain_a, Sample),
    Op = fun() ->
        arizona_render:render_view_to_iolist(arizona_bench_chain_a, Req, #{})
    end,
    profile_loop(Label, Op, Opts).

prof_http_get_e2e(Label, Opts) ->
    %% Mirrors bench_http_get_e2e (bench.escript:321). Full HTTP path:
    %% cowboy parse + arizona_http handler + view mount/render + reply
    %% writing. Maps to every e2e spec's initial `goto()` -- the cost
    %% the user pays before any WS event fires.
    %%
    %% Uses `profile_loop_server/4` so eprof traces only cowboy/arizona
    %% pids; the `gen_tcp` send/recv work in `self()` runs un-traced.
    Routes = [
        {live, <<"/">>, arizona_root_counter, #{layouts => [{arizona_layout, render}]}}
    ],
    arizona_bench_lib:with_cowboy(prof_http_e2e, Routes, fun(Port) ->
        arizona_bench_lib:with_http_socket(Port, fun(Sock) ->
            sanity_http_get(Sock, Port),
            Op = fun() ->
                {200, _} = arizona_bench_lib:http_get(Sock, Port),
                ok
            end,
            profile_loop_server(Label, Op, Opts, server_pids())
        end)
    end).

prof_ws_event_e2e(Label, Opts) ->
    %% Mirrors bench_ws_event_e2e (bench.escript:390). Full WS roundtrip:
    %% one `inc` text frame -> arizona_socket:handle_in -> live dispatch
    %% -> diff -> reply encode -> WS frame back. Maps to every counter/
    %% form click in the e2e specs.
    %%
    %% Uses `profile_loop_server/4` so the harness's per-frame WS mask
    %% (`ws_mask`/`crypto:strong_rand_bytes_nif`) and `port_command`
    %% calls don't drown out the server-side rows.
    Routes = [
        {live, <<"/">>, arizona_root_counter, #{layouts => [{arizona_layout, render}]}},
        {ws, <<"/ws">>, #{}}
    ],
    arizona_bench_lib:with_cowboy(prof_ws_e2e, Routes, fun(Port) ->
        arizona_bench_lib:with_ws_socket(Port, <<"/">>, fun(Sock) ->
            Json = iolist_to_binary(json:encode([~"counter", ~"inc", #{}])),
            sanity_ws_send(Sock, Json),
            Op = fun() ->
                ok = arizona_bench_lib:ws_send(Sock, Json),
                {text, _} = arizona_bench_lib:ws_recv(Sock, 5000),
                ok
            end,
            profile_loop_server(Label, Op, Opts, server_pids())
        end)
    end).

%% Snapshot of the running cowboy/ranch/arizona processes -- everything
%% reachable through the supervisor tree by the time the listener is up.
%% Excludes the test driver in `self()` (and its peers in the escript)
%% so the per-op `gen_tcp:send`/`recv` in the bench harness is invisible
%% to eprof. Connection processes spawned per-request are still caught
%% via `set_on_spawn => true` because their parent listener is in this
%% seed list.
server_pids() ->
    [
        P
     || P <- processes(),
        is_server_initial_call(proc_lib:initial_call(P))
    ].

is_server_initial_call({Mod, _, _}) ->
    ModStr = atom_to_list(Mod),
    lists:any(
        fun(Prefix) -> lists:prefix(Prefix, ModStr) end,
        ["arizona_", "cowboy_", "ranch_"]
    );
is_server_initial_call(_) ->
    false.

prof_mount_only(Label, Opts) ->
    %% Mirrors bench_mount_only (bench.escript:435). Cold WS connect
    %% cost: gen_server start + handler mount/2 + dep tracking setup.
    %% Per iteration: spawn a live, kill it synchronously (DOWN wait)
    %% so steady state is ~1 alive live process -- matches typical
    %% real-world WS load instead of an artificial backlog.
    Req = arizona_req_test_adapter:new(),
    sanity_mount(Req),
    Op = fun() ->
        {ok, Sock} = arizona_socket:init(arizona_root_counter, #{}, Req, #{}),
        arizona_bench_lib:kill_live(element(2, Sock)),
        ok
    end,
    profile_loop(Label, Op, Opts).

prof_stream_insert_1k(Label, Opts) ->
    %% Mirrors bench_stream_insert_1k (bench.escript:132). Per --ops
    %% iteration: insert 1000 unique items into a fresh empty stream.
    %% Maps to the `add_todo` flow in arizona_page.spec.js (and bulk
    %% data import scenarios more broadly). Catches O(n) regressions
    %% in `arizona_stream:insert/2`.
    KeyFun = fun(#{id := Id}) -> Id end,
    Items = [#{id => I, text => integer_to_binary(I)} || I <- lists:seq(1, 1000)],
    sanity_stream_insert(KeyFun, Items),
    Op = fun() ->
        Stream0 = arizona_stream:new(KeyFun),
        lists:foldl(fun(I, S) -> arizona_stream:insert(S, I) end, Stream0, Items),
        ok
    end,
    profile_loop(Label, Op, Opts).

prof_pubsub_broadcast_100(Label, Opts) ->
    %% Mirrors bench_pubsub_broadcast_100 (bench.escript:508). 100
    %% subscriber procs on one topic; per iteration: one broadcast +
    %% barrier waiting for all 100 acks. Maps to arizona_chat's
    %% cross-tab message fan-out (each connected tab is a subscriber).
    Topic = prof_pubsub_topic,
    Self = self(),
    Subs = [
        spawn_link(fun() -> pubsub_sub_loop(Topic, Self) end)
     || _ <- lists:seq(1, 100)
    ],
    ok = lists:foreach(
        fun(P) ->
            receive
                {ready, P} -> ok
            end
        end,
        Subs
    ),
    Op = fun() ->
        arizona_pubsub:broadcast(Topic, ping),
        await_acks(100)
    end,
    try
        profile_loop(Label, Op, Opts)
    after
        lists:foreach(fun(P) -> exit(P, normal) end, Subs)
    end.

pubsub_sub_loop(Topic, Reporter) ->
    ok = arizona_pubsub:subscribe(Topic, self()),
    Reporter ! {ready, self()},
    pubsub_sub_recv(Reporter).

pubsub_sub_recv(Reporter) ->
    receive
        ping ->
            Reporter ! ack,
            pubsub_sub_recv(Reporter);
        _ ->
            pubsub_sub_recv(Reporter)
    end.

await_acks(0) ->
    ok;
await_acks(N) ->
    receive
        ack -> await_acks(N - 1)
    end.

prof_stream_with_child_100(Label, Opts) ->
    %% Stream + nested stateful children. Pre-mounts arizona_stream_with_child
    %% with 100 items; the fixture wraps each stream item in two
    %% `?stateful(arizona_counter, ...)` children. Each
    %% `arizona_live:mount_and_render/1` call performs a fresh mount +
    %% render -- the live process replaces its bindings/snapshot/views
    %% per call -- so per op we walk 100 stream-item triples through
    %% `zip_stream_item/2` *and* invoke `render_ssr_val/1` for 200
    %% stateful descriptors. Exercises the intersection of streams and
    %% nested SSR that neither `render_each_100` (flat list, no
    %% stateful) nor `render_stateful_chain` (3-level chain, no
    %% stream) covers.
    %%
    %% `render_view_to_iolist/3` can't drive this -- inline stateful
    %% descriptors inside `?each` aren't unfolded by the standalone
    %% render path; only the live process's SSR snapshot path handles
    %% them. So we route through `mount_and_render/1` and seed eprof
    %% with the live pid (`server_pids/0` plus the bench infra would
    %% pull in the wrong tree -- we just want this one process).
    Items = [
        #{id => I, label => integer_to_binary(I)}
     || I <- lists:seq(1, 100)
    ],
    Stream = arizona_stream:new(fun(#{id := Id}) -> Id end, Items),
    Bindings = #{items => Stream},
    Req = arizona_req_test_adapter:new(),
    {ok, Pid} = arizona_live:start_link(
        arizona_stream_with_child, Bindings, self(), [], Req
    ),
    sanity_mount_and_render(Pid),
    Op = fun() ->
        {ok, _, _} = arizona_live:mount_and_render(Pid),
        ok
    end,
    try
        profile_loop_server(Label, Op, Opts, [Pid])
    after
        arizona_bench_lib:kill_live(Pid)
    end.

prof_stream_insert_at_100(Label, Opts) ->
    %% Position-explicit stream insert. `stream_insert_1k` exercises
    %% the bulk `arizona_stream:insert/2` path (O(1) back-buffer cons
    %% + pending queue). `arizona_stream:insert/3` is the
    %% position-aware variant: each call flattens the order list
    %% (`flat_order/1`, Front++reverse(Back)) and walks it to the
    %% insertion point (`order_insert_at/3`) -- both O(N). Maps to UI
    %% flows like drag-drop reorder where the new index isn't always
    %% the tail.
    %%
    %% Per op: start from an empty stream, do 100 `insert/3` calls at
    %% rotating positions. The pre-seeded variant would dominate the
    %% trace with `new/2`'s key/order/pending construction (each op
    %% rebuilds the 100-item seed); doing 100 inserts per op puts the
    %% work into the path we actually want to see.
    KeyFun = fun(#{id := Id}) -> Id end,
    Op = fun() ->
        S0 = arizona_stream:new(KeyFun),
        lists:foldl(
            fun(I, S) -> arizona_stream:insert(S, #{id => I}, I rem 11) end,
            S0,
            lists:seq(1, 100)
        ),
        ok
    end,
    profile_loop(Label, Op, Opts).

prof_navigate_e2e(Label, Opts) ->
    %% Full SPA navigate roundtrip: WS frame -> arizona_socket:handle_in
    %% -> handle_navigate/3 -> route resolve -> arizona_live:navigate/5
    %% (unmount old view, mount new) -> diff producing OP_REPLACE ->
    %% encode -> WS frame back. E2e specs cover navigation but no
    %% workload profiles its cost. Both routes serve `arizona_root_counter`
    %% (a tiny navigate-safe view -- no timers, mount preserves
    %% Bindings0's `id` so the framework's restricted-key check
    %% passes); the cowboy_router path differs between calls, so
    %% route resolution + unmount/remount + OP_REPLACE encode all
    %% run on each iteration.
    Routes = [
        {live, <<"/">>, arizona_root_counter, #{layouts => [{arizona_layout, render}]}},
        {live, <<"/two">>, arizona_root_counter, #{layouts => [{arizona_layout, render}]}},
        {ws, <<"/ws">>, #{}}
    ],
    arizona_bench_lib:with_cowboy(prof_navigate_e2e, Routes, fun(Port) ->
        arizona_bench_lib:with_ws_socket(Port, <<"/">>, fun(Sock) ->
            JsonAway = iolist_to_binary(
                json:encode([~"navigate", #{~"path" => ~"/two", ~"qs" => ~""}])
            ),
            JsonHome = iolist_to_binary(
                json:encode([~"navigate", #{~"path" => ~"/", ~"qs" => ~""}])
            ),
            sanity_ws_send(Sock, JsonAway),
            sanity_ws_send(Sock, JsonHome),
            Counter = counters:new(1, []),
            Op = fun() ->
                N = counters:get(Counter, 1),
                ok = counters:add(Counter, 1, 1),
                Frame =
                    case N band 1 of
                        0 -> JsonAway;
                        1 -> JsonHome
                    end,
                ok = arizona_bench_lib:ws_send(Sock, Frame),
                {text, _} = arizona_bench_lib:ws_recv(Sock, 5000),
                ok
            end,
            profile_loop_server(Label, Op, Opts, server_pids())
        end)
    end).

sanity_http_get(Sock, Port) ->
    case arizona_bench_lib:http_get(Sock, Port) of
        {200, Body} when byte_size(Body) > 0 ->
            ok;
        Other ->
            io:format("error: GET returned ~p~n", [Other]),
            halt(1)
    end.

sanity_ws_send(Sock, Json) ->
    ok = arizona_bench_lib:ws_send(Sock, Json),
    case arizona_bench_lib:ws_recv(Sock, 5000) of
        {text, Reply} when byte_size(Reply) > 0 ->
            ok;
        Other ->
            io:format("error: WS send returned ~p~n", [Other]),
            halt(1)
    end.

sanity_mount(Req) ->
    case arizona_socket:init(arizona_root_counter, #{}, Req, #{}) of
        {ok, Sock} when is_pid(element(2, Sock)) ->
            arizona_bench_lib:kill_live(element(2, Sock));
        Other ->
            io:format("error: mount returned ~p~n", [Other]),
            halt(1)
    end.

sanity_stream_insert(KeyFun, Items) ->
    Stream = lists:foldl(
        fun(I, S) -> arizona_stream:insert(S, I) end,
        arizona_stream:new(KeyFun),
        Items
    ),
    case length(arizona_stream:to_list(Stream)) of
        1000 ->
            ok;
        Other ->
            io:format("error: expected 1000 stream items, got ~p~n", [Other]),
            halt(1)
    end.

sanity_mount_and_render(Pid) ->
    case arizona_live:mount_and_render(Pid) of
        {ok, ViewId, PageContent} when
            is_binary(ViewId), (is_binary(PageContent) orelse is_map(PageContent))
        ->
            ok;
        Other ->
            io:format("error: mount_and_render returned ~p~n", [Other]),
            halt(1)
    end.

sanity_render(Module, Sample) ->
    case iolist_size(Sample) > 0 of
        true ->
            ok;
        false ->
            io:format("error: render_view_to_iolist ~p returned empty~n", [Module]),
            halt(1)
    end.

sanity_handle_in(Json, Socket) ->
    case arizona_socket:handle_in(Json, Socket) of
        {ok, _} ->
            ok;
        {reply, _, _} ->
            ok;
        Other ->
            io:format("error: handle_in returned unexpected ~p~n", [Other]),
            halt(1)
    end.

%% ---------------------------------------------------------------------------
%% Profile harness
%% ---------------------------------------------------------------------------

profile_loop(Label, OpFun, Opts) ->
    profile_loop_with(Label, OpFun, Opts, fun arizona_profiler:start/0).

%% Like profile_loop/3 but seeds eprof with `SeedPids` instead of
%% `[self()]`. Used by the e2e workloads (`prof_http_get_e2e/2`,
%% `prof_ws_event_e2e/2`) that drive a real cowboy listener from the
%% same BEAM -- seeding only server-side pids keeps the harness's
%% gen_tcp work out of the trace. fprof falls back to default seeds
%% (set_on_spawn from `self()`) -- explicit seeds aren't wired there
%% yet, but eprof is the default and dominant tool.
profile_loop_server(Label, OpFun, Opts, SeedPids) ->
    profile_loop_with(Label, OpFun, Opts, fun() -> arizona_profiler:start(SeedPids) end).

profile_loop_with(Label, OpFun, Opts, StartFun) ->
    Tool = maps:get(tool, Opts, eprof),
    Ops = maps:get(ops, Opts, ?DEFAULT_OPS),
    MinMs = maps:get(min_ms, Opts, ?DEFAULT_MIN_MS),
    OutDir = maps:get(out_dir, Opts, ?DEFAULT_OUT_DIR),
    LabelStr = binary_to_list(Label),
    LogPath = filename:join(OutDir, "arizona_profile_" ++ LabelStr ++ ".log"),
    erlang:garbage_collect(self()),
    case Tool of
        eprof ->
            ok = StartFun(),
            run_n(OpFun, Ops),
            ok = arizona_profiler:stop_and_dump(LogPath, MinMs);
        fprof ->
            TracePath = LogPath ++ ".trace",
            ok = arizona_profiler:start_fprof(TracePath),
            run_n(OpFun, Ops),
            ok = arizona_profiler:stop_fprof_and_dump(TracePath, LogPath)
    end,
    print_result(Label, Tool, MinMs, LogPath).

run_n(_Fun, 0) ->
    ok;
run_n(Fun, N) ->
    Fun(),
    run_n(Fun, N - 1).

print_result(Label, eprof, MinMs, Path) ->
    io:format(
        "~nprofile (eprof, ~s, total time, rows >= ~.2f ms) -> ~s~n",
        [Label, MinMs, Path]
    ),
    print_file(Path);
print_result(Label, fprof, _MinMs, Path) ->
    io:format("~nprofile (fprof, ~s, OWN time, sort=own) -> ~s~n", [Label, Path]),
    print_file(Path).

print_file(Path) ->
    case file:read_file(Path) of
        {ok, Bin} ->
            io:put_chars(Bin),
            io:nl();
        {error, Reason} ->
            io:format("  could not read ~s: ~p~n", [Path, Reason])
    end.

%% ---------------------------------------------------------------------------
%% Args + paths
%% ---------------------------------------------------------------------------

parse_args(Args) ->
    parse_args(Args, #{only => []}).

parse_args([], Opts) ->
    Opts#{only => lists:reverse(maps:get(only, Opts))};
parse_args(["--only", Label | Rest], Opts) ->
    Only = [list_to_binary(Label) | maps:get(only, Opts)],
    parse_args(Rest, Opts#{only => Only});
parse_args(["--tool", "eprof" | Rest], Opts) ->
    parse_args(Rest, Opts#{tool => eprof});
parse_args(["--tool", "fprof" | Rest], Opts) ->
    parse_args(Rest, Opts#{tool => fprof});
parse_args(["--tool", Other | _Rest], _Opts) ->
    io:format("error: --tool must be eprof or fprof, got ~s~n", [Other]),
    halt(1);
parse_args(["--ops", NStr | Rest], Opts) ->
    parse_args(Rest, Opts#{ops => list_to_integer(NStr)});
parse_args(["--min-ms", FStr | Rest], Opts) ->
    parse_args(Rest, Opts#{min_ms => list_to_float_lenient(FStr)});
parse_args(["--out-dir", Dir | Rest], Opts) ->
    parse_args(Rest, Opts#{out_dir => Dir});
parse_args([Other | _Rest], _Opts) ->
    io:format("error: unknown arg ~s~n", [Other]),
    halt(1).

list_to_float_lenient(Str) ->
    case string:to_float(Str) of
        {Float, ""} ->
            Float;
        _ ->
            float(list_to_integer(Str))
    end.

setup_code_paths(BaseDir) ->
    %% Prefer the test profile's lib dir so test/support/ modules
    %% (arizona_profiler, arizona_req_test_adapter, fixtures) are
    %% available. Fall back to default if test profile isn't compiled.
    Candidates = [
        filename:join([BaseDir, "_build", "test", "lib"]),
        filename:join([BaseDir, "_build", "default", "lib"])
    ],
    LibDir =
        case lists:filter(fun filelib:is_dir/1, Candidates) of
            [Found | _] ->
                Found;
            [] ->
                io:format("error: no compiled libs found; run 'rebar3 as test compile' first~n"),
                halt(1)
        end,
    {ok, Libs} = file:list_dir(LibDir),
    lists:foreach(
        fun(Lib) ->
            EbinDir = filename:join([LibDir, Lib, "ebin"]),
            case filelib:is_dir(EbinDir) of
                true -> code:add_pathz(EbinDir);
                false -> ok
            end,
            TestDir = filename:join([LibDir, Lib, "test"]),
            case filelib:is_dir(TestDir) of
                true -> code:add_pathz(TestDir);
                false -> ok
            end
        end,
        Libs
    ),
    ok.

project_dir() ->
    filename:dirname(filename:absname(filename:dirname(escript:script_name()))).

#!/usr/bin/env escript
%%% Generate real client benchmark fixtures: SSR HTML plus the ops a real
%%% diff emits for it.
%%%
%%% Run via `make bench-client`, which feeds the output to
%%% `scripts/bench_client.mjs`. Never wired into `make ci`.
%%%
%%% Usage: ./client_fixture.escript OUTDIR
%%%
%%% Why this exists rather than hand-written ops: the op SHAPE is not
%%% guessable. A bulk change collapses to one container `?OP_TEXT`, while a
%%% partial change emits per-item `?OP_ITEM_PATCH` that all share the
%%% container's az -- so a benchmark inventing one op per element measures a
%%% workload the diff engine never produces, and will happily report numbers
%%% for it.

-mode(compile).

main([OutDir]) ->
    ok = setup_code_paths(project_dir()),
    ok = filelib:ensure_path(OutDir),
    ok = gen(OutDir, ~"stream_patch", 400, 200),
    ok = gen(OutDir, ~"stream_render", 400, 400),
    %% Connect fixtures need the live process machinery, not just the renderer.
    {ok, _} = application:ensure_all_started(arizona),
    ok = gen_connect(OutDir, ~"connect_page", arizona_page, #{}),
    ok = gen_connect(OutDir, ~"connect_bulk_500", arizona_stream_bulk, #{
        items => arizona_stream:new(
            fun(#{id := Id}) -> integer_to_binary(Id) end,
            [
                #{id => I, label => <<"row ", (integer_to_binary(I))/binary>>}
             || I <- lists:seq(1, 500)
            ]
        )
    }),
    io:format("fixtures written to ~s~n", [OutDir]);
main(_) ->
    io:format(standard_error, "usage: client_fixture.escript OUTDIR~n", []),
    halt(1).

%% Connect/reconnect fixture: the SSR page a browser holds before the client
%% boots, plus the exact frames a real socket emits for it -- the first-connect
%% frame (`{"a":...}`), the deferred-reconnect connect frame, and the resync
%% frame the server sends once the promised `cached_fps` arrives. The bench
%% (`bench_client_connect.mjs`) replays these through a stubbed WebSocket, so
%% everything the client does with them is the production path.
gen_connect(OutDir, Label, Handler, Bindings) ->
    HTML = arizona_render:render_view_to_iolist(Handler, #{bindings => Bindings}),
    Req = arizona_req_test_adapter:new(),
    {reply, ConnectFrame, Sock0} = arizona_socket:init(Handler, Bindings, Req, #{}),
    ok = arizona_bench_lib:kill_live(element(2, Sock0)),
    {reply, ReconnectFrame, Sock1} = arizona_socket:init(Handler, Bindings, Req, #{
        reconnect => true, fps_follow => true
    }),
    {reply, ResyncFrame, Sock2} = arizona_socket:handle_in(
        iolist_to_binary(json:encode([~"cached_fps", []])), Sock1
    ),
    ok = arizona_bench_lib:kill_live(element(2, Sock2)),
    Meta = #{
        ~"label" => Label,
        ~"kind" => ~"connect",
        ~"connect_frame" => iolist_to_binary(ConnectFrame),
        ~"reconnect_frame" => iolist_to_binary(ReconnectFrame),
        ~"resync_frame" => iolist_to_binary(ResyncFrame)
    },
    Page = [
        ~"<!doctype html><html><head><meta charset=\"utf-8\">",
        ~"<title>connect bench</title></head><body>",
        HTML,
        ~"</body></html>"
    ],
    ok = file:write_file(
        filename:join(OutDir, <<Label/binary, ".html">>), iolist_to_binary(Page)
    ),
    ok = file:write_file(
        filename:join(OutDir, <<Label/binary, ".json">>), iolist_to_binary(json:encode(Meta))
    ).

%% `Changed` items of `Total` change their label, which is what selects the
%% op shape: a partial change patches per item, a total one collapses.
gen(OutDir, Label, Total, Changed) ->
    ViewId = ~"x",
    KeyFun = fun(#{id := Id}) -> integer_to_binary(Id) end,
    Mk = fun(Upto) ->
        [
            #{
                id => I,
                label => iolist_to_binary([
                    integer_to_binary(I),
                    case I =< Upto of
                        true -> ~"-v2";
                        false -> ~""
                    end
                ])
            }
         || I <- lists:seq(1, Total)
        ]
    end,
    Stream0 = arizona_stream:new(KeyFun, Mk(0)),
    Tpl0 = arizona_stream_bulk:render(#{id => ViewId, items => Stream0}),
    {HTML, Snapshot, Views} = arizona_render:render(Tpl0, #{}),
    Stream1 = arizona_stream:reset(Stream0, Mk(Changed)),
    Tpl1 = arizona_stream_bulk:render(#{id => ViewId, items => Stream1}),
    {Ops, _Snapshot1, _Views1} = arizona_diff:diff(Tpl1, Snapshot, Views, #{items => true}),
    Meta = #{
        ~"label" => Label,
        ~"view_id" => ViewId,
        ~"items" => Total,
        ~"changed" => Changed,
        ~"ops" => scope(ViewId, Ops)
    },
    ok = file:write_file(
        filename:join(OutDir, <<Label/binary, ".html">>), iolist_to_binary(HTML)
    ),
    ok = file:write_file(
        filename:join(OutDir, <<Label/binary, ".json">>), iolist_to_binary(json:encode(Meta))
    ).

%% Scope each op's target to its view, mirroring `arizona_socket:flatten_ops/2`
%% (private): a nested `[ChildViewId, ChildOps]` wrapper re-scopes to the CHILD,
%% everything else takes the current view. A raw `arizona_diff:diff/4` target is
%% view-relative, and the client reads a colon-less target as a VIEW ID -- so
%% skipping this makes every op miss and the bench measures nothing but warnings.
%% `bench_client.mjs` refuses to report unless the patch visibly applied, which
%% is what catches this drifting from the socket.
scope(_ViewId, []) ->
    [];
scope(ParentViewId, [[ChildViewId, ChildOps] | Rest]) when is_binary(ChildViewId) ->
    scope(ChildViewId, ChildOps) ++ scope(ParentViewId, Rest);
scope(ViewId, [[Code, Az | Tail] | Rest]) when is_binary(Az) ->
    [[Code, <<ViewId/binary, ":", Az/binary>> | Tail] | scope(ViewId, Rest)];
scope(_ViewId, [Op | _Rest]) ->
    %% Loud on purpose. Passing an unrecognised op through unscoped would leave it
    %% unresolvable, and the bench would report it as "nothing applied" -- a failure
    %% pointing at the harness rather than at this function, which is where it is.
    error({unscopable_op, Op}).

%% Same shape as `scripts/bench.escript`: prefer the test profile so
%% `test/support/` fixtures (arizona_stream_bulk) are on the path.
setup_code_paths(BaseDir) ->
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
            lists:foreach(
                fun(Sub) ->
                    Dir = filename:join([LibDir, Lib, Sub]),
                    case filelib:is_dir(Dir) of
                        true -> code:add_pathz(Dir);
                        false -> ok
                    end
                end,
                ["ebin", "test"]
            )
        end,
        Libs
    ),
    ok.

project_dir() ->
    filename:dirname(filename:absname(filename:dirname(escript:script_name()))).

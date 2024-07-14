-module(arizona_tpl_render).
-moduledoc """
Renderer.
""".

%% --------------------------------------------------------------------
%% API function exports
%% --------------------------------------------------------------------

-export([render_target/4]).
-export([render_block/2]).
-export([mount/2]).

%% --------------------------------------------------------------------
%% Types (and their exports)
%% --------------------------------------------------------------------

-type socket_target() :: [integer()].
-export_type([socket_target/0]).

%% --------------------------------------------------------------------
%% API function definitions
%% --------------------------------------------------------------------

-spec render_target(Target, Block, Changes, Assigns) -> Rendered
    when Target :: root | json:decode_value(),
         Block :: arizona_tpl_compile:block(),
         Changes :: arizona_socket:changes(),
         Assigns :: arizona_socket:assigns(),
         Rendered :: iolist().
render_target(Target, Block, Changes, Assigns) when map_size(Changes) > 0 ->
    render_target_1(Target, Block, Changes, Assigns);
render_target(_Target, _Block, _Changes, _Assigns) ->
    [].

render_target_1(root, Block, Changes, Assigns) ->
    render_changes(Block, Changes, Assigns);
render_target_1(Id, Block, Changes, Assigns) ->
    render_target_2(Id, Block, Changes, Assigns).

render_target_2([H], #{block := Block}, Changes, Assigns) ->
    render_changes(maps:get(H, Block), Changes, Assigns);
render_target_2([H | T], Block, Changes, Assigns) ->
    #{block := Nested} = maps:get(H, Block),
    render_target_2(T, Nested, Changes, Assigns).

-spec render_block(Block, Assigns) -> Rendered
    when Block :: arizona_tpl_compile:block(),
         Assigns :: arizona_socket:assigns(),
         Rendered :: iolist().
render_block(Block, Assigns0) ->
    View = maps:get(view, Block),
    Socket0 = arizona_socket:new(View, Assigns0),
    Socket = arizona_live_view:mount(View, Socket0),
    Indexes = maps:get(indexes, Block),
    Tree = maps:get(block, Block),
    Assigns = arizona_socket:get_assigns(Socket),
    render_indexes(Indexes, Tree, Assigns, false).

-spec mount(Block, Assigns) -> {Rendered, Sockets}
    when Block :: arizona_tpl_compile:block(),
         Assigns :: arizona_socket:assigns(),
         Rendered :: iolist(),
         Sockets :: #{SocketId :: socket_target() := Socket :: arizona_socket:t()}.
mount(#{id := Id, view := View} = Block, Assigns0) ->
    Socket0 = arizona_socket:new(View, Assigns0),
    Socket = arizona_live_view:mount(View, Socket0),
    Assigns = arizona_socket:get_assigns(Socket),
    To = self(),
    Pid = spawn(fun() ->
        Indexes = maps:get(indexes, Block),
        Tree = maps:get(block, Block),
        Render = render_indexes(Indexes, Tree, Assigns, {true, To}),
        To ! {self(), finished, Render}
    end),
    mount_loop(Pid, [{Id, Socket}]).

%% --------------------------------------------------------------------
%% Private
%% --------------------------------------------------------------------

render_changes(#{vars := AllVars, block := Block}, Changes, Assigns)
    when map_size(Changes) > 0 ->
    Vars = maps:with(maps:keys(Changes), AllVars),
    path_render(Vars, Block, Assigns);
render_changes(#{}, _Changes, _Assigns) ->
    [].

mount_loop(Pid, Sockets) ->
    receive
        {Pid, finished, Render} ->
            {Render, maps:from_list(Sockets)};
        {Pid, mount, Id, Socket} ->
            mount_loop(Pid, [{Id, Socket} | Sockets])
    after
        10_000 ->
            error(timeout)
    end.

render_indexes([H | T], Block, Assigns, Notify) ->
    case maps:get(H, Block) of
        #{text := Text} ->
            [Text | render_indexes(T, Block, Assigns, Notify)];
        #{expr := Expr} ->
            case Expr(Assigns) of
                ok ->
                    render_indexes(T, Block, Assigns, Notify);
                Value ->
                    [arizona_html:to_safe(Value) | render_indexes(T, Block, Assigns, Notify)]
            end;
        #{indexes := Indexes, block := NBlock, attrs := Attrs} = Nested ->
            AttrsAssigns = maps:map(fun(_K, Expr) ->
                eval(Expr, Assigns)
            end, Attrs),
            NAssigns = case maps:get(directives, Nested) of
                #{stateful := true} ->
                    NView = maps:get(view, Nested),
                    NSocket0 = arizona_socket:new(NView, AttrsAssigns),
                    NSocket = arizona_live_view:mount(NView, NSocket0),
                    _ = case Notify of
                        {true, Pid} ->
                            Pid ! {self(), mount, maps:get(id, Nested), NSocket};
                        false ->
                            ok
                    end,
                    arizona_socket:get_assigns(NSocket);
                #{} ->
                    AttrsAssigns
            end,
            [render_indexes(Indexes, NBlock, NAssigns, Notify) |
                render_indexes(T, Block, Assigns, Notify)]
    end;
render_indexes([], _Block, _Assigns, _Notify) ->
    [].

path_render(Vars, Block, Assigns) ->
    maps:fold(fun(_Var, Path, Acc) ->
        path_render_1(Path, Block, Assigns, Acc)
    end, [], Vars).

path_render_1([Path | T], Block, Assigns, Acc) ->
    case path_render_2(Path, Block, Assigns) of
        % Ignores 'ok' in favor of io:format for debugging.
        ok ->
            path_render_1(T, Block, Assigns, Acc);
        Value ->
            path_render_1(T, Block, Assigns, [[Path, arizona_html:to_safe(Value)] | Acc])
    end;
path_render_1([], _Block, _Assigns, Acc) ->
    Acc.

path_render_2([Index], Block, Assigns) ->
    #{expr := Expr} = maps:get(Index, Block),
    Expr(Assigns);
path_render_2([Index | T], Block, Assigns) ->
    #{block := NestedBlock, attrs := Attrs} = maps:get(Index, Block),
    NestedAssigns = maps:map(fun(_K, Expr) ->
        eval(Expr, Assigns)
    end, Attrs),
    path_render_2(T, NestedBlock, NestedAssigns).

eval({text, Txt}, _Assigns) ->
    Txt;
eval({expr, {Fun, _Vars}}, Assigns) ->
    Fun(Assigns).

%% --------------------------------------------------------------------
%% EUnit
%% --------------------------------------------------------------------

-ifdef(TEST).
-compile([export_all, nowarn_export_all]).
-include_lib("eunit/include/eunit.hrl").

% NOTE: Apparently, there is no changes here, but the socket set
%       changes to the assigns automatically, then they are sync.
render_changes_test() ->
    ?assertEqual([[[4, 5], <<"999">>], [[3, 5], <<"999">>]],
        arizona_tpl_render:render_changes(block(#{}),
            #{view_count => 999},
            #{title => <<"Arizona">>,
            view_count => 999,
            decr_btn_text => <<"Decrement">>})).

%% --------------------------------------------------------------------
%% Test support
%% --------------------------------------------------------------------

block(Macros) ->
    arizona_tpl_compile:compile(arizona_tpl_compile, view, Macros).

-endif.

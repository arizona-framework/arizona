%%
%% %CopyrightBegin%
%%
%% Copyright 2024 William Fank Thomé
%%
%% Licensed under the Apache License, Version 2.0 (the "License");
%% you may not use this file except in compliance with the License.
%% You may obtain a copy of the License at
%%
%%     http://www.apache.org/licenses/LICENSE-2.0
%%
%% Unless required by applicable law or agreed to in writing, software
%% distributed under the License is distributed on an "AS IS" BASIS,
%% WITHOUT WARRANTIES OR CONDITIONS OF ANY KIND, either express or implied.
%% See the License for the specific language governing permissions and
%% limitations under the License.
%%
%% %CopyrightEnd%
%%
-module(arizona_tpl_render).
-moduledoc """
Renderer.
""".
-moduledoc #{author => "William Fank Thomé <willilamthome@hotmail.com>"}.

%% API functions.
-export([render_target/4, render_block/2, render_changes/3,
         render_diff/3, mount/2]).

%% --------------------------------------------------------------------
%% API funtions.
%% --------------------------------------------------------------------

% TODO: Rename to target_changes.
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
render_target_2([H|T], Block, Changes, Assigns) ->
    #{block := Nested} = maps:get(H, Block),
    render_target_2(T, Nested, Changes, Assigns).

render_block(#{id := Id, view := View} = Block, Assigns0) ->
    % TODO: Real socket.
    Socket0 = #{
        id => Id,
        view => View,
        assigns => Assigns0,
        events => [],
        changes => #{}
    },
    {ok, Socket} = View:mount(Socket0),
    Indexes = maps:get(indexes, Block),
    Tree = maps:get(block, Block),
    Assigns = maps:get(assigns, Socket),
    render_indexes(Indexes, Tree, Assigns, false).

render_changes(#{vars := AllVars, block := Block}, Changes, Assigns)
    when map_size(Changes) > 0 ->
    Vars = maps:with(maps:keys(Changes), AllVars),
    path_render(Vars, Block, Assigns);
render_changes(#{}, _Changes, _Assigns) ->
    [].

render_diff(#{vars := AllVars} = Block, NewAssigns, OldAssigns) ->
    Changes = diff(AllVars, NewAssigns, OldAssigns),
    Assigns = maps:merge(OldAssigns, Changes),
    render_changes(Block, Changes, Assigns).

mount(#{id := Id, view := View} = Block, Assigns0) ->
    % TODO: Real socket.
    Socket0 = #{
        id => Id,
        view => View,
        assigns => Assigns0,
        events => [],
        changes => #{}
    },
    {ok, Socket} = View:mount(Socket0),
    Assigns = maps:get(assigns, Socket),
    To = self(),
    Pid = spawn(fun() ->
        Indexes = maps:get(indexes, Block),
        Tree = maps:get(block, Block),
        Render = render_indexes(Indexes, Tree, Assigns, {true, To}),
        To ! {self(), finished, Render}
    end),
    mount_loop(Pid, [{Id, Socket}]).

%% --------------------------------------------------------------------
%% Internal funtions.
%% --------------------------------------------------------------------

diff(Vars, NewAssigns, OldAssigns) ->
    #{K => V || K := V <- NewAssigns,
        is_map_key(K, Vars) andalso (
            not is_map_key(K, OldAssigns)
            orelse maps:get(K, OldAssigns) =/= V
        )}.

mount_loop(Pid, Sockets) ->
    receive
        {Pid, finished, Render} ->
            {ok, {Render, maps:from_list(Sockets)}};
        {Pid, mount, Id, Socket} ->
            mount_loop(Pid, [{Id, Socket} | Sockets])
    after
        10_000 ->
            error(timeout)
    end.

render_indexes([H|T], Block, Assigns, Notify) ->
    case maps:get(H, Block) of
        #{text := Text} ->
            [Text | render_indexes(T, Block, Assigns, Notify)];
        #{expr := Expr} ->
            case Expr(Assigns) of
                ok ->
                    render_indexes(T, Block, Assigns, Notify);
                Value ->
                    [safe_html(Value) | render_indexes(T, Block, Assigns, Notify)]
            end;
        #{indexes := Indexes, block := NBlock, attrs := Attrs} = Nested ->
            AttrsAssigns = maps:map(fun(_K, Expr) ->
                eval(Expr, Assigns)
            end, Attrs),
            NAssigns = case maps:get(directives, Nested) of
                #{stateful := true} ->
                    NId = maps:get(id, Nested),
                    NView = maps:get(view, Nested),
                    % TODO: Real socket.
                    NSocket0 = #{
                        id => NId,
                        view => NView,
                        assigns => AttrsAssigns,
                        events => [],
                        changes => #{}
                    },
                    {ok, NSocket} = NView:mount(NSocket0),
                    case Notify of
                        {true, Pid} ->
                            Pid ! {self(), mount, NId, NSocket};
                        false ->
                            ok
                    end,
                    maps:get(assigns, NSocket);
                #{} ->
                    AttrsAssigns
            end,
            [render_indexes(Indexes, NBlock, NAssigns, Notify) |
                render_indexes(T, Block, Assigns, Notify)]
    end;
render_indexes([], _Block, _Assigns, _Notify) ->
    [].

% TODO: Do this really safe for HTML.
safe_html(V) when is_binary(V) ->
    V;
safe_html(V) when is_atom(V) ->
    atom_to_binary(V, utf8);
safe_html(V) when is_integer(V) ->
    integer_to_binary(V, 10);
safe_html(V) when is_float(V) ->
    io_lib:format("~p", [V]).

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
            path_render_1(T, Block, Assigns, [[Path, safe_html(Value)] | Acc])
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
%% EUnit tests.
%% --------------------------------------------------------------------

-ifdef(TEST).
-compile([export_all, nowarn_export_all]).
-include_lib("eunit/include/eunit.hrl").

render_block_test() ->
    ?assertEqual(
       [<<"<main arz-id=\"root\">">>,<<"<h1>">>,<<"Arizona">>,
       <<"</h1>">>,
       [<<"<div arz-id=\"[4]\" id=\"">>,<<"1">>,<<"\">">>,
        <<"<span>">>,<<"Count:">>,<<"<b>">>,<<"0">>,<<"</b>">>,
        <<"</span>">>,<<"<br/>">>,<<"</br>">>,
        [<<"<button arz-target=\"[4]\" onclick=\"">>,<<"incr">>,
         <<"\" type=\"button\">">>,<<"Increment">>,<<"</button>">>],
        <<>>,<<"</div>">>],
       [<<"<div arz-id=\"[5]\" id=\"">>,<<"2">>,<<"\">">>,
        <<"<span>">>,<<"Rev. Counter:">>,<<"<b>">>,<<"0">>,
        <<"</b>">>,<<"</span>">>,<<"<br/>">>,<<"</br>">>,
        [<<"<button arz-target=\"[5]\" onclick=\"">>,<<"decr">>,
         <<"\" type=\"button\">">>,<<"Decrement">>,<<"</button>">>],
        <<>>,<<"</div>">>],
       <<"</main>">>
    ], render_block(block(#{}), #{
            title => <<"Arizona">>,
            view_count => 0,
            decr_btn_text => <<"Decrement">>})).

render_changes_test() ->
    ?assertEqual([[[5,6],<<"999">>],[[4,6],<<"999">>]],
        render_changes(block(#{}),
            #{view_count => 999},
            #{title => <<"Arizona">>,
            view_count => 0,
            decr_btn_text => <<"Decrement">>})).

mount_test() ->
    ?assertEqual(error, mount(block(#{}), #{
            title => <<"Arizona">>,
            view_count => 0,
            decr_btn_text => <<"Decrement">>})).

%% Start block support.

block(Macros) ->
    {ok, Tpl} = arizona_tpl_compile:compile({arizona_tpl_compile, view, Macros}),
    Tpl.

%% End block support.

-endif.


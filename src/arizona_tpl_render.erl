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
-export([render_block/2, render_changes/3]).

%% --------------------------------------------------------------------
%% API funtions.
%% --------------------------------------------------------------------

render_block(#{indexes := Indexes, block := Block}, Assigns) ->
    render_indexes(Indexes, Block, Assigns).

% TODO: Return the diff with the render result.
render_changes(#{vars := Vars, block := Block}, NewAssigns, OldAssigns) ->
    case diff(Vars, NewAssigns, OldAssigns) of
        Changes when map_size(Changes) > 0 ->
            Assigns = maps:merge(OldAssigns, Changes),
            % TODO: Change code to not do lists:flatten to
            %       add the possibility to return lists
            %       instead of tuples.
            lists:flatten([path_render(FullPath, Block, Assigns)
                || K := FullPath <- Vars, is_map_key(K, Changes)]);
        #{} ->
            []
    end;
render_changes(#{}, _NewAssigns, _OldAssigns) ->
    [].

%% --------------------------------------------------------------------
%% Internal funtions.
%% --------------------------------------------------------------------

diff(Vars, NewAssigns, OldAssigns) ->
    #{K => V || K := V <- NewAssigns,
        is_map_key(K, Vars) andalso (
            not is_map_key(K, OldAssigns)
            orelse maps:get(K, OldAssigns) =/= V
        )}.

render_indexes([H|T], Block, Assigns) ->
    case maps:get(H, Block) of
        #{text := Text} ->
            [Text | render_indexes(T, Block, Assigns)];
        #{expr := Expr} ->
            case Expr(Assigns) of
                ok ->
                    render_indexes(T, Block, Assigns);
                Value ->
                    [safe_html(Value) | render_indexes(T, Block, Assigns)]
            end;
        #{indexes := Indexes, block := NestedBlock, attrs := Attrs} ->
            NestedAssigns = maps:map(fun(_K, Expr) ->
                eval(Expr, Assigns)
            end, Attrs),
            [render_indexes(Indexes, NestedBlock, NestedAssigns) |
                render_indexes(T, Block, Assigns)]
    end;
render_indexes([], _Block, _Assigns) ->
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

path_render([Path | T], Block, Assigns) ->
    case do_path_render(Path, Block, Assigns) of
        ok ->
            path_render(T, Block, Assigns);
        Value ->
            [{Path, safe_html(Value)} | path_render(T, Block, Assigns)]
    end;
path_render([], _Block, _Assigns) ->
    [].

do_path_render([Index], Block, Assigns) ->
    #{expr := Expr} = maps:get(Index, Block),
    Expr(Assigns);
do_path_render([Index | T], Block, Assigns) ->
    #{block := NestedBlock, attrs := Attrs} = maps:get(Index, Block),
    NestedAssigns = maps:map(fun(_K, Expr) ->
        eval(Expr, Assigns)
    end, Attrs),
    do_path_render(T, NestedBlock, NestedAssigns).

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
    ?assertEqual([{[4,6],999},{[5,6],999}],
        render_changes(block(#{}),
            #{view_count => 999},
            #{title => <<"Arizona">>,
            view_count => 0,
            decr_btn_text => <<"Decrement">>})).

%% Start block support.

block(Macros) ->
    {ok, Tpl} = arizona_tpl_compile:compile({arizona_tpl_compile, view, Macros}),
    Tpl.

%% End block support.

-endif.


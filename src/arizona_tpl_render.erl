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

render_changes(#{vars := Vars, block := Block}, NewAssigns, OldAssigns) ->
    case diff(Vars, NewAssigns, OldAssigns) of
        Changes when map_size(Changes) > 0 ->
            Assigns = maps:merge(OldAssigns, Changes),
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
                    [Value | render_indexes(T, Block, Assigns)]
            end;
        #{indexes := Indexes, block := NestedBlock, attrs := Attrs} ->
            NestedAssigns = maps:map(fun(_K, Fun) ->
                Fun(Assigns)
            end, Attrs),
            [render_indexes(Indexes, NestedBlock, NestedAssigns) |
                render_indexes(T, Block, Assigns)]
    end;
render_indexes([], _Block, _Assigns) ->
    [].

path_render([Path | T], Block, Assigns) ->
    case do_path_render(Path, Block, Assigns) of
        ok ->
            path_render(T, Block, Assigns);
        Value ->
            [{Path, Value} | path_render(T, Block, Assigns)]
    end;
path_render([], _Block, _Assigns) ->
    [].

do_path_render([Index], Block, Assigns) ->
    #{expr := Expr} = maps:get(Index, Block),
    Expr(Assigns);
do_path_render([Index | T], Block, Assigns) ->
    #{block := NestedBlock, attrs := Attrs} = maps:get(Index, Block),
    NestedAssigns = maps:map(fun(_K, Fun) ->
        Fun(Assigns)
    end, Attrs),
    do_path_render(T, NestedBlock, NestedAssigns).

%% --------------------------------------------------------------------
%% EUnit tests.
%% --------------------------------------------------------------------

-ifdef(TEST).
-compile([export_all, nowarn_export_all]).
-include_lib("eunit/include/eunit.hrl").

render_block_test() ->
    ?assertEqual([
        <<"<div>">>,
        [<<"<span kf-id=\"[0,1,0]\">">>,99,<<"</span>">>],
        [<<"<span kf-id=\"[0,2,0]\">">>,0,<<"</span>">>],
        <<"</div>">>
    ], render_block(block(), #{root_count => 99})).

render_changes_test() ->
    ?assertEqual([{[1, 1], 100}],
        render_changes(block(),
                      #{root_count => 100},
                      #{root_count => 99})).

%% Start block support.

block() ->
   #{
        id => [0],
        block => #{
            0 => #{
                id => [0, 0],
                text => <<"<div>">>
            },
            1 => #{
                id => [0, 1],
                block => #{
                    0 => #{
                        id => [0, 1, 0],
                        text => <<"<span kf-id=\"[0,1,0]\">">>
                    },
                    1 => #{
                        id => [0, 1, 1],
                        expr => fun(Assigns) ->
                            maps:get(my_count, Assigns)
                        end
                    },
                    2 => #{
                        id => [0, 1, 2],
                        text => <<"</span>">>
                    }
                },
                indexes => [0, 1, 2],
                attrs => #{
                    my_count => fun(Assigns) ->
                        maps:get(root_count, Assigns)
                    end
                },
                vars => #{
                    my_count => [
                        [1]
                    ]
                }
            },
            2 => #{
                id => [0, 2],
                block => #{
                    0 => #{
                        id => [0, 2, 0],
                        text => <<"<span kf-id=\"[0,2,0]\">">>
                    },
                    1 => #{
                        id => [0, 2, 1],
                        expr => fun(Assigns) ->
                            maps:get(my_count, Assigns)
                        end,
                        vars => [my_count]
                    },
                    2 => #{
                        id => [0, 2, 2],
                        text => <<"</span>">>
                    }
                },
                indexes => [0, 1, 2],
                attrs => #{
                    my_count => fun(_Assigns) -> 0 end
                }
            },
            3 => #{
                id => [0, 3],
                text => <<"</div>">>
            }
        },
        indexes => [0, 1, 2, 3],
        vars => #{
            root_count => [
                [1, 1]
            ]
        }
    }.

%% End block support.

-endif.


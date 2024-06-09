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
-module(arizona_tpl_compile).
-moduledoc """
Template compiler.
""".
-moduledoc #{author => "William Fank Thomé <willilamthome@hotmail.com>"}.

%% API functions.
-export([compile_tree/1]).

%% --------------------------------------------------------------------
%% API funtions.
%% --------------------------------------------------------------------

compile_tree(Tree) ->
    compile_tree(Tree, [], 0).

%% --------------------------------------------------------------------
%% Internal funtions.
%% --------------------------------------------------------------------

compile_tree([{text, Txt} | T], P, I) ->
    [{I, text_struct(Txt, P, I)} | compile_tree(T, P, I+1)];
compile_tree([{expr, {Expr, Vars}} | T], P, I) ->
    [{I, expr_struct(Expr, Vars, P, I)} | compile_tree(T, P, I+1)];
compile_tree([{tag, Tag} | T], P, I) ->
    [{I, #{
       id => lists:reverse([I | P]),
       text => tag_header_to_str(Tag)
    }} | compile_tag(maps:get(tokens, Tag), Tag, T, P, I+1)];
compile_tree([{block, Block} | T], P, I) ->
    [{I, block_struct(Block, P, I)} | compile_tree(T, P, I+1)];
compile_tree([], _P, _I) ->
    [].

compile_tag([{text, Txt} | T], Tag, TT, P, I) ->
    [{I, text_struct(Txt, P, I)} | compile_tag(T, Tag, TT, P, I+1)];
compile_tag([{expr, {Expr, Vars}} | T], Tag, TT, P, I) ->
    [{I, expr_struct(Expr, Vars, P, I)} | compile_tag(T, Tag, TT, P, I+1)];
compile_tag([{tag, Tag} | T], TTag, TT, P, I) ->
    % I think this implementation can be optimized.
    Tokens = [{I, #{
       id => lists:reverse([I | P]),
       text => tag_header_to_str(Tag)
    }} | compile_tag(maps:get(tokens, Tag), Tag, [], P, I+1)],
    {NI, _} = lists:last(Tokens),
    Tokens ++ compile_tag(T, TTag, TT, P, NI+1);
compile_tag([{block, Block} | T], Tag, TT, P, I) ->
    [{I, block_struct(Block, P, I)} | compile_tag(T, Tag, TT, P, I+1)];
compile_tag([], #{void := true}, T, P, I) ->
    compile_tree(T, P, I);
compile_tag([], #{void := false} = Tag, T, P, I) ->
    [{I, #{
        id => lists:reverse([I | P]),
        text => tag_ending_to_str(Tag)
    }} | compile_tree(T, P, I+1)].

tag_header_to_str(#{attrs := Attrs} = Tag) ->
    iolist_to_binary([tag_open_to_str(Tag),
                      tag_attrs_to_str(Attrs),
                      tag_close_to_str(Tag)]).

tag_open_to_str(#{name := Name}) ->
    [$<, Name].

tag_attrs_to_str([{K, V} | T]) ->
    [$\s, K, $=, $", V, $" | tag_attrs_to_str(T)];
tag_attrs_to_str([K | T]) ->
    [$\s, K | tag_attrs_to_str(T)];
tag_attrs_to_str([]) ->
    [].

tag_close_to_str(#{void := true}) ->
    [$/, $>];
tag_close_to_str(#{void := false}) ->
    $>.

tag_ending_to_str(#{void := false, name := Name}) ->
    <<$<, $/, Name/binary, $>>>;
tag_ending_to_str(#{void := true}) ->
    <<>>.

text_struct(Txt, P, I) ->
    #{
        id => lists:reverse([I | P]),
        text => Txt
     }.

expr_struct(Expr, _Vars, P, I) ->
    #{
        id => lists:reverse([I | P]),
        expr => Expr
        %vars => Vars
     }.

block_struct(#{module := M, function := F}, P, I) ->
    #{
        id => lists:reverse([I | P]),
        block => maps:from_list(compile_tree(M:F(), [I | P], 0))
    }.

%% --------------------------------------------------------------------
%% EUnit tests.
%% --------------------------------------------------------------------

-ifdef(TEST).
-compile([export_all, nowarn_export_all]).
-include_lib("eunit/include/eunit.hrl").

compile_tree_test() ->
    ?assertMatch([
        {0, #{
           id := [0],
           block := #{
               0 := #{
                   id := [0, 0],
                   text := <<"<main>">>
               },
               1 := #{
                   id := [0, 1],
                   block := #{
                       0 := #{
                           id := [0, 1, 0],
                           text := <<"<div>">>
                       },
                       1 := #{
                           id := [0, 1, 1],
                           text := <<"<span>">>
                       },
                       2 := #{
                           id := [0, 1, 2],
                           expr := _
                       },
                       3 := #{
                           id := [0, 1, 3],
                           text := <<"<b>">>
                       },
                       4 := #{
                           id := [0, 1, 4],
                           expr := _
                       },
                       5 := #{
                           id := [0, 1, 5],
                           text := <<"</b>">>
                       },
                       6 := #{
                           id := [0, 1, 6],
                           text := <<"</span>">>
                       },
                       7 := #{
                           id := [0, 1, 7],
                           text := <<"<br/>">>
                       },
                       8 := #{
                           id := [0, 1, 8],
                           text := <<"<button type=\"button\">">>
                       },
                       9 := #{
                           id := [0, 1, 9],
                           text := <<"Increment">>
                       },
                       10 := #{
                           id := [0, 1, 10],
                           text := <<"</button>">>
                       },
                       11 := #{
                           id := [0, 1, 11],
                           text := <<"</div>">>
                       }
                   }
               },
               2 := #{
                   id := [0, 2],
                   block := #{
                       0 := #{
                           id := [0, 2, 0],
                           text := <<"<div>">>
                       },
                       1 := #{
                           id := [0, 2, 1],
                           text := <<"<span>">>
                       },
                       2 := #{
                           id := [0, 2, 2],
                           expr := _
                       },
                       3 := #{
                           id := [0, 2, 3],
                           text := <<"<b>">>
                       },
                       4 := #{
                           id := [0, 2, 4],
                           expr := _
                       },
                       5 := #{
                           id := [0, 2, 5],
                           text := <<"</b>">>
                       },
                       6 := #{
                           id := [0, 2, 6],
                           text := <<"</span>">>
                       },
                       7 := #{
                           id := [0, 2, 7],
                           text := <<"<br/>">>
                       },
                       8 := #{
                           id := [0, 2, 8],
                           text := <<"<button type=\"button\">">>
                       },
                       9 := #{
                           id := [0, 2, 9],
                           text := <<"Increment">>
                       },
                       10 := #{
                           id := [0, 2, 10],
                           text := <<"</button>">>
                       },
                       11 := #{
                           id := [0, 2, 11],
                           text := <<"</div>">>
                       }
                   }
               },
               3 := #{
                   id := [0, 3],
                   text := <<"</main>">>
               }
            }
        }}
    ], compile_tree([
        {block, #{module => ?MODULE, function => view}}
    ])).

%% Start compile support.

view() ->
    [{tag, #{
        name => <<"main">>,
        directives => #{
            statefull => true
        },
        attrs => [],
        void => false,
        tokens => [
            {block, #{
                module => ?MODULE,
                function => counter
            }},
            {block, #{
                module => ?MODULE,
                function => counter
            }}
        ]
    }}].

counter() ->
    [{tag, #{
        name => <<"div">>,
        directives => #{
            statefull => true
        },
        attrs => [],
        void => false,
        tokens => [
            {tag, #{
                name => <<"span">>,
                directives => #{},
                attrs => [],
                void => false,
                tokens => [
                    {expr, {fun(Assigns) ->
                        try maps:get(label, Assigns)
                        catch _:_ -> <<"Count>">> end
                    end, [label]}},
                    {tag, #{
                        name => <<"b">>,
                        directives => #{},
                        attrs => [],
                        void => false,
                        tokens => [
                            {expr, {fun(Assigns) ->
                                maps:get(counter_count, Assigns)
                            end, [counter_count]}}
                        ]
                    }}
                ]
            }},
            {tag, #{
                name => <<"br">>,
                directives => #{},
                attrs => [],
                void => true,
                tokens => []
            }},
            {tag, #{
                name => <<"button">>,
                directives => #{},
                attrs => [{<<"type">>, <<"button">>}],
                void => false,
                tokens => [
                    {text, <<"Increment">>}
                ]
            }}
        ]
    }}].

%% End compile support.

tag_header_to_str_test() ->
    [
        ?assertEqual(<<"<div id=\"foo\" hidden>">>,
            tag_header_to_str(#{void => false, name => <<"div">>,
                attrs => [{<<"id">>, <<"foo">>}, <<"hidden">>]})),
        ?assertEqual(<<"<input value=\"foo\"/>">>,
            tag_header_to_str(#{void => true, name => <<"input">>,
                attrs => [{<<"value">>, <<"foo">>}]}))
    ].

tag_ending_to_str_test() ->
    [
        ?assertEqual(<<"</div>">>,
            tag_ending_to_str(#{void => false, name => <<"div">>})),
        ?assertEqual(<<>>,
            tag_ending_to_str(#{void => true}))
    ].

-endif.


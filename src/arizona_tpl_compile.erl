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
-export([compile/1]).

%% --------------------------------------------------------------------
%% API funtions.
%% --------------------------------------------------------------------

% TODO: Concat texts.
compile(Tree) ->
    {ok, compile(Tree, [], 0)}.

%% --------------------------------------------------------------------
%% Internal funtions.
%% --------------------------------------------------------------------

compile([{text, Txt} | T], P, I) ->
    [{I, text_struct(Txt, P, I)} | compile(T, P, I+1)];
compile([{expr, {Expr, Vars}} | T], P, I) ->
    [{I, expr_struct(Expr, Vars, P, I)} | compile(T, P, I+1)];
compile([{tag, Tag} | T], P, I) ->
    compile_open_tag(Tag, T, P, I);
compile([{block, Block} | T], P, I) ->
    [{I, block_struct(Block, P, I)} | compile(T, P, I+1)];
compile([], _P, _I) ->
    [].

compile_tag([{text, Txt} | T], Tag, TT, P, I) ->
    [{I, text_struct(Txt, P, I)} | compile_tag(T, Tag, TT, P, I+1)];
compile_tag([{expr, {Expr, Vars}} | T], Tag, TT, P, I) ->
    [{I, expr_struct(Expr, Vars, P, I)} | compile_tag(T, Tag, TT, P, I+1)];
compile_tag([{tag, Tag} | T], TTag, TT, P, I) ->
    Tokens = compile_open_tag(Tag, T, P, I),
    {NI, _} = lists:last(Tokens),
    Tokens ++ compile_tag(T, TTag, TT, P, NI+1);
compile_tag([{block, Block} | T], Tag, TT, P, I) ->
    [{I, block_struct(Block, P, I)} | compile_tag(T, Tag, TT, P, I+1)];
compile_tag([], #{void := true}, T, P, I) ->
    compile(T, P, I);
compile_tag([], #{void := false} = Tag, T, P, I) ->
    [{I, #{
        id => lists:reverse([I | P]),
        text => tag_ending_to_str(Tag)
    }} | compile(T, P, I+1)].

compile_open_tag(#{name := Name, attrs := Attrs} = Tag, T, P, I) ->
    do_compile_open_tag(Attrs, Tag, T, P, I, [Name, $<]).

do_compile_open_tag([{K, {expr, {Expr, Vars}}} | T], Tag, TT, P, I, Acc) ->
    [{I, #{
        id => lists:reverse([I | P]),
        text => iolist_to_binary(lists:reverse(
                    [$", $=, K, $\s | Acc]))
    }}, {I+1, expr_struct(Expr, Vars, P, I+1)}
    | do_compile_open_tag(T, Tag, TT, P, I+2, [$"])];
do_compile_open_tag([{K, {text, Txt}} | T], Tag, TT, P, I, Acc) ->
    do_compile_open_tag(T, Tag, TT, P, I, [$", Txt, $", $=, K, $\s | Acc]);
do_compile_open_tag([K | T], Tag, TT, P, I, Acc) ->
    do_compile_open_tag(T, Tag, TT, P, I, [K, $\s | Acc]);
do_compile_open_tag([], #{void := true} = Tag, TT, P, I, Acc) ->
    [{I, #{
        id => lists:reverse([I | P]),
        text => iolist_to_binary(lists:reverse([$>, $/ | Acc]))
    }} | compile_tag(maps:get(tokens, Tag), Tag, TT, P, I+1)];
do_compile_open_tag([], #{void := false} = Tag, TT, P, I, Acc) ->
    [{I, #{
        id => lists:reverse([I | P]),
        text => iolist_to_binary(lists:reverse([$> | Acc]))
    }} | compile_tag(maps:get(tokens, Tag), Tag, TT, P, I+1)].

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
        block => maps:from_list(compile(M:F(), [I | P], 0))
    }.

%% --------------------------------------------------------------------
%% EUnit tests.
%% --------------------------------------------------------------------

-ifdef(TEST).
-compile([export_all, nowarn_export_all]).
-include_lib("eunit/include/eunit.hrl").

compile_tree_test() ->
    ?assertMatch({ok,[{0,
        #{block :=
            #{0 := #{id := [0,0],text := <<"<main>">>},
              1 :=
                  #{block :=
                        #{0 :=
                              #{id := [0,1,0],
                                text := <<"<div id=\"">>},
                          1 :=
                              #{id := [0,1,1],
                                expr :=
                                    _},
                          2 :=
                              #{id := [0,1,2],
                                text := <<"\">">>},
                          3 :=
                              #{id := [0,1,3],
                                text := <<"<span>">>},
                          4 :=
                              #{id := [0,1,4],
                                expr :=
                                    _},
                          5 :=
                              #{id := [0,1,5],
                                text := <<"<b>">>},
                          6 :=
                              #{id := [0,1,6],
                                expr :=
                                    _},
                          7 :=
                              #{id := [0,1,7],
                                text := <<"</b>">>},
                          8 :=
                              #{id := [0,1,8],
                                text := <<"</span>">>},
                          9 :=
                              #{id := [0,1,9],
                                text := <<"<br/>">>},
                          10 :=
                              #{id := [0,1,10],
                                text :=
                                    <<"<button type=\"button\">">>},
                          11 :=
                              #{id := [0,1,11],
                                text := <<"Increment">>},
                          12 :=
                              #{id := [0,1,12],
                                text := <<"</button>">>},
                          13 :=
                              #{id := [0,1,13],
                                expr :=
                                    _},
                          14 :=
                              #{id := [0,1,14],
                                text := <<"<br/>">>},
                          15 :=
                              #{id := [0,1,15],
                                text :=
                                    <<"<button type=\"button\">">>},
                          16 :=
                              #{id := [0,1,16],
                                text := <<"Increment">>},
                          17 :=
                              #{id := [0,1,17],
                                text := <<"</button>">>},
                          18 :=
                              #{id := [0,1,18],
                                expr :=
                                    _},
                          19 :=
                              #{id := [0,1,19],
                                text :=
                                    <<"<button type=\"button\">">>},
                          20 :=
                              #{id := [0,1,20],
                                text := <<"Increment">>},
                          21 :=
                              #{id := [0,1,21],
                                text := <<"</button>">>},
                          22 :=
                              #{id := [0,1,22],
                                expr :=
                                    _},
                          23 :=
                              #{id := [0,1,23],
                                expr :=
                                    _},
                          24 :=
                              #{id := [0,1,24],
                                text := <<"</div>">>}},
                    id := [0,1]},
              2 :=
                  #{block :=
                        #{0 :=
                              #{id := [0,2,0],
                                text := <<"<div id=\"">>},
                          1 :=
                              #{id := [0,2,1],
                                expr :=
                                    _},
                          2 :=
                              #{id := [0,2,2],
                                text := <<"\">">>},
                          3 :=
                              #{id := [0,2,3],
                                text := <<"<span>">>},
                          4 :=
                              #{id := [0,2,4],
                                expr :=
                                    _},
                          5 :=
                              #{id := [0,2,5],
                                text := <<"<b>">>},
                          6 :=
                              #{id := [0,2,6],
                                expr :=
                                    _},
                          7 :=
                              #{id := [0,2,7],
                                text := <<"</b>">>},
                          8 :=
                              #{id := [0,2,8],
                                text := <<"</span>">>},
                          9 :=
                              #{id := [0,2,9],
                                text := <<"<br/>">>},
                          10 :=
                              #{id := [0,2,10],
                                text :=
                                    <<"<button type=\"button\">">>},
                          11 :=
                              #{id := [0,2,11],
                                text := <<"Increment">>},
                          12 :=
                              #{id := [0,2,12],
                                text := <<"</button>">>},
                          13 :=
                              #{id := [0,2,13],
                                expr :=
                                    _},
                          14 :=
                              #{id := [0,2,14],
                                text := <<"<br/>">>},
                          15 :=
                              #{id := [0,2,15],
                                text :=
                                    <<"<button type=\"button\">">>},
                          16 :=
                              #{id := [0,2,16],
                                text := <<"Increment">>},
                          17 :=
                              #{id := [0,2,17],
                                text := <<"</button>">>},
                          18 :=
                              #{id := [0,2,18],
                                expr :=
                                    _},
                          19 :=
                              #{id := [0,2,19],
                                text :=
                                    <<"<button type=\"button\">">>},
                          20 :=
                              #{id := [0,2,20],
                                text := <<"Increment">>},
                          21 :=
                              #{id := [0,2,21],
                                text := <<"</button>">>},
                          22 :=
                              #{id := [0,2,22],
                                expr :=
                                    _},
                          23 :=
                              #{id := [0,2,23],
                                expr :=
                                    _},
                          24 :=
                              #{id := [0,2,24],
                                text := <<"</div>">>}},
                    id := [0,2]},
              3 := #{id := [0,3],text := <<"</main>">>}},
        id := [0]}
    }]}, compile([{block, #{module => ?MODULE, function => view}}])).

%% Start compile support.

view() ->
    {ok, Tokens, _EndLoc} = arizona_tpl_scan:string("""
    <main>
        <.arizona_tpl_compile:counter counter_count={_@view_count}/>
        <.arizona_tpl_compile:counter label="Other:" counter_count={0}/>
    </main>
    """),
    {ok, Tree} = arizona_tpl_parse:parse_exprs(Tokens),
    Tree.

counter() ->
    {ok, Tokens, _EndLoc} = arizona_tpl_scan:string("""
    {% NOTE: :statefull directive defines the arz-id attribue,  }
    {%       and adds this to the blocks param in the state.    }
    <div id={_@id} :statefull>
        <span>
            {% TODO: Find a better way to define default values }
            {%       in a pure Erlang implementation.           }
            {%       Using try/catch by now for this.           }
            {%       Should find a way to define a maps:get/3.  }
            {try _@label catch _:_ -> <<"Count:">> end}

            {% NOTE: _@counter_count not wrapper by try/catch,  }
            {%       so it's required.                          }
            <b>{_@counter_count}</b>
        </span>

        <br/>

        <button type="button">Increment</button>

        {% NOTE: The 'content' var is a private one.            }
        {%       It's filled with tokens when is not a void tag }
        {%       and the content between `>...</`, for example  }
        {%       > <div>mycontent</div>                         }
        {% FIXME: This implementation wont work, because it     }
        {%        needs to be compiled in tokens.               }
        {%        The 'merl:tsubst' should be used to fix this. }
        {try _@content catch _:_ -> <<>> end}
    </div>
    """),
    {ok, Tree} = arizona_tpl_parse:parse_exprs(Tokens),
    Tree.

%% End compile support.

tag_ending_to_str_test() ->
    [
        ?assertEqual(<<"</div>">>,
            tag_ending_to_str(#{void => false, name => <<"div">>})),
        ?assertEqual(<<>>,
            tag_ending_to_str(#{void => true}))
    ].

-endif.


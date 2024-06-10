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
% NOTE: Multiple tags is not allowed for a root, e.g.:
%       Good: <html><head></head><body></body></html>
%        Bad: <head></head><body></body>
compile({Mod, Fun}) when is_atom(Mod), is_atom(Fun) ->
    [{0, Block}] = compile([{block, #{
        module => Mod,
        function => Fun,
        directives => #{statefull => true}}}], [], 0),
    {ok, Block};
compile([{tag, Tag}]) ->
    [{0, Block}] = compile([{tag, Tag}], [], 0),
    {ok, Block}.

%% --------------------------------------------------------------------
%% Internal funtions.
%% --------------------------------------------------------------------

compile([{text, Txt} | T], P, I) ->
    [{I, text_struct(Txt, P, I)} | compile(T, P, I+1)];
compile([{expr, {Expr, Vars}} | T], P, I) ->
    [{I, expr_struct(Expr, Vars, P, I)} | compile(T, P, I+1)];
compile([{tag, Tag} | T], P, I) ->
    compile_tag(Tag, <<>>, T, P, I);
compile([{block, Block} | T], P, I) ->
    [{I, block_struct(Block, P, I)} | compile(T, P, I+1)];
compile([], _P, _I) ->
    [].

compile_tag(#{name := Name, attrs := Attrs} = Tag, Txt, T, P, I) ->
    do_compile_tag(Attrs, Tag, T, P, I, [Name, $<, Txt]).

do_compile_tag([{K, {expr, {Expr, Vars}}} | T], Tag, TT, P, I, Acc) ->
    [{I, #{
        id => id(P, I),
        text => iolist_to_binary(lists:reverse([$", $=, K, $\s | Acc]))
    }}, {I+1, expr_struct(Expr, Vars, P, I+1)}
    | do_compile_tag(T, Tag, TT, P, I+2, [$"])];
do_compile_tag([{K, {text, Txt}} | T], Tag, TT, P, I, Acc) ->
    do_compile_tag(T, Tag, TT, P, I, [$", Txt, $", $=, K, $\s | Acc]);
do_compile_tag([K | T], Tag, TT, P, I, Acc) ->
    do_compile_tag(T, Tag, TT, P, I, [K, $\s | Acc]);
% NOTE: It's possible to concat texts here.
do_compile_tag([], Tag, TT, P, I, Acc) ->
    Tokens = [{I, tag_open_struct(Tag, Acc, P, I)}
                | compile(maps:get(tokens, Tag) , P, I+1)],
    {NI, _} = lists:last(Tokens),
    Tokens ++ compile([{text, tag_closing(Tag)} | TT], P, NI+1).

tag_closing(#{name := Name}) ->
    <<$<, $/, Name/binary, $>>>.

id([], 0) ->
    root;
id(P, I) ->
    [0 | Id] = lists:reverse([I | P]),
    Id.

text_struct(Txt, P, I) ->
    #{
        id => id(P, I),
        text => Txt
     }.

expr_struct(Expr, Vars, P, I) ->
    #{
        id => id(P, I),
        expr => Expr,
        vars => Vars
     }.

tag_open_struct(#{void := true} = _Tag, Acc, P, I) ->
    #{
        id => id(P, I),
        text => iolist_to_binary(lists:reverse([$>, $/ | Acc]))
    };
tag_open_struct(#{void := false} = _Tag, Acc, P, I) ->
    #{
        id => id(P, I),
        text => iolist_to_binary(lists:reverse([$> | Acc]))
    }.

block_struct(Block, P, I) ->
    Tokens = block_tokens(Block, P, I),
    TokensMap = maps:from_list(Tokens),
    Attrs = block_attrs(Block),
    #{
        id => id(P, I),
        block => TokensMap,
        indexes => lists:usort(maps:keys(TokensMap)),
        directives => maps:get(directives, Block, #{}),
        attrs => Attrs,
        vars => case P =:= [] of
                    true -> root_vars(Tokens);
                    false -> block_vars(Tokens, Attrs)
                end
    }.

% TODO: Remove vars prop from expr. They will live in the block props.
block_tokens(#{module := M, function := F}, P, I) ->
    compile(M:F(), [I | P], 0).

% TODO: Use binary_to_existing_atom.
block_attrs(#{attrs := Attrs}) ->
    #{binary_to_atom(K, utf8) => V || {K, V} <- Attrs};
block_attrs(#{}) ->
    #{}.

root_vars(Tokens) ->
    tokens_vars(Tokens, false).

block_vars(Tokens, Attrs) ->
    tokens_vars(Tokens, {true, Attrs}).

tokens_vars(Tokens, Attrs) ->
    maps:groups_from_list(
        fun({Var, _Id}) -> Var end,
        fun({_Var, Id}) -> Id end,
        tokens_vars_1(Tokens, Attrs)).

tokens_vars_1([{_I, #{vars := Vars, id := Id}} | T], Attrs) when is_list(Vars) ->
    tokens_vars_2(Vars, Id, T, Attrs);
tokens_vars_1([{_I, #{vars := Vars}} | T], {true, Attrs}) when is_map(Vars) ->
    tokens_vars_4(maps:to_list(Vars), T, {true, Attrs});
tokens_vars_1([{_I, #{vars := Vars}} | T], false) when is_map(Vars) ->
    tokens_vars_7(maps:to_list(Vars), T);
tokens_vars_1([_|T], Attrs) ->
    tokens_vars_1(T, Attrs);
tokens_vars_1([], _Attrs) ->
    [].

tokens_vars_2([Var | Vars], Id, T, {true, Attrs}) ->
    case Attrs of
        #{Var := {expr, {_Fun, ExprVars}}} ->
            tokens_vars_3(ExprVars, Vars, Id, T, {true, Attrs});
        #{} ->
            tokens_vars_2(Vars, Id, T, {true, Attrs})
    end;
tokens_vars_2([Var | Vars], Id, T, false) ->
    [{Var, Id} | tokens_vars_2(Vars, Id, T, false)];
tokens_vars_2([], _Id, T, Attrs) ->
    tokens_vars_1(T, Attrs).

tokens_vars_3([Var | ExprVars], Vars, Id, T, Attrs) when is_atom(Var) ->
    [{Var, Id} | tokens_vars_3(ExprVars, Vars, Id, T, Attrs)];
tokens_vars_3([], Vars, Id, T, Attrs) ->
    tokens_vars_2(Vars, Id, T, Attrs).

tokens_vars_4([{Var, Ids} | Vars], T, {true, Attrs}) when is_atom(Var), is_list(Ids) ->
    case Attrs of
        #{Var := {expr, {_Fun, ExprVars}}} ->
            tokens_vars_5(ExprVars, Vars, Ids, T, {true, Attrs});
        #{} ->
            tokens_vars_4(Vars, T, {true, Attrs})
    end;
tokens_vars_4([], T, Attrs) ->
    tokens_vars_1(T, Attrs).

tokens_vars_5([Var | ExprVars], Vars, Ids, T, Attrs) when is_atom(Var) ->
    tokens_vars_6(Ids, Var, ExprVars, Vars, Ids, T, Attrs);
tokens_vars_5([], Vars, _Id, T, Attrs) ->
    tokens_vars_4(Vars, T, Attrs).

tokens_vars_6([Id | Ids], Var, ExprVars, Vars, VIds, T, Attrs) ->
    [{Var, Id} | tokens_vars_6(Ids, Var, ExprVars, Vars, VIds, T, Attrs)];
tokens_vars_6([], _Var, ExprVars, Vars, VIds, T, Attrs) ->
    tokens_vars_5(ExprVars, Vars, VIds, T, Attrs).

tokens_vars_7([{Var, Ids} | Vars], T) when is_atom(Var), is_list(Ids) ->
    tokens_vars_8(Ids, Var, Vars, T);
tokens_vars_7([], T) ->
    tokens_vars_1(T, false).

tokens_vars_8([Id | Ids], Var, Vars, T) ->
    [{Var, Id} | tokens_vars_8(Ids, Var, Vars, T)];
tokens_vars_8([], _Var, Vars, T) ->
    tokens_vars_7(Vars, T).

%% --------------------------------------------------------------------
%% EUnit tests.
%% --------------------------------------------------------------------

-ifdef(TEST).
-compile([export_all, nowarn_export_all]).
-include_lib("eunit/include/eunit.hrl").

compile_tree_test() ->
    ?assertMatch({ok,
        #{block :=
         #{0 := #{id := [0],text := <<"<main>">>},
           1 := #{id := [1],text := <<"<h1>">>},
           2 :=
            #{id := [2],
              expr := _,
              vars := [title]},
           3 := #{id := [3],text := <<"</h1>">>},
           4 :=
            #{block :=
               #{0 := #{id := [4,0],text := <<"<div id=\"">>},
                 1 :=
                  #{id := [4,1],
                    expr := _,
                    vars := [id]},
                 2 := #{id := [4,2],text := <<"\">">>},
                 3 := #{id := [4,3],text := <<"<span>">>},
                 4 :=
                  #{id := [4,4],
                    expr := _,
                    vars := [label]},
                 5 := #{id := [4,5],text := <<"<b>">>},
                 6 :=
                  #{id := [4,6],
                    expr := _,
                    vars := [counter_count]},
                 7 := #{id := [4,7],text := <<"</b>">>},
                 8 := #{id := [4,8],text := <<"</span>">>},
                 9 := #{id := [4,9],text := <<"<br/>">>},
                 10 := #{id := [4,10],text := <<"</br>">>},
                 11 :=
                  #{block :=
                     #{0 :=
                        #{id := [4,11,0],
                          text := <<"<button type=\"button\">">>},
                       1 :=
                        #{id := [4,11,1],
                          expr := _,
                          vars := [text]},
                       2 :=
                        #{id := [4,11,2],text := <<"</button>">>}},
                    id := [4,11],
                    attrs :=
                     #{text :=
                        {expr,
                         {_,[btn_text]}}},
                    vars := #{btn_text := [[4,11,1]]},
                    directives := #{},
                    indexes := [0,1,2]},
                 12 :=
                  #{id := [4,12],
                    expr := _,
                    vars := [content]},
                 13 := #{id := [4,13],text := <<"</div>">>}},
              id := [4],
              attrs :=
               #{id := {text,<<"1">>},
                 counter_count :=
                  {expr,{_,[view_count]}},
                 btn_text := {text,<<"Increment">>}},
              vars := #{view_count := [[4,6]]},
              directives := #{},
              indexes := [0,1,2,3,4,5,6,7,8,9,10,11,12,13]},
           5 :=
            #{block :=
               #{0 := #{id := [5,0],text := <<"<div id=\"">>},
                 1 :=
                  #{id := [5,1],
                    expr := _,
                    vars := [id]},
                 2 := #{id := [5,2],text := <<"\">">>},
                 3 := #{id := [5,3],text := <<"<span>">>},
                 4 :=
                  #{id := [5,4],
                    expr := _,
                    vars := [label]},
                 5 := #{id := [5,5],text := <<"<b>">>},
                 6 :=
                  #{id := [5,6],
                    expr := _,
                    vars := [counter_count]},
                 7 := #{id := [5,7],text := <<"</b>">>},
                 8 := #{id := [5,8],text := <<"</span>">>},
                 9 := #{id := [5,9],text := <<"<br/>">>},
                 10 := #{id := [5,10],text := <<"</br>">>},
                 11 :=
                  #{block :=
                     #{0 :=
                        #{id := [5,11,0],
                          text := <<"<button type=\"button\">">>},
                       1 :=
                        #{id := [5,11,1],
                          expr := _,
                          vars := [text]},
                       2 :=
                        #{id := [5,11,2],text := <<"</button>">>}},
                    id := [5,11],
                    attrs :=
                     #{text :=
                        {expr,
                         {_,[btn_text]}}},
                    vars := #{btn_text := [[5,11,1]]},
                    directives := #{},
                    indexes := [0,1,2]},
                 12 :=
                  #{id := [5,12],
                    expr := _,
                    vars := [content]},
                 13 := #{id := [5,13],text := <<"</div>">>}},
              id := [5],
              attrs :=
               #{id := {text,<<"2">>},
                 label := {text,<<"Rev. Counter:">>},
                 counter_count :=
                  {expr,{_,[view_count]}},
                 btn_text :=
                  {expr,
                   {_,[decr_btn_text]}}},
              vars :=
               #{view_count := [[5,6]],
                 decr_btn_text := [[5,11,1]]},
              directives := #{},
              indexes := [0,1,2,3,4,5,6,7,8,9,10,11,12,13]},
           6 := #{id := [6],text := <<"</main>">>}},
        id := root,attrs := #{},
        vars :=
         #{title := [[2]],
           view_count := [[4,6],[5,6]],
           decr_btn_text := [[5,11,1]]},
        directives := #{},
        indexes := [0,1,2,3,4,5,6]}
    }, compile({?MODULE, view})).

%% Start compile support.

view() ->
    {ok, Tokens, _EndLoc} = arizona_tpl_scan:string("""
    <main :statefull>
        <h1>{_@title}</h1>
        <.arizona_tpl_compile:counter
            id="1"
            counter_count={_@view_count}
            btn_text="Increment"
        />
        <.arizona_tpl_compile:counter
            id="2"
            label="Rev. Counter:"
            counter_count={_@view_count}
            btn_text={_@decr_btn_text}
        />
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

        <.arizona_tpl_compile:button text={_@btn_text}/>

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

button() ->
    {ok, Tokens, _EndLoc} = arizona_tpl_scan:string("""
    <button type="button">{_@text}</button>
    """),
    {ok, Tree} = arizona_tpl_parse:parse_exprs(Tokens),
    Tree.

%% End compile support.

-endif.


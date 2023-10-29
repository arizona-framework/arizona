%% @author William Fank Thomé <willilamthome@hotmail.com>
%% @copyright 2023 William Fank Thomé
%% @doc Template.

%% Copyright 2023 William Fank Thomé
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
-module(arizona_template).

%% API
-export([ compile/1
        , bind/2
        , vars/1
        , render/1
        , tree/1
        , diff/1
        , types/2
        , cast/3
        ]).

%% Macros
-define(ADAPTER, (arizona_env:get_template(adapter))).

-define(TYPE_BINARY, <<"b">>).
-define(TYPE_ATOM, <<"a">>).
-define(TYPE_LIST, <<"l">>).
-define(TYPE_MAP, <<"m">>).
-define(TYPE_BOOLEAN, <<"1">>).
-define(TYPE_INTEGER, <<"i">>).
-define(TYPE_FLOAT, <<"f">>).
-define(TYPE_PID, <<"p">>).
-define(TYPE_PORT, <<"d">>).
-define(TYPE_TUPLE, <<"t">>).
-define(TUPLE_REF, <<"r">>).

%%%=====================================================================
%%% API
%%%=====================================================================

compile(Input) ->
    ?ADAPTER:compile(Input).

bind(Bindings, State) ->
    ?ADAPTER:bind(Bindings, State).

vars(State) ->
    ?ADAPTER:vars(State).

render(State) ->
    maps:values(tree(State)).

tree(State) ->
    ?ADAPTER:tree(State).

diff(State) ->
    case ?ADAPTER:diff(State) of
        ChangedParts when map_size(ChangedParts) > 0 ->
            {ok, ChangedParts};
        #{} ->
            none
    end.

types(Bindings, RenderState) ->
    lists:foldl(fun({Var, Index}, Acc) ->
        case maps:find(Var, Bindings) of
            {ok, Val} ->
                Acc#{Index => typeof(Val)};
            error ->
                Acc
        end
    end, #{}, vars(RenderState)).

cast(Tree, Types, RenderState) ->
    lists:foldl(fun({Var, Index0}, Acc) ->
        Index = integer_to_binary(Index0),
        Type = maps:get(Index, Types),
        Acc#{Var => totype(Type, maps:get(Index, Tree))}
    end, #{}, vars(RenderState)).

%%%=====================================================================
%%% Internal functions
%%%=====================================================================

typeof(X) when is_binary(X) -> ?TYPE_BINARY;
typeof(X) when is_atom(X) -> ?TYPE_ATOM;
typeof(X) when is_list(X) -> ?TYPE_LIST;
typeof(X) when is_map(X) -> ?TYPE_MAP;
typeof(X) when is_boolean(X) -> ?TYPE_BOOLEAN;
typeof(X) when is_integer(X) -> ?TYPE_INTEGER;
typeof(X) when is_float(X) -> ?TYPE_FLOAT;
typeof(X) when is_pid(X) -> ?TYPE_PID;
typeof(X) when is_port(X) -> ?TYPE_PORT;
typeof(X) when is_tuple(X) -> ?TYPE_TUPLE;
typeof(X) when is_reference(X) -> ?TUPLE_REF.

totype(?TYPE_BINARY, X) -> X;
totype(?TYPE_ATOM, X) -> binary_to_existing_atom(X);
totype(?TYPE_LIST, X) -> arizona_json:decode(X);
totype(?TYPE_MAP, X) -> arizona_json:decode(X);
totype(?TYPE_BOOLEAN, <<"true">>) -> true;
totype(?TYPE_BOOLEAN, <<"false">>) -> false;
totype(?TYPE_INTEGER, X) -> binary_to_integer(X);
totype(?TYPE_FLOAT, X) -> io_lib:format("~p", [X]);
totype(?TYPE_PID, X) -> list_to_pid(binary_to_list(X));
totype(?TYPE_PORT, X) -> list_to_port(binary_to_list(X));
totype(?TYPE_TUPLE, X) -> list_to_tuple(arizona_json:decode(X));
totype(?TUPLE_REF, X) -> list_to_ref(binary_to_list(X)).

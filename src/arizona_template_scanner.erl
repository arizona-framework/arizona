%%
%% %CopyrightBegin%
%%
%% Copyright 2024 Arizona Framework <contact@arizonaframe.work>
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
-module(arizona_template_scanner).
-moduledoc false.

%% API functions.
-export([scan/2, new_anno/1]).

%% Types
-export_type([anno/0, anno_options/0]).

% Module and function could be undefined in favor of files scanning.
-opaque anno() :: #{
    module => module() | undefined,
    function => atom() | undefined,
    file => binary(),
    line => non_neg_integer(),
    column => non_neg_integer(),
    first_column => non_neg_integer(),
    position => non_neg_integer()
}.

-type anno_options() :: #{
    module := module(),
    function := atom(),
    file := string() | binary(),
    line := non_neg_integer(),
    column := non_neg_integer(),
    first_column := non_neg_integer(),
    position := non_neg_integer()
}.

%% --------------------------------------------------------------------
%% API funtions.
%% --------------------------------------------------------------------

scan(Bin, Anno) when is_binary(Bin), is_map(Anno) ->
    scan(Bin, _Len = 0, Anno).

new_anno(Opts) when is_map(Opts) ->
    Anno = maps:merge(default_anno(), Opts),
    maps:map(fun normalize_anno_value/2, Anno).

%% --------------------------------------------------------------------
%% Internal funtions.
%% --------------------------------------------------------------------

scan(_Bin, _Len, _Anno) ->
    error(not_implemented_yet).

default_anno() ->
    #{
        module => undefined,
        function => undefined,
        file => undefined,
        line => 1,
        column => 1,
        first_column => 1,
        position => 0
    }.

normalize_anno_value(module, Mod) when is_atom(Mod) ->
    Mod;
normalize_anno_value(function, Fun) when is_atom(Fun) ->
    Fun;
normalize_anno_value(file, File) when is_binary(File) ->
    File;
normalize_anno_value(file, File) when is_list(File) ->
    iolist_to_binary(File);
normalize_anno_value(line, Ln) when is_integer(Ln), Ln >= 0 ->
    Ln;
normalize_anno_value(column, Col) when is_integer(Col), Col >= 0 ->
    Col;
normalize_anno_value(first_column, Col) when is_integer(Col), Col >= 0 ->
    Col;
normalize_anno_value(position, Pos) when is_integer(Pos), Pos >= 0 ->
    Pos.

%% --------------------------------------------------------------------
%% EUnit tests.
%% --------------------------------------------------------------------

-ifdef(TEST).
-include_lib("eunit/include/eunit.hrl").

new_anno_test() ->
    [
        ?assertError(function_clause, new_anno(#{})),
        ?assertError(function_clause, new_anno(#{module => -1})),
        ?assertError(function_clause, new_anno(#{function => -1})),
        ?assertError(function_clause, new_anno(#{file => -1})),
        ?assertError(function_clause, new_anno(#{line => -1})),
        ?assertError(function_clause, new_anno(#{column => -1})),
        ?assertError(function_clause, new_anno(#{first_column => -1})),
        ?assertError(function_clause, new_anno(#{position => -10})),
        ?assertEqual(#{
            module => undefined,
            function => undefined,
            file => iolist_to_binary(?FILE),
            line => 1,
            column => 1,
            first_column => 1,
            position => 0
        }, new_anno(#{file => ?FILE}))
    ].

-endif.


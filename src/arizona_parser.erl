-module(arizona_parser).
-moduledoc ~"""
Template parser for Arizona Web Framework.

This module converts tokenized Arizona templates into structured data
for both stateless and stateful rendering. It processes tokens generated
by arizona_scanner and creates the appropriate data structures for template
rendering engines.

For stateless rendering: Returns a list of tokens with line numbers preserved.
For stateful rendering: Returns a map with element tracking and variable indexing.

The parser handles variable extraction from dynamic expressions and maintains
indexes for efficient template updates in stateful rendering mode.
""".

%% --------------------------------------------------------------------
%% API function exports
%% --------------------------------------------------------------------

-export([parse_stateless_tokens/1]).
-export([parse_stateful_tokens/1]).
-export([parse_list_tokens/1]).

%% --------------------------------------------------------------------
%% Types (and their exports)
%% --------------------------------------------------------------------

-doc ~"Token representation with category, line number, and content.".
-type token() :: {
    Category :: static | dynamic | comment, Line :: pos_integer(), Content :: binary()
}.
-export_type([token/0]).

-doc ~"Result type for stateless parsing - list of tokens with comments filtered out.".
-type stateless_result() :: [Token :: token()].
-export_type([stateless_result/0]).

-doc ~"""
Result type for stateful parsing with element ordering, element mapping, and variable indexes.
""".
-type stateful_result() :: #{
    elems_order := [Index :: non_neg_integer()],
    elems := #{
        Index ::
            non_neg_integer() => {
                Category :: static | dynamic, Line :: pos_integer(), Content :: binary()
            }
    },
    vars_indexes := #{VarName :: binary() => [Index :: non_neg_integer()]}
}.
-export_type([stateful_result/0]).

-doc ~"""
Result type for list parsing with static/dynamic template structure.
""".
-type list_result() :: #{
    static := [StaticContent :: binary()],
    dynamic := #{
        elems_order := [Index :: non_neg_integer()],
        elems := #{
            Index ::
                non_neg_integer() => {
                    Line :: pos_integer(),
                    ElementFun :: fun((Item :: term(), Socket :: arizona_socket:socket()) -> term())
                }
        },
        vars_indexes := #{VarName :: atom() => [Index :: non_neg_integer()]}
    }
}.
-export_type([list_result/0]).

%% --------------------------------------------------------------------
%% API Functions
%% --------------------------------------------------------------------

-doc ~"""
Parse tokens into stateless iolist structure.

Converts a list of tokens into a structure suitable for stateless rendering.
Filters out comment tokens while preserving static and dynamic tokens with
their line numbers intact.

Returns a list of tokens that can be directly processed by template renderers.
""".
-spec parse_stateless_tokens([token()]) -> stateless_result().
parse_stateless_tokens(Tokens) ->
    [Token || {Category, _Line, _Text} = Token <- Tokens, Category =/= comment].

-doc ~"""
Parse tokens into stateful template structure.

Converts a list of tokens into a structured format for stateful rendering.
This includes element ordering, element mapping, and variable indexing for
efficient template updates.

The resulting map contains:
- `elems_order`: Sequential list of element indices
- `elems`: Map of element index to token data
- `vars_indexes`: Map of variable names to their element indices

Variable extraction is performed for dynamic tokens containing
`arizona_socket:get_binding/2` calls.
""".
-spec parse_stateful_tokens([token()]) -> stateful_result().
parse_stateful_tokens(Tokens) ->
    {Elements, VarsIndexes} = process_tokens_stateful(Tokens, 0, #{}, #{}),
    #{
        elems_order => lists:seq(0, maps:size(Elements) - 1),
        elems => Elements,
        vars_indexes => VarsIndexes
    }.

-doc ~"""
Parse tokens into list template structure.

For now, this is a simple placeholder that will be enhanced by parse transform.
The structure separates static HTML parts from dynamic expressions for efficient
list rendering with minimal re-computation.
""".
-spec parse_list_tokens([token()]) -> list_result().
parse_list_tokens(Tokens) ->
    %% For runtime: simple fallback structure
    %% Parse transform will provide optimized version
    {StaticParts, DynamicElements, VarsIndexes} = process_tokens_for_list(Tokens),
    #{
        static => StaticParts,
        dynamic => #{
            elems_order => lists:seq(0, maps:size(DynamicElements) - 1),
            elems => DynamicElements,
            vars_indexes => VarsIndexes
        }
    }.

%% --------------------------------------------------------------------
%% Internal Functions
%% --------------------------------------------------------------------

%% Process tokens for stateful structure
process_tokens_stateful([], _Index, Elements, VarsIndexes) ->
    {Elements, VarsIndexes};
process_tokens_stateful([Token | Rest], Index, Elements, VarsIndexes) ->
    case Token of
        {static, Line, Text} ->
            NewElements = Elements#{Index => {static, Line, Text}},
            process_tokens_stateful(Rest, Index + 1, NewElements, VarsIndexes);
        {dynamic, Line, ExprText} ->
            %% Parse expression to find variable names
            VarNames = extract_variable_names(ExprText),

            %% Create function for expression
            NewElements = Elements#{Index => {dynamic, Line, ExprText}},

            %% Update variable indexes
            NewVarsIndexes = lists:foldl(
                fun(VarName, Acc) ->
                    CurrentIndexes = maps:get(VarName, Acc, []),
                    Acc#{VarName => [Index | CurrentIndexes]}
                end,
                VarsIndexes,
                VarNames
            ),

            process_tokens_stateful(Rest, Index + 1, NewElements, NewVarsIndexes);
        {comment, _Line, _Text} ->
            %% Skip comments (don't increment index)
            process_tokens_stateful(Rest, Index, Elements, VarsIndexes)
    end.

%% Extract variable names from expression text using regex
extract_variable_names(ExprText) ->
    expr_vars(ExprText).

%% Parse expression to find arizona_socket:get_binding calls
expr_vars(Expr) ->
    case
        re:run(
            Expr,
            ~"arizona_socket:get_binding\\(([a-z][a-zA-Z_@]*|'(.*?)'),\\s*\\w+(?:,\\s*[^)]*)?\\)",
            [global, {capture, all_but_first, binary}]
        )
    of
        {match, Vars0} ->
            Vars = lists:flatten([pick_quoted_var(List) || List <- Vars0]),
            lists:usort(Vars);
        nomatch ->
            []
    end.

%% Handle quoted and unquoted variable names
pick_quoted_var([<<$', _/binary>> = Var | _T]) ->
    %% Remove quotes from quoted variable
    Size = byte_size(Var) - 2,
    <<$', UnquotedVar:Size/binary, $'>> = Var,
    UnquotedVar;
pick_quoted_var([Var]) ->
    Var.

%% Process tokens for list template structure (similar to stateful but different output)
process_tokens_for_list(Tokens) ->
    %% For runtime fallback: create simple structure
    %% Parse transform will optimize this
    {StaticParts, DynamicElements, VarsIndexes} = separate_static_dynamic_for_list(
        Tokens, [], #{}, #{}, 0
    ),
    {StaticParts, DynamicElements, VarsIndexes}.

%% Separate static and dynamic parts for list rendering
separate_static_dynamic_for_list([], StaticAcc, DynamicAcc, VarsAcc, _Index) ->
    {lists:reverse(StaticAcc), DynamicAcc, VarsAcc};
separate_static_dynamic_for_list(
    [{static, _Line, Text} | Rest], StaticAcc, DynamicAcc, VarsAcc, Index
) ->
    separate_static_dynamic_for_list(Rest, [Text | StaticAcc], DynamicAcc, VarsAcc, Index);
separate_static_dynamic_for_list(
    [{dynamic, Line, ExprText} | Rest], StaticAcc, DynamicAcc, VarsAcc, Index
) ->
    %% For list context, create a simple function that returns the expression text
    %% Parse transform will optimize this to proper item field access
    Fun = fun(_Item, _Socket) -> ExprText end,

    %% Extract variables (only arizona_socket:get_binding calls)
    VarNames = extract_variable_names(ExprText),

    %% Update accumulators with line information
    NewDynamicAcc = DynamicAcc#{Index => {Line, Fun}},
    NewVarsAcc = lists:foldl(
        fun(VarName, Acc) ->
            CurrentIndexes = maps:get(VarName, Acc, []),
            Acc#{VarName => [Index | CurrentIndexes]}
        end,
        VarsAcc,
        VarNames
    ),

    %% Add empty static part to maintain structure
    separate_static_dynamic_for_list(
        Rest, [<<>> | StaticAcc], NewDynamicAcc, NewVarsAcc, Index + 1
    );
separate_static_dynamic_for_list(
    [{comment, _Line, _Text} | Rest], StaticAcc, DynamicAcc, VarsAcc, Index
) ->
    %% Skip comments
    separate_static_dynamic_for_list(Rest, StaticAcc, DynamicAcc, VarsAcc, Index).

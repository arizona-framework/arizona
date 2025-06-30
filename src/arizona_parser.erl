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

%% --------------------------------------------------------------------
%% Types (and their exports)
%% --------------------------------------------------------------------

-doc ~"Token representation with category, line number, and content.".
-type token() :: {static | dynamic | comment, pos_integer(), binary()}.
-export_type([token/0]).

-doc ~"Result type for stateless parsing - list of tokens with comments filtered out.".
-type stateless_result() :: [token()].
-export_type([stateless_result/0]).

-doc ~"""
Result type for stateful parsing with element ordering, element mapping, and variable indexes.
""".
-type stateful_result() :: #{
    elems_order := [non_neg_integer()],
    elems := #{non_neg_integer() => {static | dynamic, pos_integer(), binary()}},
    vars_indexes := #{binary() => [non_neg_integer()]}
}.
-export_type([stateful_result/0]).

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
`arizona_template:get_binding/2` calls.
""".
-spec parse_stateful_tokens([token()]) -> stateful_result().
parse_stateful_tokens(Tokens) ->
    {Elements, VarsIndexes} = process_tokens_stateful(Tokens, 0, #{}, #{}),
    #{
        elems_order => lists:seq(0, maps:size(Elements) - 1),
        elems => Elements,
        vars_indexes => VarsIndexes
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

%% Parse expression to find arizona_template:get_binding calls
expr_vars(Expr) ->
    case
        re:run(
            Expr,
            ~"arizona_template:get_binding\\(([a-z][a-zA-Z_@]*|'(.*?)'),\\s*\\w+(?:,\\s*[^)]*)?\\)",
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

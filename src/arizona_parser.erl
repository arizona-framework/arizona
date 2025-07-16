-module(arizona_parser).
-moduledoc ~"""
Template parser for Arizona Web Framework.

Converts tokenized Arizona templates into AST that creates optimized template structures.
Processes tokens from `arizona_scanner` and generates compile-time AST for efficient rendering.

## AST Generation

Returns AST that creates `arizona_template:template()` tuples:

```erlang
{template, Static, Dynamic, DynamicSequence, DynamicAnno}
```

Where:
- **Static**: List of UTF-8 binaries in template order
- **Dynamic**: Tuple of callback functions `fun(_@Bindings) -> binary() end`
- **DynamicSequence**: `[1,2,3,...,N]` for efficient tuple traversal
- **DynamicAnno**: Tuple of line numbers for debugging

## Features

- **Compile-time AST**: Generates optimized syntax trees for performance
- **Tuple Structure**: High-performance data layout with precomputed sequences  
- **Callback Functions**: Dynamic expressions as first-class functions
- **Line Tracking**: Preserves source locations for debugging
- **Comment Filtering**: Removes comment tokens during parsing

## Processing Pipeline

1. **Input**: Token stream from `arizona_scanner`
2. **Separation**: Static content and dynamic expressions
3. **AST Creation**: Compile-time syntax trees for template structures
4. **Output**: AST that evaluates to `arizona_template:template()` instances

Access template data through `arizona_template` module functions.
""".

%% --------------------------------------------------------------------
%% API function exports
%% --------------------------------------------------------------------

-export([parse_tokens/1]).

%% --------------------------------------------------------------------
%% Types exports
%% --------------------------------------------------------------------

-export_type([parsed_template/0]).

%% --------------------------------------------------------------------
%% Types definitions
%% --------------------------------------------------------------------

-doc ~"""
Template parsing result as AST that creates arizona_template:template() record.

Returns compile-time AST that will create #template{} instances at runtime.
Static parts are binary segments in template order.
Dynamic parts become callback functions in tuple format.
""".
-type parsed_template() :: erl_syntax:syntaxTree().

%% --------------------------------------------------------------------
%% API Functions
%% --------------------------------------------------------------------

-doc ~"""
Parse tokens into template AST.

Converts tokens into AST that creates arizona_template:template() record.
Static parts are binary segments, dynamic parts become callback functions.

Returns AST that creates #template{} record at runtime.
""".
-spec parse_tokens(Tokens) -> ParsedTemplate when
    Tokens :: [arizona_scanner:token()],
    ParsedTemplate :: parsed_template().
parse_tokens(Tokens) ->
    {StaticParts, DynamicElements} = separate_static_dynamic(Tokens),
    create_template_ast(StaticParts, DynamicElements).

%% Create AST that builds arizona_template:template() record
-spec create_template_ast(StaticParts, DynamicElements) -> erl_syntax:syntaxTree() when
    StaticParts :: [binary()],
    DynamicElements :: [{pos_integer(), binary()}].
create_template_ast(StaticParts, DynamicElements) ->
    % Create static list AST
    StaticListAST = create_static_list_ast(StaticParts),

    % Convert dynamic elements to callback functions and create tuple AST
    {DynamicTupleAST, DynamicAnnoAST, DynamicSequenceAST} = create_dynamic_ast(DynamicElements),

    % Create tuple AST: {template, Static, Dynamic, DynamicSequence, DynamicAnno}
    erl_syntax:tuple([
        erl_syntax:atom(template),
        StaticListAST,
        DynamicTupleAST,
        DynamicSequenceAST,
        DynamicAnnoAST
    ]).

%% Create AST for static list
-spec create_static_list_ast(StaticParts) -> erl_syntax:syntaxTree() when
    StaticParts :: [binary()].
create_static_list_ast(StaticParts) ->
    StaticElements = [
        erl_syntax:binary([
            erl_syntax:binary_field(
                erl_syntax:string(binary_to_list(Part)),
                none,
                [erl_syntax:atom(utf8)]
            )
        ])
     || Part <- StaticParts
    ],
    erl_syntax:list(StaticElements).

%% Create AST for dynamic elements (tuple, annotations, sequence)
-spec create_dynamic_ast(DynamicElements) ->
    {erl_syntax:syntaxTree(), erl_syntax:syntaxTree(), erl_syntax:syntaxTree()}
when
    DynamicElements :: [{pos_integer(), binary()}].
create_dynamic_ast([]) ->
    % Empty case: empty tuple, empty tuple, empty sequence
    EmptyTuple = erl_syntax:tuple([]),
    EmptySequence = erl_syntax:list([]),
    {EmptyTuple, EmptyTuple, EmptySequence};
create_dynamic_ast(DynamicElements) ->
    % Create callback functions and extract line numbers
    {CallbackFuns, LineNumbers} = lists:unzip([
        create_dynamic_callback_ast(Line, ExprText)
     || {Line, ExprText} <- DynamicElements
    ]),

    % Create tuples and sequence
    DynamicTuple = erl_syntax:tuple(CallbackFuns),
    DynamicAnno = erl_syntax:tuple([erl_syntax:integer(Line) || Line <- LineNumbers]),
    DynamicSequence = erl_syntax:list([
        erl_syntax:integer(N)
     || N <- lists:seq(1, length(DynamicElements))
    ]),

    {DynamicTuple, DynamicAnno, DynamicSequence}.

%% Create callback function AST for dynamic element
-spec create_dynamic_callback_ast(Line, ExprText) ->
    {erl_syntax:syntaxTree(), pos_integer()}
when
    Line :: pos_integer(),
    ExprText :: binary().
create_dynamic_callback_ast(Line, ExprText) ->
    % Create fun(_@Bindings) -> ExprText end
    % For now, simple approach - return the expression as binary
    % TODO: Parse ExprText into proper AST expressions

    BindingsVar = erl_syntax:variable('_@Bindings'),
    ExprBody = erl_syntax:binary([
        erl_syntax:binary_field(
            erl_syntax:string(binary_to_list(ExprText)),
            none,
            [erl_syntax:atom(utf8)]
        )
    ]),

    CallbackFun = erl_syntax:fun_expr([
        erl_syntax:clause([BindingsVar], none, [ExprBody])
    ]),

    {CallbackFun, Line}.

%% --------------------------------------------------------------------
%% Private functions
%% --------------------------------------------------------------------

%% Separate static and dynamic parts
separate_static_dynamic(Tokens) ->
    separate_static_dynamic(Tokens, [], [], undefined).

separate_static_dynamic([], StaticAcc, DynamicAcc, _PrevType) ->
    {lists:reverse(StaticAcc), lists:reverse(DynamicAcc)};
separate_static_dynamic(
    [{static, _Line, Text} | Rest], StaticAcc, DynamicAcc, _PrevType
) ->
    separate_static_dynamic(Rest, [Text | StaticAcc], DynamicAcc, static);
separate_static_dynamic(
    [{dynamic, Line, ExprText} | Rest], StaticAcc, DynamicAcc, PrevType
) ->
    %% Store dynamic element - parse transform will handle variable analysis
    NewDynamicAcc = [{Line, ExprText} | DynamicAcc],

    %% Add empty static part when:
    %% 1. First token is dynamic (PrevType == undefined)
    %% 2. Previous token was also dynamic (PrevType == dynamic)
    NewStaticAcc =
        case PrevType of
            % First token is dynamic
            undefined -> [~"" | StaticAcc];
            % Consecutive dynamics
            dynamic -> [~"" | StaticAcc];
            % After static, no empty needed
            static -> StaticAcc
        end,

    separate_static_dynamic(Rest, NewStaticAcc, NewDynamicAcc, dynamic);
separate_static_dynamic(
    [{comment, _Line, _Text} | Rest], StaticAcc, DynamicAcc, PrevType
) ->
    %% Skip comments - preserve previous type
    separate_static_dynamic(Rest, StaticAcc, DynamicAcc, PrevType).

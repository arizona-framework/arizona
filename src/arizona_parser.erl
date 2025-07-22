-module(arizona_parser).

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

-type parsed_template() :: erl_syntax:syntaxTree().

%% --------------------------------------------------------------------
%% API Functions
%% --------------------------------------------------------------------

-spec parse_tokens([Token]) -> ParsedTemplate when
    Token :: arizona_token:token(),
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
    {DynamicAST, DynamicAnnoAST, DynamicSequenceAST} = create_dynamic_ast(DynamicElements),

    % Create tuple AST: {template, Static, Dynamic, DynamicSequence, DynamicAnno}
    erl_syntax:tuple([
        erl_syntax:atom(template),
        StaticListAST,
        DynamicAST,
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
    {LineNumbers, CallbackFuns} = lists:unzip([
        try
            {Line, create_dynamic_callback_ast(ExprText)}
        catch
            Class:Reason:Stacktrace ->
                error(
                    arizona_create_dynamic_callback_failed,
                    none,
                    error_info({Line, ExprText, Class, Reason, Stacktrace})
                )
        end
     || {Line, ExprText} <- DynamicElements
    ]),

    % Create tuples and sequence
    Dynamic = erl_syntax:tuple(CallbackFuns),
    DynamicAnno = erl_syntax:tuple([erl_syntax:integer(Line) || Line <- LineNumbers]),
    DynamicSequence = erl_syntax:list([
        erl_syntax:integer(N)
     || N <- lists:seq(1, length(DynamicElements))
    ]),

    {Dynamic, DynamicAnno, DynamicSequence}.

%% Create callback function AST for dynamic element
-spec create_dynamic_callback_ast(ExprText) -> erl_syntax:syntaxTree() when
    ExprText :: binary().
create_dynamic_callback_ast(ExprText) ->
    erl_syntax:fun_expr([
        erl_syntax:clause([], none, [merl:quote(ExprText)])
    ]).

%% --------------------------------------------------------------------
%% Internal functions
%% --------------------------------------------------------------------

%% Separate static and dynamic parts
separate_static_dynamic(Tokens) ->
    separate_static_dynamic(Tokens, [], [], undefined).

separate_static_dynamic([], StaticAcc, DynamicAcc, _PrevType) ->
    {lists:reverse(StaticAcc), lists:reverse(DynamicAcc)};
separate_static_dynamic([Token | Rest], StaticAcc, DynamicAcc, PrevType) ->
    case arizona_token:get_category(Token) of
        static ->
            Text = arizona_token:get_content(Token),
            separate_static_dynamic(Rest, [Text | StaticAcc], DynamicAcc, static);
        dynamic ->
            Line = arizona_token:get_line(Token),
            ExprText = arizona_token:get_content(Token),

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
        comment ->
            %% Skip comments - preserve previous type
            separate_static_dynamic(Rest, StaticAcc, DynamicAcc, PrevType)
    end.

%% Create error_info for proper compiler diagnostics with enhanced details
-spec error_info(Cause) -> ErrorInfo when
    Cause :: term(),
    ErrorInfo :: [{error_info, map()}].
error_info(Cause) ->
    [
        {error_info, #{
            cause => Cause,
            module => ?MODULE
        }}
    ].

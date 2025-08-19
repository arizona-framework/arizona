-module(arizona_parser).

%% --------------------------------------------------------------------
%% API function exports
%% --------------------------------------------------------------------

-export([parse_tokens/3]).
-export([format_error/2]).

%% --------------------------------------------------------------------
%% Ignore xref warnings
%% --------------------------------------------------------------------

-ignore_xref([format_error/2]).

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

-spec parse_tokens(Tokens, CallbackArg, CompileOpts) -> ParsedTemplate when
    Tokens :: [arizona_token:token()],
    CallbackArg :: erl_syntax:syntaxTree(),
    CompileOpts :: [compile:option()],
    ParsedTemplate :: parsed_template().
parse_tokens(Tokens, CallbackArg, CompileOpts) ->
    {StaticParts, DynamicElements} = separate_static_dynamic(Tokens),
    create_template_ast(StaticParts, DynamicElements, CallbackArg, CompileOpts).

-spec format_error(Reason, StackTrace) -> ErrorMap when
    Reason :: arizona_create_dynamic_callback_failed | term(),
    StackTrace :: erlang:stacktrace(),
    ErrorMap :: #{general => string(), reason => io_lib:chars()}.
format_error(arizona_create_dynamic_callback_failed, [{_M, _F, _As, Info} | _]) ->
    {error_info, ErrorInfo} = proplists:lookup(error_info, Info),
    {Line, ExprText, Class, Reason, StackTrace} = maps:get(cause, ErrorInfo),
    #{
        general => "Arizona template dynamic callback creation failed",
        reason => io_lib:format(
            "Failed to create callback for expression '~s' at line ~p:\n~p:~p:~p", [
                ExprText, Line, Class, Reason, StackTrace
            ]
        )
    }.

%% --------------------------------------------------------------------
%% Internal functions
%% --------------------------------------------------------------------

%% Create AST that builds arizona_template:template() record
-spec create_template_ast(StaticParts, DynamicElements, CallbackArg, CompileOpts) -> Ast when
    StaticParts :: [binary()],
    DynamicElements :: [{pos_integer(), binary()}],
    CallbackArg :: erl_syntax:syntaxTree(),
    CompileOpts :: [compile:option()],
    Ast :: erl_syntax:syntaxTree().
create_template_ast(StaticParts, DynamicElements, CallbackArg, CompileOpts) ->
    % Create static list AST
    StaticListAST = create_static_list_ast(StaticParts),

    % Convert dynamic elements to callback functions and create tuple AST
    {DynamicAST, DynamicAnnoAST, DynamicSequenceAST} = create_dynamic_ast(
        DynamicElements, CallbackArg, CompileOpts
    ),

    % Generate fingerprint based on template structure and callback context
    FingerprintData = {StaticParts, DynamicElements, erl_syntax:revert(CallbackArg)},
    Fingerprint = erlang:phash2(FingerprintData),
    FingerprintAST = erl_syntax:integer(Fingerprint),

    % Create tuple AST: {template, Static, Dynamic, DynamicSequence, DynamicAnno, Fingerprint}
    erl_syntax:tuple([
        erl_syntax:atom(template),
        StaticListAST,
        DynamicAST,
        DynamicSequenceAST,
        DynamicAnnoAST,
        FingerprintAST
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
-spec create_dynamic_ast(DynamicElements, CallbackArg, CompileOpts) -> Result when
    DynamicElements :: [{pos_integer(), binary()}],
    CallbackArg :: erl_syntax:syntaxTree(),
    CompileOpts :: [compile:option()],
    Result :: {erl_syntax:syntaxTree(), erl_syntax:syntaxTree(), erl_syntax:syntaxTree()}.
create_dynamic_ast([], _CallbackArg, _CompileOpts) ->
    % Empty case: empty tuple, empty tuple, empty sequence
    EmptyTuple = erl_syntax:tuple([]),
    EmptySequence = erl_syntax:list([]),
    {EmptyTuple, EmptyTuple, EmptySequence};
create_dynamic_ast(DynamicElements, CallbackArg, CompileOpts) ->
    % Create callback functions and extract line numbers
    {LineNumbers, CallbackFuns} = lists:unzip([
        try
            {Line, create_dynamic_callback_ast(CallbackArg, ExprText, CompileOpts)}
        catch
            Class:Reason:StackTrace ->
                error(
                    arizona_create_dynamic_callback_failed,
                    none,
                    error_info({Line, ExprText, Class, Reason, StackTrace})
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
-spec create_dynamic_callback_ast(CallbackArg, ExprText, CompileOpts) -> Ast when
    CallbackArg :: erl_syntax:syntaxTree(),
    ExprText :: binary(),
    CompileOpts :: [compile:option()],
    Ast :: erl_syntax:syntaxTree().
create_dynamic_callback_ast(CallbackArg, ExprText, CompileOpts) ->
    Forms =
        case merl:quote(ExprText) of
            F when is_list(F) -> F;
            F -> [F]
        end,
    % Check if we're already in a recursive parse transform to prevent infinite loops
    InRecursiveTransform = proplists:get_bool(in_dynamic_callback, CompileOpts),

    FinalExpr =
        case InRecursiveTransform of
            true ->
                % Already in recursive context - don't transform again
                Forms;
            false ->
                % Apply parse transform with recursion protection flag
                RecursionProtectedOpts = [{in_dynamic_callback, true} | CompileOpts],

                % Transform the expression to handle nested arizona_template calls
                arizona_parse_transform:parse_transform(Forms, CallbackArg, RecursionProtectedOpts)
        end,
    erl_syntax:fun_expr([erl_syntax:clause([CallbackArg], none, FinalExpr)]).

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

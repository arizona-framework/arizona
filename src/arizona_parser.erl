-module(arizona_parser).
-moduledoc ~"""
Template parsing for Arizona templates.

Converts tokenized templates into Erlang AST for compile-time optimization.
Transforms tokens from the scanner into compiled template representations
that generate efficient runtime code.

## Process

1. Separates tokens into static HTML and dynamic Erlang expressions
2. Creates AST that builds `arizona_template` records at compile time
3. Generates callback functions for dynamic content evaluation
4. Handles recursive template compilation safely

## Example

```erlang
1> Tokens = arizona_scanner:scan_string(1, ~"<h1>{Title}</h1>").
[{token, static, 1, ~"<h1>"}, {token, dynamic, 1, ~"Title"}, ...]
2> arizona_parser:parse_tokens(Tokens, []).
{call, {remote, {atom, arizona_template}, {atom, new}}, [...]}
```
""".

%% --------------------------------------------------------------------
%% API function exports
%% --------------------------------------------------------------------

-export([parse_tokens/2]).
-export([quote/1]).
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

-doc ~"""
Parses tokens into an AST that creates an arizona_template record.

Transforms the token list into optimized compile-time AST that generates
efficient template rendering code with callback functions for dynamic content.
""".
-spec parse_tokens(Tokens, CompileOpts) -> ParsedTemplate when
    Tokens :: [arizona_token:token()],
    CompileOpts :: [compile:option()],
    ParsedTemplate :: parsed_template().
parse_tokens(Tokens, CompileOpts) ->
    {StaticParts, DynamicElements} = separate_static_dynamic(Tokens),
    create_template_ast(StaticParts, DynamicElements, CompileOpts).

-doc ~"""
Parses Erlang expression text into AST using merl:quote/1.

Converts UTF-8 binary text containing Erlang expressions into syntax trees
for further processing. Always returns a list of forms for consistency.
""".
-spec quote(Text) -> Ast when
    Text :: binary(),
    Ast :: [erl_syntax:syntaxTree()].
quote(Text) ->
    % Convert UTF-8 binary to proper Unicode character list for merl:quote
    SafeText = unicode:characters_to_list(Text),
    case merl:quote(SafeText) of
        F when is_list(F) -> F;
        F -> [F]
    end.

-doc ~"""
Formats parser errors for compiler diagnostics.

Converts internal parser errors into human-readable messages with context
about failed dynamic callback creation or other parsing issues.
""".
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
-spec create_template_ast(StaticParts, DynamicElements, CompileOpts) -> Ast when
    StaticParts :: [binary()],
    DynamicElements :: [{pos_integer(), binary()}],
    CompileOpts :: [compile:option()],
    Ast :: erl_syntax:syntaxTree().
create_template_ast(StaticParts, DynamicElements, CompileOpts) ->
    % Create static list AST
    StaticListAST = create_static_list_ast(StaticParts),

    % Convert dynamic elements to callback functions and create tuple AST
    {DynamicAST, DynamicAnnoAST, DynamicSequenceAST} = create_dynamic_ast(
        DynamicElements, CompileOpts
    ),

    % Generate fingerprint based on template structure and callback context
    FingerprintData = {StaticParts, DynamicElements},
    Fingerprint = erlang:phash2(FingerprintData),
    FingerprintAST = erl_syntax:integer(Fingerprint),

    % Create arizona_template:new/5 call
    erl_syntax:application(
        erl_syntax:module_qualifier(
            erl_syntax:atom(arizona_template),
            erl_syntax:atom(new)
        ),
        [StaticListAST, DynamicAST, DynamicSequenceAST, DynamicAnnoAST, FingerprintAST]
    ).

%% Create AST for static list
-spec create_static_list_ast(StaticParts) -> erl_syntax:syntaxTree() when
    StaticParts :: [binary()].
create_static_list_ast(StaticParts) ->
    StaticElements = [erl_syntax:abstract(Part) || Part <- StaticParts],
    erl_syntax:list(StaticElements).

%% Create AST for dynamic elements (tuple, annotations, sequence)
-spec create_dynamic_ast(DynamicElements, CompileOpts) -> Result when
    DynamicElements :: [{pos_integer(), binary()}],
    CompileOpts :: [compile:option()],
    Result :: {erl_syntax:syntaxTree(), erl_syntax:syntaxTree(), erl_syntax:syntaxTree()}.
create_dynamic_ast([], _CompileOpts) ->
    % Empty case: empty tuple, empty tuple, empty sequence
    EmptyTuple = erl_syntax:tuple([]),
    EmptySequence = erl_syntax:list([]),
    {EmptyTuple, EmptyTuple, EmptySequence};
create_dynamic_ast(DynamicElements, CompileOpts) ->
    % Create callback functions and extract line numbers
    {LineNumbers, CallbackFuns} = lists:unzip([
        try
            {Line, create_dynamic_callback_ast(ExprText, CompileOpts)}
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
-spec create_dynamic_callback_ast(ExprText, CompileOpts) -> Ast when
    ExprText :: binary(),
    CompileOpts :: [compile:option()],
    Ast :: erl_syntax:syntaxTree().
create_dynamic_callback_ast(ExprText, CompileOpts) ->
    Forms = quote(ExprText),

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
                arizona_parse_transform:parse_transform(Forms, RecursionProtectedOpts)
        end,
    erl_syntax:fun_expr([erl_syntax:clause([], none, FinalExpr)]).

%% Separate static and dynamic parts
separate_static_dynamic(Tokens) ->
    separate_static_dynamic(Tokens, [], [], undefined).

separate_static_dynamic([], StaticAcc, DynamicAcc, _PrevType) ->
    {lists:reverse(StaticAcc), lists:reverse(DynamicAcc)};
separate_static_dynamic([Token | Rest], StaticAcc, DynamicAcc, PrevType) ->
    case arizona_token:get_category(Token) of
        static ->
            Text = arizona_token:get_content(Token),

            % Consolidate adjacent static parts when processing comment boundaries
            % When comments are skipped with PrevType = static preserved,
            % merge the current static text with the previous static text
            % to maintain proper static part boundaries
            NewStaticAcc =
                case PrevType of
                    static ->
                        % Consolidate: merge current text with previous static part
                        PrevText = hd(StaticAcc),
                        [<<PrevText/binary, Text/binary>> | tl(StaticAcc)];
                    _ ->
                        % First static or after dynamic: add as new static part
                        [Text | StaticAcc]
                end,

            separate_static_dynamic(Rest, NewStaticAcc, DynamicAcc, static);
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

%% --------------------------------------------------------------------
%% EUnit Tests
%% --------------------------------------------------------------------

-ifdef(TEST).
-include_lib("eunit/include/eunit.hrl").

separate_static_dynamic_test_() ->
    [
        {"Adjacent static parts should consolidate after comments",
            test_comment_boundary_consolidation()},
        {"Regular static/dynamic separation", test_normal_separation()}
    ].

test_comment_boundary_consolidation() ->
    % Create tokens: static, comment, static, dynamic, static
    StaticToken1 = arizona_token:new(static, 1, ~"<div>"),
    CommentToken = arizona_token:new(comment, 1, ~" comment "),
    StaticToken2 = arizona_token:new(static, 1, ~"<section>"),
    DynamicToken = arizona_token:new(dynamic, 1, ~"title"),
    StaticToken3 = arizona_token:new(static, 1, ~"</section></div>"),

    Tokens = [StaticToken1, CommentToken, StaticToken2, DynamicToken, StaticToken3],
    {Static, Dynamic} = separate_static_dynamic(Tokens),

    % Should consolidate first two static parts due to comment boundary
    % Result: ["<div><section>", "</section></div>"], not ["<div>", "<section>", "</section></div>"]
    [
        ?_assertEqual(2, length(Static)),
        ?_assertEqual(~"<div><section>", lists:nth(1, Static)),
        ?_assertEqual(~"</section></div>", lists:nth(2, Static)),
        ?_assertEqual(1, length(Dynamic))
    ].

test_normal_separation() ->
    % Create tokens: static, dynamic, static
    StaticToken1 = arizona_token:new(static, 1, ~"<h1>"),
    DynamicToken = arizona_token:new(dynamic, 1, ~"title"),
    StaticToken2 = arizona_token:new(static, 1, ~"</h1>"),

    Tokens = [StaticToken1, DynamicToken, StaticToken2],
    {Static, Dynamic} = separate_static_dynamic(Tokens),

    % Should NOT consolidate across dynamic boundary
    [
        ?_assertEqual(2, length(Static)),
        ?_assertEqual(~"<h1>", lists:nth(1, Static)),
        ?_assertEqual(~"</h1>", lists:nth(2, Static)),
        ?_assertEqual(1, length(Dynamic))
    ].

-endif.

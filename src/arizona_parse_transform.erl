-module(arizona_parse_transform).
-moduledoc ~""""
Compile-time AST transformation for Arizona templates.

Erlang parse transform that converts Arizona template function calls into
optimized compile-time AST. Transforms `arizona_template:from_html/1`,
`arizona_template:from_markdown/1`, and `arizona_template:render_list/2` calls
for maximum runtime performance.

## Transformations

- `arizona_template:from_html/1` → Compiled template record
- `arizona_template:from_markdown/1` → Compiled markdown template record
- `arizona_template:render_list/2` → Optimized list rendering with callbacks
- Prevents infinite recursion in nested template expressions
- Evaluates template strings at compile time for validation

## Usage

Add to module compile options:

```erlang
-compile([{parse_transform, arizona_parse_transform}]).

render() ->
    arizona_template:from_html(~"""
    <h1>{Title}</h1>
    """).
    %% → Becomes compile-time optimized template record
```

## Debug

Use debug profile to output transformed modules to `/tmp/`:

```bash
$ rebar3 as debug compile
```
"""".

%% --------------------------------------------------------------------
%% API function exports
%% --------------------------------------------------------------------

-export([parse_transform/2]).
-export([transform_render_list/5]).
-export([transform_render_map/5]).
-export([extract_callback_function_body/4]).
-export([format_error/2]).

%% --------------------------------------------------------------------
%% Ignore xref warnings
%% --------------------------------------------------------------------

-ignore_xref([parse_transform/2]).
-ignore_xref([extract_callback_function_body/4]).
-ignore_xref([format_error/2]).

%% --------------------------------------------------------------------
%% Ignore hank warnings
%% --------------------------------------------------------------------

-hank([{unnecessary_function_arguments, [output_forms_to_tmp/2]}]).

%% --------------------------------------------------------------------
%% Ignore elvis warnings
%% --------------------------------------------------------------------

% format_error/2 exceeds 30 lines
-elvis([{elvis_style, max_function_length, #{max_length => 40}}]).

%% --------------------------------------------------------------------
%% Parse Transform Implementation
%% --------------------------------------------------------------------

-doc ~"""
Main parse transform entry point called by the Erlang compiler.

Transforms all applicable Arizona template calls in the given forms,
returning the modified AST for compilation.
""".
-spec parse_transform(Forms, Options) -> Forms when
    Forms :: compile:forms(),
    Options :: [compile:option()].
parse_transform(Forms, Options) ->
    Module = extract_module(Forms),
    TransformedForms = [
        transform_form(FormTree, Module, Options)
     || FormTree <- Forms
    ],
    RevertedForms = erl_syntax:revert_forms(TransformedForms),

    % Output forms to Erlang module in /tmp folder if DEBUG defined
    ok = output_forms_to_tmp(Module, RevertedForms),

    RevertedForms.

%% Transform arizona_template:render_list/2 calls
-doc ~"""
Transforms `arizona_template:render_list/2` calls into optimized AST.

Extracts the template function body, compiles it into a template record,
and creates a call to `arizona_template:render_list_template/2`.
""".
-spec transform_render_list(Module, Line, FunArg, ListArg, CompileOpts) -> AST when
    Module :: module(),
    Line :: arizona_token:line(),
    FunArg :: erl_syntax:syntaxTree(),
    ListArg :: erl_syntax:syntaxTree(),
    CompileOpts :: [compile:option()],
    AST :: erl_syntax:syntaxTree().
transform_render_list(Module, Line, FunArg, ListArg, CompileOpts) ->
    try
        % Extract the item parameter and template string from the function
        TemplateAST = extract_callback_function_body(
            Module, Line, FunArg, CompileOpts
        ),

        % Create application: arizona_template:render_list_template(Template, List)
        TemplateModule = erl_syntax:atom(arizona_template),
        TemplateFunction = erl_syntax:atom(render_list_template),
        erl_syntax:application(
            erl_syntax:module_qualifier(TemplateModule, TemplateFunction), [
                TemplateAST, ListArg
            ]
        )
    catch
        Class:Reason:StackTrace ->
            error(
                arizona_render_list_transformation_failed,
                none,
                error_info({Module, Line, Class, Reason, StackTrace})
            )
    end.

-doc ~"""
Transforms render_map calls to optimized render_map_template calls.

Similar to render_list transformation but for map rendering.
Extracts the callback function and converts it to a compiled template.
""".
-spec transform_render_map(Module, Line, FunArg, MapArg, CompileOpts) -> AST when
    Module :: module(),
    Line :: arizona_token:line(),
    FunArg :: erl_syntax:syntaxTree(),
    MapArg :: erl_syntax:syntaxTree(),
    CompileOpts :: [compile:option()],
    AST :: erl_syntax:syntaxTree().
transform_render_map(Module, Line, FunArg, MapArg, CompileOpts) ->
    try
        % Extract the item parameter and template string from the function
        % Reuse the same extraction logic as render_list since both use fun(Item) -> Template
        TemplateAST = extract_callback_function_body(
            Module, Line, FunArg, CompileOpts
        ),

        % Create application: arizona_template:render_map_template(Template, Map)
        TemplateModule = erl_syntax:atom(arizona_template),
        TemplateFunction = erl_syntax:atom(render_map_template),
        erl_syntax:application(
            erl_syntax:module_qualifier(TemplateModule, TemplateFunction), [
                TemplateAST, MapArg
            ]
        )
    catch
        Class:Reason:StackTrace ->
            error(
                arizona_render_map_transformation_failed,
                none,
                error_info({Module, Line, Class, Reason, StackTrace})
            )
    end.

%% Extract item parameter and template string from callback function
-doc ~"""
Extracts template content from render callback functions (render_list/render_map).

Analyzes the function expression to find `arizona_template:from_html/1`
calls, extracts the template string, and compiles it into optimized AST.
Generic function that works for both render_list and render_map since both
use the same callback pattern: fun(Item) -> Template.
""".
-spec extract_callback_function_body(Module, Line, FunExpr, CompileOpts) -> TemplateAST when
    Module :: module(),
    Line :: arizona_token:line(),
    FunExpr :: erl_syntax:syntaxTree(),
    CompileOpts :: [compile:option()],
    TemplateAST :: erl_syntax:syntaxTree().
extract_callback_function_body(Module, Line, FunExpr, CompileOpts) ->
    case erl_syntax:type(FunExpr) of
        fun_expr ->
            [Clause] = erl_syntax:fun_expr_clauses(FunExpr),
            [CallbackArg] = erl_syntax:clause_patterns(Clause),
            ClauseBody = erl_syntax:clause_body(Clause),
            % Find the arizona_template:from_html call in the function body
            % It might be the only statement or the last statement after variable bindings
            [TemplateCall | RevClauseBody] = lists:reverse(ClauseBody),

            % Extract template string from raw arizona_template:from_html call
            case analyze_application(TemplateCall) of
                {arizona_template, from_html, 1, [TemplateArg]} ->
                    TemplateString = eval_expr(Module, TemplateArg),

                    % Scan template content into tokens
                    Tokens = arizona_scanner:scan_string(Line + 1, TemplateString),

                    % Clear in_dynamic_callback flag for render_list templates to allow
                    % nested transforms
                    ClearedOpts = proplists:delete(in_dynamic_callback, CompileOpts),
                    TemplateAST = arizona_parser:parse_tokens(Tokens, ClearedOpts),

                    % Wrap the dynamic tuple in fun(Item) -> ... end for render_list
                    wrap_dynamic_tuple_in_function(TemplateAST, RevClauseBody, CallbackArg);
                TemplateCall ->
                    % analyze_application returned the Node itself, check if it's a template tuple
                    maybe
                        tuple ?= erl_syntax:type(TemplateCall),
                        [FirstElement | _] ?= erl_syntax:tuple_elements(TemplateCall),
                        atom ?= erl_syntax:type(FirstElement),
                        template ?= erl_syntax:atom_value(FirstElement),
                        TemplateCall
                    else
                        _ ->
                            error({not_from_html_call, TemplateCall})
                    end
            end;
        _ ->
            error({invalid_function_expression, FunExpr})
    end.

-doc ~"""
Formats parse transform errors for compiler diagnostics.

Converts internal transformation errors into human-readable messages
with context about failed template extraction or render_list/render_map transformation.
""".
-spec format_error(Reason, StackTrace) -> ErrorMap when
    Reason ::
        arizona_template_extraction_failed
        | arizona_markdown_extraction_failed
        | arizona_render_list_transformation_failed
        | arizona_render_map_transformation_failed,
    StackTrace :: erlang:stacktrace(),
    ErrorMap :: #{general => string(), reason => io_lib:chars()}.
format_error(arizona_template_extraction_failed, [{_M, _F, _As, Info} | _]) ->
    {error_info, ErrorInfo} = proplists:lookup(error_info, Info),
    {Module, Line, Class, Reason, StackTrace} = maps:get(cause, ErrorInfo),
    #{
        general => "Arizona template parsing failed",
        reason => io_lib:format("Failed to extract template in ~p at line ~p:\n~p:~p:~p", [
            Module, Line, Class, Reason, StackTrace
        ])
    };
format_error(arizona_markdown_extraction_failed, [{_M, _F, _As, Info} | _]) ->
    {error_info, ErrorInfo} = proplists:lookup(error_info, Info),
    {Module, Line, Class, Reason, StackTrace} = maps:get(cause, ErrorInfo),
    #{
        general => "Arizona markdown template parsing failed",
        reason => io_lib:format("Failed to extract markdown template in ~p at line ~p:\n~p:~p:~p", [
            Module, Line, Class, Reason, StackTrace
        ])
    };
format_error(arizona_render_list_transformation_failed, [{_M, _F, _As, Info} | _]) ->
    {error_info, ErrorInfo} = proplists:lookup(error_info, Info),
    {Module, Line, Class, Reason, StackTrace} = maps:get(cause, ErrorInfo),
    #{
        general => "Arizona render_list transformation failed",
        reason => io_lib:format("Failed to transform render_list in ~p at line ~p:\n~p:~p:~p", [
            Module, Line, Class, Reason, StackTrace
        ])
    };
format_error(arizona_render_map_transformation_failed, [{_M, _F, _As, Info} | _]) ->
    {error_info, ErrorInfo} = proplists:lookup(error_info, Info),
    {Module, Line, Class, Reason, StackTrace} = maps:get(cause, ErrorInfo),
    #{
        general => "Arizona render_map transformation failed",
        reason => io_lib:format("Failed to transform render_map in ~p at line ~p:\n~p:~p:~p", [
            Module, Line, Class, Reason, StackTrace
        ])
    }.

%% --------------------------------------------------------------------
%% Internal functions
%% --------------------------------------------------------------------

%% Output forms to /tmp folder as Erlang module
-spec output_forms_to_tmp(Module | undefined, Forms) -> ok when
    Module :: module(),
    Forms :: compile:forms().
-ifdef(DEBUG).
output_forms_to_tmp(undefined, _Forms) ->
    ok;
output_forms_to_tmp(Module, Forms) ->
    FileName = "/tmp/" ++ atom_to_list(Module) ++ ".erl",
    Content = [erl_pp:form(Form, [{linewidth, 100}]) || Form <- Forms],
    ok = file:write_file(FileName, unicode:characters_to_binary(Content)).
-else.
output_forms_to_tmp(_Module, _Forms) ->
    ok.
-endif.

%% Extract module name from forms
-spec extract_module(Forms) -> Module | undefined when
    Forms :: compile:forms(),
    Module :: module().
extract_module([{attribute, _Line, module, Module} | _]) when is_atom(Module) ->
    Module;
extract_module([_ | Rest]) ->
    extract_module(Rest);
extract_module([]) ->
    undefined.

%% Transform a single form tree
transform_form(FormTree, Module, CompileOpts) ->
    erl_syntax_lib:map(
        fun(Node) ->
            transform_node(erl_syntax:revert(Node), Module, CompileOpts)
        end,
        FormTree
    ).

%% Transform individual AST nodes
transform_node(Node, Module, CompileOpts) ->
    case erl_syntax:type(Node) of
        application ->
            transform_application(Node, Module, CompileOpts);
        case_expr ->
            transform_case_expr(Node, Module, CompileOpts);
        clause ->
            transform_clause(Node, Module, CompileOpts);
        _ ->
            Node
    end.

%% Transform function applications
transform_application(Node, Module, CompileOpts) ->
    case analyze_application(Node) of
        {arizona_template, from_html, 1, [TemplateArg]} ->
            % Check if we're in a dynamic callback context to prevent infinite recursion
            InDynamicCallback = proplists:get_bool(in_dynamic_callback, CompileOpts),

            case InDynamicCallback of
                true ->
                    % Inside dynamic callback - don't transform from_html
                    % to prevent infinite recursion
                    Node;
                false ->
                    % Normal context - transform from_html
                    Line = get_node_line(Node),
                    transform_from_html(Module, Line, TemplateArg, CompileOpts)
            end;
        {arizona_template, from_markdown, 1, [MarkdownArg]} ->
            % Check if we're in a dynamic callback context to prevent infinite recursion
            InDynamicCallback = proplists:get_bool(in_dynamic_callback, CompileOpts),

            case InDynamicCallback of
                true ->
                    % Inside dynamic callback - don't transform from_markdown
                    % to prevent infinite recursion
                    Node;
                false ->
                    % Normal context - transform from_markdown
                    Line = get_node_line(Node),
                    transform_from_markdown(Module, Line, MarkdownArg, CompileOpts)
            end;
        {arizona_template, render_list, 2, [FunArg, ListArg]} ->
            % Always transform render_list calls - they don't cause infinite recursion
            Line = get_node_line(Node),
            transform_render_list(Module, Line, FunArg, ListArg, CompileOpts);
        {arizona_template, render_map, 2, [FunArg, MapArg]} ->
            % Always transform render_map calls - they don't cause infinite recursion
            Line = get_node_line(Node),
            transform_render_map(Module, Line, FunArg, MapArg, CompileOpts);
        _ ->
            Node
    end.

%% Transform case expressions
transform_case_expr(Node, Module, CompileOpts) ->
    Argument = erl_syntax:case_expr_argument(Node),
    Clauses = erl_syntax:case_expr_clauses(Node),
    TransformedArgument = apply_transformation(Argument, Module, CompileOpts),
    TransformedClauses = [
        apply_transformation(Clause, Module, CompileOpts)
     || Clause <- Clauses
    ],
    erl_syntax:case_expr(TransformedArgument, TransformedClauses).

%% Transform clauses
transform_clause(Node, Module, CompileOpts) ->
    Patterns = erl_syntax:clause_patterns(Node),
    Guard = erl_syntax:clause_guard(Node),
    Body = erl_syntax:clause_body(Node),
    TransformedPatterns = [
        apply_transformation(Pattern, Module, CompileOpts)
     || Pattern <- Patterns
    ],
    TransformedGuard =
        case Guard of
            none -> none;
            _ -> apply_transformation(Guard, Module, CompileOpts)
        end,
    TransformedBody = [
        apply_transformation(BodyExpr, Module, CompileOpts)
     || BodyExpr <- Body
    ],
    erl_syntax:clause(TransformedPatterns, TransformedGuard, TransformedBody).

%% Generic transformation function - reusable for any AST node
apply_transformation(Node, Module, CompileOpts) ->
    erl_syntax_lib:map(
        fun(ChildNode) ->
            transform_node(erl_syntax:revert(ChildNode), Module, CompileOpts)
        end,
        Node
    ).

%% Analyze function application to extract module, function, arity, and args
-spec analyze_application(Node) -> {Module, Function, Arity, Args} | undefined | Node when
    Node :: erl_syntax:syntaxTree(),
    Module :: module(),
    Function :: atom(),
    Arity :: non_neg_integer(),
    Args :: [erl_syntax:syntaxTree()].
analyze_application(Node) ->
    try
        Operator = erl_syntax:application_operator(Node),
        Arguments = erl_syntax:application_arguments(Node),

        case erl_syntax:type(Operator) of
            module_qualifier ->
                Module = erl_syntax:module_qualifier_argument(Operator),
                Function = erl_syntax:module_qualifier_body(Operator),
                case {erl_syntax:type(Module), erl_syntax:type(Function)} of
                    {atom, atom} ->
                        ModuleName = erl_syntax:atom_value(Module),
                        FunctionName = erl_syntax:atom_value(Function),
                        Arity = length(Arguments),
                        {ModuleName, FunctionName, Arity, Arguments};
                    _ ->
                        undefined
                end;
            _ ->
                undefined
        end
    catch
        _:_ ->
            Node
    end.

%% Transform arizona_template:from_html/1 calls
transform_from_html(Module, Line, TemplateArg, CompileOpts) ->
    try
        % Extract template content and line number
        String = eval_expr(Module, TemplateArg),
        % Scan template content into tokens
        % We do Line + 1 because we consider the use of triple-quoted string
        Tokens = arizona_scanner:scan_string(Line + 1, String),
        % Parse tokens into AST
        arizona_parser:parse_tokens(Tokens, CompileOpts)
    catch
        Class:Reason:StackTrace ->
            error(
                arizona_template_extraction_failed,
                none,
                error_info({Module, Line, Class, Reason, StackTrace})
            )
    end.

%% Transform arizona_template:from_markdown/1 calls
transform_from_markdown(Module, Line, MarkdownArg, CompileOpts) ->
    try
        % Extract markdown content and line number
        Markdown = eval_expr(Module, MarkdownArg),

        % Process markdown content with Arizona template syntax
        % We do Line + 1 because we consider the use of triple-quoted string
        HTML = arizona_markdown_processor:process_markdown_template(Markdown, Line + 1),

        % Scan the final HTML into tokens
        FinalTokens = arizona_scanner:scan_string(Line + 1, HTML),

        % Parse tokens into AST
        arizona_parser:parse_tokens(FinalTokens, CompileOpts)
    catch
        Class:Reason:StackTrace ->
            error(
                arizona_markdown_extraction_failed,
                none,
                error_info({Module, Line, Class, Reason, StackTrace})
            )
    end.

%% Wrap the dynamic tuple in a function for render_list templates
-spec wrap_dynamic_tuple_in_function(TemplateAST, RevClauseBody, CallbackArg) ->
    WrappedTemplateAST
when
    TemplateAST :: erl_syntax:syntaxTree(),
    RevClauseBody :: [erl_syntax:syntaxTree()],
    CallbackArg :: erl_syntax:syntaxTree(),
    WrappedTemplateAST :: erl_syntax:syntaxTree().
wrap_dynamic_tuple_in_function(TemplateAST, RevClauseBody, CallbackArg) ->
    case analyze_application(TemplateAST) of
        {arizona_template, new, 5, [Static, Dynamic, DynamicSequence, DynamicAnno, Fingerprint]} ->
            % Wrap the Dynamic tuple in fun(CallbackArg) -> ClauseBody end
            ClauseBody = lists:reverse([Dynamic | RevClauseBody]),
            FunClause = erl_syntax:clause([CallbackArg], none, ClauseBody),
            WrappedDynamic = erl_syntax:fun_expr([FunClause]),

            % Reconstruct the arizona_template:new/5 call with wrapped dynamic
            erl_syntax:application(
                erl_syntax:module_qualifier(
                    erl_syntax:atom(arizona_template),
                    erl_syntax:atom(new)
                ),
                [Static, WrappedDynamic, DynamicSequence, DynamicAnno, Fingerprint]
            );
        _ ->
            % If not arizona_template:new/5, return as-is
            TemplateAST
    end.

%% Utility Functions

%% Get line number from syntax tree node
-spec get_node_line(Node) -> Line when
    Node :: erl_syntax:syntaxTree(),
    Line :: pos_integer().
get_node_line(Node) ->
    Pos = erl_syntax:get_pos(Node),
    case erl_anno:is_anno(Pos) of
        true ->
            erl_anno:line(Pos);
        false ->
            Pos
    end.

%% Evaluate expression
-spec eval_expr(Module, Form) -> Result when
    Module :: module(),
    Form :: erl_syntax:syntaxTree() | erl_parse:abstract_expr(),
    Result :: dynamic().
eval_expr(Module, Form) ->
    % Convert erl_syntax tree to abstract form if needed
    AbstractForm =
        case erl_syntax:is_tree(Form) of
            true -> erl_syntax:revert(Form);
            false -> Form
        end,
    erl_eval:expr(
        AbstractForm,
        #{},
        {value, fun(Function, Args) -> apply(Module, Function, Args) end},
        none,
        value
    ).

%% Create error_info for proper compiler diagnostics with enhanced details
-spec error_info(Cause) -> ErrorInfo when
    Cause :: dynamic(),
    ErrorInfo :: [{error_info, map()}].
error_info(Cause) ->
    [
        {error_info, #{
            cause => Cause,
            module => ?MODULE
        }}
    ].

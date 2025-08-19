-module(arizona_parse_transform).

%% --------------------------------------------------------------------
%% API function exports
%% --------------------------------------------------------------------

-export([parse_transform/2]).
-export([parse_transform/3]).
-export([transform_render_list/5]).
-export([format_error/2]).

%% --------------------------------------------------------------------
%% Ignore xref warnings
%% --------------------------------------------------------------------

-ignore_xref([parse_transform/2]).
-ignore_xref([format_error/2]).

%% --------------------------------------------------------------------
%% Ignore hank warnings
%% --------------------------------------------------------------------

-hank([{unnecessary_function_arguments, [output_forms_to_tmp/2]}]).

%% --------------------------------------------------------------------
%% Parse Transform Implementation
%% --------------------------------------------------------------------

-spec parse_transform(Forms, Options) -> Forms when
    Forms :: compile:forms(),
    Options :: [compile:option()].
parse_transform(Forms, Options) ->
    CallbackArgs = erl_syntax:atom(ok),
    parse_transform(Forms, CallbackArgs, Options).

-spec parse_transform(Forms, CallbackArg, Options) -> Forms when
    Forms :: compile:forms(),
    CallbackArg :: erl_syntax:syntaxTree(),
    Options :: [compile:option()].
parse_transform(Forms, CallbackArg, Options) ->
    Module = extract_module(Forms),
    TransformedForms = [
        transform_form(FormTree, CallbackArg, Module, Options)
     || FormTree <- Forms
    ],
    RevertedForms = erl_syntax:revert_forms(TransformedForms),

    % Output forms to Erlang module in /tmp folder if DEBUG defined
    ok = output_forms_to_tmp(Module, RevertedForms),

    RevertedForms.

%% Transform arizona_template:render_list/2 calls
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
        {RevClauseBody, TemplateAST} = extract_list_function_body(
            FunArg, Module, Line, CompileOpts
        ),

        % Create application: arizona_template:render_list_template(Template, List)
        TemplateModule = erl_syntax:atom(arizona_template),
        TemplateFunction = erl_syntax:atom(render_list_template),
        TemplateCall = erl_syntax:application(
            erl_syntax:module_qualifier(TemplateModule, TemplateFunction), [
                TemplateAST, ListArg
            ]
        ),

        % If we have variable bindings, wrap everything in a begin...end block
        case RevClauseBody of
            [] ->
                TemplateCall;
            _ ->
                erl_syntax:block_expr(lists:reverse([TemplateCall | RevClauseBody]))
        end
    catch
        Class:Reason:StackTrace ->
            error(
                arizona_render_list_transformation_failed,
                none,
                error_info({Module, Line, Class, Reason, StackTrace})
            )
    end.

-spec format_error(Reason, StackTrace) -> ErrorMap when
    Reason :: arizona_template_extraction_failed | arizona_render_list_transformation_failed,
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
format_error(arizona_render_list_transformation_failed, [{_M, _F, _As, Info} | _]) ->
    {error_info, ErrorInfo} = proplists:lookup(error_info, Info),
    {Module, Line, Class, Reason, StackTrace} = maps:get(cause, ErrorInfo),
    #{
        general => "Arizona render_list transformation failed",
        reason => io_lib:format("Failed to transform render_list in ~p at line ~p:\n~p:~p:~p", [
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
    ok = file:write_file(FileName, Content).
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
transform_form(FormTree, CallbackArg, Module, CompileOpts) ->
    erl_syntax_lib:map(
        fun(Node) ->
            transform_node(erl_syntax:revert(Node), CallbackArg, Module, CompileOpts)
        end,
        FormTree
    ).

%% Transform individual AST nodes
transform_node(Node, CallbackArg, Module, CompileOpts) ->
    case erl_syntax:type(Node) of
        application ->
            transform_application(Node, CallbackArg, Module, CompileOpts);
        case_expr ->
            transform_case_expr(Node, CallbackArg, Module, CompileOpts);
        clause ->
            transform_clause(Node, CallbackArg, Module, CompileOpts);
        _ ->
            Node
    end.

%% Transform function applications
transform_application(Node, CallbackArg, Module, CompileOpts) ->
    case analyze_application(Node) of
        {arizona_template, from_string, 1, [TemplateArg]} ->
            % Check if we're in a dynamic callback context to prevent infinite recursion
            InDynamicCallback = proplists:get_bool(in_dynamic_callback, CompileOpts),

            case InDynamicCallback of
                true ->
                    % Inside dynamic callback - don't transform from_string
                    % to prevent infinite recursion
                    Node;
                false ->
                    % Normal context - transform from_string
                    Line = get_node_line(Node),
                    transform_from_string(
                        Module, Line, TemplateArg, CallbackArg, CompileOpts
                    )
            end;
        {arizona_template, render_list, 2, [FunArg, ListArg]} ->
            % Always transform render_list calls - they don't cause infinite recursion
            Line = get_node_line(Node),
            transform_render_list(Module, Line, FunArg, ListArg, CompileOpts);
        _ ->
            Node
    end.

%% Transform case expressions
transform_case_expr(Node, CallbackArg, Module, CompileOpts) ->
    Argument = erl_syntax:case_expr_argument(Node),
    Clauses = erl_syntax:case_expr_clauses(Node),
    TransformedArgument = apply_transformation(Argument, CallbackArg, Module, CompileOpts),
    TransformedClauses = [
        apply_transformation(Clause, CallbackArg, Module, CompileOpts)
     || Clause <- Clauses
    ],
    erl_syntax:case_expr(TransformedArgument, TransformedClauses).

%% Transform clauses
transform_clause(Node, CallbackArg, Module, CompileOpts) ->
    Patterns = erl_syntax:clause_patterns(Node),
    Guard = erl_syntax:clause_guard(Node),
    Body = erl_syntax:clause_body(Node),
    TransformedPatterns = [
        apply_transformation(Pattern, CallbackArg, Module, CompileOpts)
     || Pattern <- Patterns
    ],
    TransformedGuard =
        case Guard of
            none -> none;
            _ -> apply_transformation(Guard, CallbackArg, Module, CompileOpts)
        end,
    TransformedBody = [
        apply_transformation(BodyExpr, CallbackArg, Module, CompileOpts)
     || BodyExpr <- Body
    ],
    erl_syntax:clause(TransformedPatterns, TransformedGuard, TransformedBody).

%% Generic transformation function - reusable for any AST node
apply_transformation(Node, CallbackArg, Module, CompileOpts) ->
    erl_syntax_lib:map(
        fun(ChildNode) ->
            transform_node(erl_syntax:revert(ChildNode), CallbackArg, Module, CompileOpts)
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

%% Transform arizona_template:from_string/1 calls
transform_from_string(Module, Line, TemplateArg, CallbackArg, CompileOpts) ->
    try
        % Extract template content and line number
        String = eval_expr(Module, TemplateArg),
        % Scan template content into tokens
        % We do Line + 1 because we consider the use of triple-quoted string
        Tokens = arizona_scanner:scan_string(Line + 1, String),
        % Parse tokens into AST
        arizona_parser:parse_tokens(Tokens, CallbackArg, CompileOpts)
    catch
        Class:Reason:StackTrace ->
            error(
                arizona_template_extraction_failed,
                none,
                error_info({Module, Line, Class, Reason, StackTrace})
            )
    end.

%% Extract item parameter and template string from list function
extract_list_function_body(FunExpr, Module, Line, CompileOpts) ->
    case erl_syntax:type(FunExpr) of
        fun_expr ->
            [Clause] = erl_syntax:fun_expr_clauses(FunExpr),
            [CallbackArg] = erl_syntax:clause_patterns(Clause),
            ClauseBody = erl_syntax:clause_body(Clause),
            % Find the arizona_template:from_string call in the function body
            % It might be the only statement or the last statement after variable bindings
            [TemplateCall | RevClauseBody] = lists:reverse(ClauseBody),

            % Extract template string from raw arizona_template:from_string call
            case analyze_application(TemplateCall) of
                {arizona_template, from_string, 1, [TemplateArg]} ->
                    TemplateString = eval_expr(Module, TemplateArg),

                    % Scan template content into tokens
                    Tokens = arizona_scanner:scan_string(Line + 1, TemplateString),

                    % Parse tokens into template AST with the actual CallbackArg (not 'ok')
                    % This ensures dynamic expressions will use the correct parameter
                    % Clear in_dynamic_callback flag for render_list templates to allow
                    % nested transforms
                    ClearedOpts = proplists:delete(in_dynamic_callback, CompileOpts),
                    TemplateAST = arizona_parser:parse_tokens(
                        Tokens, CallbackArg, ClearedOpts
                    ),

                    {RevClauseBody, TemplateAST};
                TemplateCall ->
                    % analyze_application returned the Node itself, check if it's a template tuple
                    maybe
                        tuple ?= erl_syntax:type(TemplateCall),
                        [FirstElement | _] ?= erl_syntax:tuple_elements(TemplateCall),
                        atom ?= erl_syntax:type(FirstElement),
                        template ?= erl_syntax:atom_value(FirstElement),
                        {RevClauseBody, TemplateCall}
                    else
                        _ ->
                            error({not_from_string_call, TemplateCall})
                    end
            end;
        _ ->
            error({invalid_function_expression, FunExpr})
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

-module(arizona_parse_transform).

%% --------------------------------------------------------------------
%% API function exports
%% --------------------------------------------------------------------

-export([parse_transform/2]).
-export([format_error/2]).

%% --------------------------------------------------------------------
%% Ignore xref warnings
%% --------------------------------------------------------------------

-ignore_xref([parse_transform/2]).

%% --------------------------------------------------------------------
%% Parse Transform Implementation
%% --------------------------------------------------------------------

-spec parse_transform(Forms, Options) -> Forms when
    Forms :: compile:forms(),
    Options :: [compile:option()].
parse_transform(Forms, Options) ->
    ModuleName = extract_module_name(Forms),
    erl_syntax:revert_forms([
        transform_form(FormTree, ModuleName, Options)
     || FormTree <- Forms
    ]).

-spec format_error(Reason, StackTrace) -> ErrorMap when
    Reason :: arizona_template_extraction_failed,
    StackTrace :: erlang:stacktrace(),
    ErrorMap :: #{general => string(), reason => io_lib:chars()}.
format_error(arizona_template_extraction_failed, [{_M, _F, _As, Info} | _]) ->
    {error_info, ErrorInfo} = proplists:lookup(error_info, Info),
    {ModuleName, Line, Class, Reason, Stacktrace} = maps:get(cause, ErrorInfo),
    #{
        general => "Arizona template parsing failed",
        reason => io_lib:format("Failed to extract template in ~p at line ~p:\n~p:~p:~p", [
            ModuleName, Line, Class, Reason, Stacktrace
        ])
    }.

%% --------------------------------------------------------------------
%% Internal functions
%% --------------------------------------------------------------------

%% Extract module name from forms
-spec extract_module_name(Forms) -> ModuleName when
    Forms :: compile:forms(),
    ModuleName :: atom() | undefined.
extract_module_name([{attribute, _Line, module, ModuleName} | _]) when is_atom(ModuleName) ->
    ModuleName;
extract_module_name([_ | Rest]) ->
    extract_module_name(Rest);
extract_module_name([]) ->
    undefined.

%% Transform a single form tree
transform_form(FormTree, ModuleName, CompileOpts) ->
    erl_syntax_lib:map(
        fun(Node) ->
            transform_node(erl_syntax:revert(Node), ModuleName, CompileOpts)
        end,
        FormTree
    ).

%% Transform individual AST nodes
transform_node(Node, ModuleName, CompileOpts) ->
    case erl_syntax:type(Node) of
        application ->
            transform_application(Node, ModuleName, CompileOpts);
        _ ->
            Node
    end.

%% Transform function applications
transform_application(Node, ModuleName, CompileOpts) ->
    case analyze_application(Node) of
        {arizona_template, from_string, 1, [TemplateArg]} ->
            Pos = erl_syntax:get_pos(Node),
            Line =
                case erl_anno:is_anno(Pos) of
                    true ->
                        erl_anno:line(Pos);
                    false ->
                        Pos
                end,
            transform_from_string(ModuleName, Line, TemplateArg, CompileOpts);
        {arizona_template, render_list, 2, [FunArg, ListArg]} ->
            % Transform the function argument to process nested from_string calls
            TransformedFun = erl_syntax_lib:map(
                fun(InnerNode) ->
                    transform_node(erl_syntax:revert(InnerNode), ModuleName, CompileOpts)
                end,
                FunArg
            ),
            % Reconstruct the render_list call with the transformed function
            Module = erl_syntax:atom(arizona_template),
            Function = erl_syntax:atom(render_list),
            erl_syntax:application(erl_syntax:module_qualifier(Module, Function), [
                TransformedFun, ListArg
            ]);
        _ ->
            Node
    end.

%% Analyze function application to extract module, function, arity, and args
-spec analyze_application(Node) -> {Module, Function, Arity, Args} | undefined when
    Node :: erl_syntax:syntaxTree(),
    Module :: module(),
    Function :: atom(),
    Arity :: non_neg_integer(),
    Args :: [erl_syntax:syntaxTree()].
analyze_application(Node) ->
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
    end.

%% Transform arizona_template:from_string/1 calls
transform_from_string(ModuleName, Line, TemplateArg, CompileOpts) ->
    try
        % Extract template content and line number
        String = eval_expr(ModuleName, TemplateArg),
        % Scan template content into tokens
        Tokens = arizona_scanner:scan_string(Line, String),
        % Parse tokens into AST
        arizona_parser:parse_tokens(Tokens, CompileOpts)
    catch
        Class:Reason:Stacktrace ->
            error(
                arizona_template_extraction_failed,
                none,
                error_info({ModuleName, Line, Class, Reason, Stacktrace})
            )
    end.

%% Utility Functions

%% Evaluate expression
-spec eval_expr(Module, BinaryForm) -> Result when
    Module :: module(),
    BinaryForm :: erl_parse:abstract_expr(),
    Result :: dynamic().
eval_expr(Module, BinaryForm) ->
    erl_eval:expr(
        BinaryForm,
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

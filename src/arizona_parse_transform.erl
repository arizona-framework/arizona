-module(arizona_parse_transform).

%% --------------------------------------------------------------------
%% API function exports
%% --------------------------------------------------------------------

-export([parse_transform/2]).

%% --------------------------------------------------------------------
%% Testing helper exports
%% --------------------------------------------------------------------

-export([extract_module_name/1]).
-export([analyze_application/1]).
-export([eval_expr/3]).
-export([error_info/1]).

%% --------------------------------------------------------------------
%% Parse Transform Implementation
%% --------------------------------------------------------------------

-spec parse_transform(Forms, Options) -> Forms when
    Forms :: [erl_parse:abstract_form()],
    Options :: [compile:option()].
parse_transform(Forms, Options) ->
    ModuleName = extract_module_name(Forms),
    erl_syntax:revert_forms([
        transform_form(FormTree, ModuleName, Options)
     || FormTree <- Forms
    ]).

%% --------------------------------------------------------------------
%% Internal functions
%% --------------------------------------------------------------------

%% Extract module name from forms
-spec extract_module_name(Forms) -> ModuleName when
    Forms :: [erl_parse:abstract_form()],
    ModuleName :: atom() | undefined.
extract_module_name([{attribute, _Line, module, ModuleName} | _]) when is_atom(ModuleName) ->
    ModuleName;
extract_module_name([_ | Rest]) ->
    extract_module_name(Rest);
extract_module_name([]) ->
    undefined.

%% Transform a single form tree
transform_form(FormTree, ModuleName, _Options) ->
    erl_syntax_lib:map(
        fun(Node) ->
            transform_node(erl_syntax:revert(Node), ModuleName)
        end,
        FormTree
    ).

%% Transform individual AST nodes
transform_node(Node, ModuleName) ->
    case erl_syntax:type(Node) of
        application ->
            transform_application(Node, ModuleName);
        _ ->
            Node
    end.

%% Transform function applications
transform_application(Node, ModuleName) ->
    case analyze_application(Node) of
        {arizona_template, from_string, 2, [TemplateArg, _BindingsArg]} ->
            Anno = erl_anno:from_term(erl_syntax:get_ann(Node)),
            Line = erl_anno:line(Anno),
            transform_from_string(ModuleName, Line, TemplateArg);
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

%% Transform arizona_template:from_string/2 calls
transform_from_string(ModuleName, Line, TemplateArg) ->
    try
        % Extract template content and line number
        case eval_expr(ModuleName, TemplateArg, #{}) of
            String when is_binary(String) ->
                % Scan template content into tokens
                Tokens = arizona_scanner:scan_string(Line, String),

                % Parse tokens into AST
                arizona_parser:parse_tokens(Tokens)
        end
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
-spec eval_expr(Module, BinaryForm, Bindings) -> Result when
    Module :: module(),
    BinaryForm :: erl_parse:abstract_expr(),
    Bindings :: #{atom() => dynamic()},
    Result :: term().
eval_expr(Module, BinaryForm, Bindings) ->
    erl_eval:expr(
        BinaryForm,
        Bindings,
        {value, fun(Function, Args) -> apply(Module, Function, Args) end},
        none,
        value
    ).

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

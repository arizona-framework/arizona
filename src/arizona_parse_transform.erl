-module(arizona_parse_transform).
-moduledoc ~"""
Arizona Template Parse Transform.

This module provides compile-time transformation of Arizona template syntax
into optimized structured formats for high-performance rendering with enhanced
variable assignment support.

## Basic Transformations

- `arizona_html:render_stateless(~"template", Socket)` →
  `arizona_html:render_stateless([...], Socket)`
- `arizona_html:render_stateful(~"template", Socket)` →
  `arizona_html:render_stateful(#{...}, Socket)`
- `arizona_html:render_live(~"template", Socket)` →
  `arizona_html:render_live(#{...}, Socket)`
- Template expressions use `arizona_socket:get_binding/2` for variable access

## Enhanced Parse Transform

Use the `-arizona_parse_transform([function/arity])` attribute to enable
variable assignments before templates:

```erlang
-module(my_live).
-compile({parse_transform, arizona_parse_transform}).
-arizona_parse_transform([render/1]).

render(Socket) ->
    UserName = arizona_socket:get_binding(user_name, Socket),
    Count = arizona_socket:get_binding(count, Socket),
    arizona_html:render_live(~"<div>Hello {UserName}! Count: {Count}</div>", Socket).
```

The enhanced mode automatically generates `vars_indexes` for efficient change detection:
```erlang
vars_indexes => #{user_name => [1], count => [3]}
```

## Features

- **Variable Assignment Support**: Extract variables before templates
- **Dependency Tracking**: Handles nested and conditional binding calls
- **Change Detection**: Generates proper `vars_indexes` with atom keys
- **Multi-Function Support**: Different variable contexts per function
- **Type Safety**: AST-based transformation without `binary_to_atom/1`

## Limitations

- Only works with literal binary templates (compile-time determinable)
- Runtime-constructed templates are not supported
- Enhanced mode requires `-arizona_parse_transform([function/arity])` attribute

The parse transform analyzes function calls at compile time and replaces
them with optimized versions that avoid runtime template parsing overhead.
""".

%% --------------------------------------------------------------------
%% API function exports
%% --------------------------------------------------------------------

-export([parse_transform/2]).
-export([format_error/2]).
-export([transform_stateful_to_ast/1]).
-export([transform_stateless_to_ast/1]).

%% --------------------------------------------------------------------
%% Testing helper exports
%% --------------------------------------------------------------------

-export([extract_arizona_functions/1]).
-export([analyze_function_for_bindings/1]).
-export([generate_vars_indexes/2]).
-export([parse_template_for_stateful_with_context/5]).
-export([build_function_bindings_map/2]).

%% --------------------------------------------------------------------
%% Ignore xref warnings
%% --------------------------------------------------------------------

-ignore_xref([parse_transform/2]).
-ignore_xref([format_error/2]).

%% --------------------------------------------------------------------
%% Types exports
%% --------------------------------------------------------------------

-export_type([ast_node/0]).
-export_type([template_content/0]).
-export_type([compile_options/0]).
-export_type([function_spec/0]).
-export_type([enhanced_stateful_result/0]).

%% --------------------------------------------------------------------
%% Types definitions
%% --------------------------------------------------------------------

-doc ~"""
Abstract syntax tree node for parse transform processing.

Represents an AST node that can be either an abstract form (top-level
construct) or an abstract expression (sub-expression).
""".
-type ast_node() :: erl_parse:abstract_form() | erl_parse:abstract_expr().

-doc ~"""
Template content with extracted string and line number information.

Tuple containing the binary template string and the line number where
it was found, used for error reporting during parse transform processing.
""".
-type template_content() :: {binary(), pos_integer()}.

-doc ~"""
Function specification for arizona_parse_transform attribute.

Tuple containing function name and arity for functions that should
be analyzed for variable bindings.
""".
-type function_spec() :: {FunctionName :: atom(), Arity :: non_neg_integer()}.

-doc ~"""
Enhanced stateful result with vars_indexes for efficient change detection.

Extends the basic stateful_result from arizona_parser with vars_indexes
mapping that enables efficient template updates.
""".
-type enhanced_stateful_result() :: #{
    elems_order := [Index :: non_neg_integer()],
    elems := #{
        Index ::
            non_neg_integer() => {
                Category :: static | dynamic, Line :: pos_integer(), Content :: binary()
            }
    },
    vars_indexes := #{BindingName :: atom() => [ElementIndex :: non_neg_integer()]}
}.

-doc ~"""
Compiler options passed to the parse transform.

List of compiler options that can affect parse transform behavior.
""".
-type compile_options() :: [term()].

-doc ~"""
Callback function for normalizing dynamic expressions in templates.

Takes a socket variable name and expression text, returns a normalized expression
that can be safely evaluated in the template context. Used to wrap expressions
in anonymous functions with depth-specific socket variables to avoid shadowing.

Example:
```erlang
fun(<<"_@Socket0">>, <<"Count">>) ->
    <<"fun(_@Socket0) -> Count end">>
end
```
""".
-type expression_norm_callback() :: fun(
    (SocketVarName :: binary(), Expression :: binary()) -> NormExpression :: binary()
).

%% --------------------------------------------------------------------
%% API function definitions
%% --------------------------------------------------------------------

-doc ~"""
Main parse transform function called by the Erlang compiler.

This function is the entry point for the parse transform. It processes
the abstract syntax tree and transforms Arizona template function calls
into optimized versions.

The transformation is applied recursively to all forms in the module,
looking for calls to arizona_html functions with literal binary
templates that can be optimized at compile time.
""".
-spec parse_transform(AbstractSyntaxTrees, CompilerOptions) -> AbstractSyntaxTrees1 when
    AbstractSyntaxTrees :: [erl_parse:abstract_form()],
    CompilerOptions :: compile_options(),
    AbstractSyntaxTrees1 :: [erl_parse:abstract_form()].
parse_transform(AbstractSyntaxTrees, CompilerOptions) ->
    ModuleName = extract_module_name(AbstractSyntaxTrees),
    ArizonaFunctions = extract_arizona_functions(AbstractSyntaxTrees),

    %% Selective processing: only process modules with explicit arizona_parse_transform attributes
    case ArizonaFunctions of
        [] ->
            %% No Arizona functions declared - cause compilation error with proper module info
            error(arizona_no_parse_transform_attribute, none, error_info(ModuleName));
        _ ->
            %% Validate that declared functions exist and are exported
            validate_arizona_functions(AbstractSyntaxTrees, ArizonaFunctions, ModuleName),
            %% Process declared Arizona functions normally using existing logic
            parse_transform_with_depth(AbstractSyntaxTrees, CompilerOptions, 0)
    end.

%% Validate that declared Arizona functions exist and are exported
-spec validate_arizona_functions(AbstractSyntaxTrees, ArizonaFunctions, ModuleName) -> ok when
    AbstractSyntaxTrees :: [erl_parse:abstract_form()],
    ArizonaFunctions :: [function_spec()],
    ModuleName :: atom().
validate_arizona_functions(AbstractSyntaxTrees, ArizonaFunctions, ModuleName) ->
    %% Extract exported functions from module
    ExportedFunctions = extract_exported_functions(AbstractSyntaxTrees),
    %% Extract defined functions from module
    DefinedFunctions = extract_defined_functions(AbstractSyntaxTrees),

    %% Check each declared Arizona function
    lists:foreach(
        fun({FuncName, Arity}) ->
            %% Check if function is defined
            case lists:member({FuncName, Arity}, DefinedFunctions) of
                false ->
                    error(
                        arizona_function_not_defined,
                        none,
                        error_info({ModuleName, FuncName, Arity, "Function not defined in module"})
                    );
                true ->
                    %% Check if function is exported
                    case lists:member({FuncName, Arity}, ExportedFunctions) of
                        false ->
                            error(
                                arizona_function_not_exported,
                                none,
                                error_info(
                                    {ModuleName, FuncName, Arity,
                                        "Function not exported from module"}
                                )
                            );
                        true ->
                            ok
                    end
            end
        end,
        ArizonaFunctions
    ).

%% Parse transform with depth tracking for recursive optimization
-spec parse_transform_with_depth(AbstractSyntaxTrees, CompilerOptions, Depth) ->
    AbstractSyntaxTrees1
when
    AbstractSyntaxTrees :: [erl_parse:abstract_form()],
    CompilerOptions :: compile_options(),
    Depth :: non_neg_integer(),
    AbstractSyntaxTrees1 :: [erl_parse:abstract_form()].
parse_transform_with_depth(AbstractSyntaxTrees, CompilerOptions, Depth) ->
    ModuleName = extract_module_name(AbstractSyntaxTrees),
    ArizonaFunctions = extract_arizona_functions(AbstractSyntaxTrees),

    %% Build function-to-bindings mapping for arizona functions
    FunctionBindings = build_function_bindings_map(AbstractSyntaxTrees, ArizonaFunctions),

    erl_syntax:revert_forms([
        transform_form_with_context(
            FormTree,
            ModuleName,
            CompilerOptions,
            Depth,
            ArizonaFunctions,
            FunctionBindings
        )
     || FormTree <- AbstractSyntaxTrees
    ]).

-doc ~"""
Format error messages for compilation diagnostics with enhanced details.

This function is called by the Erlang compiler's error formatting system
to provide detailed error messages for parse transform failures.
""".
-spec format_error(Reason, Stacktrace) -> ErrorMap when
    Reason ::
        arizona_template_parse_failed
        | arizona_badarg
        | arizona_no_parse_transform_attribute
        | arizona_function_not_defined
        | arizona_function_not_exported,
    Stacktrace :: erlang:stacktrace(),
    ErrorMap :: #{pos_integer() => unicode:chardata()}.
format_error(Reason, [{_M, _F, _As, Info} | _]) ->
    ErrorInfoMap = proplists:get_value(error_info, Info, #{}),
    Cause = maps:get(cause, ErrorInfoMap, Reason),
    ErrorMsg = format_detailed_error(Reason, Cause),
    #{1 => ErrorMsg}.

%% Format detailed error messages based on cause information
-spec format_detailed_error(Reason, Cause) -> unicode:chardata() when
    Reason :: term(),
    Cause :: term().
format_detailed_error(arizona_template_parse_failed, {ModuleName, Line}) ->
    io_lib:format(
        "Failed to parse Arizona template in module ~w at line ~w. "
        "Check template syntax for proper expressions and balanced braces.",
        [ModuleName, Line]
    );
format_detailed_error(
    arizona_template_parse_failed, {ModuleName, Line, Error, Reason, Stacktrace}
) ->
    io_lib:format(
        "Failed to parse Arizona template in module ~w at line ~w. "
        "Original error: ~w:~p. Stacktrace: ~p. "
        "Check template syntax for proper expressions and balanced braces.",
        [ModuleName, Line, Error, Reason, Stacktrace]
    );
format_detailed_error(arizona_badarg, {ModuleName, Line}) ->
    io_lib:format(
        "Invalid arguments to Arizona template function in module ~w at line ~w. "
        "Expected literal binary template.",
        [ModuleName, Line]
    );
format_detailed_error(arizona_no_parse_transform_attribute, ModuleName) ->
    io_lib:format(
        "Module ~w uses Arizona parse transform but has no "
        "-arizona_parse_transform([function/arity]) attribute. "
        "Add -arizona_parse_transform([function/arity]) to declare which functions "
        "use Arizona templates. Example: -arizona_parse_transform([render/1]).",
        [ModuleName]
    );
format_detailed_error(arizona_function_not_defined, {ModuleName, FuncName, Arity, _Message}) ->
    io_lib:format(
        "Function ~w/~w declared in -arizona_parse_transform attribute is not "
        "defined in module ~w. Either define the function or remove it from the "
        "-arizona_parse_transform([function/arity]) list.",
        [FuncName, Arity, ModuleName]
    );
format_detailed_error(arizona_function_not_exported, {ModuleName, FuncName, Arity, _Message}) ->
    io_lib:format(
        "Function ~w/~w declared in -arizona_parse_transform attribute is not "
        "exported from module ~w. Either export the function with -export([~w/~w]) "
        "or remove it from the -arizona_parse_transform([function/arity]) list.",
        [FuncName, Arity, ModuleName, FuncName, Arity]
    ).

-doc ~"""
Transform stateful template result to optimized AST.

Converts a parsed stateful template result into an optimized AST representation
that can be used for efficient runtime rendering.
""".
-spec transform_stateful_to_ast(StatefulResult) -> SyntaxTree when
    StatefulResult :: arizona_parser:stateful_result() | enhanced_stateful_result(),
    SyntaxTree :: erl_syntax:syntaxTree().
transform_stateful_to_ast(#{elems_order := Order, elems := Elements} = StatefulResult) ->
    %% Get vars_indexes or generate empty one for runtime fallback
    VarsIndexes = maps:get(vars_indexes, StatefulResult, #{}),

    %% Create AST for optimized template data map
    OrderAST = erl_syntax:list([erl_syntax:integer(I) || I <- Order]),
    ElementsAST = create_elements_map_ast(Elements),
    VarsIndexesAST = create_vars_indexes_map_ast(VarsIndexes),

    %% Build the template data map AST
    erl_syntax:map_expr([
        erl_syntax:map_field_assoc(
            erl_syntax:atom(elems_order),
            OrderAST
        ),
        erl_syntax:map_field_assoc(
            erl_syntax:atom(elems),
            ElementsAST
        ),
        erl_syntax:map_field_assoc(
            erl_syntax:atom(vars_indexes),
            VarsIndexesAST
        )
    ]).

-doc ~"""
Transform stateless template result to optimized AST.

Converts a parsed stateless template result into an optimized AST representation
that can be used for efficient runtime rendering.
""".
-spec transform_stateless_to_ast(StatelessResult) -> SyntaxTree when
    StatelessResult :: arizona_parser:stateless_result(),
    SyntaxTree :: erl_syntax:syntaxTree().
transform_stateless_to_ast(StatelessList) when is_list(StatelessList) ->
    %% Convert stateless list to AST representation
    ListItems = [create_element_ast(Element) || Element <- StatelessList],
    erl_syntax:list(ListItems).

%% --------------------------------------------------------------------
%% Private functions
%% --------------------------------------------------------------------

%% Extract module name from the abstract syntax tree forms
-spec extract_module_name(AbstractSyntaxTrees) -> ModuleName when
    AbstractSyntaxTrees :: [erl_parse:abstract_form()],
    ModuleName :: atom().
extract_module_name([{attribute, _Anno, module, ModuleName} | _Rest]) ->
    ModuleName;
extract_module_name([_Form | Rest]) ->
    extract_module_name(Rest);
extract_module_name([]) ->
    unknown_module.

%% Extract arizona_parse_transform attribute from the abstract syntax tree forms
-spec extract_arizona_functions(AbstractSyntaxTrees) -> ArizonaFunctions when
    AbstractSyntaxTrees :: [erl_parse:abstract_form()],
    ArizonaFunctions :: [function_spec()].
extract_arizona_functions(AbstractSyntaxTrees) ->
    lists:foldl(
        fun
            ({attribute, _Anno, arizona_parse_transform, FunctionList}, Acc) when
                is_list(FunctionList)
            ->
                FunctionList ++ Acc;
            (_, Acc) ->
                Acc
        end,
        [],
        AbstractSyntaxTrees
    ).

%% Extract exported functions from module
-spec extract_exported_functions(AbstractSyntaxTrees) -> ExportedFunctions when
    AbstractSyntaxTrees :: [erl_parse:abstract_form()],
    ExportedFunctions :: [{atom(), arity()}].
extract_exported_functions(AbstractSyntaxTrees) ->
    lists:foldl(
        fun
            ({attribute, _Anno, export, FunctionList}, Acc) when is_list(FunctionList) ->
                FunctionList ++ Acc;
            (_, Acc) ->
                Acc
        end,
        [],
        AbstractSyntaxTrees
    ).

%% Extract defined functions from module
-spec extract_defined_functions(AbstractSyntaxTrees) -> DefinedFunctions when
    AbstractSyntaxTrees :: [erl_parse:abstract_form()],
    DefinedFunctions :: [{atom(), arity()}].
extract_defined_functions(AbstractSyntaxTrees) ->
    lists:foldl(
        fun
            ({function, _Anno, FuncName, Arity, _Clauses}, Acc) ->
                [{FuncName, Arity} | Acc];
            (_, Acc) ->
                Acc
        end,
        [],
        AbstractSyntaxTrees
    ).

%% TODO: Add warning injection functions in a future PR
%% has_arizona_html_calls/1, inject_arizona_warning/2, split_forms_at_first_function/1,2

%% Build mapping of function specs to their variable-to-binding mappings
-spec build_function_bindings_map(AbstractSyntaxTrees, ArizonaFunctions) -> FunctionBindings when
    AbstractSyntaxTrees :: [erl_parse:abstract_form()],
    ArizonaFunctions :: [function_spec()],
    FunctionBindings :: #{function_spec() => #{binary() => binary()}}.
build_function_bindings_map(AbstractSyntaxTrees, ArizonaFunctions) ->
    lists:foldl(
        fun(Form, Acc) ->
            case Form of
                {function, _Line, FuncName, Arity, _Clauses} = Function ->
                    FuncSpec = {FuncName, Arity},
                    case lists:member(FuncSpec, ArizonaFunctions) of
                        true ->
                            Bindings = analyze_function_for_bindings(Function),
                            Acc#{FuncSpec => Bindings};
                        false ->
                            Acc
                    end;
                _ ->
                    Acc
            end
        end,
        #{},
        AbstractSyntaxTrees
    ).

%% Analyze function AST to extract arizona_socket:get_binding calls
-spec analyze_function_for_bindings(Function) -> VarBindings when
    Function :: erl_parse:abstract_form(),
    VarBindings :: #{binary() => [binary()]}.
analyze_function_for_bindings({function, _Line, _Name, _Arity, _Clauses} = Function) ->
    %% Use erl_syntax_lib:map to traverse ALL AST nodes systematically
    CollectedBindings = collect_bindings_from_function(Function),
    %% Resolve transitive dependencies
    resolve_transitive_dependencies(CollectedBindings);
analyze_function_for_bindings(_) ->
    #{}.

%% Collect all arizona_socket:get_binding calls from function using erl_syntax traversal
-spec collect_bindings_from_function(Function) -> Bindings when
    Function :: erl_parse:abstract_form(),
    Bindings :: #{binary() => binary()}.
collect_bindings_from_function(Function) ->
    erl_syntax_lib:fold(
        fun(Node, Acc) ->
            analyze_node_for_bindings(erl_syntax:revert(Node), Acc)
        end,
        #{},
        Function
    ).

%% Analyze individual AST node for arizona_socket:get_binding patterns
-spec analyze_node_for_bindings(AstNode, Acc) -> NewAcc when
    AstNode :: erl_parse:abstract_expr(),
    Acc :: #{binary() => binary() | [binary()]},
    NewAcc :: #{binary() => binary() | [binary()]}.
analyze_node_for_bindings(AstNode, Acc) ->
    case AstNode of
        %% Match: Var = RHS (any assignment)
        {match, _Line, {var, _, VarName}, RHS} ->
            %% Collect all bindings this variable depends on (direct + transitive)
            BindingDependencies = collect_binding_dependencies(RHS),
            VariableDependencies = collect_variable_dependencies(RHS),
            AllDependencies = lists:usort(BindingDependencies ++ VariableDependencies),
            case AllDependencies of
                [] ->
                    Acc;
                _ ->
                    VarBinary = atom_to_binary(VarName),
                    Acc#{VarBinary => AllDependencies}
            end;
        _ ->
            Acc
    end.

%% Collect all binding dependencies from a potentially nested arizona_socket:get_binding call
-spec collect_binding_dependencies(AST) -> Dependencies when
    AST :: erl_parse:abstract_expr(),
    Dependencies :: [binary()].
collect_binding_dependencies(AST) ->
    Dependencies = collect_bindings_recursively(AST, []),
    % Remove duplicates and sort
    lists:usort(Dependencies).

%% Collect all variable dependencies from an expression (for transitive dependencies)
-spec collect_variable_dependencies(AST) -> Variables when
    AST :: erl_parse:abstract_expr(),
    Variables :: [binary()].
collect_variable_dependencies(AST) ->
    Variables = collect_variables_recursively(AST, []),
    % Filter out special variables like Socket, and remove duplicates
    FilteredVars = lists:filter(fun is_valid_dependency_variable/1, Variables),
    lists:usort(FilteredVars).

%% Check if a variable should be considered as a dependency
-spec is_valid_dependency_variable(VarName) -> boolean() when
    VarName :: binary().
is_valid_dependency_variable(VarName) ->
    % Exclude Socket parameter and other special variables
    not lists:member(VarName, [~"Socket", ~"_", ~"_Socket"]) andalso
        % Include variables that start with uppercase (standard Erlang variables)
        case binary:first(VarName) of
            C when C >= $A, C =< $Z -> true;
            _ -> false
        end.

%% Recursively collect all arizona_socket:get_binding calls
-spec collect_bindings_recursively(AST, Acc) -> Dependencies when
    AST :: term(),
    Acc :: [binary()],
    Dependencies :: [binary()].
collect_bindings_recursively(AST, Acc) ->
    case AST of
        %% Found arizona_socket:get_binding call
        {call, _, {remote, _, {atom, _, arizona_socket}, {atom, _, get_binding}}, Args} ->
            NewAcc =
                case extract_binding_name_from_args(Args) of
                    {ok, BindingName} ->
                        BindingBinary = atom_to_binary(BindingName),
                        [BindingBinary | Acc];
                    error ->
                        Acc
                end,
            %% Continue traversing arguments for nested calls
            lists:foldl(fun collect_bindings_recursively/2, NewAcc, Args);
        %% Traverse tuple elements
        Tuple when is_tuple(Tuple) ->
            lists:foldl(fun collect_bindings_recursively/2, Acc, tuple_to_list(Tuple));
        %% Traverse list elements
        List when is_list(List) ->
            lists:foldl(fun collect_bindings_recursively/2, Acc, List);
        %% Base case: atomic values
        _ ->
            Acc
    end.

%% Recursively collect all variable references
-spec collect_variables_recursively(AST, Acc) -> Variables when
    AST :: term(),
    Acc :: [binary()],
    Variables :: [binary()].
collect_variables_recursively(AST, Acc) ->
    case AST of
        %% Found variable reference
        {var, _, VarName} ->
            VarBinary = atom_to_binary(VarName),
            [VarBinary | Acc];
        %% Traverse tuple elements
        Tuple when is_tuple(Tuple) ->
            lists:foldl(fun collect_variables_recursively/2, Acc, tuple_to_list(Tuple));
        %% Traverse list elements
        List when is_list(List) ->
            lists:foldl(fun collect_variables_recursively/2, Acc, List);
        %% Base case: atomic values
        _ ->
            Acc
    end.

%% Resolve transitive dependencies by replacing variable references with their binding dependencies
-spec resolve_transitive_dependencies(Bindings) -> ResolvedBindings when
    Bindings :: #{binary() => binary() | [binary()]},
    ResolvedBindings :: #{binary() => [binary()]}.
resolve_transitive_dependencies(Bindings) ->
    %% Normalize values to lists first
    NormalizedBindings = maps:map(
        fun(_Key, Value) ->
            case Value of
                Bin when is_binary(Bin) -> [Bin];
                List when is_list(List) -> List
            end
        end,
        Bindings
    ),
    %% Iteratively resolve until no more changes occur
    resolve_until_stable(NormalizedBindings, NormalizedBindings).

%% Keep resolving dependencies until no changes occur
-spec resolve_until_stable(Current, Previous) -> Resolved when
    Current :: #{binary() => [binary()]},
    Previous :: #{binary() => [binary()]},
    Resolved :: #{binary() => [binary()]}.
resolve_until_stable(Current, Previous) ->
    Resolved = resolve_dependencies_once(Current),
    case maps_equal(Resolved, Previous) of
        true ->
            %% No changes, we're done
            Current;
        false ->
            %% Continue resolving
            resolve_until_stable(Resolved, Current)
    end.

%% Compare two maps for equality (maps:equal/2 replacement for older Erlang)
-spec maps_equal(Map1, Map2) -> boolean() when
    Map1 :: map(),
    Map2 :: map().
maps_equal(Map1, Map2) ->
    maps:size(Map1) =:= maps:size(Map2) andalso
        maps:fold(
            fun
                (Key, Value, true) ->
                    case maps:get(Key, Map2, undefined) of
                        Value -> true;
                        _ -> false
                    end;
                (_, _, false) ->
                    false
            end,
            true,
            Map1
        ).

%% Perform one pass of dependency resolution
-spec resolve_dependencies_once(Bindings) -> ResolvedBindings when
    Bindings :: #{binary() => [binary()]},
    ResolvedBindings :: #{binary() => [binary()]}.
resolve_dependencies_once(Bindings) ->
    maps:map(
        fun(_Var, Dependencies) ->
            lists:usort(
                lists:flatmap(
                    fun(Dep) ->
                        case maps:get(Dep, Bindings, undefined) of
                            undefined ->
                                %% Not a variable, assume it's a binding
                                [Dep];
                            VarDeps ->
                                %% It's a variable, expand its dependencies
                                VarDeps
                        end
                    end,
                    Dependencies
                )
            )
        end,
        Bindings
    ).

%% Extract binding name from arizona_socket:get_binding arguments
-spec extract_binding_name_from_args(Args) -> Result when
    Args :: [erl_parse:abstract_expr()],
    Result :: {ok, atom()} | error.
extract_binding_name_from_args([{atom, _, BindingName}, _SocketArg]) ->
    {ok, BindingName};
extract_binding_name_from_args([{atom, _, BindingName}, _SocketArg, _DefaultArg]) ->
    {ok, BindingName};
extract_binding_name_from_args(_) ->
    error.

%% Generate vars_indexes map from template structure and variable context
-spec generate_vars_indexes(
    #{elems := #{non_neg_integer() => {static | dynamic, pos_integer(), binary()}}},
    #{binary() => binary() | [binary()]}
) -> #{binary() => [non_neg_integer()]}.
generate_vars_indexes(#{elems := Elements}, VarToBinding) ->
    % Resolve transitive dependencies before processing template elements
    ResolvedVarToBinding = resolve_transitive_dependencies(VarToBinding),
    maps:fold(
        fun(Index, Element, Acc) ->
            case Element of
                {dynamic, _Line, ExprText} ->
                    VarNames = extract_variables_from_expression(ExprText),
                    process_element_variables(VarNames, Index, ResolvedVarToBinding, Acc);
                _ ->
                    % Static elements don't affect vars_indexes
                    Acc
            end
        end,
        #{},
        Elements
    ).

%% Process variables found in a dynamic element
process_element_variables(VarNames, Index, VarToBinding, Acc) ->
    lists:foldl(
        fun(VarName, InnerAcc) ->
            case maps:get(VarName, VarToBinding, undefined) of
                undefined ->
                    %% For basic parse transform, variable name IS the binding name
                    %% For enhanced parse transform with empty VarToBinding,
                    %% also treat as direct binding
                    case map_size(VarToBinding) of
                        0 ->
                            %% Basic parse transform: variable name = binding name
                            add_binding_index(VarName, Index, InnerAcc);
                        _ ->
                            %% Enhanced parse transform: variable not mapped to any binding
                            InnerAcc
                    end;
                BindingName when is_binary(BindingName) ->
                    %% Single binding dependency
                    add_binding_index(BindingName, Index, InnerAcc);
                BindingNames when is_list(BindingNames) ->
                    %% Multiple binding dependencies
                    lists:foldl(
                        fun(BindingName, Acc2) ->
                            add_binding_index(BindingName, Index, Acc2)
                        end,
                        InnerAcc,
                        BindingNames
                    )
            end
        end,
        Acc,
        VarNames
    ).

%% Add index to binding's list of affected elements
add_binding_index(BindingName, Index, Acc) ->
    CurrentIndexes = maps:get(BindingName, Acc, []),
    Acc#{BindingName => [Index | CurrentIndexes]}.

%% Extract variable names from an expression string (keep as binaries)
-spec extract_variables_from_expression(ExprText) -> Variables when
    ExprText :: binary(),
    Variables :: [binary()].
extract_variables_from_expression(ExprText) ->
    %% Find variable references in template expressions
    %% This handles both simple variables like {VarName} and binding calls
    %% like {arizona_socket:get_binding(binding_name, Socket)}

    %% First, try to match simple variable patterns like "Count"
    SimpleVars =
        case re:run(ExprText, ~"^([A-Z][a-zA-Z0-9_]*)$", [{capture, all_but_first, binary}]) of
            {match, [Match]} ->
                [Match];
            nomatch ->
                []
        end,

    %% Also look for arizona_socket:get_binding calls
    BindingVars = expr_vars(ExprText),

    %% Return all found variables
    lists:usort(SimpleVars ++ BindingVars).

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
            Vars = lists:flatten([extract_var_binary(List) || List <- Vars0]),
            lists:usort(Vars);
        nomatch ->
            []
    end.

%% Extract variable name as binary, handling quoted and unquoted forms
extract_var_binary([<<$', _/binary>> = Var | _T]) ->
    %% Remove quotes from quoted variable
    Size = byte_size(Var) - 2,
    <<$', UnquotedVar:Size/binary, $'>> = Var,
    UnquotedVar;
extract_var_binary([Var]) ->
    Var.

%% Transform a function with variable binding context
-spec transform_function_with_bindings(Function) -> TransformedFunction when
    Function :: erl_parse:abstract_form(),
    TransformedFunction :: erl_parse:abstract_form().
transform_function_with_bindings(Function) ->
    %% For now, just return the function as-is
    %% TODO: Implement function-level transformation with variable context
    Function.

%% Transform a form (top-level AST element) with context tracking
-spec transform_form_with_context(
    erl_parse:abstract_form(),
    atom(),
    compile_options(),
    non_neg_integer(),
    [function_spec()],
    #{function_spec() => #{binary() => binary()}}
) -> erl_parse:abstract_form().
transform_form_with_context(
    {function, _Line, FuncName, Arity, _Clauses} = Function,
    ModuleName,
    CompilerOptions,
    Depth,
    ArizonaFunctions,
    FunctionBindings
) ->
    %% Check if this is an Arizona function
    FuncSpec = {FuncName, Arity},
    CurrentFunctionBindings =
        case lists:member(FuncSpec, ArizonaFunctions) of
            true -> maps:get(FuncSpec, FunctionBindings, #{});
            false -> #{}
        end,

    %% Transform with current function context
    erl_syntax_lib:map(
        fun(Node) ->
            transform_ast_node_with_context(
                erl_syntax:revert(Node),
                ModuleName,
                CompilerOptions,
                Depth,
                ArizonaFunctions,
                FunctionBindings,
                CurrentFunctionBindings
            )
        end,
        Function
    );
transform_form_with_context(
    Form,
    ModuleName,
    CompilerOptions,
    Depth,
    ArizonaFunctions,
    FunctionBindings
) ->
    %% For non-function forms, use empty current function context
    erl_syntax_lib:map(
        fun(Node) ->
            transform_ast_node_with_context(
                erl_syntax:revert(Node),
                ModuleName,
                CompilerOptions,
                Depth,
                ArizonaFunctions,
                FunctionBindings,
                #{}
            )
        end,
        Form
    ).

%% Transform an individual AST node with enhanced context
-spec transform_ast_node_with_context(
    erl_parse:abstract_expr(),
    atom(),
    compile_options(),
    non_neg_integer(),
    [function_spec()],
    #{function_spec() => #{binary() => binary()}},
    #{binary() => binary()}
) -> erl_parse:abstract_expr().
transform_ast_node_with_context(
    AstNode,
    ModuleName,
    CompilerOptions,
    Depth,
    ArizonaFunctions,
    FunctionBindings,
    CurrentFunctionBindings
) ->
    transform_template_calls_with_context(
        AstNode,
        ModuleName,
        CompilerOptions,
        Depth,
        ArizonaFunctions,
        FunctionBindings,
        CurrentFunctionBindings
    ).

%% Transform arizona_html function calls with enhanced context
-spec transform_template_calls_with_context(
    erl_parse:abstract_expr(),
    atom(),
    compile_options(),
    non_neg_integer(),
    [function_spec()],
    #{function_spec() => #{binary() => binary()}},
    #{binary() => binary()}
) -> erl_parse:abstract_expr().
transform_template_calls_with_context(
    {function, _Line, FuncName, Arity, _Clauses} = Function,
    _ModuleName,
    _CompilerOptions,
    _Depth,
    ArizonaFunctions,
    FunctionBindings,
    _CurrentFunctionBindings
) ->
    %% Transform function if it's an Arizona function
    FuncSpec = {FuncName, Arity},
    case lists:member(FuncSpec, ArizonaFunctions) of
        true ->
            _VarBindings = maps:get(FuncSpec, FunctionBindings, #{}),
            transform_function_with_bindings(Function);
        false ->
            %% Regular transformation for non-Arizona functions
            Function
    end;
transform_template_calls_with_context(
    {call, CallAnnotations,
        {remote, _RemoteAnnotations, {atom, _ModuleAnnotations, arizona_html},
            {atom, _FunctionAnnotations, FunctionName}} = RemoteCall,
        Args},
    ModuleName,
    CompilerOptions,
    Depth,
    _ArizonaFunctions,
    _FunctionBindings,
    CurrentFunctionBindings
) ->
    transform_arizona_html_call_with_context(
        FunctionName,
        CallAnnotations,
        RemoteCall,
        Args,
        ModuleName,
        CompilerOptions,
        Depth,
        CurrentFunctionBindings
    );
transform_template_calls_with_context(
    AstNode,
    _ModuleName,
    _CompilerOptions,
    _Depth,
    _ArizonaFunctions,
    _FunctionBindings,
    _CurrentFunctionBindings
) ->
    AstNode.

%% Transform specific arizona_html function calls with context
-spec transform_arizona_html_call_with_context(
    atom(),
    erl_anno:anno(),
    erl_parse:abstract_expr(),
    [erl_parse:abstract_expr()],
    atom(),
    compile_options(),
    non_neg_integer(),
    #{binary() => binary()}
) -> erl_parse:abstract_expr().
transform_arizona_html_call_with_context(
    render_stateful,
    CallAnnotations,
    RemoteCall,
    Args,
    ModuleName,
    _CompilerOptions,
    _Depth,
    CurrentFunctionBindings
) ->
    %% Use current function bindings for enhanced template processing
    case CurrentFunctionBindings of
        EmptyMap when map_size(EmptyMap) =:= 0 ->
            %% No current function context, use regular transformation
            transform_render_stateful_call(
                CallAnnotations, RemoteCall, Args, ModuleName
            );
        _ ->
            %% We have function bindings, use enhanced transformation
            transform_render_stateful_call_with_context(
                CallAnnotations,
                RemoteCall,
                Args,
                ModuleName,
                CurrentFunctionBindings
            )
    end;
transform_arizona_html_call_with_context(
    render_live,
    CallAnnotations,
    RemoteCall,
    Args,
    ModuleName,
    _CompilerOptions,
    _Depth,
    CurrentFunctionBindings
) ->
    %% Treat render_live the same as render_stateful for enhanced processing
    case CurrentFunctionBindings of
        EmptyMap when map_size(EmptyMap) =:= 0 ->
            %% No current function context, use regular transformation
            transform_render_stateful_call(
                CallAnnotations, RemoteCall, Args, ModuleName
            );
        _ ->
            %% We have function bindings, use enhanced transformation
            transform_render_stateful_call_with_context(
                CallAnnotations,
                RemoteCall,
                Args,
                ModuleName,
                CurrentFunctionBindings
            )
    end;
transform_arizona_html_call_with_context(
    FunctionName,
    CallAnnotations,
    RemoteCall,
    Args,
    ModuleName,
    CompilerOptions,
    Depth,
    _CurrentFunctionBindings
) ->
    %% Fall back to regular transformation for other functions
    transform_arizona_html_call(
        FunctionName, CallAnnotations, RemoteCall, Args, ModuleName, CompilerOptions, Depth
    ).

%% Transform specific arizona_html function calls
-spec transform_arizona_html_call(
    atom(),
    erl_anno:anno(),
    erl_parse:abstract_expr(),
    [erl_parse:abstract_expr()],
    atom(),
    compile_options(),
    non_neg_integer()
) -> erl_parse:abstract_expr().
transform_arizona_html_call(
    render_stateless, CallAnnotations, RemoteCall, Args, ModuleName, CompilerOptions, Depth
) ->
    transform_render_stateless_call(
        CallAnnotations, RemoteCall, Args, ModuleName, CompilerOptions, Depth
    );
transform_arizona_html_call(
    render_stateful, CallAnnotations, RemoteCall, Args, ModuleName, _CompilerOptions, _Depth
) ->
    transform_render_stateful_call(
        CallAnnotations, RemoteCall, Args, ModuleName
    );
transform_arizona_html_call(
    render_list, CallAnnotations, RemoteCall, Args, ModuleName, CompilerOptions, Depth
) ->
    transform_render_list_call(
        CallAnnotations, RemoteCall, Args, ModuleName, CompilerOptions, Depth
    );
transform_arizona_html_call(
    render_slot, CallAnnotations, RemoteCall, Args, ModuleName, CompilerOptions, Depth
) ->
    transform_render_slot_call(
        CallAnnotations, RemoteCall, Args, ModuleName, CompilerOptions, Depth
    );
transform_arizona_html_call(
    render_live, CallAnnotations, RemoteCall, Args, ModuleName, _CompilerOptions, _Depth
) ->
    transform_render_stateful_call(
        CallAnnotations, RemoteCall, Args, ModuleName
    );
transform_arizona_html_call(
    _FunctionName, CallAnnotations, RemoteCall, Args, _ModuleName, _CompilerOptions, _Depth
) ->
    {call, CallAnnotations, RemoteCall, Args}.

%% Transform render_stateless function calls
-spec transform_render_stateless_call(
    erl_anno:anno(),
    erl_parse:abstract_expr(),
    [erl_parse:abstract_expr()],
    atom(),
    compile_options(),
    non_neg_integer()
) -> erl_parse:abstract_expr().
transform_render_stateless_call(
    CallAnnotations,
    RemoteCall,
    [{bin, _BinaryAnnotations, _BinaryFields} = BinaryTemplate, SocketArg],
    ModuleName,
    CompilerOptions,
    Depth
) ->
    transform_stateless_template_call(
        CallAnnotations, RemoteCall, BinaryTemplate, SocketArg, ModuleName, CompilerOptions, Depth
    );
transform_render_stateless_call(
    CallAnnotations, _RemoteCall, _Args, ModuleName, _CompilerOptions, _Depth
) ->
    Line = erl_anno:line(CallAnnotations),
    error(arizona_badarg, none, error_info({ModuleName, Line})).

%% Transform render_stateful function calls with current function context
-spec transform_render_stateful_call_with_context(
    erl_anno:anno(),
    erl_parse:abstract_expr(),
    [erl_parse:abstract_expr()],
    atom(),
    #{binary() => binary()}
) -> erl_parse:abstract_expr().
transform_render_stateful_call_with_context(
    CallAnnotations,
    RemoteCall,
    [{bin, _BinaryAnnotations, _BinaryFields} = BinaryTemplate, SocketArg],
    ModuleName,
    CurrentFunctionBindings
) ->
    transform_stateful_template_call_with_context(
        CallAnnotations,
        RemoteCall,
        BinaryTemplate,
        SocketArg,
        ModuleName,
        CurrentFunctionBindings
    );
transform_render_stateful_call_with_context(
    CallAnnotations,
    _RemoteCall,
    _Args,
    ModuleName,
    _CurrentFunctionBindings
) ->
    Line = erl_anno:line(CallAnnotations),
    error(arizona_badarg, none, error_info({ModuleName, Line})).

%% Transform render_stateful function calls
-spec transform_render_stateful_call(
    erl_anno:anno(),
    erl_parse:abstract_expr(),
    [erl_parse:abstract_expr()],
    atom()
) -> erl_parse:abstract_expr().
transform_render_stateful_call(
    CallAnnotations,
    RemoteCall,
    [{bin, _BinaryAnnotations, _BinaryFields} = BinaryTemplate, SocketArg],
    ModuleName
) ->
    transform_stateful_template_call(
        CallAnnotations, RemoteCall, BinaryTemplate, SocketArg, ModuleName
    );
transform_render_stateful_call(
    CallAnnotations, _RemoteCall, _Args, ModuleName
) ->
    Line = erl_anno:line(CallAnnotations),
    error(arizona_badarg, none, error_info({ModuleName, Line})).

%% Transform render_list function calls
-spec transform_render_list_call(
    erl_anno:anno(),
    erl_parse:abstract_expr(),
    [erl_parse:abstract_expr()],
    atom(),
    compile_options(),
    non_neg_integer()
) -> erl_parse:abstract_expr().
transform_render_list_call(
    CallAnnotations,
    RemoteCall,
    [ItemFun, Items, SocketArg],
    ModuleName,
    CompilerOptions,
    Depth
) ->
    transform_list_template_call(
        CallAnnotations,
        RemoteCall,
        ItemFun,
        Items,
        SocketArg,
        ModuleName,
        CompilerOptions,
        Depth
    );
transform_render_list_call(
    CallAnnotations, _RemoteCall, _Args, ModuleName, _CompilerOptions, _Depth
) ->
    Line = erl_anno:line(CallAnnotations),
    error(arizona_badarg, none, error_info({ModuleName, Line})).

%% Transform render_slot function calls
-spec transform_render_slot_call(
    erl_anno:anno(),
    erl_parse:abstract_expr(),
    [erl_parse:abstract_expr()],
    atom(),
    compile_options(),
    non_neg_integer()
) -> erl_parse:abstract_expr().
transform_render_slot_call(
    CallAnnotations,
    RemoteCall,
    [SlotNameArg, SocketArg, {bin, _BinaryAnnotations, _BinaryFields} = BinaryTemplate],
    ModuleName,
    CompilerOptions,
    Depth
) ->
    transform_slot_template_call(
        CallAnnotations,
        RemoteCall,
        SlotNameArg,
        SocketArg,
        BinaryTemplate,
        ModuleName,
        CompilerOptions,
        Depth
    );
transform_render_slot_call(
    CallAnnotations, RemoteCall, Args, _ModuleName, _CompilerOptions, _Depth
) ->
    % render_slot calls without binary template (2 args or non-binary 3rd arg) - pass through
    {call, CallAnnotations, RemoteCall, Args}.

%% Slot Template Transformation

%% Transform render_slot call with binary template default
-spec transform_slot_template_call(
    erl_anno:anno(),
    erl_parse:abstract_expr(),
    erl_parse:abstract_expr(),
    erl_parse:abstract_expr(),
    erl_parse:abstract_expr(),
    atom(),
    compile_options(),
    non_neg_integer()
) -> erl_parse:abstract_expr().
transform_slot_template_call(
    CallAnnotations,
    RemoteCall,
    SlotNameArg,
    SocketArg,
    BinaryTemplate,
    ModuleName,
    CompilerOptions,
    Depth
) ->
    Line = erl_anno:line(CallAnnotations),
    try
        % Extract and parse the template at compile time (same as stateless)
        {TemplateString, LineNumber} = extract_template_content(BinaryTemplate),
        IoListStructure = parse_template_for_stateless(
            TemplateString, LineNumber, CompilerOptions, Depth
        ),

        % Generate the new function call with parsed structure as default
        create_slot_stateless_call(
            CallAnnotations, RemoteCall, SlotNameArg, SocketArg, IoListStructure
        )
    catch
        Error:Reason:Stacktrace ->
            error(
                arizona_template_parse_failed,
                none,
                error_info({ModuleName, Line, Error, Reason, Stacktrace})
            )
    end.

%% Stateless Template Transformation

%% Transform render_stateless call with depth tracking
-spec transform_stateless_template_call(
    erl_anno:anno(),
    erl_parse:abstract_expr(),
    erl_parse:abstract_expr(),
    erl_parse:abstract_expr(),
    atom(),
    compile_options(),
    non_neg_integer()
) -> erl_parse:abstract_expr().
transform_stateless_template_call(
    CallAnnotations, RemoteCall, BinaryTemplate, SocketArg, ModuleName, CompilerOptions, Depth
) ->
    Line = erl_anno:line(CallAnnotations),
    try
        % Extract and parse the template at compile time
        {TemplateString, LineNumber} = extract_template_content(BinaryTemplate),
        IoListStructure = parse_template_for_stateless(
            TemplateString, LineNumber, CompilerOptions, Depth
        ),

        % Generate the new function call with parsed structure
        create_stateless_parsed_call(CallAnnotations, RemoteCall, IoListStructure, SocketArg)
    catch
        Error:Reason:Stacktrace ->
            error(
                arizona_template_parse_failed,
                none,
                error_info({ModuleName, Line, Error, Reason, Stacktrace})
            )
    end.

%% Parse template string into iolist structure with depth tracking
-spec parse_template_for_stateless(binary(), pos_integer(), compile_options(), non_neg_integer()) ->
    [term()].
parse_template_for_stateless(TemplateString, LineNumber, CompilerOptions, Depth) ->
    TokenList = arizona_scanner:scan(#{line => LineNumber}, TemplateString),
    ParsedElements = arizona_parser:parse_stateless_tokens(TokenList),
    convert_to_iolist_format(ParsedElements, CompilerOptions, Depth).

%% Convert parsed elements to iolist format with depth tracking
-spec convert_to_iolist_format([term()], compile_options(), non_neg_integer()) -> [term()].
convert_to_iolist_format(ParsedElements, CompilerOptions, Depth) ->
    lists:reverse(
        lists:foldl(
            fun(Element, Acc) ->
                convert_element_to_iolist(
                    Element, standard_expression_norm_callback(), Acc, CompilerOptions, Depth
                )
            end,
            [],
            ParsedElements
        )
    ).

%% Convert individual element to iolist format with depth tracking
-spec convert_element_to_iolist(
    term(), expression_norm_callback(), [term()], compile_options(), non_neg_integer()
) -> [term()].
convert_element_to_iolist(
    {static, Line, StaticText}, _ExpressionTextNormCallback, Accumulator, _CompilerOptions, _Depth
) ->
    StaticElement = format_static_element(Line, StaticText),
    [StaticElement | Accumulator];
convert_element_to_iolist(
    {dynamic, Line, ExpressionText}, ExpressionTextNormCallback, Accumulator, CompilerOptions, Depth
) ->
    DynamicElement = format_dynamic_element(
        Line, ExpressionText, ExpressionTextNormCallback, CompilerOptions, Depth
    ),
    [DynamicElement | Accumulator].

%% Stateful Template Transformation

%% Transform render_stateful call with current function context and depth tracking
-spec transform_stateful_template_call_with_context(
    erl_anno:anno(),
    erl_parse:abstract_expr(),
    erl_parse:abstract_expr(),
    erl_parse:abstract_expr(),
    atom(),
    #{binary() => binary()}
) -> erl_parse:abstract_expr().
transform_stateful_template_call_with_context(
    CallAnnotations,
    RemoteCall,
    BinaryTemplate,
    SocketArg,
    ModuleName,
    CurrentFunctionBindings
) ->
    Line = erl_anno:line(CallAnnotations),
    try
        % Extract and parse the template at compile time with function context
        {TemplateString, LineNumber} = extract_template_content(BinaryTemplate),
        StatefulResult = parse_template_for_stateful_result_with_context(
            TemplateString, LineNumber, CurrentFunctionBindings
        ),

        % Generate AST directly instead of string-based approach
        TemplateDataAST = transform_stateful_to_ast(StatefulResult),
        create_stateful_ast_call(CallAnnotations, RemoteCall, TemplateDataAST, SocketArg)
    catch
        Error:Reason:Stacktrace ->
            error(
                arizona_template_parse_failed,
                none,
                error_info({ModuleName, Line, Error, Reason, Stacktrace})
            )
    end.

%% Transform render_stateful call with depth tracking
-spec transform_stateful_template_call(
    erl_anno:anno(),
    erl_parse:abstract_expr(),
    erl_parse:abstract_expr(),
    erl_parse:abstract_expr(),
    atom()
) -> erl_parse:abstract_expr().
transform_stateful_template_call(
    CallAnnotations, RemoteCall, BinaryTemplate, SocketArg, ModuleName
) ->
    Line = erl_anno:line(CallAnnotations),
    try
        % Extract and parse the template at compile time
        {TemplateString, LineNumber} = extract_template_content(BinaryTemplate),
        StatefulResult = parse_template_for_stateful_result_with_context(
            TemplateString, LineNumber, #{}
        ),

        % Generate AST directly instead of string-based approach
        TemplateDataAST = transform_stateful_to_ast(StatefulResult),
        create_stateful_ast_call(CallAnnotations, RemoteCall, TemplateDataAST, SocketArg)
    catch
        Error:Reason:Stacktrace ->
            error(
                arizona_template_parse_failed,
                none,
                error_info({ModuleName, Line, Error, Reason, Stacktrace})
            )
    end.

%% Parse template string into structured format with variable context
-spec parse_template_for_stateful_with_context(
    binary(), pos_integer(), compile_options(), non_neg_integer(), #{binary() => binary()}
) -> binary().
parse_template_for_stateful_with_context(
    TemplateString, LineNumber, CompilerOptions, Depth, VarBindings
) ->
    TokenList = arizona_scanner:scan(#{line => LineNumber}, TemplateString),
    #{
        elems_order := ElementOrder,
        elems := ElementsMap
    } = arizona_parser:parse_stateful_tokens(TokenList),

    %% Generate vars_indexes using our new function
    VariableIndexes = generate_vars_indexes(#{elems => ElementsMap}, VarBindings),

    build_template_data_structure(
        ElementOrder, ElementsMap, VariableIndexes, CompilerOptions, Depth
    ).

%% Parse template string into stateful result with variable context for AST generation
-spec parse_template_for_stateful_result_with_context(
    binary(), pos_integer(), #{binary() => binary()}
) ->
    #{
        elems_order := [non_neg_integer()],
        elems := #{non_neg_integer() => {static | dynamic, pos_integer(), binary()}},
        vars_indexes := #{binary() => [non_neg_integer()]}
    }.
parse_template_for_stateful_result_with_context(TemplateString, LineNumber, VarBindings) ->
    TokenList = arizona_scanner:scan(#{line => LineNumber}, TemplateString),
    StatefulResult = arizona_parser:parse_stateful_tokens(TokenList),

    %% Generate vars_indexes using enhanced function
    #{elems := ElementsMap} = StatefulResult,
    VariableIndexes = generate_vars_indexes(#{elems => ElementsMap}, VarBindings),

    %% Add vars_indexes to the stateful result for AST generation
    StatefulResult#{vars_indexes => VariableIndexes}.

%% Build the template data structure with depth tracking
-spec build_template_data_structure(
    [integer()], map(), map(), compile_options(), non_neg_integer()
) -> binary().
build_template_data_structure(ElementOrder, ElementsMap, VariableIndexes, CompilerOptions, Depth) ->
    OrderString = format_element_order(ElementOrder),
    ElementsString = format_elements_map(
        ElementsMap, standard_expression_norm_callback(), CompilerOptions, Depth
    ),
    VariablesString = format_variables_indexes(VariableIndexes),

    iolist_to_binary(io_lib:format(~"""
    #{
        elems_order => [~s],
        elems => #{~s},
        vars_indexes => #{~s}
    }
    """, [OrderString, ElementsString, VariablesString])).

%% Template Data Formatting Helpers

%% Format element order as comma-separated string
-spec format_element_order([integer()]) -> string().
format_element_order(ElementOrder) ->
    lists:join(", ", lists:map(fun integer_to_binary/1, ElementOrder)).

%% Format elements map as key-value pairs with depth tracking
-spec format_elements_map(
    map(), expression_norm_callback(), compile_options(), non_neg_integer()
) ->
    string().
format_elements_map(ElementsMap, ExpressionTextNormCallback, CompilerOptions, Depth) ->
    ElementPairs = maps:fold(
        fun(K, V, Acc) ->
            format_element_entry(K, V, ExpressionTextNormCallback, Acc, CompilerOptions, Depth)
        end,
        [],
        ElementsMap
    ),
    lists:join(", ", lists:reverse(ElementPairs)).

%% Format a single element entry with depth tracking for recursion
-spec format_element_entry(
    integer(), term(), expression_norm_callback(), [string()], compile_options(), non_neg_integer()
) ->
    [string()].
format_element_entry(
    ElementIndex,
    {static, Line, StaticText},
    _ExpressionTextNormCallback,
    Accumulator,
    _CompilerOptions,
    _Depth
) ->
    StaticElemText = format_static_element(Line, StaticText),
    FormattedEntry = iolist_to_binary(io_lib:format("~p => ~s", [ElementIndex, StaticElemText])),
    [FormattedEntry | Accumulator];
format_element_entry(
    ElementIndex,
    {dynamic, Line, ExpressionText},
    ExpressionTextNormCallback,
    Accumulator,
    CompilerOptions,
    Depth
) ->
    DynamicElemText = format_dynamic_element(
        Line, ExpressionText, ExpressionTextNormCallback, CompilerOptions, Depth
    ),
    FormattedEntry = iolist_to_binary(io_lib:format("~p => ~s", [ElementIndex, DynamicElemText])),
    [FormattedEntry | Accumulator].

%% Optimize dynamic expressions with depth tracking to avoid socket variable shadowing
-spec optimize_dynamic_expression(binary(), compile_options(), non_neg_integer()) -> binary().
optimize_dynamic_expression(ExpressionText, CompilerOptions, Depth) ->
    %% Parse expression into AST
    ExprAST =
        case merl:quote(ExpressionText) of
            AST when is_list(AST) ->
                AST;
            AST when is_tuple(AST) ->
                [AST]
        end,

    %% Apply parse transform recursively with incremented depth to avoid variable shadowing
    TransformedExprAST = parse_transform_with_depth(ExprAST, CompilerOptions, Depth + 1),

    %% Convert back to text representation
    OptimizedText = erl_pp:exprs(erl_syntax:revert_forms(TransformedExprAST)),

    %% Return as binary, removing trailing newlines
    iolist_to_binary(string:trim(OptimizedText)).

%% Format variable indexes map as key-value pairs
-spec format_variables_indexes(map()) -> string().
format_variables_indexes(VariableIndexes) ->
    VariablePairs = [format_variable_entry(V, I) || V := I <- VariableIndexes],
    lists:join(", ", lists:reverse(VariablePairs)).

%% Format a single variable index entry
-spec format_variable_entry(term(), [integer()]) -> binary().
format_variable_entry(VariableName, IndexList) ->
    iolist_to_binary(io_lib:format("~p => ~p", [VariableName, IndexList])).

%% List Template Transformation

%% Transform render_list call with depth tracking
-spec transform_list_template_call(
    erl_anno:anno(),
    erl_parse:abstract_expr(),
    erl_parse:abstract_expr(),
    erl_parse:abstract_expr(),
    erl_parse:abstract_expr(),
    atom(),
    compile_options(),
    non_neg_integer()
) -> erl_parse:abstract_expr().
transform_list_template_call(
    CallAnnotations,
    RemoteCall,
    ItemFun,
    Items,
    SocketArg,
    ModuleName,
    CompilerOptions,
    Depth
) ->
    Line = erl_anno:line(CallAnnotations),
    try
        % Parse the ItemFun template content if it's a binary template
        ListTemplateData = parse_template_for_list(ItemFun, CompilerOptions, Depth),

        % Generate the new function call using arizona_renderer:render_list
        create_list_structured_call(
            CallAnnotations, RemoteCall, ListTemplateData, Items, SocketArg
        )
    catch
        Error:Reason:Stacktrace ->
            error(
                arizona_template_parse_failed,
                none,
                error_info({ModuleName, Line, Error, Reason, Stacktrace})
            )
    end.

%% Parse template content for list rendering with depth tracking
-spec parse_template_for_list(
    erl_parse:abstract_expr(), compile_options(), non_neg_integer()
) -> binary().
parse_template_for_list(ItemFun, CompilerOptions, Depth) ->
    % Extract template content from ItemFun
    [Clause] = erl_syntax:fun_expr_clauses(ItemFun),
    [BinaryTemplate] = erl_syntax:clause_body(Clause),
    {TemplateString, LineNumber} = extract_template_content(BinaryTemplate),

    % Parse the extracted template
    TokenList = arizona_scanner:scan(#{line => LineNumber}, TemplateString),
    #{
        static := StaticParts,
        dynamic := #{
            elems_order := ElemsOrder,
            elems := DynamicElements
        }
    } = arizona_parser:parse_list_tokens(TokenList),

    %% Generate vars_indexes using empty variable context for now
    %% TODO: Support function-level variable context for list templates
    VariableIndexes = generate_vars_indexes(#{elems => DynamicElements}, #{}),

    % Build list template data structure
    build_list_template_data_structure(
        StaticParts, ElemsOrder, DynamicElements, VariableIndexes, ItemFun, CompilerOptions, Depth
    ).

%% Build list template data structure
-spec build_list_template_data_structure(
    [binary()],
    [integer()],
    map(),
    map(),
    erl_parse:abstract_expr(),
    compile_options(),
    non_neg_integer()
) -> binary().
build_list_template_data_structure(
    StaticParts, ElemsOrder, DynamicElements, VariableIndexes, ItemFun, CompilerOptions, Depth
) ->
    StaticString = format_static_parts(StaticParts),
    OrderString = format_element_order(ElemsOrder),
    DynamicString = format_elements_map(
        DynamicElements, list_expression_norm_callback(ItemFun), CompilerOptions, Depth
    ),
    VariablesString = format_variables_indexes(VariableIndexes),

    iolist_to_binary(
        io_lib:format(
            ~"""
            #{
                static => [~s],
                dynamic => #{
                    elems_order => [~s],
                    elems => #{~s},
                    vars_indexes => #{~s}
                }
            }
            """,
            [StaticString, OrderString, DynamicString, VariablesString]
        )
    ).

%% Format static parts for list templates
format_static_parts(StaticParts) ->
    FormattedParts = [io_lib:format("~p", [Part]) || Part <- StaticParts],
    lists:join(", ", FormattedParts).

%% Function Call Generation

%% Create render_stateless function call with parsed structure
-spec create_stateless_parsed_call(
    erl_anno:anno(),
    erl_parse:abstract_expr(),
    [term()],
    erl_parse:abstract_expr()
) -> erl_parse:abstract_expr().
create_stateless_parsed_call(
    CallAnnotations,
    {remote, RemoteAnnotations, ModuleAtom, FunctionAtom},
    IoListStructure,
    SocketArg
) ->
    IoListBinary = iolist_to_binary(["[", lists:join(", ", IoListStructure), "]"]),
    IoListForm = merl:quote(IoListBinary),
    {call, CallAnnotations, {remote, RemoteAnnotations, ModuleAtom, FunctionAtom}, [
        IoListForm, SocketArg
    ]}.

%% Create optimized render_slot call with {stateless, ParsedTemplate} default
-spec create_slot_stateless_call(
    erl_anno:anno(),
    erl_parse:abstract_expr(),
    erl_parse:abstract_expr(),
    erl_parse:abstract_expr(),
    [term()]
) -> erl_parse:abstract_expr().
create_slot_stateless_call(
    CallAnnotations,
    {remote, RemoteAnnotations, ModuleAtom, FunctionAtom},
    SlotNameArg,
    SocketArg,
    IoListStructure
) ->
    % Create IoList AST from the parsed structure
    IoListBinary = iolist_to_binary(["[", lists:join(", ", IoListStructure), "]"]),
    IoListForm = merl:quote(IoListBinary),

    % Create {stateless, ParsedTemplate} tuple
    StatelessTuple =
        {tuple, CallAnnotations, [
            {atom, CallAnnotations, stateless},
            IoListForm
        ]},

    % Create render_slot(SlotName, Socket, {stateless, ParsedTemplate}) call
    {call, CallAnnotations, {remote, RemoteAnnotations, ModuleAtom, FunctionAtom}, [
        SlotNameArg, SocketArg, StatelessTuple
    ]}.

%% Create render_stateful function call with AST data
-spec create_stateful_ast_call(
    erl_anno:anno(),
    erl_parse:abstract_expr(),
    erl_syntax:syntaxTree(),
    erl_parse:abstract_expr()
) -> erl_parse:abstract_expr().
create_stateful_ast_call(CallAnnotations, RemoteCall, TemplateDataAST, SocketArg) ->
    {call, CallAnnotations, RemoteCall, [erl_syntax:revert(TemplateDataAST), SocketArg]}.

%% Create render_list function call with structured data
-spec create_list_structured_call(
    erl_anno:anno(),
    erl_parse:abstract_expr(),
    binary(),
    erl_parse:abstract_expr(),
    erl_parse:abstract_expr()
) -> erl_parse:abstract_expr().
create_list_structured_call(
    CallAnnotations, RemoteCall, ListTemplateDataBinary, Items, SocketArg
) ->
    ListTemplateDataForm = merl:quote(ListTemplateDataBinary),
    {call, CallAnnotations, RemoteCall, [ListTemplateDataForm, Items, SocketArg]}.

%% Utility Functions

%% Extract template content and line number from binary AST node
-spec extract_template_content(BinaryForm) -> TemplateContent when
    BinaryForm :: erl_parse:abstract_expr(),
    TemplateContent :: {binary(), pos_integer()}.
extract_template_content({bin, BinaryAnnotations, _BinaryFields} = BinaryForm) ->
    {value, TemplateString, #{}} = erl_eval:expr(BinaryForm, #{}),
    LineNumber = erl_anno:line(BinaryAnnotations),
    {TemplateString, LineNumber}.

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

%% Helper functions for AST generation

%% Create AST for elements map
create_elements_map_ast(Elements) ->
    MapFields = [
        erl_syntax:map_field_assoc(
            erl_syntax:integer(Index),
            create_element_ast(Element)
        )
     || Index := Element <- Elements
    ],
    erl_syntax:map_expr(MapFields).

%% Create AST for a single element (static or dynamic)
create_element_ast({static, Line, Content}) ->
    erl_syntax:tuple([
        erl_syntax:atom(static),
        erl_syntax:integer(Line),
        erl_syntax:binary([erl_syntax:binary_field(erl_syntax:string(binary_to_list(Content)))])
    ]);
create_element_ast({dynamic, Line, ExpressionText}) ->
    %% Convert expression to optimized function AST
    %% ExprBinary is the original expression like "arizona_socket:get_binding(name, Socket)"
    %% Use depth 0 for runtime AST creation (not parse transform)
    OptimizedExpressionText = optimize_dynamic_expression(ExpressionText, [], 0),
    FunctionBinary = create_socket_threaded_function(
        OptimizedExpressionText, standard_expression_norm_callback(), 0
    ),

    erl_syntax:tuple([
        erl_syntax:atom(dynamic),
        erl_syntax:integer(Line),
        merl:quote(FunctionBinary)
    ]).

%% Create AST for vars_indexes map
create_vars_indexes_map_ast(VarsIndexes) ->
    MapFields = [
        erl_syntax:map_field_assoc(
            %% Create atom AST from variable name - handle both binary and atom
            case VarName of
                Bin when is_binary(Bin) -> erl_syntax:atom(binary_to_list(Bin));
                Atom when is_atom(Atom) -> erl_syntax:atom(Atom)
            end,
            erl_syntax:list([erl_syntax:integer(Idx) || Idx <- IndexList])
        )
     || VarName := IndexList <- VarsIndexes
    ],
    erl_syntax:map_expr(MapFields).

%% Get socket variable name with depth to avoid nesting shadowing
-spec get_socket_var_name(Depth) -> SocketVarName when
    Depth :: non_neg_integer(),
    SocketVarName :: binary().
get_socket_var_name(Depth) when Depth >= 0 ->
    iolist_to_binary([~"_@Socket", integer_to_binary(Depth)]).

%% Replace Socket variable with depth-specific variable to avoid shadowing
-spec make_safe_expression(ExpressionText, Depth) -> SafeExpression when
    ExpressionText :: binary(),
    Depth :: non_neg_integer(),
    SafeExpression :: binary().
make_safe_expression(ExpressionText, Depth) ->
    SocketVarName = get_socket_var_name(Depth),
    re:replace(ExpressionText, ~"\\bSocket\\b", SocketVarName, [
        global, {return, binary}
    ]).

%% Create a function with depth-specific socket variable to avoid nesting shadowing
-spec create_socket_threaded_function(binary(), expression_norm_callback(), non_neg_integer()) ->
    binary().
create_socket_threaded_function(ExpressionText, ExpressionTextNormCallback, Depth) ->
    SocketVarName = get_socket_var_name(Depth),
    SafeExpression = make_safe_expression(ExpressionText, Depth),
    apply(ExpressionTextNormCallback, [SocketVarName, SafeExpression]).

%% Standard expression normalization callback for regular templates
-spec standard_expression_norm_callback() -> expression_norm_callback().
standard_expression_norm_callback() ->
    fun(SocketVarName, SafeExpression) ->
        iolist_to_binary(io_lib:format(~"""
        fun(~s) -> ~s end
        """, [SocketVarName, SafeExpression]))
    end.

%% List expression normalization callback for list templates with ItemFun
-spec list_expression_norm_callback(erl_parse:abstract_expr()) -> expression_norm_callback().
list_expression_norm_callback(ItemFun) ->
    fun(SocketVarName, SafeExpression) ->
        %% Extract the item variable name from the ItemFun AST
        [Clause] = erl_syntax:fun_expr_clauses(ItemFun),
        [FirstParameter] = erl_syntax:clause_patterns(Clause),
        ItemVarName = atom_to_binary(erl_syntax:variable_name(FirstParameter), utf8),

        iolist_to_binary(io_lib:format(~"""
        fun(~s, ~s) -> ~s end
        """, [ItemVarName, SocketVarName, SafeExpression]))
    end.

%% Format static element as binary string
-spec format_static_element(pos_integer(), binary()) -> binary().
format_static_element(Line, Content) ->
    iolist_to_binary(io_lib:format("{static, ~p, ~p}", [Line, Content])).

%% Format dynamic element with depth-specific socket variable
-spec format_dynamic_element(
    pos_integer(), binary(), expression_norm_callback(), compile_options(), non_neg_integer()
) ->
    binary().
format_dynamic_element(Line, ExpressionText, ExpressionTextNormCallback, CompilerOptions, Depth) ->
    OptimizedExpressionText = optimize_dynamic_expression(ExpressionText, CompilerOptions, Depth),
    FunctionText = create_socket_threaded_function(
        OptimizedExpressionText, ExpressionTextNormCallback, Depth
    ),
    iolist_to_binary(io_lib:format("{dynamic, ~p, ~s}", [Line, FunctionText])).

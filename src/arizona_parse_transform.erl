-module(arizona_parse_transform).
-moduledoc ~"""
Arizona Template Parse Transform.

This module provides compile-time transformation of Arizona template syntax
into optimized structured formats for high-performance rendering.

Transformations:
- `arizona_html:render_stateless(~"template", Socket)` →
  `arizona_html:render_stateless([...], Socket)`
- `arizona_html:render_stateful(~"template", Socket)` →
  `arizona_html:render_stateful(#{...}, Socket)`
- Template expressions use `arizona_socket:get_binding/2` for variable access

Limitations:
- Only works with literal binary templates (compile-time determinable)
- Multi-line templates are not supported
- Runtime-constructed templates are not supported

The parse transform analyzes function calls at compile time and replaces
them with optimized versions that avoid runtime template parsing overhead.
""".

%% --------------------------------------------------------------------
%% API function exports
%% --------------------------------------------------------------------

-export([
    parse_transform/2,
    format_error/1,
    transform_stateful_to_ast/1,
    transform_stateless_to_ast/1
]).

%% --------------------------------------------------------------------
%% Types (and their exports)
%% --------------------------------------------------------------------

-doc ~"Abstract syntax tree node for parse transform processing.".
-type ast_node() :: erl_parse:abstract_form() | erl_parse:abstract_expr().
-export_type([ast_node/0]).

-doc ~"Template content with extracted string and line number information.".
-type template_content() :: {binary(), pos_integer()}.
-export_type([template_content/0]).

-doc ~"Compiler options passed to the parse transform.".
-type compile_options() :: [term()].
-export_type([compile_options/0]).

%% --------------------------------------------------------------------
%% API Functions
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
-spec parse_transform([erl_parse:abstract_form()], compile_options()) ->
    [erl_parse:abstract_form()].
parse_transform(AbstractSyntaxTrees, CompilerOptions) ->
    parse_transform_with_depth(AbstractSyntaxTrees, CompilerOptions, 0).

%% Parse transform with depth tracking for recursive optimization
-spec parse_transform_with_depth([erl_parse:abstract_form()], compile_options(), non_neg_integer()) ->
    [erl_parse:abstract_form()].
parse_transform_with_depth(AbstractSyntaxTrees, CompilerOptions, Depth) ->
    ModuleName = extract_module_name(AbstractSyntaxTrees),
    erl_syntax:revert_forms([
        erl_syntax_lib:map(
            fun(Node) ->
                transform_ast_node(erl_syntax:revert(Node), ModuleName, CompilerOptions, Depth)
            end,
            FormTree
        )
     || FormTree <- AbstractSyntaxTrees
    ]).

%% Format error messages for compilation diagnostics
-spec format_error(term()) -> string().
format_error(template_parse_failed) ->
    "Failed to parse Arizona template - invalid template syntax";
format_error(badarg) ->
    "Arizona parse transform requires literal binary templates, variables are not supported";
format_error(Other) ->
    io_lib:format("Unknown Arizona parse transform error: ~p", [Other]).

%% Transform stateful template result to optimized AST
-spec transform_stateful_to_ast(arizona_parser:stateful_result()) -> erl_syntax:syntaxTree().
transform_stateful_to_ast(#{
    elems_order := Order, elems := Elements, vars_indexes := VarsIndexes
}) ->
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

%% Transform stateless template result to optimized AST
-spec transform_stateless_to_ast(arizona_parser:stateless_result()) -> erl_syntax:syntaxTree().
transform_stateless_to_ast(StatelessList) when is_list(StatelessList) ->
    %% Convert stateless list to AST representation
    ListItems = [
        case Item of
            Bin when is_binary(Bin) ->
                erl_syntax:binary([
                    erl_syntax:binary_field(erl_syntax:string(binary_to_list(Bin)))
                ]);
            _ ->
                erl_syntax:abstract(Item)
        end
     || Item <- StatelessList
    ],
    erl_syntax:list(ListItems).

%% --------------------------------------------------------------------
%% Internal Functions
%% --------------------------------------------------------------------

%% Extract module name from the abstract syntax tree forms
-spec extract_module_name([erl_parse:abstract_form()]) -> atom().
extract_module_name([{attribute, _Anno, module, ModuleName} | _Rest]) ->
    ModuleName;
extract_module_name([_Form | Rest]) ->
    extract_module_name(Rest);
extract_module_name([]) ->
    unknown_module.

%% Transform an individual AST node with depth tracking
-spec transform_ast_node(erl_parse:abstract_expr(), atom(), compile_options(), non_neg_integer()) ->
    erl_parse:abstract_expr().
transform_ast_node(AstNode, ModuleName, CompilerOptions, Depth) ->
    transform_template_calls(AstNode, ModuleName, CompilerOptions, Depth).

%% Template Call Recognition and Transformation

%% Transform arizona_html function calls with binary templates and depth tracking
-spec transform_template_calls(
    erl_parse:abstract_expr(), atom(), compile_options(), non_neg_integer()
) ->
    erl_parse:abstract_expr().

%% Transform stateless rendering calls
transform_template_calls(
    {call, CallAnnotations,
        {remote, _RemoteAnnotations, {atom, _ModuleAnnotations, arizona_html},
            {atom, _FunctionAnnotations, render_stateless}} = RemoteCall,
        [{bin, _BinaryAnnotations, _BinaryFields} = BinaryTemplate, SocketArg]},
    ModuleName,
    CompilerOptions,
    Depth
) ->
    transform_stateless_template_call(
        CallAnnotations, RemoteCall, BinaryTemplate, SocketArg, ModuleName, CompilerOptions, Depth
    );
transform_template_calls(
    {call, CallAnnotations,
        {remote, _RemoteAnnotations, {atom, _ModuleAnnotations, arizona_html},
            {atom, _FunctionAnnotations, render_stateless}},
        [_NonBinaryTemplate, _SocketArg]},
    ModuleName,
    _CompilerOptions,
    _Depth
) ->
    Line = erl_anno:line(CallAnnotations),
    % Non-binary template - raise badarg
    raise_template_error(badarg, ModuleName, Line);
%% Transform stateful rendering calls
transform_template_calls(
    {call, CallAnnotations,
        {remote, _RemoteAnnotations, {atom, _ModuleAnnotations, arizona_html},
            {atom, _FunctionAnnotations, render_stateful}} = RemoteCall,
        [{bin, _BinaryAnnotations, _BinaryFields} = BinaryTemplate, SocketArg]},
    ModuleName,
    CompilerOptions,
    Depth
) ->
    transform_stateful_template_call(
        CallAnnotations, RemoteCall, BinaryTemplate, SocketArg, ModuleName, CompilerOptions, Depth
    );
%% Handle arizona_html calls with non-binary templates (should error)
transform_template_calls(
    {call, CallAnnotations,
        {remote, _RemoteAnnotations, {atom, _ModuleAnnotations, arizona_html},
            {atom, _FunctionAnnotations, render_stateful}},
        [_NonBinaryTemplate, _SocketArg]},
    ModuleName,
    _CompilerOptions,
    _Depth
) ->
    Line = erl_anno:line(CallAnnotations),
    % Non-binary template - raise badarg
    raise_template_error(badarg, ModuleName, Line);
%% Not a function call we're interested in
transform_template_calls(AstNode, _ModuleName, _CompilerOptions, _Depth) ->
    AstNode.

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
        _Error:_Reason ->
            raise_template_error(template_parse_failed, ModuleName, Line)
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
                convert_element_to_iolist(Element, Acc, CompilerOptions, Depth)
            end,
            [],
            ParsedElements
        )
    ).

%% Convert individual element to iolist format with depth tracking
-spec convert_element_to_iolist(term(), [term()], compile_options(), non_neg_integer()) -> [term()].
convert_element_to_iolist({static, Line, StaticText}, Accumulator, _CompilerOptions, _Depth) ->
    StaticElement = format_static_element(Line, StaticText),
    [StaticElement | Accumulator];
convert_element_to_iolist({dynamic, Line, ExpressionText}, Accumulator, CompilerOptions, Depth) ->
    DynamicElement = format_dynamic_element(Line, ExpressionText, CompilerOptions, Depth),
    [DynamicElement | Accumulator].

%% Stateful Template Transformation

%% Transform render_stateful call with depth tracking
-spec transform_stateful_template_call(
    erl_anno:anno(),
    erl_parse:abstract_expr(),
    erl_parse:abstract_expr(),
    erl_parse:abstract_expr(),
    atom(),
    compile_options(),
    non_neg_integer()
) -> erl_parse:abstract_expr().
transform_stateful_template_call(
    CallAnnotations, RemoteCall, BinaryTemplate, SocketArg, ModuleName, CompilerOptions, Depth
) ->
    Line = erl_anno:line(CallAnnotations),
    try
        % Extract and parse the template at compile time
        {TemplateString, LineNumber} = extract_template_content(BinaryTemplate),
        TemplateDataMap = parse_template_for_stateful(
            TemplateString, LineNumber, CompilerOptions, Depth
        ),

        % Generate the new function call
        create_stateful_structured_call(CallAnnotations, RemoteCall, TemplateDataMap, SocketArg)
    catch
        _Error:_Reason ->
            raise_template_error(template_parse_failed, ModuleName, Line)
    end.

%% Parse template string into structured format with depth tracking
-spec parse_template_for_stateful(binary(), pos_integer(), compile_options(), non_neg_integer()) ->
    binary().
parse_template_for_stateful(TemplateString, LineNumber, CompilerOptions, Depth) ->
    TokenList = arizona_scanner:scan(#{line => LineNumber}, TemplateString),
    #{
        elems_order := ElementOrder,
        elems := ElementsMap,
        vars_indexes := VariableIndexes
    } = arizona_parser:parse_stateful_tokens(TokenList),

    build_template_data_structure(
        ElementOrder, ElementsMap, VariableIndexes, CompilerOptions, Depth
    ).

%% Build the template data structure with depth tracking
-spec build_template_data_structure(
    [integer()], map(), map(), compile_options(), non_neg_integer()
) -> binary().
build_template_data_structure(ElementOrder, ElementsMap, VariableIndexes, CompilerOptions, Depth) ->
    OrderString = format_element_order(ElementOrder),
    ElementsString = format_elements_map(ElementsMap, CompilerOptions, Depth),
    VariablesString = format_variables_indexes(VariableIndexes),

    iolist_to_binary([
        "#{",
        ["elems_order => [", OrderString, "], "],
        ["elems => #{", ElementsString, "}, "],
        ["vars_indexes => #{", VariablesString, "}"],
        "}"
    ]).

%% Template Data Formatting Helpers

%% Format element order as comma-separated string
-spec format_element_order([integer()]) -> string().
format_element_order(ElementOrder) ->
    lists:join(", ", lists:map(fun integer_to_binary/1, ElementOrder)).

%% Format elements map as key-value pairs with depth tracking
-spec format_elements_map(map(), compile_options(), non_neg_integer()) -> string().
format_elements_map(ElementsMap, CompilerOptions, Depth) ->
    ElementPairs = maps:fold(
        fun(K, V, Acc) -> format_element_entry(K, V, Acc, CompilerOptions, Depth) end,
        [],
        ElementsMap
    ),
    lists:join(", ", lists:reverse(ElementPairs)).

%% Format a single element entry with depth tracking for recursion
-spec format_element_entry(integer(), term(), [string()], compile_options(), non_neg_integer()) ->
    [string()].
format_element_entry(
    ElementIndex, {static, Line, StaticText}, Accumulator, _CompilerOptions, _Depth
) ->
    StaticElemText = format_static_element(Line, StaticText),
    FormattedEntry = iolist_to_binary(io_lib:format("~p => ~s", [ElementIndex, StaticElemText])),
    [FormattedEntry | Accumulator];
format_element_entry(
    ElementIndex, {dynamic, Line, ExpressionText}, Accumulator, CompilerOptions, Depth
) ->
    OptimizedExpressionText = optimize_dynamic_expression(ExpressionText, CompilerOptions, Depth),
    DynamicElemText = format_dynamic_element(Line, OptimizedExpressionText, CompilerOptions, Depth),
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

%% Create render_stateful function call with structured data
-spec create_stateful_structured_call(
    erl_anno:anno(),
    erl_parse:abstract_expr(),
    binary(),
    erl_parse:abstract_expr()
) -> erl_parse:abstract_expr().
create_stateful_structured_call(CallAnnotations, RemoteCall, TemplateDataBinary, SocketArg) ->
    TemplateDataForm = merl:quote(TemplateDataBinary),
    {call, CallAnnotations, RemoteCall, [TemplateDataForm, SocketArg]}.

%% Utility Functions

%% Extract template content and line number from binary AST node
-spec extract_template_content(erl_parse:abstract_expr()) -> {binary(), pos_integer()}.
extract_template_content({bin, BinaryAnnotations, _BinaryFields} = BinaryForm) ->
    {value, TemplateString, #{}} = erl_eval:expr(BinaryForm, #{}),
    LineNumber = erl_anno:line(BinaryAnnotations),
    {TemplateString, LineNumber}.

%% Helper function to raise template errors with proper error_info
-spec raise_template_error(atom(), atom(), pos_integer()) -> no_return().
raise_template_error(Reason, ModuleName, Line) ->
    error(Reason, none, error_info(ModuleName, Line)).

%% Create error_info for proper compiler diagnostics
-spec error_info(atom(), pos_integer()) -> [{error_info, map()}].
error_info(ModuleName, Line) ->
    [
        {error_info, #{
            cause => #{module => ModuleName, line => Line},
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
create_element_ast({dynamic, Line, ExprBinary}) ->
    %% Convert expression to optimized function AST
    %% ExprBinary is the original expression like "arizona_socket:get_binding(name, Socket)"
    %% Use depth 0 for runtime AST creation (not parse transform)
    FunctionBinary = create_socket_threaded_function(ExprBinary, 0),

    erl_syntax:tuple([
        erl_syntax:atom(dynamic),
        erl_syntax:integer(Line),
        merl:quote(FunctionBinary)
    ]).

%% Create AST for vars_indexes map
create_vars_indexes_map_ast(VarsIndexes) ->
    MapFields = [
        erl_syntax:map_field_assoc(
            erl_syntax:atom(binary_to_list(VarName)),
            erl_syntax:list([erl_syntax:integer(Idx) || Idx <- IndexList])
        )
     || VarName := IndexList <- VarsIndexes
    ],
    erl_syntax:map_expr(MapFields).

%% Get socket variable name with depth to avoid nesting shadowing
-spec get_socket_var_name(non_neg_integer()) -> binary().
get_socket_var_name(Depth) when Depth >= 0 ->
    iolist_to_binary([<<"_@Socket">>, integer_to_binary(Depth)]).

%% Create a function with depth-specific socket variable to avoid nesting shadowing
-spec create_socket_threaded_function(binary(), non_neg_integer()) -> binary().
create_socket_threaded_function(ExpressionText, Depth) ->
    SocketVarName = get_socket_var_name(Depth),
    %% Replace any existing Socket variable with safe socket name to avoid shadowing
    SafeExpression = re:replace(ExpressionText, <<"\\bSocket\\b">>, SocketVarName, [
        global, {return, binary}
    ]),
    <<"fun(", SocketVarName/binary, ") -> ", SafeExpression/binary, " end">>.

%% Format static element as binary string
-spec format_static_element(pos_integer(), binary()) -> binary().
format_static_element(Line, Content) ->
    iolist_to_binary(io_lib:format("{static, ~p, ~p}", [Line, Content])).

%% Format dynamic element with depth-specific socket variable
-spec format_dynamic_element(pos_integer(), binary(), compile_options(), non_neg_integer()) ->
    binary().
format_dynamic_element(Line, ExpressionText, _CompilerOptions, Depth) ->
    FunctionText = create_socket_threaded_function(ExpressionText, Depth),
    iolist_to_binary(io_lib:format("{dynamic, ~p, ~s}", [Line, FunctionText])).

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

-export([parse_transform/2]).
-export([format_error/1]).
-export([transform_stateful_to_ast/1]).
-export([transform_stateless_to_ast/1]).

%% --------------------------------------------------------------------
%% Ignore xref warnings
%% --------------------------------------------------------------------

-ignore_xref([parse_transform/2]).
-ignore_xref([format_error/1]).

%% --------------------------------------------------------------------
%% Types exports
%% --------------------------------------------------------------------

-export_type([ast_node/0]).
-export_type([template_content/0]).
-export_type([compile_options/0]).

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
Compiler options passed to the parse transform.

List of compiler options that can affect parse transform behavior.
""".
-type compile_options() :: [term()].

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
    parse_transform_with_depth(AbstractSyntaxTrees, CompilerOptions, 0).

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
    erl_syntax:revert_forms([
        erl_syntax_lib:map(
            fun(Node) ->
                transform_ast_node(erl_syntax:revert(Node), ModuleName, CompilerOptions, Depth)
            end,
            FormTree
        )
     || FormTree <- AbstractSyntaxTrees
    ]).

-doc ~"""
Format error messages for compilation diagnostics.

Converts parse transform error terms into human-readable string messages
for compiler error reporting.
""".
-spec format_error(Error) -> ErrorMessage when
    Error :: term(),
    ErrorMessage :: string().
format_error(template_parse_failed) ->
    "Failed to parse Arizona template - invalid template syntax";
format_error(badarg) ->
    "Arizona parse transform requires literal binary templates, variables are not supported";
format_error(Other) ->
    io_lib:format("Unknown Arizona parse transform error: ~p", [Other]).

-doc ~"""
Transform stateful template result to optimized AST.

Converts a parsed stateful template result into an optimized AST representation
that can be used for efficient runtime rendering.
""".
-spec transform_stateful_to_ast(StatefulResult) -> SyntaxTree when
    StatefulResult :: arizona_parser:stateful_result(),
    SyntaxTree :: erl_syntax:syntaxTree().
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
transform_template_calls(
    {call, CallAnnotations,
        {remote, _RemoteAnnotations, {atom, _ModuleAnnotations, arizona_html},
            {atom, _FunctionAnnotations, FunctionName}} = RemoteCall,
        Args},
    ModuleName,
    CompilerOptions,
    Depth
) ->
    transform_arizona_html_call(
        FunctionName, CallAnnotations, RemoteCall, Args, ModuleName, CompilerOptions, Depth
    );
transform_template_calls(AstNode, _ModuleName, _CompilerOptions, _Depth) ->
    AstNode.

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
    render_stateful, CallAnnotations, RemoteCall, Args, ModuleName, CompilerOptions, Depth
) ->
    transform_render_stateful_call(
        CallAnnotations, RemoteCall, Args, ModuleName, CompilerOptions, Depth
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
    raise_template_error(badarg, ModuleName, Line).

%% Transform render_stateful function calls
-spec transform_render_stateful_call(
    erl_anno:anno(),
    erl_parse:abstract_expr(),
    [erl_parse:abstract_expr()],
    atom(),
    compile_options(),
    non_neg_integer()
) -> erl_parse:abstract_expr().
transform_render_stateful_call(
    CallAnnotations,
    RemoteCall,
    [{bin, _BinaryAnnotations, _BinaryFields} = BinaryTemplate, SocketArg],
    ModuleName,
    CompilerOptions,
    Depth
) ->
    transform_stateful_template_call(
        CallAnnotations, RemoteCall, BinaryTemplate, SocketArg, ModuleName, CompilerOptions, Depth
    );
transform_render_stateful_call(
    CallAnnotations, _RemoteCall, _Args, ModuleName, _CompilerOptions, _Depth
) ->
    Line = erl_anno:line(CallAnnotations),
    raise_template_error(badarg, ModuleName, Line).

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
    raise_template_error(badarg, ModuleName, Line).

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
        _Error:_Reason ->
            raise_template_error(template_parse_failed, ModuleName, Line)
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
        _Error:_Reason ->
            raise_template_error(template_parse_failed, ModuleName, Line)
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
            elems := DynamicElements,
            vars_indexes := VariableIndexes
        }
    } = arizona_parser:parse_list_tokens(TokenList),

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
            erl_syntax:atom(binary_to_list(VarName)),
            erl_syntax:list([erl_syntax:integer(Idx) || Idx <- IndexList])
        )
     || VarName := IndexList <- VarsIndexes
    ],
    erl_syntax:map_expr(MapFields).

%% Get socket variable name with depth to avoid nesting shadowing
-spec get_socket_var_name(non_neg_integer()) -> binary().
get_socket_var_name(Depth) when Depth >= 0 ->
    iolist_to_binary([~"_@Socket", integer_to_binary(Depth)]).

%% Replace Socket variable with depth-specific variable to avoid shadowing
-spec make_safe_expression(binary(), non_neg_integer()) -> binary().
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

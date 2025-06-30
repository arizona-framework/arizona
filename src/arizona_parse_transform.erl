-module(arizona_parse_transform).
-moduledoc ~"""
Arizona Template Parse Transform.

This module provides compile-time transformation of Arizona template syntax
into optimized structured formats for high-performance rendering.

Transformations:
- `render_stateless(~"template", Socket)` → `render_stateless_iolist([...], Socket)`
- `render_stateful(~"template", Socket)` → `render_stateful(#{...}, Socket)`
- `render_list(~"template", Items, KeyFun)` → `arizona_list:render_list(...)`

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

-export([parse_transform/2, format_error/1]).

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
-type compile_options() :: [compile:option()].
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
looking for calls to arizona_template functions with literal binary
templates that can be optimized at compile time.
""".
-spec parse_transform([erl_parse:abstract_form()], compile_options()) ->
    [erl_parse:abstract_form()].
parse_transform(AbstractSyntaxTrees, _CompilerOptions) ->
    ModuleName = extract_module_name(AbstractSyntaxTrees),
    erl_syntax:revert_forms([
        erl_syntax_lib:map(
            fun(Node) ->
                transform_ast_node(erl_syntax:revert(Node), ModuleName)
            end,
            FormTree
        )
     || FormTree <- AbstractSyntaxTrees
    ]).

%% Format error messages for compilation diagnostics
-spec format_error(term()) -> string().
format_error(template_parse_failed) ->
    "Failed to parse Arizona template - invalid template syntax";
format_error(Other) ->
    io_lib:format("Unknown Arizona parse transform error: ~p", [Other]).

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

%% Transform an individual AST node, looking for template calls
-spec transform_ast_node(erl_parse:abstract_expr(), atom()) -> erl_parse:abstract_expr().
transform_ast_node(AstNode, ModuleName) ->
    transform_template_calls(AstNode, ModuleName).

%% Template Call Recognition and Transformation

%% Transform arizona_template function calls with binary templates
-spec transform_template_calls(erl_parse:abstract_expr(), atom()) -> erl_parse:abstract_expr().

%% Transform stateless rendering calls
transform_template_calls(
    {call, CallAnnotations,
        {remote, _RemoteAnnotations, {atom, _ModuleAnnotations, arizona_template},
            {atom, _FunctionAnnotations, render_stateless}} = RemoteCall,
        [{bin, _BinaryAnnotations, _BinaryFields} = BinaryTemplate, SocketArg]},
    ModuleName
) ->
    transform_stateless_template_call(
        CallAnnotations, RemoteCall, BinaryTemplate, SocketArg, ModuleName
    );
%% Transform stateful rendering calls
transform_template_calls(
    {call, CallAnnotations,
        {remote, _RemoteAnnotations, {atom, _ModuleAnnotations, arizona_template},
            {atom, _FunctionAnnotations, render_stateful}} = RemoteCall,
        [{bin, _BinaryAnnotations, _BinaryFields} = BinaryTemplate, SocketArg]},
    ModuleName
) ->
    transform_stateful_template_call(
        CallAnnotations, RemoteCall, BinaryTemplate, SocketArg, ModuleName
    );
%% Transform list rendering calls - 3-arity version
transform_template_calls(
    {call, CallAnnotations,
        {remote, _RemoteAnnotations, {atom, _ModuleAnnotations, arizona_template},
            {atom, _FunctionAnnotations, render_list}} = RemoteCall,
        [{bin, _BinaryAnnotations, _BinaryFields} = BinaryTemplate, ItemsArg, KeyFunArg]},
    ModuleName
) ->
    transform_list_template_call(
        CallAnnotations,
        RemoteCall,
        BinaryTemplate,
        ItemsArg,
        KeyFunArg,
        undefined,
        ModuleName
    );
%% Transform list rendering calls - 4-arity version
transform_template_calls(
    {call, CallAnnotations,
        {remote, _RemoteAnnotations, {atom, _ModuleAnnotations, arizona_template},
            {atom, _FunctionAnnotations, render_list}} = RemoteCall,
        [
            {bin, _BinaryAnnotations, _BinaryFields} = BinaryTemplate,
            ItemsArg,
            KeyFunArg,
            SocketArg
        ]},
    ModuleName
) ->
    transform_list_template_call(
        CallAnnotations,
        RemoteCall,
        BinaryTemplate,
        ItemsArg,
        KeyFunArg,
        SocketArg,
        ModuleName
    );
%% Not a function call we're interested in
transform_template_calls(AstNode, _ModuleName) ->
    AstNode.

%% Stateless Template Transformation

%% Transform render_stateless call with binary template to render_stateless_iolist
-spec transform_stateless_template_call(
    erl_anno:anno(),
    erl_parse:abstract_expr(),
    erl_parse:abstract_expr(),
    erl_parse:abstract_expr(),
    atom()
) -> erl_parse:abstract_expr().
transform_stateless_template_call(
    CallAnnotations, RemoteCall, BinaryTemplate, SocketArg, ModuleName
) ->
    Line = erl_anno:line(CallAnnotations),
    try
        % Extract and parse the template at compile time
        {TemplateString, LineNumber} = extract_template_content(BinaryTemplate),
        IoListStructure = parse_template_for_stateless(TemplateString, LineNumber),

        % Generate the new function call
        create_stateless_iolist_call(CallAnnotations, RemoteCall, IoListStructure, SocketArg)
    catch
        _Error:_Reason ->
            raise_template_error(template_parse_failed, ModuleName, Line)
    end.

%% Parse template string into iolist structure for stateless rendering
-spec parse_template_for_stateless(binary(), pos_integer()) -> [term()].
parse_template_for_stateless(TemplateString, LineNumber) ->
    TokenList = arizona_scanner:scan(#{line => LineNumber}, TemplateString),
    ParsedElements = arizona_parser:parse_stateless_tokens(TokenList),
    convert_to_iolist_format(ParsedElements).

%% Convert parsed elements to iolist format with proper function wrapping
-spec convert_to_iolist_format([term()]) -> [term()].
convert_to_iolist_format(ParsedElements) ->
    lists:reverse(lists:foldl(fun convert_element_to_iolist/2, [], ParsedElements)).

%% Convert individual element to iolist format
-spec convert_element_to_iolist(term(), [term()]) -> [term()].
convert_element_to_iolist({static, Line, StaticText}, Accumulator) ->
    StaticElement = iolist_to_binary(io_lib:format("{static, ~p, ~p}", [Line, StaticText])),
    [StaticElement | Accumulator];
convert_element_to_iolist({dynamic, Line, ExpressionText}, Accumulator) ->
    FunctionText = create_socket_threaded_function(ExpressionText),
    DynamicElement = iolist_to_binary(io_lib:format("{dynamic, ~p, ~s}", [Line, FunctionText])),
    [DynamicElement | Accumulator].

%% Stateful Template Transformation

%% Transform render_stateful call with binary template to structured format
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
        TemplateDataMap = parse_template_for_stateful(TemplateString, LineNumber),

        % Generate the new function call
        create_stateful_structured_call(CallAnnotations, RemoteCall, TemplateDataMap, SocketArg)
    catch
        _Error:_Reason ->
            raise_template_error(template_parse_failed, ModuleName, Line)
    end.

%% Parse template string into structured format for stateful rendering
-spec parse_template_for_stateful(binary(), pos_integer()) -> binary().
parse_template_for_stateful(TemplateString, LineNumber) ->
    TokenList = arizona_scanner:scan(#{line => LineNumber}, TemplateString),
    #{
        elems_order := ElementOrder,
        elems := ElementsMap,
        vars_indexes := VariableIndexes
    } = arizona_parser:parse_stateful_tokens(TokenList),

    build_template_data_structure(ElementOrder, ElementsMap, VariableIndexes).

%% Build the template data structure as a binary string for merl:quote
-spec build_template_data_structure([integer()], map(), map()) -> binary().
build_template_data_structure(ElementOrder, ElementsMap, VariableIndexes) ->
    OrderString = format_element_order(ElementOrder),
    ElementsString = format_elements_map(ElementsMap),
    VariablesString = format_variables_indexes(VariableIndexes),

    iolist_to_binary([
        "#{",
        "elems_order => [",
        OrderString,
        "], ",
        "elems => #{",
        ElementsString,
        "}, ",
        "vars_indexes => #{",
        VariablesString,
        "}",
        "}"
    ]).

%% Template Data Formatting Helpers

%% Format element order as comma-separated string
-spec format_element_order([integer()]) -> string().
format_element_order(ElementOrder) ->
    lists:join(", ", lists:map(fun integer_to_binary/1, ElementOrder)).

%% Format elements map as key-value pairs
-spec format_elements_map(map()) -> string().
format_elements_map(ElementsMap) ->
    ElementPairs = maps:fold(fun format_element_entry/3, [], ElementsMap),
    lists:join(", ", lists:reverse(ElementPairs)).

%% Format a single element entry
-spec format_element_entry(integer(), term(), [string()]) -> [string()].
format_element_entry(ElementIndex, {static, Line, StaticText}, Accumulator) ->
    FormattedEntry = iolist_to_binary(
        io_lib:format("~p => {static, ~p, ~p}", [ElementIndex, Line, StaticText])
    ),
    [FormattedEntry | Accumulator];
format_element_entry(ElementIndex, {dynamic, Line, ExpressionText}, Accumulator) ->
    FunctionText = create_socket_threaded_function(ExpressionText),
    FormattedEntry = iolist_to_binary(
        io_lib:format("~p => {dynamic, ~p, ~s}", [ElementIndex, Line, FunctionText])
    ),
    [FormattedEntry | Accumulator].

%% Format variable indexes map as key-value pairs
-spec format_variables_indexes(map()) -> string().
format_variables_indexes(VariableIndexes) ->
    VariablePairs = [format_variable_entry(V, I) || V := I <- VariableIndexes],
    lists:join(", ", lists:reverse(VariablePairs)).

%% Format a single variable index entry
-spec format_variable_entry(term(), [integer()]) -> string().
format_variable_entry(VariableName, IndexList) ->
    iolist_to_binary(io_lib:format("~p => ~p", [VariableName, IndexList])).

%% Function Call Generation

%% Create render_stateless_iolist function call
-spec create_stateless_iolist_call(
    erl_anno:anno(),
    erl_parse:abstract_expr(),
    [term()],
    erl_parse:abstract_expr()
) -> erl_parse:abstract_expr().
create_stateless_iolist_call(
    CallAnnotations,
    {remote, RemoteAnnotations, ModuleAtom, FunctionAtom},
    IoListStructure,
    SocketArg
) ->
    IoListBinary = iolist_to_binary(["[", lists:join(", ", IoListStructure), "]"]),
    IoListForm = merl:quote(IoListBinary),
    NewFunctionAtom = {atom, element(2, FunctionAtom), render_stateless_iolist},
    NewRemoteCall = {remote, RemoteAnnotations, ModuleAtom, NewFunctionAtom},
    {call, CallAnnotations, NewRemoteCall, [IoListForm, SocketArg]}.

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

%% List Template Transformation

%% Transform render_list call with binary template to arizona_list:render_list
%% with optimized template
-spec transform_list_template_call(
    erl_anno:anno(),
    erl_parse:abstract_expr(),
    erl_parse:abstract_expr(),
    erl_parse:abstract_expr(),
    erl_parse:abstract_expr(),
    erl_parse:abstract_expr() | undefined,
    atom()
) -> erl_parse:abstract_expr().
transform_list_template_call(
    CallAnnotations, _RemoteCall, BinaryTemplate, ItemsArg, KeyFunArg, SocketArg, ModuleName
) ->
    Line = erl_anno:line(CallAnnotations),
    try
        % Extract and parse the template at compile time
        {TemplateString, LineNumber} = extract_template_content(BinaryTemplate),
        ListTemplateStructure = parse_template_for_list(TemplateString, LineNumber),

        % Generate call to arizona_list:render_list with optimized template
        create_optimized_list_call(
            CallAnnotations, ListTemplateStructure, ItemsArg, KeyFunArg, SocketArg
        )
    catch
        _Error:_Reason ->
            raise_template_error(template_parse_failed, ModuleName, Line)
    end.

%% Parse template string into optimized list template structure
-spec parse_template_for_list(binary(), pos_integer()) -> binary().
parse_template_for_list(TemplateString, LineNumber) ->
    TokenList = arizona_scanner:scan(#{line => LineNumber}, TemplateString),
    ParsedElements = arizona_parser:parse_stateless_tokens(TokenList),
    convert_to_list_template_format(ParsedElements).

%% Convert parsed elements to template element list for arizona_list:create_list_template
-spec convert_to_list_template_format([term()]) -> binary().
convert_to_list_template_format(ParsedElements) ->
    % Convert to simple {static, Content} and {dynamic, ExpressionText} format
    % The actual function creation will happen at runtime
    Elements = lists:map(fun convert_element_for_list_template/1, ParsedElements),
    iolist_to_binary(io_lib:format("~p", [Elements])).

%% Convert individual element for list template
-spec convert_element_for_list_template(term()) -> term().
convert_element_for_list_template({static, _Line, Content}) ->
    {static, Content};
convert_element_for_list_template({dynamic, _Line, ExpressionText}) ->
    % Store the expression text - arizona_list will convert it to a function
    {dynamic, ExpressionText}.

%% Create optimized arizona_list:render_list call using arizona_list:create_list_template
-spec create_optimized_list_call(
    erl_anno:anno(),
    binary(),
    erl_parse:abstract_expr(),
    erl_parse:abstract_expr(),
    erl_parse:abstract_expr() | undefined
) -> erl_parse:abstract_expr().
create_optimized_list_call(CallAnnotations, ListTemplateElements, ItemsArg, KeyFunArg, SocketArg) ->
    % Parse the template elements back into AST form
    ElementsForm = merl:quote(ListTemplateElements),

    % Create call to arizona_list:create_list_template
    CreateTemplateCall =
        {call, CallAnnotations,
            {remote, CallAnnotations, {atom, CallAnnotations, arizona_list},
                {atom, CallAnnotations, create_list_template}},
            % Empty vars_indexes for now
            [ElementsForm, {map, CallAnnotations, []}]},

    % Create the remote call to arizona_list:render_list
    RemoteCall =
        {remote, CallAnnotations, {atom, CallAnnotations, arizona_list},
            {atom, CallAnnotations, render_list}},

    % Generate call with or without socket argument
    create_list_call_with_optional_socket(
        CallAnnotations, RemoteCall, CreateTemplateCall, ItemsArg, KeyFunArg, SocketArg
    ).

%% Utility Functions

%% Create list call with optional socket argument (DRY helper)
-spec create_list_call_with_optional_socket(
    erl_anno:anno(),
    erl_parse:abstract_expr(),
    erl_parse:abstract_expr(),
    erl_parse:abstract_expr(),
    erl_parse:abstract_expr(),
    erl_parse:abstract_expr() | undefined
) -> erl_parse:abstract_expr().
create_list_call_with_optional_socket(
    CallAnnotations, RemoteCall, FirstArg, ItemsArg, KeyFunArg, SocketArg
) ->
    case SocketArg of
        undefined ->
            {call, CallAnnotations, RemoteCall, [FirstArg, ItemsArg, KeyFunArg]};
        _SocketValue ->
            {call, CallAnnotations, RemoteCall, [FirstArg, ItemsArg, KeyFunArg, SocketArg]}
    end.

%% Extract template content and line number from binary AST node
-spec extract_template_content(erl_parse:abstract_expr()) -> {binary(), pos_integer()}.
extract_template_content({bin, BinaryAnnotations, _BinaryFields} = BinaryForm) ->
    {value, TemplateString, #{}} = erl_eval:expr(BinaryForm, #{}),
    LineNumber = erl_anno:line(BinaryAnnotations),
    {TemplateString, LineNumber}.

%% Create a function that receives Socket parameter, handling variable shadowing
-spec create_socket_threaded_function(binary()) -> binary().
create_socket_threaded_function(ExpressionText) ->
    % Replace any existing Socket variable with _@Socket to avoid shadowing
    SafeExpression = re:replace(ExpressionText, "\\bSocket\\b", "_@Socket", [
        global, {return, binary}
    ]),
    iolist_to_binary(io_lib:format("fun(_@Socket) -> ~s end", [SafeExpression])).

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

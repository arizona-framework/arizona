-module(arizona_parse_transform).
-moduledoc ~""""
Compile-time AST transformation for Arizona templates.

Erlang parse transform that converts Arizona template function calls into
optimized compile-time AST. Transforms `arizona_template:from_html/1`,
`arizona_template:from_markdown/1`, and `arizona_template:render_list/2` calls
for maximum runtime performance. Supports file-based template compilation.

## Transformations

- `arizona_template:from_html/1` → Compiled template record
- `arizona_template:from_markdown/1` → Compiled markdown template record
- `arizona_template:render_list/2` → Optimized list rendering with callbacks
- Prevents infinite recursion in nested template expressions
- Evaluates template strings and files at compile time for validation
- Supports file-based templates: `{file, "path.html"}`, `{priv_file, app, "template.html"}`

## Usage

Add to module compile options:

```erlang
-compile([{parse_transform, arizona_parse_transform}]).

render() ->
    arizona_template:from_html(~"""
    <h1>{Title}</h1>
    """).
    %% → Becomes compile-time optimized template record

render_from_file() ->
    arizona_template:from_html({file, "/path/to/template.html"}).
    %% → File content compiled at build time

render_from_priv() ->
    arizona_template:from_html({priv_file, myapp, "templates/page.html"}).
    %% → Priv file content compiled at build time
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
-export([transform_render_list/6]).
-export([transform_render_map/6]).
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

-elvis([{elvis_style, max_module_length, disable}]).
-elvis([{elvis_style, max_function_length, disable}]).
-elvis([{elvis_style, max_function_clause_length, disable}]).

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
Transforms `arizona_template:render_list/3` calls into optimized AST.

Extracts the template function body, compiles it into a template record,
and creates a call to `arizona_template:render_list_template/3`.
""".
-spec transform_render_list(Module, Line, FunArg, ListArg, OptionsArg, CompileOpts) -> AST when
    Module :: module(),
    Line :: arizona_token:line(),
    FunArg :: erl_syntax:syntaxTree(),
    ListArg :: erl_syntax:syntaxTree(),
    OptionsArg :: erl_syntax:syntaxTree(),
    CompileOpts :: [compile:option()],
    AST :: erl_syntax:syntaxTree().
transform_render_list(Module, Line, FunArg, ListArg, OptionsArg, CompileOpts) ->
    try
        % Extract the item parameter and template string from the function
        TemplateAST = extract_callback_function_body(
            Module, Line, FunArg, CompileOpts
        ),

        % Create application: arizona_template:render_list_template(Template, List, Options)
        TemplateModule = erl_syntax:atom(arizona_template),
        TemplateFunction = erl_syntax:atom(render_list_template),
        erl_syntax:application(
            erl_syntax:module_qualifier(TemplateModule, TemplateFunction), [
                TemplateAST, ListArg, OptionsArg
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
-spec transform_render_map(Module, Line, FunArg, MapArg, OptionsArg, CompileOpts) -> AST when
    Module :: module(),
    Line :: arizona_token:line(),
    FunArg :: erl_syntax:syntaxTree(),
    MapArg :: erl_syntax:syntaxTree(),
    OptionsArg :: erl_syntax:syntaxTree(),
    CompileOpts :: [compile:option()],
    AST :: erl_syntax:syntaxTree().
transform_render_map(Module, Line, FunArg, MapArg, OptionsArg, CompileOpts) ->
    try
        % Extract the item parameter and template string from the function
        % Reuse the same extraction logic as render_list since both use fun(Item) -> Template
        TemplateAST = extract_callback_function_body(
            Module, Line, FunArg, CompileOpts
        ),

        % Create application: arizona_template:render_map_template(Template, Map, Options)
        TemplateModule = erl_syntax:atom(arizona_template),
        TemplateFunction = erl_syntax:atom(render_map_template),
        erl_syntax:application(
            erl_syntax:module_qualifier(TemplateModule, TemplateFunction), [
                TemplateAST, MapArg, OptionsArg
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
                    TemplateString = extract_template_content(Module, TemplateArg),

                    % Scan template content into tokens
                    Tokens = arizona_scanner:scan_string(Line + 1, TemplateString),

                    % Clear in_dynamic_callback flag for render_list templates to allow
                    % nested transforms
                    ClearedOpts = proplists:delete(in_dynamic_callback, CompileOpts),
                    TemplateAST = arizona_parser:parse_tokens(Tokens, ClearedOpts),

                    % Wrap the dynamic tuple in fun(Item) -> ... end for render_list
                    wrap_dynamic_tuple_in_function(TemplateAST, RevClauseBody, CallbackArg);
                {arizona_template, new, 5, _Args} ->
                    % Already transformed (from_erl/from_html was transformed during AST walk)
                    % Just wrap the already-compiled template
                    wrap_dynamic_tuple_in_function(TemplateCall, RevClauseBody, CallbackArg);
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
        | arizona_erl_extraction_failed
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
format_error(arizona_erl_extraction_failed, [{_M, _F, _As, Info} | _]) ->
    {error_info, ErrorInfo} = proplists:lookup(error_info, Info),
    {Module, Line, Class, Reason, StackTrace} = maps:get(cause, ErrorInfo),
    #{
        general => "Arizona Erlang term template parsing failed",
        reason => io_lib:format(
            "Failed to convert Erlang terms to template in ~p at line ~p:\n~p:~p:~p", [
                Module, Line, Class, Reason, StackTrace
            ]
        )
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
    Filename = "/tmp/" ++ atom_to_list(Module) ++ ".erl",
    Content = [erl_pp:form(Form, [{linewidth, 100}]) || Form <- Forms],
    ok = file:write_file(Filename, unicode:characters_to_binary(Content)).
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
            transform_template_function(Node, Module, from_html, TemplateArg, CompileOpts);
        {arizona_template, from_markdown, 1, [MarkdownArg]} ->
            transform_template_function(Node, Module, from_markdown, MarkdownArg, CompileOpts);
        {arizona_template, from_erl, 1, [ErlTermArg]} ->
            transform_template_function(Node, Module, from_erl, ErlTermArg, CompileOpts);
        {arizona_template, render_list, 2, [FunArg, ListArg]} ->
            transform_render_collection(
                Node, Module, render_list, FunArg, ListArg, #{}, CompileOpts
            );
        {arizona_template, render_list, 3, [FunArg, ListArg, OptionsArg]} ->
            transform_render_collection(
                Node, Module, render_list, FunArg, ListArg, OptionsArg, CompileOpts
            );
        {arizona_template, render_map, 2, [FunArg, MapArg]} ->
            transform_render_collection(Node, Module, render_map, FunArg, MapArg, #{}, CompileOpts);
        {arizona_template, render_map, 3, [FunArg, MapArg, OptionsArg]} ->
            transform_render_collection(
                Node, Module, render_map, FunArg, MapArg, OptionsArg, CompileOpts
            );
        _ ->
            Node
    end.

%% Transform template functions (from_html, from_markdown, from_erl)
transform_template_function(Node, Module, FunctionName, TemplateArg, CompileOpts) ->
    Line = get_node_line(Node),
    case FunctionName of
        from_html ->
            transform_from_html(Module, Line, TemplateArg, CompileOpts);
        from_markdown ->
            transform_from_markdown(Module, Line, TemplateArg, CompileOpts);
        from_erl ->
            transform_from_erl(Module, Line, TemplateArg, CompileOpts)
    end.

%% Transform render collection functions (render_list, render_map)
transform_render_collection(
    Node, Module, FunctionName, FunArg, CollectionArg, OptionsArg, CompileOpts
) ->
    Line = get_node_line(Node),
    % Convert options to AST if it's a literal map
    OptionsAST =
        case OptionsArg of
            #{} -> erl_syntax:abstract(#{});
            _ -> OptionsArg
        end,
    case FunctionName of
        render_list ->
            transform_render_list(Module, Line, FunArg, CollectionArg, OptionsAST, CompileOpts);
        render_map ->
            transform_render_map(Module, Line, FunArg, CollectionArg, OptionsAST, CompileOpts)
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
        % Extract template content based on argument type
        String = extract_template_content(Module, TemplateArg),
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
        % Extract markdown content based on argument type
        Markdown = extract_template_content(Module, MarkdownArg),

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

%% Transform arizona_template:from_erl/1 calls
transform_from_erl(Module, Line, ErlTermArg, CompileOpts) ->
    try
        % Convert Erlang term AST to HTML string
        HTML = arizona_erl:ast_to_html(ErlTermArg),

        % Scan the HTML into tokens
        Tokens = arizona_scanner:scan_string(Line + 1, HTML),

        % Parse tokens into AST
        arizona_parser:parse_tokens(Tokens, CompileOpts)
    catch
        Class:Reason:StackTrace ->
            error(
                arizona_erl_extraction_failed,
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

%% Extract template content from various sources (string, file, priv_file)
-spec extract_template_content(Module, TemplateArg) -> String when
    Module :: module(),
    TemplateArg :: erl_syntax:syntaxTree(),
    String :: binary().
extract_template_content(Module, TemplateArg) ->
    case erl_syntax:type(TemplateArg) of
        tuple ->
            extract_from_tuple(Module, TemplateArg);
        _ ->
            % Direct string/binary content
            eval_expr(Module, TemplateArg)
    end.

%% Extract template content from tuple expressions
-spec extract_from_tuple(Module, TupleArg) -> String when
    Module :: module(),
    TupleArg :: erl_syntax:syntaxTree(),
    String :: binary().
extract_from_tuple(Module, TupleArg) ->
    Elements = erl_syntax:tuple_elements(TupleArg),
    case Elements of
        [FileAtom, FilenameArg] ->
            extract_from_two_tuple(Module, FileAtom, FilenameArg, TupleArg);
        [AppAtom, AppArg, FilenameArg] ->
            extract_from_three_tuple(Module, AppAtom, AppArg, FilenameArg, TupleArg);
        _ ->
            % Not a recognized tuple format, treat as regular expression
            eval_expr(Module, TupleArg)
    end.

%% Handle {file, Filename} tuples
-spec extract_from_two_tuple(Module, FileAtom, FilenameArg, TupleArg) -> String when
    Module :: module(),
    FileAtom :: erl_syntax:syntaxTree(),
    FilenameArg :: erl_syntax:syntaxTree(),
    TupleArg :: erl_syntax:syntaxTree(),
    String :: binary().
extract_from_two_tuple(Module, FileAtom, FilenameArg, TupleArg) ->
    case {erl_syntax:type(FileAtom), erl_syntax:atom_value(FileAtom)} of
        {atom, file} ->
            % {file, Filename} - read from absolute path
            Filename = eval_expr(Module, FilenameArg),
            read_file_content(Filename);
        _ ->
            % Not a recognized file tuple, treat as regular expression
            eval_expr(Module, TupleArg)
    end.

%% Handle {priv_file, App, Filename} tuples
-spec extract_from_three_tuple(Module, AppAtom, AppArg, FilenameArg, TupleArg) -> String when
    Module :: module(),
    AppAtom :: erl_syntax:syntaxTree(),
    AppArg :: erl_syntax:syntaxTree(),
    FilenameArg :: erl_syntax:syntaxTree(),
    TupleArg :: erl_syntax:syntaxTree(),
    String :: binary().
extract_from_three_tuple(Module, AppAtom, AppArg, FilenameArg, TupleArg) ->
    case {erl_syntax:type(AppAtom), erl_syntax:atom_value(AppAtom)} of
        {atom, priv_file} ->
            % {priv_file, App, Filename} - read from priv directory
            App = eval_expr(Module, AppArg),
            Filename = eval_expr(Module, FilenameArg),
            PrivFilename = get_priv_filename(App, Filename),
            read_file_content(PrivFilename);
        _ ->
            % Not a recognized priv_file tuple, treat as regular expression
            eval_expr(Module, TupleArg)
    end.

%% Read file content and return as binary
-spec read_file_content(Filename) -> Content when
    Filename :: file:filename_all(),
    Content :: binary().
read_file_content(Filename) ->
    case file:read_file(Filename) of
        {ok, Content} ->
            Content;
        {error, Reason} ->
            error({file_read_error, Filename, Reason})
    end.

%% Get priv directory filename
-spec get_priv_filename(App, Filename) -> PrivFilename when
    App :: atom(),
    Filename :: file:filename_all(),
    PrivFilename :: file:filename_all().
get_priv_filename(App, Filename) ->
    case code:priv_dir(App) of
        {error, Reason} ->
            error({priv_dir_error, App, Reason});
        PrivDir ->
            filename:join(PrivDir, Filename)
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

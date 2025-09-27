-module(arizona_template).
-moduledoc ~""""
Core template record and rendering abstraction.

Defines the template data structure and provides high-level rendering
interface that abstracts over different rendering modes. Templates can
be created at compile-time via parse transforms or at runtime.

## Template Structure

- `static` - List of static HTML text parts
- `dynamic` - Tuple of callback functions for dynamic expressions
- `dynamic_sequence` - Sequence numbers for dynamic parts
- `dynamic_anno` - Line number annotations for debugging
- `fingerprint` - Hash for template identity and caching

## API vs Template DSL Functions

**Regular API functions:**
- Template creation: `new/5`, `from_html/1`, `from_html/4`, `from_markdown/1`, `from_markdown/4`
- Type checking: `is_template/1`
- Accessors: `get_static/1`, `get_dynamic/1`, etc.
- Collection rendering: `render_list_template/2`, `render_map_template/2`

**Template DSL functions (only for use inside template strings):**
- Variable access: `get_binding/2`, `get_binding/3`, `find_binding/2`
- Components: `render_stateful/2`, `render_stateless/3`, `render_slot/1`
- Collections: `render_list/2`, `render_map/2`

## Example

```erlang
1> Template = arizona_template:from_html(~"""
.. <h1>{arizona_template:get_binding(title, Bindings)}</h1>
.. """).
#template{...}
2> {Html, View1} = arizona_renderer:render_template(Template, ParentId, View).
{[~\"<h1>\", ~\"Hello\", ~\"</h1>\"], UpdatedView}
```
"""".
-compile({nowarn_redefined_builtin_type, [dynamic/0]}).

%% --------------------------------------------------------------------
%% Ignore elvis warnings
%% --------------------------------------------------------------------

-elvis([{elvis_style, god_modules, disable}]).
-elvis([{elvis_style, max_module_length, disable}]).

%% --------------------------------------------------------------------
%% API function exports
%% --------------------------------------------------------------------

-export([new/5]).
-export([from_html/1]).
-export([is_template/1]).
-export([from_html/4]).
-export([from_markdown/1]).
-export([from_markdown/4]).
-export([get_static/1]).
-export([get_dynamic/1]).
-export([get_dynamic_sequence/1]).
-export([get_dynamic_anno/1]).
-export([get_fingerprint/1]).
-export([get_binding/2]).
-export([get_binding/3]).
-export([get_binding_lazy/3]).
-export([find_binding/2]).
-export([render_stateful/2]).
-export([render_stateful/3]).
-export([render_stateless/3]).
-export([render_stateless/4]).
-export([render_slot/1]).
-export([render_list/2]).
-export([render_list/3]).
-export([render_list_template/2]).
-export([render_list_template/3]).
-export([render_map/2]).
-export([render_map/3]).
-export([render_map_template/2]).
-export([render_map_template/3]).

%% --------------------------------------------------------------------
%% Ignore xref warnings
%% --------------------------------------------------------------------

-ignore_xref([new/5]).
-ignore_xref([from_html/1]).
-ignore_xref([from_html/4]).
-ignore_xref([from_markdown/1]).
-ignore_xref([from_markdown/4]).
-ignore_xref([get_dynamic_anno/1]).
-ignore_xref([get_fingerprint/1]).
-ignore_xref([get_binding/2]).
-ignore_xref([get_binding/3]).
-ignore_xref([get_binding_lazy/3]).
-ignore_xref([find_binding/2]).
-ignore_xref([render_stateful/2]).
-ignore_xref([render_stateful/3]).
-ignore_xref([render_stateless/3]).
-ignore_xref([render_stateless/4]).
-ignore_xref([render_slot/1]).
-ignore_xref([render_list/2]).
-ignore_xref([render_list/3]).
-ignore_xref([render_list_template/2]).
-ignore_xref([render_list_template/3]).
-ignore_xref([render_map/2]).
-ignore_xref([render_map/3]).
-ignore_xref([render_map_template/2]).
-ignore_xref([render_map_template/3]).

%% --------------------------------------------------------------------
%% Types exports
%% --------------------------------------------------------------------

-export_type([template/0]).
-export_type([static/0]).
-export_type([dynamic/0]).
-export_type([dynamic_tuple/0]).
-export_type([dynamic_tuple_callback/0]).
-export_type([dynamic_sequence/0]).
-export_type([dynamic_anno/0]).
-export_type([fingerprint/0]).
-export_type([render_callback/0]).
-export_type([render_mode/0]).
-export_type([render_options/0]).

%% --------------------------------------------------------------------
%% Types definitions
%% --------------------------------------------------------------------

-record(template, {
    static :: static(),
    dynamic :: dynamic(),
    dynamic_sequence :: dynamic_sequence(),
    dynamic_anno :: dynamic_anno(),
    fingerprint :: fingerprint()
}).

-opaque template() :: #template{}.
-nominal static() :: [binary()].
-nominal dynamic() :: dynamic_tuple() | dynamic_tuple_callback().
-nominal dynamic_tuple() :: tuple().
-nominal dynamic_tuple_callback() :: fun((CallbackArg :: term()) -> dynamic_tuple()).
-nominal dynamic_sequence() :: [pos_integer()].
-nominal dynamic_anno() :: tuple().
-nominal fingerprint() :: non_neg_integer().
-nominal render_callback() :: fun(
    (
        render_mode(),
        arizona_stateful:id(),
        arizona_tracker:element_index(),
        arizona_view:view()
    ) -> {dynamic(), arizona_view:view()}
).
-nominal render_mode() :: render | diff | hierarchical.
-nominal render_options() :: #{
    % false = skip diff updates for this component
    update => boolean()
}.

%% --------------------------------------------------------------------
%% API Functions
%% --------------------------------------------------------------------

-doc ~"""
Creates a new template record with all required components.

Usually called by the parse transform at compile time. Contains static
HTML parts, dynamic callback functions, and metadata for rendering.
""".
-spec new(Static, Dynamic, DynamicSequence, DynamicAnno, Fingerprint) -> Template when
    Static :: static(),
    Dynamic :: dynamic(),
    DynamicSequence :: dynamic_sequence(),
    DynamicAnno :: dynamic_anno(),
    Fingerprint :: fingerprint(),
    Template :: template().
new(Static, Dynamic, DynamicSequence, DynamicAnno, Fingerprint) ->
    #template{
        static = Static,
        dynamic = Dynamic,
        dynamic_sequence = DynamicSequence,
        dynamic_anno = DynamicAnno,
        fingerprint = Fingerprint
    }.

-doc #{equiv => from_html(erlang, 0, String, #{})}.
-spec from_html(Payload) -> Template when
    Payload :: {file, Filename} | {priv_file, App, Filename} | HTML,
    Filename :: file:filename_all(),
    App :: atom(),
    HTML :: arizona_html:html(),
    Template :: template().
from_html(String) ->
    from_html(erlang, 0, String, #{}).

-doc ~"""""
Compiles a template string into a template record at runtime.

This function provides compile-time template creation with full context
for debugging and bindings resolution. Used internally by the parse
transform system to create templates with proper metadata.

For best performance, use compile-time parse transforms instead.

## Template Syntax

Arizona templates embed Erlang expressions within HTML using curly braces:

- `{}` - Contains Erlang expressions evaluated at render time
- `\{` - Escaped opening brace (renders as literal `{`)
- Everything else - Treated as literal HTML text

## Expression Examples

> The following examples use `from_html/1` for simplicity.

### Simple variable access

```erlang
arizona_template:from_html(~"""
<h1>{arizona_template:get_binding(title, Bindings)}</h1>
""").
```

### Complex expressions with case statements

```erlang
arizona_template:from_html(~"""
<div class="todo {case maps:get(completed, Todo) of
      true -> ~"completed";
      false -> ~""
  end}">
""").
```

### Variable assignment within expressions

```erlang
arizona_template:from_html(~""""
{
    Todos = arizona_template:get_binding(todos, Bindings),
    Filter = arizona_template:get_binding(filter, Bindings),
    arizona_template:render_list(fun(Todo) ->
    arizona_template:from_html(~"""
    <li>{maps:get(text, Todo)}</li>
    """)
    end, filter_todos(Todos, Filter))
}
"""").
```

### Conditional rendering

```erlang
arizona_template:from_html(~"""
{
    case length(TodoList) > 0 of
        true -> arizona_template:render_stateless(Module, render_footer, #{});
        false -> ~""
    end
}
""").
```

### Escaped braces in CSS

```erlang
arizona_template:from_html(~"""
<style>
    .nav-container \{
        display: flex;
    }
</style>
""").
```

## Expression Context

Within `{}` expressions, you have access to:
- All standard Erlang functions, operators, and control structures
- The `Bindings` variable containing template data
- Template DSL functions like `get_binding/2`, `render_stateful/2`, `render_list/2`
- Local variables assigned within the same expression block
- Any functions from modules imported in the calling context

## Processing Pipeline

1. **Scanning** - Template string is tokenized into text and expression parts
2. **Parsing** - Tokens are parsed into an Abstract Syntax Tree (AST)
3. **Evaluation** - AST is evaluated to create the final template record
""""".
-spec from_html(Module, Line, Payload, Bindings) -> Template when
    Module :: module(),
    Line :: arizona_token:line(),
    Payload :: {file, Filename} | {priv_file, App, Filename} | HTML,
    Filename :: file:filename_all(),
    App :: atom(),
    HTML :: arizona_html:html(),
    Bindings :: arizona_binder:map(),
    Template :: template().
from_html(Module, Line, Payload, Bindings) when is_atom(Module), is_map(Bindings) ->
    % Extract template content from file, priv_file, or direct content
    HTML = extract_template_content(Payload),

    % Scan template content into tokens
    Tokens = arizona_scanner:scan_string(Line, HTML),

    % Parse tokens into AST
    AST = arizona_parser:parse_tokens(Tokens, []),

    % Evaluate AST to get template record
    erl_eval:expr(
        erl_syntax:revert(AST),
        #{'Bindings' => Bindings},
        {value, fun(Function, Args) ->
            apply(Module, Function, Args)
        end},
        none,
        value
    ).

-doc #{equiv => from_html(erlang, 0, String, #{})}.
-spec from_markdown(Payload) -> Template when
    Payload :: {file, Filename} | {priv_file, App, Filename} | Markdown,
    Filename :: file:filename_all(),
    App :: atom(),
    Markdown :: arizona_markdown:markdown(),
    Template :: template().
from_markdown(String) ->
    from_markdown(erlang, 0, String, #{}).

-doc ~"""""
Compiles a markdown string with Arizona template syntax into a template record at runtime.

This function processes markdown content through GitHub Flavored Markdown parser while
preserving Arizona template expressions (`{...}`) and Erlang comments (`%`).
The markdown is first converted to HTML, then processed as a regular template.

For best performance, use compile-time parse transforms instead.

## Markdown + Template Syntax

Arizona markdown templates combine standard GitHub Flavored Markdown with embedded
Erlang expressions:

- Standard markdown syntax (headers, emphasis, lists, tables, etc.)
- `{}` - Contains Erlang expressions evaluated at render time
- `%` - Erlang comments (preserved in final template)
- `\{` - Escaped opening brace (renders as literal `{`)

## Expression Examples

> The following examples use `from_markdown/1` for simplicity.

### Markdown with dynamic content

```erlang
arizona_template:from_markdown(~"""
# Welcome {arizona_template:get_binding(user, Bindings)}!

You have **{length(arizona_template:get_binding(todos, Bindings))}** tasks.

## Tasks
{arizona_template:render_list(fun(Todo) ->
    arizona_template:from_html(~"""
    - {maps:get(text, Todo)}
    """)
end, arizona_template:get_binding(todos, Bindings))}
""").
```

### Tables with dynamic data

```erlang
arizona_template:from_markdown(~""""
| Name | Score | Status |
|------|-------|--------|
{arizona_template:render_list(fun(Player) ->
    arizona_template:from_html(~"""
    | {maps:get(name, Player)} | {maps:get(score, Player)} | {maps:get(status, Player)} |
    """)
end, arizona_template:get_binding(players, Bindings))}
"""").
```

### Conditional markdown blocks

```erlang
arizona_template:from_markdown(~""""
# Dashboard

{case arizona_template:get_binding(user_type, Bindings) of
    admin ->
        ~"""
        ## Admin Panel
        - [Manage Users](/admin/users)
        - [View Reports](/admin/reports)
        """;
    user ->
        ~"""
        ## User Dashboard
        Welcome back! Check your recent activity below.
        """
end}
"""").
```

## Expression Context

Within `{}` expressions, you have access to:
- All standard Erlang functions, operators, and control structures
- The `Bindings` variable containing template data
- Template DSL functions like `get_binding/2`, `render_stateful/2`, `render_list/2`
- Local variables assigned within the same expression block
- Any functions from modules imported in the calling context

## Processing Pipeline

1. **Scanning** - Markdown content is tokenized into text and expression parts
2. **Protection** - Dynamic expressions and comments are protected from markdown processing
3. **Markdown Conversion** - Protected content is processed through GitHub Flavored Markdown
4. **Restoration** - Protected expressions are restored to original Arizona syntax
5. **Template Creation** - Final HTML is processed as a regular Arizona template
""""".
-spec from_markdown(Module, Line, Payload, Bindings) -> Template when
    Module :: module(),
    Line :: arizona_token:line(),
    Payload :: {file, Filename} | {priv_file, App, Filename} | Markdown,
    Filename :: file:filename_all(),
    App :: atom(),
    Markdown :: arizona_markdown:markdown(),
    Bindings :: arizona_binder:map(),
    Template :: template().
from_markdown(Module, Line, Payload, Bindings) when is_atom(Module), is_map(Bindings) ->
    % Extract template content from file, priv_file, or direct content
    Markdown = extract_template_content(Payload),

    % Process markdown content with Arizona template syntax
    HTML = arizona_markdown_processor:process_markdown_template(Markdown, Line),

    % Process final HTML as template
    from_html(Module, Line, HTML, Bindings).

-doc ~"""
Checks if the given value is a template record.

Type guard function for template validation.
""".
-spec is_template(dynamic()) -> boolean().
is_template(#template{}) -> true;
is_template(_) -> false.

-doc ~"""
Extracts static HTML parts from a template record.

Returns the list of static text segments used for template rendering.
""".
-spec get_static(Template) -> StaticContent when
    Template :: template(),
    StaticContent :: static().
get_static(#template{static = Static}) ->
    Static.

-doc ~"""
Extracts dynamic callback functions from a template record.

Returns the tuple of callbacks used for dynamic content evaluation.
""".
-spec get_dynamic(Template) -> DynamicContent when
    Template :: template(),
    DynamicContent :: dynamic().
get_dynamic(#template{dynamic = Dynamic}) ->
    Dynamic.

-doc ~"""
Extracts dynamic element sequence from a template record.

Returns the list of sequence numbers for processing dynamic elements.
""".
-spec get_dynamic_sequence(Template) -> DynamicSequence when
    Template :: template(),
    DynamicSequence :: dynamic_sequence().
get_dynamic_sequence(#template{dynamic_sequence = Sequence}) ->
    Sequence.

-doc ~"""
Extracts dynamic element annotations from a template record.

Returns line number annotations for debugging dynamic elements.
""".
-spec get_dynamic_anno(Template) -> DynamicAnno when
    Template :: template(),
    DynamicAnno :: dynamic_anno().
get_dynamic_anno(#template{dynamic_anno = Anno}) ->
    Anno.

-doc ~"""
Extracts template fingerprint for identity comparison.

Returns the hash used for template caching and diff comparisons.
""".
-spec get_fingerprint(Template) -> Fingerprint when
    Template :: template(),
    Fingerprint :: fingerprint().
get_fingerprint(#template{fingerprint = Fingerprint}) ->
    Fingerprint.

-doc ~"""
Retrieves a variable binding value with dependency tracking.

Template DSL function - only use inside `arizona_template:from_html/1` strings.
Records the variable dependency for differential updates and returns the bound value.
""".
-spec get_binding(Key, Bindings) -> Value when
    Key :: arizona_binder:key(),
    Bindings :: arizona_binder:bindings(),
    Value :: arizona_binder:value().
get_binding(Key, Bindings) ->
    % Record variable dependency for runtime tracking
    _OldTracker = arizona_tracker_dict:record_variable_dependency(Key),
    arizona_binder:get(Key, Bindings).

-doc ~"""
Retrieves a variable binding value with default and dependency tracking.

Template DSL function - only use inside `arizona_template:from_html/1` strings.
Returns the bound value or provided default value if key not found.
""".
-spec get_binding(Key, Bindings, Default) -> Value when
    Key :: arizona_binder:key(),
    Bindings :: arizona_binder:bindings(),
    Default :: arizona_binder:value(),
    Value :: arizona_binder:value().
get_binding(Key, Bindings, Default) ->
    % Record variable dependency for runtime tracking
    _OldTracker = arizona_tracker_dict:record_variable_dependency(Key),
    arizona_binder:get(Key, Bindings, Default).

-doc ~"""
Retrieves a variable binding value with lazy default function and dependency tracking.

Template DSL function - only use inside `arizona_template:from_html/1` strings.
Returns the bound value or calls default function if key not found. Useful for
expensive computations that should only be performed when needed.
""".
-spec get_binding_lazy(Key, Bindings, DefaultFun) -> Value when
    Key :: arizona_binder:key(),
    Bindings :: arizona_binder:bindings(),
    DefaultFun :: arizona_binder:default_fun(),
    Value :: arizona_binder:value().
get_binding_lazy(Key, Bindings, DefaultFun) ->
    % Record variable dependency for runtime tracking
    _OldTracker = arizona_tracker_dict:record_variable_dependency(Key),
    arizona_binder:get_lazy(Key, Bindings, DefaultFun).

-doc ~"""
Safely finds a variable binding value with dependency tracking.

Template DSL function - only use inside `arizona_template:from_html/1` strings.
Returns `{ok, Value}` if found, `error` if not found.
""".
-spec find_binding(Key, Bindings) -> {ok, Value} | error when
    Key :: arizona_binder:key(),
    Bindings :: arizona_binder:bindings(),
    Value :: arizona_binder:value().
find_binding(Key, Bindings) ->
    % Record variable dependency for runtime tracking
    _OldTracker = arizona_tracker_dict:record_variable_dependency(Key),
    arizona_binder:find(Key, Bindings).

-doc ~"""
Creates a stateful component rendering callback.

Template DSL function - only use inside `arizona_template:from_html/1` strings.
Returns a callback that handles all three rendering modes for the component.
""".
-spec render_stateful(Module, Bindings) -> Callback when
    Module :: module(),
    Bindings :: arizona_binder:map(),
    Callback :: render_callback().
render_stateful(Module, Bindings) ->
    render_stateful(Module, Bindings, #{}).

-doc ~"""
Creates a stateful component rendering callback with options.

Template DSL function - only use inside `arizona_template:from_html/1` strings.
Returns a callback that handles all three rendering modes for the component.

See `t:render_options/0` for available options.
""".
-spec render_stateful(Module, Bindings, Options) -> Callback when
    Module :: module(),
    Bindings :: arizona_binder:map(),
    Options :: render_options(),
    Callback :: render_callback().
render_stateful(Module, Bindings, Options) ->
    fun
        (render, _ParentId, _ElementIndex, View) ->
            arizona_renderer:render_stateful(Module, Bindings, View);
        (diff, ParentId, ElementIndex, View) ->
            case Options of
                #{update := false} ->
                    {nodiff, View};
                #{} ->
                    arizona_differ:diff_stateful(Module, Bindings, ParentId, ElementIndex, View)
            end;
        (hierarchical, ParentId, ElementIndex, View) ->
            arizona_hierarchical:hierarchical_stateful(
                Module, Bindings, ParentId, ElementIndex, View
            )
    end.

-doc ~"""
Creates a stateless component rendering callback.

Template DSL function - only use inside `arizona_template:from_html/1` strings.
Returns a callback that handles all three rendering modes for the component.
""".
-spec render_stateless(Module, Function, Bindings) -> Callback when
    Module :: module(),
    Function :: atom(),
    Bindings :: arizona_binder:map(),
    Callback :: render_callback().
render_stateless(Module, Fun, Bindings) ->
    render_stateless(Module, Fun, Bindings, #{}).

-doc ~"""
Creates a stateless component rendering callback with options.

Template DSL function - only use inside `arizona_template:from_html/1` strings.
Returns a callback that handles all three rendering modes for the component.

See `t:render_options/0` for available options.
""".
-spec render_stateless(Module, Function, Bindings, Options) -> Callback when
    Module :: module(),
    Function :: atom(),
    Bindings :: arizona_binder:map(),
    Options :: render_options(),
    Callback :: render_callback().
render_stateless(Module, Fun, Bindings, Options) ->
    fun
        (render, ParentId, _ElementIndex, View) ->
            arizona_renderer:render_stateless(Module, Fun, Bindings, ParentId, View);
        (diff, ParentId, ElementIndex, View) ->
            case Options of
                #{update := false} ->
                    {nodiff, View};
                #{} ->
                    arizona_differ:diff_stateless(
                        Module, Fun, Bindings, ParentId, ElementIndex, View
                    )
            end;
        (hierarchical, ParentId, ElementIndex, View) ->
            arizona_hierarchical:hierarchical_stateless(
                Module, Fun, Bindings, ParentId, ElementIndex, View
            )
    end.

-doc ~"""
Renders a slot with view, template, or HTML content.

Template DSL function - only use inside `arizona_template:from_html/1` strings.
Handles different slot types: view (current view), template records, or HTML values.
""".
-spec render_slot(Slot) -> Callback when
    Slot :: view | template() | arizona_html:value(),
    Callback :: render_callback() | arizona_html:html().
render_slot(view) ->
    fun
        (render, _ParentId, _ElementIndex, View) ->
            arizona_renderer:render_view(View);
        (diff, _ParentId, _ElementIndex, View) ->
            arizona_differ:diff_view(View);
        (hierarchical, _ParentId, _ElementIndex, View) ->
            arizona_hierarchical:hierarchical_view(View)
    end;
render_slot(#template{} = Template) ->
    fun
        (render, ParentId, _ElementIndex, View) ->
            arizona_renderer:render_template(Template, ParentId, View);
        (diff, ParentId, ElementIndex, View) ->
            arizona_differ:diff_template(Template, ParentId, ElementIndex, slot, View);
        (hierarchical, ParentId, ElementIndex, View) ->
            arizona_hierarchical:hierarchical_template(Template, ParentId, ElementIndex, slot, View)
    end;
render_slot(Term) ->
    arizona_html:to_html(Term).

-doc ~"""
Creates a list rendering callback from an item template function.

Template DSL function - only use inside `arizona_template:from_html/1` strings.
At compile time, arizona_parse_transform converts this into an optimized
`render_list_template/2` call for better performance.

Requires `debug_info` when used at runtime without parse transform.
""".
-spec render_list(ItemCallback, List) -> Callback when
    ItemCallback :: fun((Item) -> arizona_template:template()),
    List :: [Item],
    Item :: dynamic(),
    Callback :: render_callback().
render_list(ItemCallback, List) ->
    render_list(ItemCallback, List, #{}).

-doc ~"""
Creates a list rendering callback from an item template function with options.

Template DSL function - only use inside `arizona_template:from_html/1` strings.
At compile time, arizona_parse_transform converts this into an optimized
`render_list_template/3` call for better performance.

See `t:render_options/0` for available options.

Requires `debug_info` when used at runtime without parse transform.
""".
-spec render_list(ItemCallback, List, Options) -> Callback when
    ItemCallback :: fun((Item) -> arizona_template:template()),
    List :: [Item],
    Item :: dynamic(),
    Options :: render_options(),
    Callback :: render_callback().
render_list(ItemCallback, List, Options) ->
    render_callback_collection(
        ItemCallback, List, Options, fun arizona_parse_transform:transform_render_list/6
    ).

-doc ~"""
Creates a list rendering callback from a compiled template.

Regular API function for rendering lists with pre-compiled templates.
Used internally by the parse transform optimization.
""".
-spec render_list_template(Template, List) -> Callback when
    Template :: template(),
    List :: [dynamic()],
    Callback :: render_callback().
render_list_template(#template{} = Template, List) ->
    render_list_template(Template, List, #{}).

-doc ~"""
Creates a list rendering callback from a compiled template with options.

Regular API function for rendering lists with pre-compiled templates.
Used internally by the parse transform optimization.

See `t:render_options/0` for available options.
""".
-spec render_list_template(Template, List, Options) -> Callback when
    Template :: template(),
    List :: [dynamic()],
    Options :: render_options(),
    Callback :: render_callback().
render_list_template(#template{} = Template, List, Options) ->
    fun
        (render, ParentId, _ElementIndex, View) ->
            arizona_renderer:render_list(Template, List, ParentId, View);
        (diff, ParentId, ElementIndex, View) ->
            case Options of
                #{update := false} ->
                    {nodiff, View};
                #{} ->
                    arizona_differ:diff_list(Template, List, ParentId, ElementIndex, View)
            end;
        (hierarchical, ParentId, ElementIndex, View) ->
            arizona_hierarchical:hierarchical_list(Template, List, ParentId, ElementIndex, View)
    end.

-spec render_map(ItemCallback, Map) -> Callback when
    ItemCallback :: fun((Item) -> arizona_template:template()),
    Map :: map(),
    Item :: {Key, Value},
    Key :: dynamic(),
    Value :: dynamic(),
    Callback :: render_callback().
render_map(ItemCallback, Map) ->
    render_map(ItemCallback, Map, #{}).

-doc ~"""
Creates a map rendering callback from an item template function with options.

Template DSL function - only use inside `arizona_template:from_html/1` strings.
At compile time, arizona_parse_transform converts this into an optimized
`render_map_template/3` call for better performance.

See `t:render_options/0` for available options.

Requires `debug_info` when used at runtime without parse transform.
""".
-spec render_map(ItemCallback, Map, Options) -> Callback when
    ItemCallback :: fun((Item) -> arizona_template:template()),
    Map :: map(),
    Item :: {Key, Value},
    Key :: dynamic(),
    Value :: dynamic(),
    Options :: render_options(),
    Callback :: render_callback().
render_map(ItemCallback, Map, Options) ->
    render_callback_collection(
        ItemCallback, Map, Options, fun arizona_parse_transform:transform_render_map/6
    ).

-spec render_map_template(Template, Map) -> Callback when
    Template :: template(),
    Map :: map(),
    Callback :: render_callback().
render_map_template(#template{} = Template, Map) ->
    render_map_template(Template, Map, #{}).

-doc ~"""
Creates a map rendering callback from a compiled template with options.

Regular API function for rendering maps with pre-compiled templates.
Used internally by the parse transform optimization.

See `t:render_options/0` for available options.
""".
-spec render_map_template(Template, Map, Options) -> Callback when
    Template :: template(),
    Map :: map(),
    Options :: render_options(),
    Callback :: render_callback().
render_map_template(#template{} = Template, Map, Options) ->
    fun
        (render, ParentId, _ElementIndex, View) ->
            arizona_renderer:render_map(Template, Map, ParentId, View);
        (diff, ParentId, ElementIndex, View) ->
            case Options of
                #{update := false} ->
                    {nodiff, View};
                #{} ->
                    arizona_differ:diff_map(Template, Map, ParentId, ElementIndex, View)
            end;
        (hierarchical, ParentId, ElementIndex, View) ->
            arizona_hierarchical:hierarchical_map(Template, Map, ParentId, ElementIndex, View)
    end.

%% --------------------------------------------------------------------
%% Internal helper functions
%% --------------------------------------------------------------------

%% Extract template content from various sources (string, file, priv_file)
-spec extract_template_content(Payload) -> Content when
    Payload :: {file, Filename} | {priv_file, App, Filename} | Content,
    Filename :: file:filename_all(),
    App :: atom(),
    Content :: arizona_html:html() | arizona_markdown:markdown().
extract_template_content({file, Filename}) ->
    read_file_content(Filename);
extract_template_content({priv_file, App, Filename}) ->
    case priv_filename(App, Filename) of
        {ok, PrivFilename} ->
            read_file_content(PrivFilename);
        {error, Reason} ->
            error({priv_dir_error, App, Reason})
    end;
extract_template_content(Content) when is_binary(Content); is_list(Content) ->
    Content.

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

priv_filename(App, Filename) ->
    case code:priv_dir(App) of
        {error, Reason} ->
            {error, Reason};
        PrivDir ->
            {ok, filename:join(PrivDir, Filename)}
    end.

%% Generic helper for render_list and render_map callbacks.
%%
%% Extracts function clauses from callback environment, converts to AST,
%% applies the specified transformation function, and evaluates the result.
%% Both render_list and render_map use this same pattern with different
%% transformation functions.
-spec render_callback_collection(ItemCallback, Collection, Options, TransformFun) -> Callback when
    ItemCallback :: fun((Item) -> arizona_template:template()),
    Collection :: [Item] | map(),
    Item :: dynamic(),
    Options :: render_options(),
    TransformFun :: fun(
        (
            module(),
            arizona_token:line(),
            erl_syntax:syntaxTree(),
            erl_syntax:syntaxTree(),
            render_options(),
            [compile:option()]
        ) -> erl_syntax:syntaxTree()
    ),
    Callback :: render_callback().
render_callback_collection(ItemCallback, Collection, Options, TransformFun) ->
    % Extract function clauses from the callback's environment
    % This requires the function to be compiled with debug_info
    case erlang:fun_info(ItemCallback, env) of
        {env, [{_, _, _, _, _, FunClauses}]} ->
            % Convert function clauses back to AST format
            FunArg = erl_syntax:revert(erl_syntax:fun_expr(FunClauses)),
            CollectionArg = merl:term(Collection),

            % Use the provided transformation function with options
            AST = TransformFun(erlang, 0, FunArg, CollectionArg, Options, []),

            % Evaluate the transformed AST to get the final render callback
            erl_eval:expr(
                erl_syntax:revert(AST),
                #{},
                {value, fun(Function, Args) ->
                    apply(erlang, Function, Args)
                end},
                none,
                value
            );
        Other ->
            error(
                {function_info_failed,
                    io_lib:format(
                        "Unable to extract environment from function. "
                        "This usually means the function was not compiled with debug_info or "
                        "is not a local function. Got: ~p",
                        [Other]
                    )}
            )
    end.

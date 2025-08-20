-module(arizona_template).
-compile({nowarn_redefined_builtin_type, [dynamic/0]}).

%% --------------------------------------------------------------------
%% API function exports
%% --------------------------------------------------------------------

-export([from_string/1]).
-export([is_template/1]).
-export([from_string/4]).
-export([get_static/1]).
-export([get_dynamic/1]).
-export([get_dynamic_sequence/1]).
-export([get_dynamic_anno/1]).
-export([get_fingerprint/1]).
-export([get_binding/2]).
-export([get_binding/3]).
-export([find_binding/2]).
-export([render_stateful/2]).
-export([render_stateless/3]).
-export([render_slot/1]).
-export([render_list/2]).
-export([render_list_template/2]).

%% --------------------------------------------------------------------
%% Ignore xref warnings
%% --------------------------------------------------------------------

-ignore_xref([from_string/1]).
-ignore_xref([from_string/4]).
-ignore_xref([get_dynamic_anno/1]).
-ignore_xref([get_fingerprint/1]).
-ignore_xref([get_binding/2]).
-ignore_xref([get_binding/3]).
-ignore_xref([find_binding/2]).
-ignore_xref([render_stateful/2]).
-ignore_xref([render_stateless/3]).
-ignore_xref([render_slot/1]).
-ignore_xref([render_list/2]).
-ignore_xref([render_list_template/2]).

%% --------------------------------------------------------------------
%% Types exports
%% --------------------------------------------------------------------

-export_type([template/0]).
-export_type([static/0]).
-export_type([dynamic/0]).
-export_type([dynamic_sequence/0]).
-export_type([dynamic_anno/0]).
-export_type([fingerprint/0]).
-export_type([render_callback/0]).
-export_type([render_mode/0]).

%% --------------------------------------------------------------------
%% Types definitions
%% --------------------------------------------------------------------

-record(template, {
    static :: static(),
    dynamic :: dynamic() | fun((CallbackArg :: term()) -> dynamic()),
    dynamic_sequence :: dynamic_sequence(),
    dynamic_anno :: dynamic_anno(),
    fingerprint :: fingerprint()
}).

-opaque template() :: #template{}.
-nominal static() :: [binary()].
-nominal dynamic() :: tuple().
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

%% --------------------------------------------------------------------
%% API Functions
%% --------------------------------------------------------------------

-spec from_string(String) -> Template when
    String :: binary(),
    Template :: template().
from_string(String) ->
    from_string(erlang, 0, String, #{}).

-spec from_string(Module, Line, String, Bindings) -> Template when
    Module :: module(),
    Line :: arizona_token:line(),
    String :: string() | binary(),
    Bindings :: arizona_binder:map(),
    Template :: template().
from_string(Module, Line, String, Bindings) when is_atom(Module), is_map(Bindings) ->
    % Scan template content into tokens
    Tokens = arizona_scanner:scan_string(Line, String),

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

-spec is_template(dynamic()) -> boolean().
is_template(#template{}) -> true;
is_template(_) -> false.

-spec get_static(Template) -> StaticContent when
    Template :: template(),
    StaticContent :: static().
get_static(#template{static = Static}) ->
    Static.

-spec get_dynamic(Template) -> DynamicContent when
    Template :: template(),
    DynamicContent :: dynamic().
get_dynamic(#template{dynamic = Dynamic}) ->
    Dynamic.

-spec get_dynamic_sequence(Template) -> DynamicSequence when
    Template :: template(),
    DynamicSequence :: dynamic_sequence().
get_dynamic_sequence(#template{dynamic_sequence = Sequence}) ->
    Sequence.

-spec get_dynamic_anno(Template) -> DynamicAnno when
    Template :: template(),
    DynamicAnno :: dynamic_anno().
get_dynamic_anno(#template{dynamic_anno = Anno}) ->
    Anno.

-spec get_fingerprint(Template) -> Fingerprint when
    Template :: template(),
    Fingerprint :: fingerprint().
get_fingerprint(#template{fingerprint = Fingerprint}) ->
    Fingerprint.

-spec get_binding(Key, Bindings) -> Value when
    Key :: arizona_binder:key(),
    Bindings :: arizona_binder:bindings(),
    Value :: arizona_binder:value().
get_binding(Key, Bindings) ->
    % Record variable dependency for runtime tracking
    _OldTracker = arizona_tracker_dict:record_variable_dependency(Key),
    arizona_binder:get(Key, Bindings).

-spec get_binding(Key, Bindings, Default) -> Value when
    Key :: arizona_binder:key(),
    Bindings :: arizona_binder:bindings(),
    Default :: arizona_binder:default_fun(),
    Value :: arizona_binder:value().
get_binding(Key, Bindings, Default) ->
    % Record variable dependency for runtime tracking
    _OldTracker = arizona_tracker_dict:record_variable_dependency(Key),
    arizona_binder:get(Key, Bindings, Default).

-spec find_binding(Key, Bindings) -> {ok, Value} | error when
    Key :: arizona_binder:key(),
    Bindings :: arizona_binder:bindings(),
    Value :: arizona_binder:value().
find_binding(Key, Bindings) ->
    % Record variable dependency for runtime tracking
    _OldTracker = arizona_tracker_dict:record_variable_dependency(Key),
    arizona_binder:find(Key, Bindings).

-spec render_stateful(Module, Bindings) -> Callback when
    Module :: module(),
    Bindings :: arizona_binder:map(),
    Callback :: render_callback().
render_stateful(Module, Bindings) ->
    fun
        (render, _ParentId, _ElementIndex, View) ->
            arizona_renderer:render_stateful(Module, Bindings, View);
        (diff, ParentId, ElementIndex, View) ->
            arizona_differ:diff_stateful(Module, Bindings, ParentId, ElementIndex, View);
        (hierarchical, ParentId, ElementIndex, View) ->
            arizona_hierarchical:hierarchical_stateful(
                Module, Bindings, ParentId, ElementIndex, View
            )
    end.

-spec render_stateless(Module, Function, Bindings) -> Callback when
    Module :: module(),
    Function :: atom(),
    Bindings :: arizona_binder:map(),
    Callback :: render_callback().
render_stateless(Module, Fun, Bindings) ->
    fun
        (render, ParentId, _ElementIndex, View) ->
            arizona_renderer:render_stateless(Module, Fun, Bindings, ParentId, View);
        (diff, ParentId, ElementIndex, View) ->
            arizona_differ:diff_stateless(Module, Fun, Bindings, ParentId, ElementIndex, View);
        (hierarchical, ParentId, ElementIndex, View) ->
            arizona_hierarchical:hierarchical_stateless(
                Module, Fun, Bindings, ParentId, ElementIndex, View
            )
    end.

-spec render_slot(Slot) -> Callback when
    Slot :: view | tuple(),
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
            arizona_differ:diff_template(Template, ParentId, ElementIndex, View);
        (hierarchical, ParentId, ElementIndex, View) ->
            arizona_hierarchical:hierarchical_template(Template, ParentId, ElementIndex, View)
    end;
render_slot(Term) ->
    arizona_html:to_html(Term).

-spec render_list(ItemCallback, List) -> Callback when
    ItemCallback :: fun((Item) -> arizona_template:template()),
    List :: [Item],
    Item :: dynamic(),
    Callback :: render_callback().
render_list(ItemCallback, List) ->
    % Extract function clauses from the callback's environment
    % This requires the function to be compiled with debug_info
    case erlang:fun_info(ItemCallback, env) of
        {env, [{_, _, _, _, _, FunClauses}]} ->
            % Convert function clauses back to AST format
            FunArg = erl_syntax:revert(erl_syntax:fun_expr(FunClauses)),
            ListArg = merl:term(List),

            % Use parse transform to process the function and create render_list_template call
            AST = arizona_parse_transform:transform_render_list(erlang, 0, FunArg, ListArg, []),

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

-spec render_list_template(Template, List) -> Callback when
    Template :: template(),
    List :: [dynamic()],
    Callback :: render_callback().
render_list_template(#template{} = Template, List) ->
    fun
        (render, ParentId, _ElementIndex, View) ->
            arizona_renderer:render_list(Template, List, ParentId, View);
        (diff, ParentId, ElementIndex, View) ->
            arizona_differ:diff_list(Template, List, ParentId, ElementIndex, View);
        (hierarchical, ParentId, ElementIndex, View) ->
            arizona_hierarchical:hierarchical_list(Template, List, ParentId, ElementIndex, View)
    end.

-module(arizona_template).
-compile({nowarn_redefined_builtin_type, [dynamic/0]}).

%% --------------------------------------------------------------------
%% API function exports
%% --------------------------------------------------------------------

-export([from_string/2]).
-export([from_string/4]).
-export([get_static/1]).
-export([get_dynamic/1]).
-export([get_dynamic_sequence/1]).
-export([get_dynamic_anno/1]).
-export([get_binding/2]).
-export([get_binding/3]).
-export([find_binding/2]).
-export([render_stateful/2]).
-export([render_stateless/3]).

%% --------------------------------------------------------------------
%% Types exports
%% --------------------------------------------------------------------

-export_type([template/0]).
-export_type([static/0]).
-export_type([dynamic/0]).
-export_type([dynamic_sequence/0]).
-export_type([dynamic_anno/0]).

%% --------------------------------------------------------------------
%% Types definitions
%% --------------------------------------------------------------------

-record(template, {
    static :: static(),
    dynamic :: dynamic(),
    dynamic_sequence :: dynamic_sequence(),
    dynamic_anno :: dynamic_anno()
}).

-opaque template() :: #template{}.
-nominal static() :: [binary()].
-nominal dynamic() :: tuple().
-nominal dynamic_sequence() :: [pos_integer()].
-nominal dynamic_anno() :: tuple().

%% --------------------------------------------------------------------
%% API Functions
%% --------------------------------------------------------------------

-spec from_string(String, Bindings) -> template() when
    String :: binary(),
    Bindings :: arizona_binder:bindings().
from_string(String, Bindings) ->
    from_string(erlang, 1, String, Bindings).

-spec from_string(Module, Line, String, Bindings) -> template() when
    Module :: module(),
    Line :: pos_integer(),
    String :: binary(),
    Bindings :: arizona_binder:bindings().
from_string(Module, Line, String, Bindings) when
    is_atom(Module), is_integer(Line), is_binary(String), is_map(Bindings)
->
    % Scan template content into tokens
    Tokens = arizona_scanner:scan_string(Line, String),

    % Parse tokens into AST
    AST = arizona_parser:parse_tokens(Tokens),

    % Evaluate AST to get template record
    erl_eval:expr(
        erl_syntax:revert(AST),
        Bindings#{'Bindings' => Bindings},
        {value, fun(Function, Args) ->
            apply(Module, Function, Args)
        end},
        none,
        value
    ).

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

-spec get_binding(Key, Bindings) -> Value when
    Key :: arizona_binder:key(),
    Bindings :: arizona_binder:bindings(),
    Value :: arizona_binder:value().
get_binding(Key, Bindings) ->
    % Record variable dependency for runtime tracking
    % Send cast to self (the live process)
    ok = gen_server:cast(self(), {record_variable_dependency, Key}),
    arizona_binder:get(Key, Bindings).

-spec get_binding(Key, Bindings, Default) -> Value when
    Key :: arizona_binder:key(),
    Bindings :: arizona_binder:bindings(),
    Default :: arizona_binder:default_fun(),
    Value :: arizona_binder:value().
get_binding(Key, Bindings, Default) ->
    ok = gen_server:cast(self(), {record_variable_dependency, Key}),
    arizona_binder:get(Key, Bindings, Default).

-spec find_binding(Key, Bindings) -> {ok, Value} | error when
    Key :: arizona_binder:key(),
    Bindings :: arizona_binder:bindings(),
    Value :: arizona_binder:value().
find_binding(Key, Bindings) ->
    % Record variable dependency for runtime tracking
    % Send cast to self (the live process)
    ok = gen_server:cast(self(), {record_variable_dependency, Key}),
    arizona_binder:find(Key, Bindings).

-spec render_stateful(Module, Bindings) -> Callback when
    Module :: module(),
    Bindings :: arizona_binder:bindings(),
    Callback :: fun((arizona_view:view()) -> {iodata() | term(), arizona_view:view()}).
render_stateful(Module, Bindings) ->
    fun(View) ->
        case arizona_view:get_render_mode(View) of
            render ->
                arizona_renderer:render_stateful(Module, Bindings, View);
            diff ->
                arizona_differ:diff_stateful(Module, Bindings, View);
            hierarchical ->
                arizona_hierarchical:hierarchical_stateful(Module, Bindings, View)
        end
    end.

-spec render_stateless(Module, Function, Bindings) -> Callback when
    Module :: module(),
    Function :: atom(),
    Bindings :: arizona_binder:bindings(),
    Callback :: fun((arizona_view:view()) -> {iodata() | term(), arizona_view:view()}).
render_stateless(Module, Fun, Bindings) ->
    fun(View) ->
        case arizona_view:get_render_mode(View) of
            render ->
                arizona_renderer:render_stateless(Module, Fun, Bindings, View);
            diff ->
                arizona_differ:diff_stateless(Module, Fun, Bindings, View);
            hierarchical ->
                arizona_hierarchical:hierarchical_stateless(Module, Fun, Bindings, View)
        end
    end.

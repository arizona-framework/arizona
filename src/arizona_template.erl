-module(arizona_template).

%% --------------------------------------------------------------------
%% API function exports
%% --------------------------------------------------------------------

-export([from_string/2]).
-export([from_string/4]).
-export([static/1]).
-export([dynamic/1]).
-export([dynamic_sequence/1]).
-export([dynamic_anno/1]).
-export([get_binding/2]).
-export([get_binding/3]).
-export([find_binding/2]).
-export([render_stateful/2]).
-export([render_stateless/3]).

%% --------------------------------------------------------------------
%% Types exports
%% --------------------------------------------------------------------

-export_type([template/0]).

%% --------------------------------------------------------------------
%% Types definitions
%% --------------------------------------------------------------------

-record(template, {
    static :: [StaticContent :: binary()],
    dynamic :: tuple(),
    dynamic_sequence :: [pos_integer()],
    dynamic_anno :: tuple()
}).

-opaque template() :: #template{}.

%% --------------------------------------------------------------------
%% API Functions
%% --------------------------------------------------------------------

-spec from_string(TemplateContent, Bindings) -> template() when
    TemplateContent :: binary(),
    Bindings :: arizona_binder:bindings().
from_string(TemplateContent, Bindings) ->
    from_string(erlang, 1, TemplateContent, Bindings).

-spec from_string(Module, Line, TemplateContent, Bindings) -> template() when
    Module :: module(),
    Line :: pos_integer(),
    TemplateContent :: binary(),
    Bindings :: arizona_binder:bindings().
from_string(Module, Line, TemplateContent, Bindings) when
    is_atom(Module), is_integer(Line), is_binary(TemplateContent), is_map(Bindings)
->
    % Scan template content into tokens
    Tokens = arizona_scanner:scan(#{line => Line}, TemplateContent),

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

-spec static(template()) -> [binary()].
static(#template{static = Static}) ->
    Static.

-spec dynamic(template()) -> tuple().
dynamic(#template{dynamic = Dynamic}) ->
    Dynamic.

-spec dynamic_sequence(template()) -> [pos_integer()].
dynamic_sequence(#template{dynamic_sequence = Sequence}) ->
    Sequence.

-spec dynamic_anno(template()) -> tuple().
dynamic_anno(#template{dynamic_anno = Anno}) ->
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
                arizona_template_renderer:render_stateful(Module, Bindings, View);
            diff ->
                arizona_template_differ:diff_stateful(Module, Bindings, View);
            hierarchical ->
                arizona_template_hierarchical:hierarchical_stateful(Module, Bindings, View)
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
                arizona_template_renderer:render_stateless(Module, Fun, Bindings, View);
            diff ->
                arizona_template_differ:diff_stateless(Module, Fun, Bindings, View);
            hierarchical ->
                arizona_template_hierarchical:hierarchical_stateless(Module, Fun, Bindings, View)
        end
    end.

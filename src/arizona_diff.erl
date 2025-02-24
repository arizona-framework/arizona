-module(arizona_diff).

%% --------------------------------------------------------------------
%% Support function exports
%% --------------------------------------------------------------------

-export([diff/4]).
-export([diff/6]).

%

-ignore_xref([diff/6]).

%% --------------------------------------------------------------------
%% Types (and their exports)
%% --------------------------------------------------------------------

-type diff() :: [{index(), arizona_renderer:rendered_value() | diff()}].
-export_type([diff/0]).

-type index() :: non_neg_integer().
-export_type([index/0]).

-type var() :: atom().
-export_type([var/0]).

-type token_callback() :: fun(() -> arizona_renderer:token()).
-export_type([token_callback/0]).

-type options() :: #{force_changed => boolean()}.
-export_type([options/0]).

%% --------------------------------------------------------------------
%% Support function definitions
%% --------------------------------------------------------------------

-spec diff(Payload, Index, View0, Socket0) -> {View1, Socket1} when
    Payload :: Token | RenderedValue,
    Token :: arizona_renderer:token(),
    RenderedValue :: arizona_renderer:rendered_value(),
    Index :: index(),
    View0 :: arizona_view:view(),
    Socket0 :: arizona_socket:socket(),
    View1 :: arizona_view:view(),
    Socket1 :: arizona_socket:socket().
diff({view_template, _Static, Dynamic}, _Index, View, Socket) ->
    diff_view_template(View, Socket, Dynamic);
diff({component_template, _Static, Dynamic}, _Index, View, Socket) ->
    diff_component_template(View, Socket, Dynamic);
diff({nested_template, _Static, Dynamic}, Index, ParentView, Socket) ->
    diff_nested_template(ParentView, Socket, Dynamic, Index);
diff({list_template, _Static, DynamicCallback, List}, Index, ParentView, Socket) ->
    diff_list_template(ParentView, Socket, DynamicCallback, List, Index);
diff({view, Mod, Bindings}, Index, ParentView, Socket) ->
    diff_view(ParentView, Socket, Mod, Bindings, Index);
diff({component, Mod, Fun, Bindings}, Index, ParentView, Socket) ->
    diff_component(ParentView, Socket, Mod, Fun, Bindings, Index);
diff({list, _Static, DynamicList}, Index, ParentView, Socket) ->
    diff_list(ParentView, Socket, DynamicList, Index);
diff(Diff, Index, View0, Socket) when is_binary(Diff); is_list(Diff) ->
    View = arizona_view:put_diff(Index, Diff, View0),
    {View, Socket}.

-spec diff(Index, Vars, TokenCallback, ViewAcc0, Socket0, Opts) -> {ViewAcc1, Socket1} when
    Index :: index(),
    Vars :: [var()],
    TokenCallback :: token_callback(),
    ViewAcc0 :: arizona_view:view(),
    Socket0 :: arizona_socket:socket(),
    Opts :: options(),
    ViewAcc1 :: arizona_view:view(),
    Socket1 :: arizona_socket:socket().
diff(Index, Vars, TokenCallback, ViewAcc, Socket, Opts) ->
    case maps:get(force_changed, Opts, false) of
        true ->
            diff_1(Index, TokenCallback, ViewAcc, Socket);
        false ->
            Bindings = arizona_view:bindings(ViewAcc),
            ChangedBindings = arizona_view:changed_bindings(ViewAcc),
            case changed(Bindings, ChangedBindings, Vars) of
                true ->
                    diff_1(Index, TokenCallback, ViewAcc, Socket);
                false ->
                    {ViewAcc, Socket}
            end
    end.

%% --------------------------------------------------------------------
%% Private functions
%% --------------------------------------------------------------------

diff_1(Index, TokenCallback, ViewAcc0, Socket0) ->
    Token = erlang:apply(TokenCallback, []),
    {ViewAcc, Socket} = diff(Token, Index, ViewAcc0, Socket0),
    {ViewAcc, Socket}.

changed(Bindings, ChangedBindings, Vars) ->
    lists:any(
        fun(Var) ->
            case ChangedBindings of
                #{Var := Value} ->
                    Value =/= maps:get(Var, Bindings);
                #{} ->
                    false
            end
        end,
        Vars
    ).

diff_view_template(View0, Socket0, Dynamic) ->
    {View1, Socket1} = diff_dynamic(Dynamic, View0, Socket0, #{}),
    View = arizona_view:merge_changed_bindings(View1),
    Socket = arizona_socket:put_view(View, Socket1),
    {View, Socket}.

diff_component_template(View0, Socket0, Dynamic) ->
    {View, Socket} = diff_dynamic(Dynamic, View0, Socket0, #{}),
    {View, Socket}.

diff_nested_template(ParentView0, Socket0, Dynamic, Index) ->
    Bindings = arizona_view:bindings(ParentView0),
    ChangedBindings = arizona_view:changed_bindings(ParentView0),
    View0 = arizona_view:new(undefined, Bindings, ChangedBindings, [], [], []),
    {View, Socket} = diff_dynamic(Dynamic, View0, Socket0, #{}),
    Diff = arizona_view:diff(View),
    ParentView = arizona_view:put_diff(Index, Diff, ParentView0),
    {ParentView, Socket}.

diff_list_template(ParentView0, Socket, Callback, List, Index) ->
    View = arizona_view:new(arizona_view:bindings(ParentView0)),
    Diff = diff_dynamic_list_callback(List, Callback, Index, View, Socket),
    ParentView = arizona_view:put_diff(Index, Diff, ParentView0),
    {ParentView, Socket}.

diff_dynamic_list_callback([], _Callback, _Index, _View, _Socket) ->
    [];
diff_dynamic_list_callback([Item | T], Callback, Index, View, Socket) ->
    Dynamic = erlang:apply(Callback, [Item]),
    {DiffView, _Socket} = diff(Dynamic, Index, View, Socket),
    [
        arizona_view:diff(DiffView)
        | diff_dynamic_list_callback(T, Callback, Index, View, Socket)
    ].

diff_view(ParentView, Socket, Mod, Bindings, Index) ->
    ViewId = maps:get(id, Bindings),
    case arizona_socket:get_view(ViewId, Socket) of
        {ok, View0} ->
            View = arizona_view:set_changed_bindings(Bindings, View0),
            diff_view_1(ParentView, Socket, Mod, Bindings, Index, View, ViewId);
        error ->
            mount_view(ParentView, Socket, Mod, Bindings, Index)
    end.

diff_view_1(ParentView0, Socket0, Mod, NewBindings, Index, View0, ViewId) ->
    {view_template, _Static, Dynamic} = arizona_view:render(View0),
    OldBindings = arizona_view:bindings(View0),
    ChangedBindings = view_changed_bindings(OldBindings, NewBindings),
    View1 = arizona_view:set_changed_bindings(ChangedBindings, View0),
    {View2, Socket1} = diff_dynamic(Dynamic, View1, Socket0, #{}),
    case ChangedBindings of
        #{id := _NewViewId} ->
            Socket2 = arizona_socket:remove_view(ViewId, Socket1),
            mount_view(ParentView0, Socket2, Mod, NewBindings, Index);
        #{} ->
            View3 = arizona_view:merge_changed_bindings(View2),
            Diff = arizona_view:diff(View3),
            ParentView = arizona_view:put_diff(Index, Diff, ParentView0),
            View = arizona_view:set_diff([], View3),
            Socket = arizona_socket:put_view(View, Socket1),
            {ParentView, Socket}
    end.

view_changed_bindings(OldBindings, NewBindings) ->
    maps:filter(
        fun(Key, Value) ->
            maps:get(Key, OldBindings) =/= Value
        end,
        NewBindings
    ).

mount_view(ParentView0, Socket0, Mod, Bindings, Index) ->
    case arizona_view:mount(Mod, Bindings, Socket0) of
        {ok, View0} ->
            Token = arizona_view:render(View0),
            Socket1 = arizona_socket:set_render_context(render, Socket0),
            {View1, Socket2} = arizona_renderer:render(Token, View0, ParentView0, Socket1),
            Rendered = arizona_view:tmp_rendered(View1),
            ParentView = arizona_view:put_diff(Index, Rendered, ParentView0),
            View2 = arizona_view:set_tmp_rendered([], View1),
            View = arizona_view:set_diff([], View2),
            Socket3 = arizona_socket:put_view(View, Socket2),
            Socket = arizona_socket:set_render_context(diff, Socket3),
            {ParentView, Socket};
        ignore ->
            {ParentView0, Socket0}
    end.

diff_component(ParentView0, Socket0, Mod, Fun, Bindings, Index) ->
    View0 = arizona_view:new(Bindings),
    {component_template, _Static, Dynamic} = arizona_component:render(Mod, Fun, View0),
    {View, Socket} = diff_dynamic(Dynamic, View0, Socket0, #{force_changed => true}),
    Diff = arizona_view:diff(View),
    ParentView = arizona_view:put_diff(Index, Diff, ParentView0),
    {ParentView, Socket}.

diff_list(ParentView0, Socket, DynamicList0, Index) ->
    View = arizona_view:new(arizona_view:bindings(ParentView0)),
    Diff = diff_dynamic_list(DynamicList0, View, Socket),
    ParentView = arizona_view:put_diff(Index, Diff, ParentView0),
    {ParentView, Socket}.

diff_dynamic_list([], _View, _Socket) ->
    [];
diff_dynamic_list([Dynamic | T], View, Socket) ->
    {DiffView, _Socket} = diff_dynamic(Dynamic, View, Socket, #{force_changed => true}),
    [arizona_view:diff(DiffView) | diff_dynamic_list(T, View, Socket)].

diff_dynamic([], View, Socket, _Opts) ->
    {View, Socket};
diff_dynamic([Callback | T], View0, Socket0, Opts) ->
    {View, Socket} = erlang:apply(Callback, [View0, Socket0, Opts]),
    diff_dynamic(T, View, Socket, Opts).

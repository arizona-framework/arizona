-module(arizona_diff).

%% --------------------------------------------------------------------
%% API function exports
%% --------------------------------------------------------------------

-export([diff/5]).

%

-ignore_xref([diff/5]).

%% --------------------------------------------------------------------
%% Types (and their exports)
%% --------------------------------------------------------------------

-type index() :: non_neg_integer().
-export_type([index/0]).

-type var() :: atom().
-export_type([var/0]).

-type tokens_callback() :: fun(() -> arizona_render:token()).
-export_type([tokens_callback/0]).

%% --------------------------------------------------------------------
%% API function definitions
%% --------------------------------------------------------------------

-spec diff(Index, Vars, TokensCallback, ViewAcc0, Socket0) -> {ViewAcc1, Socket1} when
    Index :: index(),
    Vars :: [var()],
    TokensCallback :: tokens_callback(),
    ViewAcc0 :: arizona_view:view(),
    Socket0 :: arizona_socket:socket(),
    ViewAcc1 :: arizona_view:view(),
    Socket1 :: arizona_socket:socket().
diff(Index, Vars, TokenCallback, ViewAcc0, Socket0) ->
    Assigns = arizona_view:assigns(ViewAcc0),
    ChangedAssigns = arizona_view:changed_assigns(ViewAcc0),
    case changed(Assigns, ChangedAssigns, Vars) of
        true ->
            Token = erlang:apply(TokenCallback, []),
            {ViewAcc, Socket} = diff(Token, Index, ViewAcc0, Socket0),
            {ViewAcc, Socket};
        false ->
            {ViewAcc0, Socket0}
    end.

%% --------------------------------------------------------------------
%% Private functions
%% --------------------------------------------------------------------

changed(Assigns, ChangedAssigns, Vars) ->
    lists:any(
        fun(Var) ->
            case ChangedAssigns of
                #{Var := Value} when Value =/= map_get(Var, Assigns) ->
                    true;
                #{} ->
                    false
            end
        end,
        Vars
    ).

diff({view_template, _Static, Dynamic}, _Index, View, Socket) ->
    diff_view_template(View, Socket, Dynamic);
diff({component_template, _Static, Dynamic}, _Index, View, Socket) ->
    diff_component_template(View, Socket, Dynamic);
diff({nested_template, _Static, Dynamic}, Index, ParentView, Socket) ->
    diff_nested_template(ParentView, Socket, Dynamic, Index);
diff({view, Mod, Assigns}, Index, ParentView, Socket) ->
    diff_view(ParentView, Socket, Mod, Assigns, Index);
diff({component, Mod, Fun, Assigns}, Index, ParentView, Socket) ->
    diff_component(ParentView, Socket, Mod, Fun, Assigns, Index);
diff(Diff, Index, View0, Socket) when is_binary(Diff); is_list(Diff) ->
    View = arizona_view:put_diff(Index, Diff, View0),
    {View, Socket}.

diff_view_template(View0, Socket0, Dynamic) ->
    {View1, Socket1} = diff_dynamic(Dynamic, View0, Socket0),
    View = arizona_view:merge_changed_assigns(View1),
    Socket = arizona_socket:put_view(View, Socket1),
    {View, Socket}.

diff_component_template(View0, Socket0, Dynamic) ->
    {View, Socket} = diff_dynamic(Dynamic, View0, Socket0),
    {View, Socket}.

diff_nested_template(ParentView0, Socket0, Dynamic, Index) ->
    Assigns = arizona_view:assigns(ParentView0),
    ChangedAssigns = arizona_view:changed_assigns(ParentView0),
    View0 = arizona_view:new(undefined, Assigns, ChangedAssigns, []),
    {View, Socket} = diff_dynamic(Dynamic, View0, Socket0),
    Diff = arizona_view:rendered(View),
    ParentView = arizona_view:put_diff(Index, Diff, ParentView0),
    {ParentView, Socket}.

diff_view(ParentView, Socket, Mod, Assigns, Index) ->
    ViewId = maps:get(id, Assigns),
    case arizona_socket:get_view(ViewId, Socket) of
        {ok, View} ->
            render_view(ParentView, Socket, Mod, Assigns, Index, View, ViewId);
        error ->
            mount_view(ParentView, Socket, Mod, Assigns, Index)
    end.

render_view(ParentView0, Socket0, Mod, NewAssigns, Index, View0, ViewId) ->
    {view_template, _Static, Dynamic} = arizona_view:render(Mod, View0),
    OldAssigns = arizona_view:assigns(View0),
    ChangedAssigns = view_changed_assigns(OldAssigns, NewAssigns),
    View1 = arizona_view:set_changed_assigns(ChangedAssigns, View0),
    {View2, Socket1} = diff_dynamic(Dynamic, View1, Socket0),
    case ChangedAssigns of
        #{id := _NewViewId} ->
            Socket2 = arizona_socket:remove_view(ViewId, Socket1),
            mount_view(ParentView0, Socket2, Mod, NewAssigns, Index);
        #{} ->
            View3 = arizona_view:merge_changed_assigns(View2),
            Rendered = arizona_view:rendered(View3),
            ParentView = arizona_view:put_diff(Index, Rendered, ParentView0),
            View = arizona_view:set_rendered([], View3),
            Socket = arizona_socket:put_view(View, Socket1),
            {ParentView, Socket}
    end.

view_changed_assigns(OldAssigns, NewAssigns) ->
    maps:filter(
        fun(Key, Value) ->
            maps:get(Key, OldAssigns) =/= Value
        end,
        NewAssigns
    ).

mount_view(ParentView0, Socket0, Mod, Assigns, Index) ->
    case arizona_view:mount(Mod, Assigns, Socket0) of
        {ok, View0} ->
            Token = arizona_view:render(Mod, View0),
            Socket1 = arizona_socket:set_render_context(render, Socket0),
            {View1, Socket2} = arizona_render:render(Token, View0, ParentView0, Socket1),
            Rendered = arizona_view:rendered(View1),
            ParentView = arizona_view:put_diff(Index, Rendered, ParentView0),
            View = arizona_view:set_rendered([], View1),
            Socket3 = arizona_socket:put_view(View, Socket2),
            Socket = arizona_socket:set_render_context(diff, Socket3),
            {ParentView, Socket};
        ignore ->
            {ParentView0, Socket0}
    end.

diff_component(ParentView0, Socket0, Mod, Fun, Assigns, Index) ->
    View0 = arizona_view:new(undefined, Assigns, Assigns, []),
    {component_template, _Static, Dynamic} = arizona_component:render(Mod, Fun, View0),
    {View, Socket} = diff_dynamic(Dynamic, View0, Socket0),
    Diff = arizona_view:rendered(View),
    ParentView = arizona_view:put_diff(Index, Diff, ParentView0),
    {ParentView, Socket}.

diff_dynamic([], View, Socket) ->
    {View, Socket};
diff_dynamic([Callback | T], View0, Socket0) ->
    {View, Socket} = erlang:apply(Callback, [View0, Socket0]),
    diff_dynamic(T, View, Socket).

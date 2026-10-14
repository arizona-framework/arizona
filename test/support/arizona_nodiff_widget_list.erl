-module(arizona_nodiff_widget_list).
-include("arizona_stateful.hrl").
-export([mount/1]).
-export([render/1]).
-export([handle_event/3]).

%% A plain list whose items are client-owned widgets (the `Widget` hook in
%% `arizona_layout`): each item is an az-nodiff region, so any change to the list
%% falls back to re-rendering it wholesale.

-spec mount(az:bindings()) -> az:mount_ret().
mount(Init) ->
    Bindings = #{
        id => ~"page",
        title => maps:get(title, Init, ~"NodiffWidgetList"),
        items => [~"a", ~"b"]
    },
    {Bindings, #{}}.

-spec render(az:bindings()) -> az:template().
render(Bindings) ->
    ?html(
        {main, [{id, ?get(id)}], [
            {button, [{id, ~"add"}, {az_click, arizona_js:push_event(~"add")}], [~"Add"]},
            {button, [{id, ~"drop"}, {az_click, arizona_js:push_event(~"drop")}], [~"Drop"]},
            {ul, [{id, ~"list"}], [
                ?each(
                    fun(Item) ->
                        {li, ['az-nodiff', {az_hook, ~"Widget"}, {data_kind, Item}], [Item]}
                    end,
                    ?get(items)
                )
            ]}
        ]}
    ).

-spec handle_event(az:event_name(), az:event_payload(), az:bindings()) ->
    az:handle_event_ret().
handle_event(~"add", _Payload, #{items := Items} = Bindings) ->
    Item = <<"i", (integer_to_binary(length(Items) + 1))/binary>>,
    {Bindings#{items => Items ++ [Item]}, #{}, []};
handle_event(~"drop", _Payload, #{items := Items} = Bindings) ->
    {Bindings#{items => lists:droplast(Items)}, #{}, []}.

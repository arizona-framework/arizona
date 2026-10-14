-module(arizona_nodiff_widget_list).
-include("arizona_stateful.hrl").
-export([mount/1]).
-export([render/1]).
-export([handle_event/3]).

%% A plain list whose items are client-owned widgets (the `Widget` hook in
%% `arizona_layout`): each item is an az-nodiff element, so any change to the list
%% re-renders it wholesale. The `keyed` route binding gives each item an `az_key`.

-spec mount(az:bindings()) -> az:mount_ret().
mount(Init) ->
    Bindings = #{
        id => ~"page",
        title => maps:get(title, Init, ~"NodiffWidgetList"),
        keyed => maps:get(keyed, Init, false),
        items => [~"a", ~"b"]
    },
    {Bindings, #{}}.

-spec render(az:bindings()) -> az:template().
render(Bindings) ->
    ?html(
        {main, [{id, ?get(id)}], [
            {button, [{id, ~"add"}, {az_click, arizona_js:push_event(~"add")}], [~"Add"]},
            {button, [{id, ~"drop"}, {az_click, arizona_js:push_event(~"drop")}], [~"Drop"]},
            {button, [{id, ~"drop-first"}, {az_click, arizona_js:push_event(~"drop_first")}], [
                ~"Drop first"
            ]},
            {button, [{id, ~"reverse"}, {az_click, arizona_js:push_event(~"reverse")}], [
                ~"Reverse"
            ]},
            {ul, [{id, ~"list"}], [
                case ?get(keyed) of
                    true ->
                        ?each(
                            fun(Item) ->
                                {li,
                                    [
                                        'az-nodiff',
                                        {az_hook, ~"Widget"},
                                        {data_kind, Item},
                                        {az_key, Item}
                                    ],
                                    [Item]}
                            end,
                            ?get(items)
                        );
                    false ->
                        ?each(
                            fun(Item) ->
                                {li, ['az-nodiff', {az_hook, ~"Widget"}, {data_kind, Item}], [Item]}
                            end,
                            ?get(items)
                        )
                end
            ]}
        ]}
    ).

-spec handle_event(az:event_name(), az:event_payload(), az:bindings()) ->
    az:handle_event_ret().
handle_event(~"add", _Payload, #{items := Items} = Bindings) ->
    Item = <<"i", (integer_to_binary(length(Items) + 1))/binary>>,
    {Bindings#{items => Items ++ [Item]}, #{}, []};
handle_event(~"drop", _Payload, #{items := Items} = Bindings) ->
    {Bindings#{items => lists:droplast(Items)}, #{}, []};
handle_event(~"drop_first", _Payload, #{items := [_ | Items]} = Bindings) ->
    {Bindings#{items => Items}, #{}, []};
handle_event(~"reverse", _Payload, #{items := Items} = Bindings) ->
    {Bindings#{items => lists:reverse(Items)}, #{}, []}.

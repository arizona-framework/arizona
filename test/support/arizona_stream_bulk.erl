-module(arizona_stream_bulk).
-include("arizona_stateful.hrl").
-export([mount/1, render/1]).

%% A keyed stream whose item template carries substantial STATICS against a tiny
%% dynamic value. That ratio is what makes a bulk change amplify: every
%% `?OP_INSERT` re-sends the statics, while one container re-render sends them
%% once. Paired with `arizona_stream_bulk_child`, which is this template plus a
%% `?stateful` child, so the two differ only in whether child views are present.

-spec mount(az:bindings()) -> az:mount_ret().
mount(Init) ->
    {#{id => maps:get(id, Init, ~"bulk"), items => maps:get(items, Init, [])}, #{}}.

-spec render(az:bindings()) -> az:template().
render(Bindings) ->
    ?html(
        {'div', [{id, ?get(id)}], [
            {ul, [{class, ~"bulk-list"}], [
                ?each(
                    fun(Item, Key) ->
                        {li,
                            [
                                {az_key, Key},
                                {class, ~"bulk-row bulk-row--wide bulk-row--striped"},
                                {~"data-role", ~"listitem"},
                                {~"data-section", ~"bulk-section-main"}
                            ],
                            [maps:get(label, Item)]}
                    end,
                    ?get(items)
                )
            ]}
        ]}
    ).

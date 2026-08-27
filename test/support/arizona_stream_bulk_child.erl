-module(arizona_stream_bulk_child).
-include("arizona_stateful.hrl").
-export([mount/1, render/1]).

%% Same item template as `arizona_stream_bulk`, plus a `?stateful` child per item.
%% dynamic value. That ratio is what makes a bulk change amplify: every
%% `?OP_INSERT` re-sends the statics, while one container re-render sends them
%% once. Paired with `arizona_stream_bulk_child`, which is this template plus a
%% `?stateful` child, so the two differ only in whether child views are present.

-spec mount(az:bindings()) -> az:mount_ret().
mount(Init) ->
    {#{id => maps:get(id, Init, ~"bulkc"), items => maps:get(items, Init, [])}, #{}}.

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
                                {class, <<
                                    "bulk-row bulk-row--wide bulk-row--striped "
                                    "bulk-row--compact bulk-row--bordered "
                                    "bulk-row--elevated bulk-row--interactive "
                                    "bulk-row--selectable bulk-row--dense bulk-row--framed"
                                >>},
                                {~"data-role", ~"listitem"},
                                {~"data-section", ~"bulk-section-main-primary-content-region"},
                                {~"data-testid", ~"bulk-row-item-element-under-test"}
                            ],
                            [
                                maps:get(label, Item),
                                ?stateful(arizona_counter, #{
                                    id => <<"c-", (Key)/binary>>, count => 0
                                })
                            ]}
                    end,
                    ?get(items)
                )
            ]}
        ]}
    ).

-module(arizona_opaque_nested).
-moduledoc """
Fixture: opaque command attributes (a `?get` the parse transform cannot prove is
a command) nested where only render-time observation can find them -- in a
conditional branch, in a plain-list `?each` item, and in an embedded `?stateful`
child (`arizona_opaque_nested_child`). Each uses a distinct `az-*` name, so a
render path that skips one observation drains a different name set.
""".
-include("arizona_stateful.hrl").

-export([mount/1]).
-export([render/1]).

-spec mount(az:bindings()) -> az:mount_ret().
mount(_Init) ->
    {
        #{
            id => ~"page",
            branch_cmd => arizona_js:push_event(~"branch"),
            item_cmd => arizona_js:push_event(~"item"),
            items => [~"a", ~"b"],
            show => true
        },
        #{}
    }.

-spec render(az:bindings()) -> az:template().
render(Bindings) ->
    ?html(
        {main, [{id, ?get(id)}], [
            case ?get(show) of
                true -> {p, [{az_focusin, ?get(branch_cmd)}], [~"branch"]};
                false -> <<>>
            end,
            {ul, [], [
                ?each(
                    fun(I) -> {li, [{az_keyup, ?get(item_cmd)}], [I]} end,
                    ?get(items)
                )
            ]},
            ?stateful(arizona_opaque_nested_child, #{
                id => ~"child", cmd => arizona_js:push_event(~"child")
            })
        ]}
    ).

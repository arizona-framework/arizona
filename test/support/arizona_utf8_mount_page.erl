-module(arizona_utf8_mount_page).
-include("arizona_stateful.hrl").
-export([mount/1]).
-export([render/1]).

%% Fixture: a page and its `?stateful` child (arizona_utf8_mount_child) that each
%% report their `mount/1` to the `notify` pid, so a suite can count the mounts the
%% dev-mode UTF-8 check costs a failing page. `text` feeds the page's slot and
%% `child_text` the child's.

-spec mount(az:bindings()) -> az:mount_ret().
mount(Bindings) ->
    Notify = maps:get(notify, Bindings),
    Notify ! {mounted, ?MODULE},
    {
        #{
            id => ~"mp",
            notify => Notify,
            text => maps:get(text, Bindings, ~"página"),
            child_text => maps:get(child_text, Bindings, ~"子")
        },
        #{}
    }.

-spec render(az:bindings()) -> az:template().
render(Bindings) ->
    ?html(
        {'div', [{id, ?get(id)}], [
            {p, [], [?get(text)]},
            ?stateful(arizona_utf8_mount_child, #{
                id => ~"mc", notify => ?get(notify), text => ?get(child_text)
            })
        ]}
    ).

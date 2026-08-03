-module(arizona_stream_siblings_parent).
-include("arizona_stateful.hrl").
-export([mount/1]).
-export([render/1]).

%% Embeds `arizona_stream_siblings` as a child view, so the child's dynamics diff
%% through the child-view path (`diff_child_dynamics/2` -> `make_ops/4` ->
%% `make_op/3`) -- the second of the two stream container full-render emitters.
%% The child's each slot is marker-only there too, so the op must be `?OP_TEXT`.

-spec mount(az:bindings()) -> az:mount_ret().
mount(Bindings) ->
    {
        #{
            id => ~"siblings-parent",
            title => maps:get(title, Bindings, ~"T"),
            items => maps:get(items, Bindings)
        },
        #{}
    }.

-spec render(az:bindings()) -> az:template().
render(Bindings) ->
    ?html(
        {'div', [{id, ?get(id)}], [
            ?stateful(arizona_stream_siblings, #{
                id => ~"siblings",
                title => ?get(title),
                items => ?get(items)
            })
        ]}
    ).

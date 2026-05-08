-module(arizona_bench_chain_b).
-moduledoc """
Bench fixture: middle stateful link in the 3-level chain
(view -> chain_b -> chain_c -> leaves). Embeds a `chain_c` stateful
child and adds a static heading -- enough mass to force a real child
snapshot allocation at this level.
""".
-include("arizona_stateful.hrl").

-export([mount/1]).
-export([render/1]).
-export([handle_update/2]).

-spec mount(az:bindings()) -> az:mount_ret().
mount(Bindings) ->
    {maps:merge(#{id => ~"chain_b"}, Bindings), #{}}.

-spec handle_update(az:bindings(), az:bindings()) -> az:handle_update_ret().
handle_update(Props, Bindings) ->
    {maps:merge(Bindings, Props), #{}}.

-spec render(az:bindings()) -> az:template().
render(Bindings) ->
    ?html(
        {'div', [{id, ?get(id)}, {class, ~"level-b"}], [
            {h2, [], [~"Level B"]},
            ?stateful(arizona_bench_chain_c, #{id => ~"chain_c"})
        ]}
    ).

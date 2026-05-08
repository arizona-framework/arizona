-module(arizona_bench_chain_a).
-moduledoc """
Bench fixture: top-level view that embeds one `arizona_bench_chain_b`
stateful child. First link in a 3-level stateful chain
(view -> chain_b -> chain_c -> ?each leaves) used to profile the
recursive `arizona_render:render_ssr_val/1` propagation through
nested stateful descriptors.
""".
-include("arizona_view.hrl").

-export([mount/2]).
-export([render/1]).

-spec mount(az:bindings(), az:request()) -> az:mount_ret().
mount(_Bindings, _Req) ->
    {#{id => ~"chain_a"}, #{}}.

-spec render(az:bindings()) -> az:template().
render(Bindings) ->
    ?html(
        {main, [{id, ?get(id)}], [
            ?stateful(arizona_bench_chain_b, #{id => ~"chain_b"})
        ]}
    ).

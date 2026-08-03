-module(arizona_twin_parent).
-include("arizona_stateful.hrl").
-export([mount/1]).
-export([render/1]).

%% Root embedding TWO instances of the SAME stateful handler. An element's `az`
%% is derived from its template's fingerprint, so both instances address their
%% slots with identical `az` values and only the view id tells them apart. A
%% push driven by `?send`/`?send_after` must therefore be scoped by the EMITTING
%% child's view id -- scoped by the root's, the client resolves the target on
%% the wrong twin.

-spec mount(az:bindings()) -> az:mount_ret().
mount(_Bindings) ->
    {#{id => ~"twins"}, #{}}.

-spec render(az:bindings()) -> az:template().
render(Bindings) ->
    ?html(
        {main, [{id, ?get(id)}], [
            ?stateful(arizona_toast, #{id => ~"twin_a"}),
            ?stateful(arizona_toast, #{id => ~"twin_b"})
        ]}
    ).

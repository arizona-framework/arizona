-module(arizona_restricted_id_child).
-include("arizona_stateful.hrl").
-export([mount/1]).
-export([render/1]).
-export([host/0]).

%% Deliberately buggy fixture: `mount/1` rewrites the framework-restricted `id`
%% binding to a value different from the incoming prop. Both the SSR and the live
%% stateful-child mount paths must reject it with `restricted_key_modified`, so a
%% mistake surfaces at SSR instead of only at WS connect.
-spec mount(az:bindings()) -> az:mount_ret().
mount(Bindings) ->
    {Bindings#{id => ~"rewritten"}, #{}}.

-spec render(az:bindings()) -> az:template().
render(Bindings) ->
    ?html({'div', [{id, ?get(id)}], [~"child"]}).

%% Parent template embedding the child with an explicit `id` prop the child then
%% (wrongly) rewrites in its own `mount/1`.
-spec host() -> az:template().
host() ->
    ?html({main, [], [?stateful(?MODULE, #{id => ~"c1"})]}).

-module(arizona_dropped_id_child).
-include("arizona_stateful.hrl").
-export([mount/1]).
-export([render/1]).
-export([host/0]).

%% Deliberately buggy fixture: `mount/1` builds its bindings without the
%% framework-restricted `id` the parent passed as a prop. Dropping the key is a
%% modification too, so both mount paths must reject it with
%% `restricted_key_modified`.
-spec mount(az:bindings()) -> az:mount_ret().
mount(Bindings) ->
    {#{label => maps:get(label, Bindings, ~"child")}, #{}}.

-spec render(az:bindings()) -> az:template().
render(Bindings) ->
    ?html({'div', [{id, ?get(id)}], [?get(label)]}).

%% Parent template embedding the child with an explicit `id` prop the child then
%% (wrongly) leaves out of its own `mount/1` result.
-spec host() -> az:template().
host() ->
    ?html({main, [], [?stateful(?MODULE, #{id => ~"c1"})]}).

-module(arizona_json_nested).
-include("arizona_stateful.hrl").
-export([mount/1]).
-export([render/1]).

%% JSON view embedding TWO stateful child counters. Each child has its
%% own handle_event, so tapping a child's button routes to THAT child's view id
%% -- proving true JSON nested event routing. (The multi example, by contrast,
%% routes per-region events to a single view.)

-spec mount(az:bindings()) -> az:mount_ret().
mount(_Bindings) ->
    {#{id => ~"json_nested"}, #{}}.

-spec render(az:bindings()) -> az:template().
render(Bindings) ->
    ?json(
        {'Column', [{id, ?get(id)}], [
            ?stateful(arizona_json_child_counter, #{id => ~"child_a", label => ~"A", count => 0}),
            ?stateful(arizona_json_child_counter, #{id => ~"child_b", label => ~"B", count => 0})
        ]}
    ).

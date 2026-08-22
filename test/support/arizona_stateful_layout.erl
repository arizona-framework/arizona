-module(arizona_stateful_layout).
-include("arizona_stateless.hrl").
-export([render/1]).

%% A layout that embeds a ?stateful directly. Rejected at render: a layout never
%% joins the live view tree, so the child's az-view marker would name a view the
%% server never registers.
-spec render(az:bindings()) -> az:template().
render(Bindings) ->
    ?html(
        {body, [], [
            ?stateful(arizona_counter, #{id => ~"chrome"}),
            ?inner_content
        ]}
    ).

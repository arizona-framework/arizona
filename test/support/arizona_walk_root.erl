-module(arizona_walk_root).
-moduledoc """
Walk fixture root: one compile-proven name of its own, a literal `?stateless`
dependency (transform-recorded), and a MANUAL `-arizona_az_deps` declaration
beside the transform's -- the walk must union every instance of the attribute,
which is what lets a handler extend the graph by hand.
""".
-include("arizona_stateless.hrl").

-arizona_az_deps([arizona_walk_extra]).

-export([render/1]).

-spec render(az:bindings()) -> az:template().
render(_Bindings) ->
    ?html(
        {'div', [{az_wheel, arizona_js:push_event(~"w")}], [
            ?stateless(arizona_walk_child, render, #{})
        ]}
    ).

-module(arizona_native_ssr_shapes).
-include("arizona_stateless.hrl").
-export([templates/0]).

%% One `?native` template per value shape and position, rendered with the same
%% bindings, for comparing the zipped document against what a client builds
%% from the wire.

-spec templates() -> [{atom(), az:template()}].
templates() ->
    Bindings = #{
        text => ~"he said \"hi\" <&> \\ ção",
        items => [~"a", ~"b"],
        none => [],
        flag => true,
        off => false,
        int => -5,
        float => 1.5,
        atom => foo,
        stream => arizona_stream:new(fun(X) -> X end, [~"x", ~"y"]),
        map => #{k1 => ~"v1", k2 => ~"v2"}
    },
    [{Name, Fun(Bindings)} || {Name, Fun} <- shapes()].

shapes() ->
    [
        {top, fun top/1},
        {scalars, fun scalars/1},
        {attrs, fun attrs/1},
        {branch, fun branch/1},
        {list_each, fun list_each/1},
        {empty_each, fun empty_each/1},
        {stream_each, fun stream_each/1},
        {map_each, fun map_each/1},
        {stateless_child, fun stateless_child/1},
        {nodiff, fun nodiff/1},
        {nodiff_empty_each, fun nodiff_empty_each/1},
        {list_fragment, fun list_fragment/1},
        {branch_fragment, fun branch_fragment/1}
    ].

top(Bindings) ->
    ?native({'Text', [], [?get(text)]}).

scalars(Bindings) ->
    ?native(
        {'Column', [], [
            {'Text', [], [?get(int)]},
            {'Text', [], [?get(float)]},
            {'Text', [], [?get(atom)]},
            {'Text', [], [?get(flag)]}
        ]}
    ).

attrs(Bindings) ->
    ?native(
        {'Text',
            [
                {color, ?get(text)},
                {size, ?get(int)},
                {enabled, ?get(flag)},
                {selected, ?get(off)},
                {visible, false}
            ],
            [
                ~"x"
            ]}
    ).

branch(Bindings) ->
    ?native(
        {'Column', [], [
            case ?get(flag) of
                true -> {'Text', [], [?get(text)]};
                false -> ~""
            end
        ]}
    ).

list_each(Bindings) ->
    ?native({'Column', [], [?each(fun(I) -> {'Text', [], [I]} end, ?get(items))]}).

empty_each(Bindings) ->
    ?native(
        {'Column', [], [
            {'Text', [], [~"a"]},
            ?each(fun(I) -> {'Text', [], [I]} end, ?get(none)),
            {'Text', [], [~"b"]}
        ]}
    ).

stream_each(Bindings) ->
    ?native(
        {'Column', [], [?each(fun(I, Key) -> {'Text', [{az_key, Key}], [I]} end, ?get(stream))]}
    ).

map_each(Bindings) ->
    ?native({'Column', [], [?each(fun(K, V) -> {'Text', [{key, K}], [V]} end, ?get(map))]}).

stateless_child(Bindings) ->
    ?native({'Column', [], [?stateless(fun card/1, #{label => ?get(text)})]}).

card(Bindings) ->
    ?native({'Text', [], [?get(label)]}).

nodiff(Bindings) ->
    ?native(
        {'Column', [az_nodiff], [
            {'Text', [], [?get(text)]},
            ?each(fun(I) -> {'Text', [], [I]} end, ?get(items))
        ]}
    ).

nodiff_empty_each(Bindings) ->
    ?native(
        {'Column', [az_nodiff], [
            {'Text', [], [~"a"]},
            ?each(fun(I) -> {'Text', [], [I]} end, ?get(none)),
            {'Text', [], [?get(text)]}
        ]}
    ).

%% A content-slot list mixing static text with an element compiles as a fragment; its
%% members are JSON array members, separated like an element's children.
list_fragment(Bindings) ->
    ?native({'Column', [], [[~"x", {'Text', [], [?get(text)]}]]}).

branch_fragment(Bindings) ->
    ?native(
        {'Column', [], [
            case ?get(flag) of
                true -> [~"x", {'Text', [], [?get(text)]}];
                false -> ~""
            end
        ]}
    ).

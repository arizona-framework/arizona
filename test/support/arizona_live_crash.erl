-module(arizona_live_crash).
-include("arizona_stateful.hrl").
-export([mount/1]).
-export([render/1]).
-export([handle_event/3]).
-export([line/1]).

%% Fixture: one slot per shape a live crash can sit in -- a top-level value, an
%% attribute, an `?each` item, a conditional branch, a `?stateless` body, an
%% `?each` source and the closure of a slot holding an az-nodiff element -- each fed
%% by its own binding, so `set` can break exactly one of them after mount.
%% Every binding defaults to a valid value; a crash raises `{boom, V}`.
%%
%% `line/1` counts down from its own line to the template, so a line inserted
%% between it and `render/1` shifts every pin and the location tests fail.

-spec mount(az:bindings()) -> az:mount_ret().
mount(Bindings) ->
    {
        #{
            id => ~"crash",
            top => maps:get(top, Bindings, 1),
            attr => maps:get(attr, Bindings, 1),
            items => maps:get(items, Bindings, [1]),
            branch => maps:get(branch, Bindings, 1),
            sless => maps:get(sless, Bindings, 1),
            source => maps:get(source, Bindings, [1]),
            nodiff => maps:get(nodiff, Bindings, 1)
        },
        #{}
    }.

-spec line(atom()) -> pos_integer().
line(top) -> ?LINE + 12;
line(attr) -> ?LINE + 12;
line(item) -> ?LINE + 12;
line(branch) -> ?LINE + 14;
line(sless) -> ?LINE + 26;
line(source) -> ?LINE + 15;
line(nodiff) -> ?LINE + 15.

-spec render(az:bindings()) -> az:template().
render(Bindings) ->
    ?html(
        {'div', [{id, ?get(id)}], [
            {p, [], [check(?get(top))]},
            {p, [{title, check(?get(attr))}], [~"a"]},
            {ul, [], [?each(fun(I) -> {li, [], [check(I)]} end, ?get(items))]},
            case ?get(branch) of
                none -> <<>>;
                B -> {p, [], [check(B)]}
            end,
            ?stateless(fun cell/1, #{v => ?get(sless)}),
            {ol, [], [?each(fun(I) -> {li, [], [I]} end, check_list(?get(source)))]},
            case check(?get(nodiff)) of
                ~"0" -> <<>>;
                _ -> {section, ['az-nodiff'], [~"r"]}
            end
        ]}
    ).

-spec cell(az:bindings()) -> az:template().
cell(Bindings) ->
    ?html({span, [], [check(?get(v))]}).

-spec handle_event(az:event_name(), az:event_payload(), az:bindings()) ->
    az:handle_event_ret().
handle_event(~"set", #{~"key" := Key, ~"value" := V}, Bindings) ->
    {Bindings#{binary_to_existing_atom(Key) => V}, #{}, []}.

check(V) when is_integer(V) -> integer_to_binary(V);
check(V) -> error({boom, V}).

check_list(L) when is_list(L) -> L;
check_list(V) -> error({boom, V}).

-module(arizona_stream_item_child_badge).
-include("arizona_stateful.hrl").

-export([mount/1]).
-export([handle_update/3]).
-export([render/1]).

%% The `?stateful` child of `arizona_stream_item_child`'s stream items. Its `label`
%% is a PROP, so a parent item update re-renders it and its inner dynamics differ.
-spec mount(az:bindings()) -> az:mount_ret().
mount(Bindings) ->
    {
        #{
            id => maps:get(id, Bindings),
            label => maps:get(label, Bindings, ~"")
        },
        #{}
    }.

-spec handle_update(az:bindings(), az:bindings(), az:effects()) -> az:handle_update_ret().
handle_update(Props, Bindings, Effects) ->
    {Bindings#{label => maps:get(label, Props, maps:get(label, Bindings))}, #{}, Effects}.

-spec render(az:bindings()) -> az:template().
render(Bindings) ->
    %% The conditional's element branch compiles to a NESTED TEMPLATE, so this slot's
    %% patch payload is an object (a `f`/`s`/`d` zip map) rather than a bare string.
    %% That is what makes the payload need resolving before it can be applied.
    ?html(
        {'span', [{id, ?get(id)}], [
            case ?get(label) of
                ~"" -> <<>>;
                Label -> {em, [], [~"badge:", Label]}
            end
        ]}
    ).

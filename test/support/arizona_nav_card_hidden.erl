-module(arizona_nav_card_hidden).
-include("arizona_stateful.hrl").
-export([mount/1, render/1, handle_info/2]).

%% Mounts WITHOUT the shared card, then shows it from handle_info/2, so the
%% card's statics first leave the server in a push rather than at mount.

-spec mount(az:bindings()) -> az:mount_ret().
mount(Bindings) ->
    {#{id => maps:get(id, Bindings, ~"nca"), show => false, label => ~""}, #{}}.

-spec render(az:bindings()) -> az:template().
render(Bindings) ->
    ?html(
        {'div', [{id, ?get(id)}], [
            case ?get(show) of
                true -> ?stateless(arizona_nav_card, render, #{label => ?get(label)});
                false -> <<>>
            end
        ]}
    ).

-spec handle_info(term(), az:bindings()) -> az:handle_info_ret().
handle_info({show_card, Label}, Bindings) ->
    {Bindings#{show => true, label => Label}, #{}, []};
handle_info({show_card_titled, Label}, Bindings) ->
    {Bindings#{show => true, label => Label}, #{}, [arizona_js:set_title(Label)]};
handle_info({show_card_with, Label, Effects}, Bindings) ->
    {Bindings#{show => true, label => Label}, #{}, Effects}.

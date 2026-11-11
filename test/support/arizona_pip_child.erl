-module(arizona_pip_child).
-include("arizona_stateful.hrl").
-export([mount/1, render/1, handle_info/2]).

%% A poppable child for `arizona_pip_parent`: a server tick proves diffs reach it
%% in its window, and a client-owned widget (the `Widget` hook in `arizona_layout`)
%% in an az-nodiff element shows what a re-render keeps.

-spec mount(az:bindings()) -> az:mount_ret().
mount(Bindings) ->
    ?connected andalso ?send(arizona_connected),
    {#{id => maps:get(id, Bindings), count => 0, widget => true}, #{}}.

-spec render(az:bindings()) -> az:template().
render(Bindings) ->
    Id = ?get(id),
    ?html(
        {'div', [{id, Id}], [
            {p, [{class, ~"pip-count"}], [~"Tick: ", ?get(count)]},
            {button, [{az_click, arizona_js:request_pip(Id)}], [~"Pop out"]},
            %% A conditional branch: only a template's root az-nodiff element is kept.
            case ?get(widget) of
                true -> {'div', ['az-nodiff', {az_hook, ~"Widget"}, {data_kind, ~"pip"}], []};
                false -> <<>>
            end
        ]}
    ).

-spec handle_info(term(), az:bindings()) -> az:handle_info_ret().
handle_info(arizona_connected, Bindings) ->
    ?send_after(1000, tick),
    {Bindings, #{}, []};
handle_info(tick, Bindings) ->
    ?send_after(1000, tick),
    {Bindings#{count => maps:get(count, Bindings) + 1}, #{}, []}.

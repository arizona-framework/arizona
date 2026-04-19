-module(arizona_scroll_about).
-include("arizona_stateful.hrl").
-export([mount/1, render/1, handle_info/2]).

-spec mount(az:bindings()) -> az:mount_ret().
mount(Bindings0) ->
    Bindings = maps:merge(
        #{id => ~"page", title => ~"Scroll About", connected => false},
        Bindings0
    ),
    ?connected andalso ?send(arizona_connected),
    {Bindings, #{}}.

-spec render(az:bindings()) -> az:template().
render(Bindings) ->
    ?html(
        {main, [{id, ?get(id)}], [
            {h1, [], [?get(title)]},
            {span, [{id, ~"status"}], [
                case ?get(connected, false) of
                    true -> ~"Connected";
                    false -> ~"Connecting..."
                end
            ]},
            %% Leading spacer offsets the section below the initial viewport.
            {'div', [{style, ~"height: 1500px"}], []},
            %% Hash navigation target.
            {section, [{id, ~"section"}, {style, ~"height: 200px"}], [~"anchor target"]},
            %% Trailing spacer so the section can sit at viewport top even when
            %% it is near the bottom of content.
            {'div', [{style, ~"height: 1500px"}], []}
        ]}
    ).

-spec handle_info(term(), az:bindings()) -> az:handle_info_ret().
handle_info(arizona_connected, Bindings) ->
    {Bindings#{connected => true}, #{}, []}.

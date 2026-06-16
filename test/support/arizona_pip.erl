-module(arizona_pip).
-include("arizona_stateful.hrl").
-export([mount/1, render/1, handle_info/2]).

-spec mount(az:bindings()) -> az:mount_ret().
mount(Bindings) ->
    %% Drive a 1s server-side tick so the panel keeps updating after it is moved
    %% into the floating window -- proving diffs still reach a popped-out view.
    ?connected andalso ?send(arizona_connected),
    {
        #{
            id => maps:get(id, Bindings, ~"pip_demo"),
            count => 0
        },
        #{}
    }.

-spec render(az:bindings()) -> az:template().
render(Bindings) ->
    Id = ?get(id),
    ?html(
        {main, [{id, Id}], [
            {style, [],
                ~"""
                .pip-count {
                    font-size: 3rem;
                    font-variant-numeric: tabular-nums;
                    margin: 0.5rem 0;
                }
                .pip-actions button { margin-right: 0.5rem; }
                """},
            {h1, [], [~"Picture-in-Picture"]},
            {p, [],
                ~"""
                Pop this panel into a floating, always-on-top window. It keeps
                ticking (server diffs) and the buttons keep working while it lives
                in the PiP document.
                """},
            {p, [{class, ~"pip-count"}], [~"Tick: ", ?get(count)]},
            {'div', [{class, ~"pip-actions"}], [
                {button, [{az_click, arizona_js:request_pip(Id)}], [~"Pop out"]},
                {button, [{az_click, arizona_js:request_pip(Id, #{width => 440, height => 940})}], [
                        ~"Pop out (sized)"
                    ]},
                {button, [{az_click, arizona_js:exit_pip(Id)}], [~"Pop in"]}
            ]},
            {p, [], [
                {small, [], [~"Document Picture-in-Picture is Chromium-only (Chrome/Edge)."]}
            ]}
        ]}
    ).

-spec handle_info(term(), az:bindings()) -> az:handle_info_ret().
handle_info(arizona_connected, Bindings) ->
    ?send_after(1000, tick),
    {Bindings, #{}, []};
handle_info(tick, Bindings) ->
    ?send_after(1000, tick),
    {Bindings#{count => maps:get(count, Bindings) + 1}, #{}, []}.

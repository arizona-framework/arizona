-module(arizona_local).
-include("arizona_view.hrl").
-export([mount/2, render/1]).

-spec mount(az:bindings(), az:request()) -> az:mount_ret().
mount(Bindings, _Req) ->
    {#{id => maps:get(id, Bindings, ~"local_demo")}, #{}}.

-spec render(az:bindings()) -> az:template().
render(Bindings) ->
    ?html(
        {main, [{id, ?get(id)}], [
            {h1, [], [~"Client-owned slots (?local)"]},
            {p, [],
                ~"Every control below updates the DOM with no WebSocket frame (watch the WS tab)."},

            %% Dialog: bound `open` attribute + bound title content. One click
            %% chains two client-only sets; Close clears `open`.
            {section, [], [
                {h2, [], [~"Dialog"]},
                {button,
                    [
                        {az_click, [
                            arizona_js:set(~"dialog_open", true),
                            arizona_js:set(~"dialog_title", ~"Hello from the client!")
                        ]}
                    ],
                    [~"Open dialog"]},
                {dialog, [{open, ?local(~"dialog_open", false)}], [
                    {h3, [], [{span, [], [?local(~"dialog_title", ~"")]}]},
                    {p, [], [~"Opened and closed with zero server round-trips."]},
                    {button, [{az_click, arizona_js:set(~"dialog_open", false)}], [~"Close"]}
                ]}
            ]},

            %% Tabs: one bound `data-active` attribute on the container; CSS
            %% attribute-selectors derive per-tab styling and panel visibility.
            {section, [], [
                {h2, [], [~"Tabs"]},
                {style, [],
                    ~"""
                    .tabs .panel { display: none; }
                    .tabs[data-active=home] .panel-home { display: block; }
                    .tabs[data-active=settings] .panel-settings { display: block; }
                    .tabs[data-active=home] .tab-home,
                    .tabs[data-active=settings] .tab-settings { font-weight: bold; }
                    """},
                {'div', [{class, ~"tabs"}, {'data-active', ?local(~"tab", ~"home")}], [
                    {button,
                        [{class, ~"tab tab-home"}, {az_click, arizona_js:set(~"tab", ~"home")}], [
                            ~"Home"
                        ]},
                    {button,
                        [
                            {class, ~"tab tab-settings"},
                            {az_click, arizona_js:set(~"tab", ~"settings")}
                        ],
                        [~"Settings"]},
                    {'div', [{class, ~"panel panel-home"}], [~"Home panel content"]},
                    {'div', [{class, ~"panel panel-settings"}], [~"Settings panel content"]}
                ]}
            ]}
        ]}
    ).

-module(arizona_local_app).
-include("arizona_stateful.hrl").
-export([mount/1, render/1]).

%% A fully client-only "app": nested stateless (the tab bar) and stateful (two
%% widgets) elements, every interaction driven by client-owned ?local slots
%% (`arizona_js:set`) -- zero server round-trips after the initial render.
-spec mount(az:bindings()) -> az:mount_ret().
mount(Bindings) ->
    {#{id => maps:get(id, Bindings, ~"local_app")}, #{}}.

-spec render(az:bindings()) -> az:template().
render(Bindings) ->
    ?html(
        {main, [{id, ?get(id)}], [
            {style, [],
                ~"""
                .app { padding: 1rem; }
                .app.theme-dark { background: #222; color: #eee; }
                .tabs .panel { display: none; }
                .tabs[data-active=home] .panel-home { display: block; }
                .tabs[data-active=settings] .panel-settings { display: block; }
                .status-active { color: orange; }
                .status-done { color: green; }
                .card { border: 1px solid #ccc; padding: .5rem; margin: .5rem 0; }
                """},

            %% Theme is a client-owned slot interpolated into the wrapper's class.
            {'div', [{class, [~"app theme-", ?local(~"theme", ~"light")]}], [
                {h1, [], [~"Client-only app (no server round-trips)"]},
                {'div', [{class, ~"theme-switch"}], [
                    {button, [{az_click, arizona_js:set(~"theme", ~"light")}], [~"Light"]},
                    {button, [{az_click, arizona_js:set(~"theme", ~"dark")}], [~"Dark"]}
                ]},

                %% Stateless child: a CSS-driven tab bar (one bound attribute).
                ?stateless(render_tabs, #{}),

                %% Stateful children: each independently client-interactive.
                {'div', [{class, ~"widgets"}], [
                    ?stateful(arizona_local_widget, #{id => ~"widget_a", title => ~"Widget A"}),
                    ?stateful(arizona_local_widget, #{id => ~"widget_b", title => ~"Widget B"})
                ]}
            ]}
        ]}
    ).

render_tabs(_Bindings) ->
    ?html(
        {'div', [{class, ~"tabs"}, {'data-active', ?local(~"tab", ~"home")}], [
            {button, [{az_click, arizona_js:set(~"tab", ~"home")}], [~"Home"]},
            {button, [{az_click, arizona_js:set(~"tab", ~"settings")}], [~"Settings"]},
            {'div', [{class, ~"panel panel-home"}], [~"Home panel content"]},
            {'div', [{class, ~"panel panel-settings"}], [~"Settings panel content"]}
        ]}
    ).

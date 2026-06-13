-module(arizona_transitions).
-include("arizona_stateful.hrl").
-export([mount/1, render/1]).

-spec mount(az:bindings()) -> az:mount_ret().
mount(Init) ->
    Bindings = #{
        id => ~"page",
        title => maps:get(title, Init, ~"Transitions")
    },
    {Bindings, #{}}.

-spec render(az:bindings()) -> az:template().
render(Bindings) ->
    ?html(
        {main, [{id, ?get(id)}], [
            {style, [], [
                ~"""
                @view-transition { navigation: auto; }
                .vt-card, .vt-panel {
                    border: 1px solid #888;
                    padding: 1rem;
                    margin: 1rem 0;
                }
                /* Only the card carries the shared name; a second element with
                   the same view-transition-name aborts the transition. */
                .vt-card { view-transition-name: vt-card; }
                ::view-transition-old(root),
                ::view-transition-new(root) { animation-duration: 0.8s; }
                :active-view-transition-type(slide)::view-transition-new(root) {
                    animation: 0.8s vt-slide-in;
                }
                @keyframes vt-slide-in { from { transform: translateX(100px); opacity: 0; } }
                """
            ]},
            {h1, [], [?get(title)]},
            {'div', [{class, ~"vt-card"}, {id, ~"card"}], [
                ~"Shared card -- morphs across the navigation"
            ]},
            {p, [], [
                %% B: SPA navigation, default cross-fade (az_transition bare attribute)
                {a, [{href, ~"/transitions/detail"}, az_navigate, az_transition], [
                    ~"Open (cross-fade)"
                ]},
                ~" | ",
                %% B: SPA navigation with a view-transition type (slide)
                {a, [{href, ~"/transitions/detail"}, az_navigate, {az_transition, ~"slide"}], [
                    ~"Open (slide)"
                ]},
                ~" | ",
                %% A: pure anchor -- the browser transitions it via @view-transition CSS
                {a, [{href, ~"/transitions/detail"}], [~"Open (full reload)"]}
            ]},
            %% C: client-side effect wrapped in a transition (no server round-trip)
            {button, [{az_click, [arizona_js:transition(), arizona_js:toggle(~"#panel")]}], [
                ~"Toggle panel"
            ]},
            {'div', [{id, ~"panel"}, {class, ~"vt-panel"}, hidden], [
                ~"Client-side toggle, wrapped in a transition"
            ]}
        ]}
    ).

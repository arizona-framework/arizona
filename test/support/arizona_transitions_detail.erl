-module(arizona_transitions_detail).
-include("arizona_stateful.hrl").
-export([mount/1, render/1]).

-spec mount(az:bindings()) -> az:mount_ret().
mount(Init) ->
    Bindings = #{
        id => ~"page",
        title => maps:get(title, Init, ~"Transition detail")
    },
    {Bindings, #{}}.

-spec render(az:bindings()) -> az:template().
render(Bindings) ->
    ?html(
        {main, [{id, ?get(id)}], [
            {style, [], [
                ~"""
                @view-transition { navigation: auto; }
                .vt-card {
                    view-transition-name: vt-card;
                    border: 1px solid #888;
                    padding: 1rem;
                    margin: 1rem 0;
                }
                ::view-transition-old(root),
                ::view-transition-new(root) { animation-duration: 0.8s; }
                :active-view-transition-type(slide)::view-transition-new(root) {
                    animation: 0.8s vt-slide-in;
                }
                @keyframes vt-slide-in { from { transform: translateX(100px); opacity: 0; } }
                """
            ]},
            {h1, [], [?get(title)]},
            %% Distinct geometry from the list card, so the shared-element morph
            %% (same view-transition-name) has size/position to animate.
            {'div',
                [
                    {class, ~"vt-card"},
                    {id, ~"card"},
                    {style, ~"max-width: 18rem; margin-left: auto; background: #eef;"}
                ],
                [~"Shared card -- now on the detail page"]},
            {p, [], [
                {a, [{href, ~"/transitions"}, az_navigate, az_transition], [~"Back (cross-fade)"]},
                ~" | ",
                {a, [{href, ~"/transitions"}, az_navigate, {az_transition, ~"slide"}], [
                    ~"Back (slide)"
                ]}
            ]}
        ]}
    ).

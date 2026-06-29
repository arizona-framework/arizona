-module(arizona_patch_demo).
-include("arizona_stateful.hrl").
-export([mount/1, render/1, handle_event/3, handle_update/3]).

%% E2E + `make start` demo for in-place SPA navigation (`az-patch`). The whole
%% view is persistent chrome with a section-driven content slot. A `bump`
%% counter lives in the chrome; az-patch links switch the section. Patching must
%% swap the content while keeping the counter (the root view is NOT remounted).

-spec mount(az:bindings()) -> az:mount_ret().
mount(Bindings) ->
    %% `section` arrives as a binary-keyed path binding (extract/1); store it
    %% under the atom `section` the template reads.
    {
        #{
            id => ~"patch-demo",
            section => maps:get(~"section", Bindings, ~"overview"),
            clicks => 0
        },
        #{}
    }.

%% Chrome-owned state: must survive an az-patch navigation (proves no remount).
-spec handle_event(az:event_name(), az:event_payload(), az:bindings()) ->
    az:handle_event_ret().
handle_event(~"bump", _Payload, Bindings) ->
    {Bindings#{clicks => maps:get(clicks, Bindings) + 1}, #{}, []}.

%% A patch delivers the new `:section`; set it and update the document title
%% (the set_title effect rides the patch reply).
-spec handle_update(az:bindings(), az:bindings(), az:effects()) -> az:handle_update_ret().
handle_update(Params, Bindings, Effects) ->
    Section = maps:get(~"section", Params, maps:get(section, Bindings)),
    {Bindings#{section => Section}, #{}, [arizona_js:set_title(Section) | Effects]}.

-spec render(az:bindings()) -> az:template().
render(Bindings) ->
    Section = ?get(section),
    ?html(
        {'div', [{id, ?get(id)}], [
            {nav, [], [
                {a,
                    [
                        {href, ~"/patch-demo/overview"},
                        az_patch,
                        {class, nav_class(Section, ~"overview")}
                    ],
                    [
                        ~"Overview"
                    ]},
                {a,
                    [
                        {href, ~"/patch-demo/projects"},
                        az_patch,
                        {class, nav_class(Section, ~"projects")}
                    ],
                    [
                        ~"Projects"
                    ]},
                {a,
                    [
                        {href, ~"/patch-demo/settings"},
                        az_patch,
                        {class, nav_class(Section, ~"settings")}
                    ],
                    [
                        ~"Settings"
                    ]},
                {button, [{id, ~"bump"}, {az_click, arizona_js:push_event(~"bump")}], [~"bump"]},
                {span, [{id, ~"clicks"}], [integer_to_binary(?get(clicks))]}
            ]},
            {main, [{id, ~"content"}], [Section]}
        ]}
    ).

nav_class(S, S) -> ~"active";
nav_class(_, _) -> <<>>.

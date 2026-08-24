-module(arizona_svg_each_page).
-include("arizona_stateful.hrl").
-export([mount/1]).
-export([render/1]).
-export([handle_event/3]).

%% An `?each` inside `<svg>` whose list is EMPTY at mount and fills in later.
%%
%% The page parser namespace-adjusts inside `<svg>`, so SSR content is always in
%% the SVG namespace no matter how the client behaves. Only nodes the DIFF creates
%% can land in the wrong one, and an `HTMLUnknownElement` renders nothing while
%% carrying every attribute correctly -- so the failure is invisible until a list
%% that starts empty is populated by a patch. That is this shape.
%%
%% The jsdom tests in `arizona.test.js` pin the namespace the client creates; this
%% exists so a real browser proves the node also LAYS OUT, which is the half a
%% jsdom probe cannot settle (an unknown element has no `getBBox` and no box).

-spec mount(az:bindings()) -> az:mount_ret().
mount(Init) ->
    Bindings = #{
        id => ~"page",
        title => maps:get(title, Init, ~"SvgEach"),
        %% Empty on purpose -- a non-empty list would be SSR-parsed and could
        %% never catch this.
        bars => []
    },
    {Bindings, #{}}.

-spec render(az:bindings()) -> az:template().
render(Bindings) ->
    ?html(
        {'div', [{id, ?get(id)}], [
            {button, [{id, ~"add"}, {az_click, arizona_js:push_event(~"add")}], [~"Add"]},
            {svg, [{id, ~"chart"}, {viewBox, ~"0 0 100 20"}, {width, ~"100"}, {height, ~"20"}], [
                {g, [{id, ~"bars"}], ?each(fun bar/1, ?get(bars))}
            ]}
        ]}
    ).

bar(X) ->
    {rect, [{x, X}, {y, ~"0"}, {width, ~"5"}, {height, ~"20"}, {fill, ~"red"}], []}.

-spec handle_event(az:event_name(), az:event_payload(), az:bindings()) ->
    az:handle_event_ret().
handle_event(~"add", _Payload, Bindings) ->
    Bars = maps:get(bars, Bindings),
    X = integer_to_binary(length(Bars) * 10),
    {Bindings#{bars => Bars ++ [X]}, #{}, []}.

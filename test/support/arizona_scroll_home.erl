-module(arizona_scroll_home).
-include("arizona_view.hrl").
-export([mount/2, render/1, handle_info/2]).

-spec mount(az:bindings(), az:request()) -> az:mount_ret().
mount(Bindings0, _Req) ->
    Bindings = maps:merge(
        #{id => ~"page", title => ~"Scroll Home", connected => false},
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
            {button,
                [
                    {id, ~"replace-nav"},
                    {az_click, arizona_js:navigate(~"/scroll-home?x=1", #{replace => true})}
                ],
                [~"Replace nav"]},
            %% Tall spacer so scroll-restoration E2E tests have vertical range.
            {'div', [{id, ~"spacer"}, {style, ~"height: 3000px"}], []}
        ]}
    ).

-spec handle_info(term(), az:bindings()) -> az:handle_info_ret().
handle_info(arizona_connected, Bindings) ->
    {Bindings#{connected => true}, #{}, []}.

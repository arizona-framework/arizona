-module(arizona_hello_world_view).
-behaviour(arizona_view).
-compile({parse_transform, arizona_parse_transform}).
-export([mount/2]).
-export([layout/1]).
-export([render/1]).
-export([handle_event/3]).

mount(_Arg, Req) ->
    Bindings = #{id => ~"view"},
    Path = arizona_request:get_path(Req),
    Layout = {?MODULE, layout, main_content, #{active_url => Path}},
    arizona_view:new(?MODULE, Bindings, Layout).

layout(Bindings) ->
    arizona_template:from_erl([
        ~"<!DOCTYPE html>",
        {html, [], [
            {head, [], [
                {title, [], ~"Arizona Test Hello World"},
                {script, [{type, ~"module"}], ~"""
                import Arizona from '/assets/js/arizona.min.js';
                globalThis.arizona = new Arizona();
                arizona.connect('/live');
                """}
            ]},
            {body, [], [
                arizona_template:render_stateless(arizona_test_components, render_menu, #{
                    active_url => maps:get(active_url, Bindings)
                }),
                arizona_template:render_slot(maps:get(main_content, Bindings))
            ]}
        ]}
    ]).

render(Bindings) ->
    arizona_template:from_erl(
        {'div', [{id, arizona_template:get_binding(id, Bindings)}],
            case arizona_template:find_binding(name, Bindings) of
                {ok, Name} ->
                    [~"Hello, ", Name, ~"!"];
                error ->
                    arizona_template:from_erl(
                        {button, [{onclick, ~"arizona.pushEvent('hello_world')"}], ~"Say Hello!"}
                    )
            end}
    ).

handle_event(~"hello_world", _Params, View) ->
    State = arizona_view:get_state(View),
    UpdatedState = arizona_stateful:put_binding(name, ~"World", State),
    {[], arizona_view:update_state(UpdatedState, View)}.

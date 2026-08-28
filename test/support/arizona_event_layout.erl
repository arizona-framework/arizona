-module(arizona_event_layout).
-moduledoc """
A layout declaring an effect attribute. Layouts render once, at SSR, so no
frame can ever carry this markup -- `az-mouseover` reaches the client only
because the connect walk takes the route's layout modules as roots beside the
handler.
""".
-include("arizona_stateless.hrl").

-export([render/1]).

-spec render(az:bindings()) -> az:template().
render(Bindings) ->
    ?html([
        ~"<!DOCTYPE html>",
        {html, [], [
            {head, [], [
                {meta, [{charset, ~"utf-8"}], []},
                {title, [], [maps:get(title, Bindings, ~"Arizona")]}
            ]},
            {body, [], [
                {button, [{id, ~"menu-btn"}, {az_mouseover, arizona_js:toggle(~"#menu")}], [
                    ~"Menu"
                ]},
                {nav, [{id, ~"menu"}, {hidden, true}], [~"menu"]},
                ?inner_content
            ]}
        ]}
    ]).

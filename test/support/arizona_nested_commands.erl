-module(arizona_nested_commands).
-moduledoc """
Fixture for a command list holding nested lists and empty ones.

A conditional command is naturally written as a list item whose branch yields a
command, a list of commands, or `[]` for none. Each attribute exercises one
position of that pattern: an empty or nested item after the first command, one
leading the list, a list whose only item is a conditional (so it may be all
empty), and a literal list the parse transform folds at compile time.
""".
-include("arizona_stateless.hrl").
-export([render/1]).

-spec render(az:bindings()) -> az:template().
render(Bindings) ->
    ?html(
        {button,
            [
                {az_click, [
                    arizona_js:push_event(~"x"),
                    case ?get(on) of
                        true -> [arizona_js:hide(~"#a"), arizona_js:show(~"#b")];
                        false -> []
                    end
                ]},
                {az_dblclick, [
                    case ?get(on) of
                        true -> arizona_js:hide(~"#a");
                        false -> []
                    end,
                    arizona_js:push_event(~"y")
                ]},
                {az_mouseenter, [
                    case ?get(on) of
                        true -> arizona_js:hide(~"#a");
                        false -> []
                    end
                ]},
                {az_keyup, [arizona_js:push_event(~"k"), [], [arizona_js:hide(~"#k")]]}
            ],
            [~"b"]}
    ).

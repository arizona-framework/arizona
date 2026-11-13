-module(arizona_pip_file).
-include("arizona_stateful.hrl").
-export([mount/1, render/1]).

%% A poppable child of `arizona_pip_form` holding a file input and a textarea
%% wrapping hard by its laid-out lines (a set width and font, one whose `font`
%% shorthand reads back empty).

-spec mount(az:bindings()) -> az:mount_ret().
mount(Bindings) ->
    {#{id => maps:get(id, Bindings)}, #{}}.

-spec render(az:bindings()) -> az:template().
render(Bindings) ->
    Id = ?get(id),
    ?html(
        {'div', [{id, Id}], [
            {input, [{type, ~"file"}, {name, ~"up"}]},
            {textarea,
                [
                    {name, ~"ta"},
                    {wrap, ~"hard"},
                    {style, ~"width: 7.5em; padding: 0; font: 20px monospace; font-kerning: none"}
                ],
                [~"abcdefghijklmnopqrstuvwxyz"]},
            {button, [{type, ~"button"}, {az_click, arizona_js:request_pip(Id)}], [~"Pop out"]}
        ]}
    ).

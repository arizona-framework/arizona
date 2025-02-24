-module(arizona_example_layout).
-behaviour(arizona_layout).

-export([mount/2]).
-export([render/1]).

mount(Bindings, _Socket) ->
    arizona:new_view(?MODULE, Bindings).

render(View) ->
    arizona:render_layout_template(View, {file, template_file(View)}).

template_file(View) ->
    filename:join(arizona:get_binding(data_dir, View), "layout.herl").

-module(arizona_example_layout).

-export([render/1]).

render(View) ->
    arizona:render_component_template(View, {file, template_file(View)}).

template_file(View) ->
    filename:join(arizona:get_assign(data_dir, View), "layout.herl").

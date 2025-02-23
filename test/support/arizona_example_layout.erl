-module(arizona_example_layout).
-behaviour(arizona_layout).

-export([mount/2]).
-export([render/1]).

mount(Assigns, _Socket) ->
    arizona:new_view(?MODULE, Assigns).

render(View) ->
    arizona:render_layout_template(View, {file, template_file(View)}).

template_file(View) ->
    filename:join(arizona:get_assign(data_dir, View), "layout.herl").

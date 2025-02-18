-module(arizona_example_new_id).
-behaviour(arizona_view).

-export([mount/2]).
-export([render/1]).

mount(#{ignore := true}, _Socket) ->
    ignore;
mount(Assigns, _Socket) ->
    View = arizona_view:new(?MODULE, Assigns),
    {ok, View}.

render(View) ->
    arizona_render:view_template(View, ~""""
    <div id="{arizona_view:get_assign(id, View)}">
        Hello, {arizona_view:get_assign(name, View)}!
    </div>
    """").

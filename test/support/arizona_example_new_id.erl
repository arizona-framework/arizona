-module(arizona_example_new_id).
-behaviour(arizona_view).

-export([mount/2]).
-export([render/1]).
-export([handle_event/3]).

mount(#{ignore := true}, _Socket) ->
    ignore;
mount(Assigns, _Socket) ->
    View = arizona_view:new(?MODULE, Assigns),
    {ok, View}.

render(View) ->
    arizona_renderer:view_template(View, ~""""
    <div id="{arizona_view:get_assign(id, View)}">
        Hello, {arizona_view:get_assign(name, View)}!
    </div>
    """").

handle_event(_Event, _Payload, View) ->
    View.

-module(arizona_example_template_new_id).
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
    arizona_renderer:view_template(View, ~"""
    <div id="{arizona_view:get_assign(id, View)}">
        {arizona_renderer:view(arizona_example_new_id, #{
            id => arizona_view:get_assign(view_id, View),
            name =>  arizona_view:get_assign(name, View),
            ignore => arizona_view:get_assign(ignore, View)
        })}
    </div>
    """).

handle_event(_Event, _Payload, View) ->
    View.

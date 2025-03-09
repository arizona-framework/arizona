-module(arizona_example_template_new_id).
-behaviour(arizona_view).

-export([mount/2]).
-export([render/1]).
-export([handle_event/4]).

mount(#{ignore := true}, _Socket) ->
    ignore;
mount(Bindings, _Socket) ->
    View = arizona:new_view(?MODULE, Bindings),
    {ok, View}.

render(View) ->
    arizona:render_view_template(View, ~"""
    <div id="{arizona:get_binding(id, View)}">
        {arizona:render_view(arizona_example_new_id, #{
            id => arizona:get_binding(view_id, View),
            name =>  arizona:get_binding(name, View),
            ignore => arizona:get_binding(ignore, View)
        })}
    </div>
    """).

handle_event(_Event, _Payload, _From, View) ->
    {noreply, View}.

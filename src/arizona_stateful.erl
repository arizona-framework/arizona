-module(arizona_stateful).

-type bindings() :: map().
-type resets() :: map().
-type effect() :: arizona_js:cmd().
-type effects() :: [effect()].

-export_type([bindings/0, resets/0, effects/0]).

-callback mount(Bindings :: bindings()) -> {bindings(), resets()}.
-callback render(Bindings :: bindings()) -> arizona_template:template().
-callback handle_event(Event :: binary(), Payload :: map(), Bindings :: bindings()) ->
    {bindings(), resets(), effects()}.
-callback handle_info(Info :: term(), Bindings :: bindings()) ->
    {bindings(), resets(), effects()}.
-callback handle_update(Props :: map(), Bindings :: bindings()) -> {bindings(), resets()}.
-callback unmount(Bindings :: bindings()) -> ok.

-optional_callbacks([handle_event/3, handle_info/2, handle_update/2, unmount/1]).

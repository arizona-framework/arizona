-module(az).
-export([
    get/2, get/3,
    get_lazy/3,
    inner_content/1,
    track/1,
    html/1,
    stateful/2,
    stateless/2, stateless/3,
    each/2
]).
-ignore_xref([
    get/2,
    get/3,
    get_lazy/3,
    inner_content/1,
    track/1,
    html/1,
    stateful/2,
    stateless/2,
    stateless/3,
    each/2
]).

%% --- Binding access with dep tracking ----------------------------------------

get(Key, Bindings) ->
    arizona_template:get(Key, Bindings).
get(Key, Bindings, Default) ->
    arizona_template:get(Key, Bindings, Default).
get_lazy(Key, Bindings, DefaultFun) ->
    arizona_template:get_lazy(Key, Bindings, DefaultFun).

inner_content(Bindings) ->
    maps:get(inner_content, Bindings).

track(Key) ->
    arizona_template:track(Key).

%% --- Template construction (replaced by parse transform) ---------------------

-spec html(term()) -> no_return().
html(Elems) ->
    arizona_template:html(Elems).

%% --- Descriptor constructors -------------------------------------------------

stateful(Handler, Props) ->
    arizona_template:stateful(Handler, Props).

stateless(Callback, Props) ->
    arizona_template:stateless(Callback, Props).
stateless(Handler, Fun, Props) ->
    arizona_template:stateless(Handler, Fun, Props).

each(Fun, Source) ->
    arizona_template:each(Fun, Source).

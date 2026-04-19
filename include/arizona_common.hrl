%% Connection context -- true inside a connected live process, false during SSR
-define(connected, arizona_live:connected()).

%% Binding access -- requires `Bindings` variable in scope
-define(get(Key), arizona_template:get(Key, Bindings)).
-define(get(Key, Default), arizona_template:get(Key, Bindings, Default)).
-define(get_lazy(Key, Fun), arizona_template:get_lazy(Key, Bindings, Fun)).

%% Layout inner content -- requires `Bindings` variable in scope
-define(inner_content, az:inner_content(Bindings)).

%% Template construction (parse transform intercepts the expanded calls)
-define(html(Elems), arizona_template:html(Elems)).
-define(each(Fun, Source), arizona_template:each(Fun, Source)).

%% Descriptor constructors
-define(stateful(Handler, Props), arizona_template:stateful(Handler, Props)).
-define(stateless(Fun, Props), arizona_template:stateless(Fun, Props)).
-define(stateless(Module, Fun, Props), arizona_template:stateless(Module, Fun, Props)).

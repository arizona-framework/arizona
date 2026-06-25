%% Connection context -- true inside a connected live process, false during SSR
-define(connected, arizona_live:connected()).

%% Native-shell capability negotiation -- which OS capabilities the embedding
%% shell (Electron, Tauri, ...) advertised at connect; `false`/`#{}` in a plain
%% browser or during SSR. A UI/effect hint only, never an authorization input.
-define(capability(Key), arizona_live:capability(Key)).
-define(capabilities, arizona_live:capabilities()).

%% Binding access -- requires `Bindings` variable in scope
-define(get(Key), arizona_template:get(Key, Bindings)).
-define(get(Key, Default), arizona_template:get(Key, Bindings, Default)).
-define(get_lazy(Key, Fun), arizona_template:get_lazy(Key, Bindings, Fun)).

%% Layout inner content -- requires `Bindings` variable in scope
-define(inner_content, az:inner_content(Bindings)).

%% Template construction (parse transform intercepts the expanded calls)
-define(html(Elems), arizona_template:html(Elems)).
-define(native(Elems), arizona_template:native(Elems)).
-define(terminal(Elems), arizona_template:terminal(Elems)).
-define(each(Fun, Source), arizona_template:each(Fun, Source)).

%% Escape opt-out -- splices a trusted, already-safe HTML fragment verbatim into a
%% content slot or attribute value instead of HTML-escaping it. The parse transform
%% only recognizes the opt-out when the `raw` call is literal at the template site,
%% so wrap values here, never inside a helper. Never use for user-controlled data.
-define(raw(Value), arizona_template:raw(Value)).

%% Descriptor constructors
-define(stateful(Handler, Props), arizona_template:stateful(Handler, Props)).
-define(stateless(Fun, Props), arizona_template:stateless(Fun, Props)).
-define(stateless(Module, Fun, Props), arizona_template:stateless(Module, Fun, Props)).

%% Client-owned slot -- server renders Init once and never diffs it; the browser
%% owns the value via the local Key (parse transform intercepts the expanded call)
-define(local(Key, Init), arizona_template:local(Key, Init)).

-module(arizona_non_utf8_view).
-include("arizona_stateful.hrl").
-export([mount/1]).
-export([render/1]).
-export([handle_event/3]).
-export([line/1]).
-export([terminal/1]).
-export([native/1]).
-export([native_attr/1]).
-export([latin1/0]).

%% Fixture: every render target writes UTF-8, so a value holding another encoding
%% must be refused where it is rendered, located at its template line. One slot per
%% shape a value can take on the way out -- an escaped content value, an attribute,
%% a `?raw` content payload, a `?raw` attribute, an `?each` item -- each fed by its
%% own binding, so `break` can corrupt exactly one of them after mount. Every
%% binding defaults to valid UTF-8 (with multibyte characters, so the valid path
%% exercises the non-ASCII leg of each check).
%%
%% `line/1` counts down from its own line to the template, so a line inserted
%% between it and `render/1` shifts every pin and the location tests fail.

-spec mount(az:bindings()) -> az:mount_ret().
mount(Bindings) ->
    {
        #{
            id => ~"nu8",
            content => maps:get(content, Bindings, ~"café"),
            attr => maps:get(attr, Bindings, ~"naïve"),
            raw => maps:get(raw, Bindings, ~"<b>ünï</b>"),
            raw_attr => maps:get(raw_attr, Bindings, ~"/ü"),
            items => maps:get(items, Bindings, [~"日本"])
        },
        #{}
    }.

-spec line(atom()) -> pos_integer().
line(content) -> ?LINE + 13;
line(attr) -> ?LINE + 13;
line(raw) -> ?LINE + 13;
line(raw_attr) -> ?LINE + 13;
line(items) -> ?LINE + 13;
line(terminal) -> ?LINE + 25;
line(native) -> ?LINE + 28;
line(native_attr) -> ?LINE + 31.

-spec render(az:bindings()) -> az:template().
render(Bindings) ->
    ?html(
        {'div', [{id, ?get(id)}], [
            {p, [], [?get(content)]},
            {p, [{title, ?get(attr)}], [~"a"]},
            {p, [], [?raw(?get(raw))]},
            {a, [{href, ?raw(?get(raw_attr))}], [~"b"]},
            {ul, [], [?each(fun(I) -> {li, [], [I]} end, ?get(items))]}
        ]}
    ).

-spec handle_event(az:event_name(), az:event_payload(), az:bindings()) ->
    az:handle_event_ret().
handle_event(~"break", #{~"key" := ~"items"}, Bindings) ->
    {Bindings#{items => [~"ok", latin1()]}, #{}, []};
handle_event(~"break", #{~"key" := Key}, Bindings) ->
    {Bindings#{binary_to_existing_atom(Key) => latin1()}, #{}, []}.

-spec terminal(az:bindings()) -> az:template().
terminal(Bindings) ->
    ?terminal({line, [], [?get(text)]}).

-spec native(az:bindings()) -> az:template().
native(Bindings) ->
    ?native({'Text', [], [?get(text)]}).

-spec native_attr(az:bindings()) -> az:template().
native_attr(Bindings) ->
    ?native({'Text', [{label, ?get(text)}], []}).

%% "café" as Latin-1: 0xE9 is a lead byte with no continuation, never valid UTF-8.
%% Spelled with an explicit byte so this file's own encoding cannot make it valid --
%% a bare `<<"café">>` in a helper is how real code produces it.
-spec latin1() -> binary().
latin1() ->
    <<"caf", 16#E9>>.

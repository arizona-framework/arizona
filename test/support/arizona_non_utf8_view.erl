-module(arizona_non_utf8_view).
-include("arizona_stateful.hrl").
-export([mount/1]).
-export([render/1]).
-export([handle_event/3]).
-export([native/1]).
-export([native_cmd/1]).
-export([latin1/0]).

%% Fixture: every render target writes UTF-8, so a value holding another encoding
%% is refused, at its template line wherever that can be named. One slot per shape
%% a value can take on the way out -- an escaped content value, an attribute, a
%% `?raw` content payload, a `?raw` attribute, an `?each` item, a `?stateless`
%% child's value -- each fed by its own binding, so `break` can corrupt exactly
%% one of them after mount -- also an effect command's argument, and a value only
%% the connected render shows (`?connected`). Every binding defaults to valid UTF-8
%% with multibyte characters. The suite finds each slot's line by the `?get(<key>)` on it.

-spec mount(az:bindings()) -> az:mount_ret().
mount(Bindings) ->
    {
        #{
            id => ~"nu8",
            content => maps:get(content, Bindings, ~"café"),
            attr => maps:get(attr, Bindings, ~"naïve"),
            raw => maps:get(raw, Bindings, ~"<b>ünï</b>"),
            raw_attr => maps:get(raw_attr, Bindings, ~"/ü"),
            items => maps:get(items, Bindings, [~"日本"]),
            child => maps:get(child, Bindings, ~"子"),
            effect => maps:get(effect, Bindings, ~"ação"),
            live => maps:get(live, Bindings, ~"ao vivo")
        },
        #{}
    }.

-spec render(az:bindings()) -> az:template().
render(Bindings) ->
    ?html(
        {'div', [{id, ?get(id)}], [
            {p, [], [?get(content)]},
            {p, [{title, ?get(attr)}], [~"a"]},
            {p, [], [?raw(?get(raw))]},
            {a, [{href, ?raw(?get(raw_attr))}], [~"b"]},
            {ul, [], [?each(fun(I) -> {li, [], [I]} end, ?get(items))]},
            ?stateless(fun child/1, #{text => ?get(child)}),
            {button, [{az_click, arizona_js:push_event(~"go", #{~"v" => ?get(effect)})}], [~"c"]},
            {p, [], [
                case ?connected of
                    true -> ?get(live);
                    false -> ~""
                end
            ]}
        ]}
    ).

child(Bindings) ->
    ?html({em, [], [?get(text)]}).

-spec handle_event(az:event_name(), az:event_payload(), az:bindings()) ->
    az:handle_event_ret().
handle_event(~"break", #{~"key" := ~"items"}, Bindings) ->
    {Bindings#{items => [~"ok", latin1()]}, #{}, []};
handle_event(~"break", #{~"key" := Key}, Bindings) ->
    {Bindings#{binary_to_existing_atom(Key) => latin1()}, #{}, []}.

-spec native(az:bindings()) -> az:template().
native(Bindings) ->
    ?native({'Text', [{label, ?get(label)}], [?get(text)]}).

-spec native_cmd(az:bindings()) -> az:template().
native_cmd(Bindings) ->
    ?native({'Button', [{on_tap, arizona_android:push_event(~"go", #{~"v" => ?get(cmd)})}], []}).

%% "café" as Latin-1: 0xE9 is a lead byte with no continuation, never valid UTF-8.
%% Spelled with an explicit byte so this file's own encoding cannot make it valid --
%% a bare `<<"café">>` outside a template is how real code produces it, and the
%% parse transform warns about that.
-spec latin1() -> binary().
latin1() ->
    <<"caf", 16#E9>>.

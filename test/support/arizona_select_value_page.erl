-module(arizona_select_value_page).
-moduledoc """
A live page whose selects take their choice from a `value`: written literally, from a
list `?each`, from a stream `?each`, `multiple`, from a component, from a child
view (`arizona_select_value_child`), and from a `?local` under `multiple`, whole and
interpolated. Buttons move the choice and add an option to the
child, so the e2e can check the page loads on the chosen option and every update
follows.
""".
-include("arizona_stateful.hrl").
-export([mount/1, render/1, handle_event/3]).

-spec mount(az:bindings()) -> az:mount_ret().
mount(Init) ->
    Items = [#{id => Id, label => string:uppercase(Id)} || Id <- [~"a", ~"b", ~"c"]],
    Bindings = #{
        id => ~"select-value",
        v => maps:get(v, Init, ~"b"),
        vs => maps:get(vs, Init, [~"a", ~"c"]),
        opts => [~"a", ~"b", ~"c"],
        child_opts => [~"a", ~"b", ~"c"],
        items => arizona_stream:new(fun(#{id := Id}) -> Id end, Items)
    },
    {Bindings, #{}}.

-spec render(az:bindings()) -> az:template().
render(Bindings) ->
    ?html(
        {'div', [{id, ?get(id)}], [
            {button,
                [{class, ~"pick-a"}, {az_click, arizona_js:push_event(~"pick", #{~"v" => ~"a"})}], [
                    ~"a"
                ]},
            {button,
                [{class, ~"pick-c"}, {az_click, arizona_js:push_event(~"pick", #{~"v" => ~"c"})}], [
                    ~"c"
                ]},
            {button, [{class, ~"pick-many"}, {az_click, arizona_js:push_event(~"pick_many")}], [
                ~"many"
            ]},
            {button,
                [{class, ~"pick-d"}, {az_click, arizona_js:push_event(~"pick", #{~"v" => ~"d"})}], [
                    ~"d"
                ]},
            {button, [{class, ~"add-d"}, {az_click, arizona_js:push_event(~"add_d")}], [
                ~"add d"
            ]},
            {select, [{class, ~"lit"}, {value, ?get(v)}], [
                {option, [{value, ~"a"}], [~"A"]},
                {optgroup, [{label, ~"More"}], [
                    {option, [{value, ~"b"}], [~"B"]},
                    {option, [{value, ~"c"}], [~"C"]}
                ]}
            ]},
            {select, [{class, ~"list"}, {value, ?get(v)}], [
                ?each(fun(O) -> {option, [{value, O}], [O]} end, ?get(opts))
            ]},
            {select, [{class, ~"stream"}, {value, ?get(v)}], [
                ?each(
                    fun(O, Key) ->
                        {option, [{az_key, Key}, {value, arizona_template:get(id, O)}], [
                            arizona_template:get(label, O)
                        ]}
                    end,
                    ?get(items)
                )
            ]},
            {select, [{class, ~"multi"}, multiple, {value, ?get(vs)}], [
                {option, [{value, ~"a"}], [~"A"]},
                {option, [{value, ~"b"}], [~"B"]},
                {option, [{value, ~"c"}], [~"C"]}
            ]},
            {select, [{class, ~"component"}, {value, ?get(v)}], [
                ?stateless(arizona_select_value, option_items, #{opts => ?get(opts)})
            ]},
            {select, [{class, ~"child"}, {value, ?get(v)}], [
                ?stateful(arizona_select_value_child, #{
                    id => ~"select-value-child", opts => ?get(child_opts)
                })
            ]},
            {button, [{class, ~"set-many"}, {az_click, arizona_js:set(many, [~"c", ~"ac"])}], [
                ~"set many"
            ]},
            {button, [{class, ~"set-tail"}, {az_click, arizona_js:set(tail, ~"")}], [
                ~"set tail"
            ]},
            {select, [{class, ~"local-many"}, multiple, {value, ?local(many, [~"a", ~"c"])}], [
                {option, [], [~"a"]},
                {option, [], [~"c"]},
                {option, [], [~"ac"]}
            ]},
            {select, [{class, ~"local-tail"}, multiple, {value, [~"a", ?local(tail, ~"c")]}], [
                {option, [], [~"a"]},
                {option, [], [~"c"]},
                {option, [], [~"ac"]}
            ]}
        ]}
    ).

-spec handle_event(az:event_name(), az:event_payload(), az:bindings()) ->
    az:handle_event_ret().
handle_event(~"pick", #{~"v" := V}, Bindings) ->
    {Bindings#{v => V}, #{}, []};
handle_event(~"add_d", _Payload, Bindings) ->
    {Bindings#{child_opts => [~"a", ~"b", ~"c", ~"d"]}, #{}, []};
handle_event(~"pick_many", _Payload, Bindings) ->
    {Bindings#{vs => [~"b"]}, #{}, []}.

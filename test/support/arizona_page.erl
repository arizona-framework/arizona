-module(arizona_page).
-include("arizona_stateful.hrl").
-export([mount/1, render/1, handle_event/3, handle_info/2]).

-spec mount(az:bindings()) -> az:mount_ret().
mount(Bindings0) ->
    Todos = arizona_stream:new(fun(#{id := Id}) -> Id end),
    Bindings = maps:merge(
        #{
            id => ~"page",
            title => ~"Welcome",
            theme => ~"light",
            count => 0,
            connected => false,
            todos => Todos,
            next_id => 1
        },
        Bindings0
    ),
    ?connected andalso ?send(arizona_connected),
    {Bindings, #{}}.

-spec render(az:bindings()) -> az:template().
render(Bindings) ->
    ?html(
        {main, [{id, ?get(id)}], [
            {h1, [], [?get(title, ~"Welcome")]},
            {button, [{az_click, arizona_js:push_event(~"add")}], [~"Add +1 to both"]},
            {'div',
                [
                    {class, ?get(theme, ~"light")}
                ],
                [
                    ?stateful(arizona_counter, #{
                        id => ~"counter",
                        count => ?get(count, 0)
                    })
                ]},
            ?stateful(arizona_counter, #{
                id => ~"counter2",
                count => ?get(count, 0)
            }),
            ?stateful(arizona_counter, #{
                id => ~"counter3",
                count => 42
            }),
            {span, [{id, ~"status"}], [
                case ?get(connected, false) of
                    true -> ~"Connected";
                    false -> ~"Connecting..."
                end
            ]},
            {form,
                [{id, ~"add-todo"}, {az_submit, arizona_js:push_event(~"add_todo")}, az_form_reset],
                [
                    {input,
                        [
                            {name, ~"text"},
                            {type, ~"text"},
                            {placeholder, ~"What needs to be done?"}
                        ],
                        []},
                    {button, [{type, ~"submit"}], [~"Add Todo"]}
                ]},
            {button, [{az_click, arizona_js:push_event(~"clear_todos")}], [~"Clear"]},
            {ul, [{az_drop, arizona_js:push_event(~"reorder_todo")}], [
                ?each(
                    fun(#{id := Id, text := Text}, Key) ->
                        {li, [{az_key, Key}], [
                            {input,
                                [
                                    {az_focusout,
                                        arizona_js:push_event(~"update_todo", #{~"id" => Id})},
                                    {az_keydown,
                                        arizona_js:on_key(
                                            enter,
                                            arizona_js:push_event(~"update_todo", #{~"id" => Id})
                                        )},
                                    {draggable, ~"true"},
                                    {value, Text}
                                ],
                                []},
                            {button,
                                [
                                    {az_click,
                                        arizona_js:push_event(~"remove_todo", #{~"id" => Id})}
                                ],
                                [<<"&#xd7;">>]}
                        ]}
                    end,
                    ?get(todos)
                )
            ]}
        ]}
    ).

-spec handle_event(az:event_name(), az:event_payload(), az:bindings()) ->
    az:handle_event_ret().
handle_event(~"title_change", _Payload, Bindings) ->
    {Bindings#{title => ~"Changed"}, #{}, []};
handle_event(~"add", _Payload, Bindings) ->
    {Bindings#{count => maps:get(count, Bindings, 0) + 1}, #{}, []};
handle_event(~"add_todo", Payload, Bindings) ->
    Id = maps:get(next_id, Bindings),
    Text =
        case maps:get(~"text", Payload, <<>>) of
            <<>> -> <<"Todo ", (integer_to_binary(Id))/binary>>;
            Custom -> Custom
        end,
    S = arizona_stream:insert(maps:get(todos, Bindings), #{id => Id, text => Text}),
    {Bindings#{todos => S, next_id => Id + 1}, #{}, []};
handle_event(~"update_todo", #{~"id" := Id, ~"value" := Text}, Bindings) ->
    S = arizona_stream:update(maps:get(todos, Bindings), Id, #{id => Id, text => Text}),
    {Bindings#{todos => S}, #{}, []};
handle_event(~"reorder_todo", #{~"data_transfer" := IdBin, ~"drop_index" := Pos}, Bindings) ->
    Id = binary_to_integer(IdBin),
    S = arizona_stream:move(maps:get(todos, Bindings), Id, Pos),
    {Bindings#{todos => S}, #{}, []};
handle_event(~"remove_todo", #{~"id" := Id}, Bindings) ->
    S = arizona_stream:delete(maps:get(todos, Bindings), Id),
    {Bindings#{todos => S}, #{}, []};
handle_event(~"clear_todos", _Payload, Bindings) ->
    S = arizona_stream:reset(maps:get(todos, Bindings)),
    {Bindings#{todos => S}, #{}, []}.

-spec handle_info(term(), az:bindings()) -> az:handle_info_ret().
handle_info(arizona_connected, Bindings) ->
    {Bindings#{connected => true}, #{}, [arizona_js:set_title(maps:get(title, Bindings))]}.

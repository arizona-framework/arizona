-module(arizona_datatable).
-include("arizona_view.hrl").
-export([mount/2, render/1, handle_event/3, handle_info/2]).

-spec mount(az:bindings(), az:request()) -> az:mount_ret().
mount(Bindings0, _Req) ->
    Items = [
        #{id => 1, name => ~"Alice", age => 30},
        #{id => 2, name => ~"Bob", age => 25},
        #{id => 3, name => ~"Charlie", age => 35},
        #{id => 4, name => ~"Diana", age => 28},
        #{id => 5, name => ~"Eve", age => 32}
    ],
    Rows = arizona_stream:new(fun(#{id := Id}) -> Id end, Items),
    Bindings = maps:merge(
        #{
            id => ~"page",
            title => ~"DataTable",
            rows => Rows,
            next_id => 6,
            sort_col => id,
            sort_dir => asc,
            connected => false
        },
        Bindings0
    ),
    ?connected andalso ?send(arizona_connected),
    {Bindings, #{}}.

-spec render(az:bindings()) -> az:template().
render(Bindings) ->
    ?html(
        {main, [{id, ?get(id)}], [
            {h1, [], [?get(title)]},
            {'div', [], [
                {button, [{az_click, arizona_js:push_event(~"add_row")}], [~"Add Row"]},
                {button, [{az_click, arizona_js:push_event(~"reset_data")}], [~"Reset"]},
                {button, [{az_click, arizona_js:push_event(~"shuffle")}], [~"Shuffle"]}
            ]},
            {table, [], [
                {thead, [], [
                    {tr, [], [
                        {th, [{az_click, arizona_js:push_event(~"sort", #{~"col" => ~"id"})}], [
                            ~"ID"
                        ]},
                        {th, [{az_click, arizona_js:push_event(~"sort", #{~"col" => ~"name"})}], [
                            ~"Name"
                        ]},
                        {th, [{az_click, arizona_js:push_event(~"sort", #{~"col" => ~"age"})}], [
                            ~"Age"
                        ]},
                        {th, [], [~"Actions"]}
                    ]}
                ]},
                {tbody, [], [
                    ?each(
                        fun(#{id := Id, name := Name, age := Age}, Key) ->
                            {tr, [{az_key, Key}], [
                                {td, [], [Id]},
                                {td, [], [Name]},
                                {td, [], [Age]},
                                {td, [], [
                                    {button,
                                        [
                                            {az_click,
                                                arizona_js:push_event(
                                                    ~"delete_row", #{~"id" => Id}
                                                )}
                                        ],
                                        [~"Delete"]},
                                    {button,
                                        [
                                            {az_click,
                                                arizona_js:push_event(~"move_top", #{~"id" => Id})}
                                        ],
                                        [~"Top"]}
                                ]}
                            ]}
                        end,
                        ?get(rows)
                    )
                ]}
            ]}
        ]}
    ).

-spec handle_event(az:event_name(), az:event_payload(), az:bindings()) ->
    az:handle_event_ret().
handle_event(~"sort", #{~"col" := ColBin}, Bindings) ->
    Col = binary_to_existing_atom(ColBin),
    OldCol = maps:get(sort_col, Bindings),
    OldDir = maps:get(sort_dir, Bindings),
    Dir =
        case {Col, OldCol, OldDir} of
            {Same, Same, asc} -> desc;
            _ -> asc
        end,
    Cmp =
        case Dir of
            asc -> fun(A, B) -> maps:get(Col, A) =< maps:get(Col, B) end;
            desc -> fun(A, B) -> maps:get(Col, A) >= maps:get(Col, B) end
        end,
    S = arizona_stream:sort(maps:get(rows, Bindings), Cmp),
    {Bindings#{rows => S, sort_col => Col, sort_dir => Dir}, #{}, []};
handle_event(~"add_row", _Payload, Bindings) ->
    Id = maps:get(next_id, Bindings),
    Row = #{id => Id, name => <<"New ", (integer_to_binary(Id))/binary>>, age => 20},
    S = arizona_stream:insert(maps:get(rows, Bindings), Row),
    {Bindings#{rows => S, next_id => Id + 1}, #{}, []};
handle_event(~"delete_row", #{~"id" := Id}, Bindings) ->
    S = arizona_stream:delete(maps:get(rows, Bindings), Id),
    {Bindings#{rows => S}, #{}, []};
handle_event(~"move_top", #{~"id" := Id}, Bindings) ->
    S = arizona_stream:move(maps:get(rows, Bindings), Id, 0),
    {Bindings#{rows => S}, #{}, []};
handle_event(~"reset_data", _Payload, Bindings) ->
    Items = [
        #{id => 1, name => ~"Alice", age => 30},
        #{id => 2, name => ~"Bob", age => 25},
        #{id => 3, name => ~"Charlie", age => 35},
        #{id => 4, name => ~"Diana", age => 28},
        #{id => 5, name => ~"Eve", age => 32}
    ],
    S = arizona_stream:reset(maps:get(rows, Bindings), Items),
    {Bindings#{rows => S, next_id => 6, sort_col => id, sort_dir => asc}, #{}, []};
handle_event(~"shuffle", _Payload, Bindings) ->
    Items = arizona_stream:to_list(maps:get(rows, Bindings)),
    Shuffled = [X || {_, X} <:- lists:sort([{rand:uniform(), I} || I <:- Items])],
    S = arizona_stream:reset(maps:get(rows, Bindings), Shuffled),
    {Bindings#{rows => S}, #{}, []}.

-spec handle_info(term(), az:bindings()) -> az:handle_info_ret().
handle_info(arizona_connected, Bindings) ->
    {Bindings#{connected => true}, #{}, [arizona_js:set_title(maps:get(title, Bindings))]}.

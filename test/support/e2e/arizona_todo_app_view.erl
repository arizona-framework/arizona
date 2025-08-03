-module(arizona_todo_app_view).
-behaviour(arizona_view).
-compile({parse_transform, arizona_parse_transform}).
-export([mount/1]).
-export([render/1]).
-export([handle_event/3]).
-export([render_stats/1]).
-export([render_filters/1]).
-export([render_clear_button/1]).
-export([filter_todos/2]).

mount(_Req) ->
    InitialTodos = [
        #{id => 1, text => ~"Learn Erlang", completed => false},
        #{id => 2, text => ~"Build web app", completed => true},
        #{id => 3, text => ~"Write tests", completed => false}
    ],
    Layout = {arizona_todo_app_layout, render, main_content, #{}},
    arizona_view:new(
        ?MODULE,
        #{
            id => ~"todos",
            todos => InitialTodos,
            filter => all,
            new_todo_text => ~"",
            next_id => 4
        },
        Layout
    ).

handle_event(~"add_todo", _Params, State) ->
    NewTodoText = arizona_stateful:get_binding(new_todo_text, State),
    case string:trim(NewTodoText) of
        ~"" ->
            {noreply, State};
        Text ->
            Todos = arizona_stateful:get_binding(todos, State),
            NextId = arizona_stateful:get_binding(next_id, State),
            NewTodo = #{id => NextId, text => Text, completed => false},
            UpdatedTodos = Todos ++ [NewTodo],
            UpdatedState = arizona_stateful:merge_bindings(
                #{
                    todos => UpdatedTodos,
                    new_todo_text => ~"",
                    next_id => NextId + 1
                },
                State
            ),
            {noreply, UpdatedState}
    end;
handle_event(~"toggle_todo", #{~"id" := IdBin}, State) ->
    Id = binary_to_integer(IdBin),
    Todos = arizona_stateful:get_binding(todos, State),
    UpdatedTodos = lists:map(
        fun(Todo = #{id := TodoId}) ->
            case TodoId of
                Id -> Todo#{completed => not maps:get(completed, Todo)};
                _ -> Todo
            end
        end,
        Todos
    ),
    UpdatedState = arizona_stateful:put_binding(todos, UpdatedTodos, State),
    {noreply, UpdatedState};
handle_event(~"delete_todo", #{~"id" := IdBin}, State) ->
    Id = binary_to_integer(IdBin),
    Todos = arizona_stateful:get_binding(todos, State),
    UpdatedTodos = lists:filter(fun(#{id := TodoId}) -> TodoId =/= Id end, Todos),
    UpdatedState = arizona_stateful:put_binding(todos, UpdatedTodos, State),
    {noreply, UpdatedState};
handle_event(~"set_filter", #{~"filter" := Filter}, State) ->
    FilterAtom = binary_to_existing_atom(Filter),
    UpdatedState = arizona_stateful:put_binding(filter, FilterAtom, State),
    {noreply, UpdatedState};
handle_event(~"update_new_todo", #{~"value" := Value}, State) ->
    UpdatedState = arizona_stateful:put_binding(new_todo_text, Value, State),
    {noreply, UpdatedState};
handle_event(~"clear_completed", _Params, State) ->
    Todos = arizona_stateful:get_binding(todos, State),
    UpdatedTodos = lists:filter(fun(#{completed := Completed}) -> not Completed end, Todos),
    UpdatedState = arizona_stateful:put_binding(todos, UpdatedTodos, State),
    {noreply, UpdatedState}.

render(Bindings) ->
    arizona_template:from_string(~""""
    <div
        id="{arizona_binder:get(id, Bindings)}"
        class="todo-app"
    >
        <header class="header">
            <h1>todos</h1>
            <input
                class="new-todo"
                placeholder="What needs to be done?"
                data-testid="new-todo-input"
                value="{arizona_template:get_binding(new_todo_text, Bindings)}"
                onkeyup="if(event.key === 'Enter') arizona.sendEvent('add_todo'); else arizona.sendEvent('update_new_todo', \{value: event.target.value})"
            />
        </header>

        <main class="main" data-testid="main-section">
            {arizona_template:render_list(
                fun(Todo) -> arizona_template:from_string(~"""
                <div class="todo-item {case maps:get(completed, Todo) of true -> ~"completed"; false -> ~"" end}" data-testid="todo-{maps:get(id, Todo)}">
                    <input
                        type="checkbox"
                        class="toggle"
                        data-testid="toggle-{maps:get(id, Todo)}"
                        {case maps:get(completed, Todo) of true -> ~"checked"; false -> ~"" end}
                        onclick="arizona.sendEvent('toggle_todo', \{id: '{maps:get(id, Todo)}'})"
                    />
                    <label class="todo-text" data-testid="todo-text-{maps:get(id, Todo)}">{maps:get(text, Todo)}</label>
                    <button
                        class="destroy"
                        data-testid="delete-{maps:get(id, Todo)}"
                        onclick="arizona.sendEvent('delete_todo', \{id: '{maps:get(id, Todo)}'})"
                    >&times;</button>
                </div>
                """) end,
                arizona_todo_app_view:filter_todos(arizona_template:get_binding(todos, Bindings), arizona_template:get_binding(filter, Bindings))
            )}
        </main>

        <footer class="footer" data-testid="footer">
            {arizona_template:render_stateless(arizona_todo_app_view, render_stats, Bindings)}
            {arizona_template:render_stateless(arizona_todo_app_view, render_filters, Bindings)}
            {case length(arizona_template:get_binding(todos, Bindings)) > length(lists:filter(fun(#{completed := Completed}) -> not Completed end, arizona_template:get_binding(todos, Bindings))) of
                true -> arizona_template:render_stateless(arizona_todo_app_view, render_clear_button, Bindings);
                false -> ~""
            end}
        </footer>
    </div>
    """").

render_stats(Bindings) ->
    arizona_template:from_string(~"""
    <span class="todo-count" data-testid="todo-count">
        <strong>{length(lists:filter(fun(#{completed := Completed}) -> not Completed end, arizona_template:get_binding(todos, Bindings)))}</strong> {case length(lists:filter(fun(#{completed := Completed}) -> not Completed end, arizona_template:get_binding(todos, Bindings))) of 1 -> ~"item"; _ -> ~"items" end} left
    </span>
    """).

render_filters(Bindings) ->
    arizona_template:from_string(~"""
    <ul class="filters" data-testid="filters">
        <li>
            <a class="{case arizona_template:get_binding(filter, Bindings) of all -> ~"selected"; _ -> ~"" end}"
               data-testid="filter-all"
               onclick="arizona.sendEvent('set_filter', \{filter: 'all'})">All</a>
        </li>
        <li>
            <a class="{case arizona_template:get_binding(filter, Bindings) of active -> ~"selected"; _ -> ~"" end}"
               data-testid="filter-active"
               onclick="arizona.sendEvent('set_filter', \{filter: 'active'})">Active</a>
        </li>
        <li>
            <a class="{case arizona_template:get_binding(filter, Bindings) of completed -> ~"selected"; _ -> ~"" end}"
               data-testid="filter-completed"
               onclick="arizona.sendEvent('set_filter', \{filter: 'completed'})">Completed</a>
        </li>
    </ul>
    """).

render_clear_button(_Bindings) ->
    arizona_template:from_string(~"""
    <button
        class="clear-completed"
        data-testid="clear-completed"
        onclick="arizona.sendEvent('clear_completed')"
    >
        Clear completed
    </button>
    """).

filter_todos(Todos, all) ->
    Todos;
filter_todos(Todos, active) ->
    lists:filter(fun(#{completed := Completed}) -> not Completed end, Todos);
filter_todos(Todos, completed) ->
    lists:filter(fun(#{completed := Completed}) -> Completed end, Todos).

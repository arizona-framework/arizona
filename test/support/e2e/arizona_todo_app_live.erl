-module(arizona_todo_app_live).
-encoding(utf8).
-behaviour(arizona_live).
-compile({parse_transform, arizona_parse_transform}).
-arizona_parse_transform([render/1]).
-export([
    mount/2,
    handle_event/3,
    render/1,
    render_stats/1,
    render_filters/1,
    render_clear_button/1,
    filter_todos/2
]).

mount(_Params, Socket) ->
    InitialTodos = [
        #{id => 1, text => ~"Learn Erlang", completed => false},
        #{id => 2, text => ~"Build web app", completed => true},
        #{id => 3, text => ~"Write tests", completed => false}
    ],
    Socket1 = arizona_socket:put_bindings(
        #{
            todos => InitialTodos,
            filter => all,
            new_todo_text => ~"",
            next_id => 4
        },
        Socket
    ),
    arizona_socket:set_layout({arizona_todo_app_layout, render, main_content}, Socket1).

handle_event(~"add_todo", _Params, Socket) ->
    NewTodoText = arizona_socket:get_binding(new_todo_text, Socket),
    case string:trim(NewTodoText) of
        ~"" ->
            {noreply, Socket};
        Text ->
            Todos = arizona_socket:get_binding(todos, Socket),
            NextId = arizona_socket:get_binding(next_id, Socket),
            NewTodo = #{id => NextId, text => Text, completed => false},
            UpdatedTodos = Todos ++ [NewTodo],
            UpdatedSocket = arizona_socket:put_bindings(
                #{
                    todos => UpdatedTodos,
                    new_todo_text => ~"",
                    next_id => NextId + 1
                },
                Socket
            ),
            {noreply, UpdatedSocket}
    end;
handle_event(~"toggle_todo", #{~"id" := IdBin}, Socket) ->
    Id = binary_to_integer(IdBin),
    Todos = arizona_socket:get_binding(todos, Socket),
    UpdatedTodos = lists:map(
        fun(Todo = #{id := TodoId}) ->
            case TodoId of
                Id -> Todo#{completed => not maps:get(completed, Todo)};
                _ -> Todo
            end
        end,
        Todos
    ),
    UpdatedSocket = arizona_socket:put_binding(todos, UpdatedTodos, Socket),
    {noreply, UpdatedSocket};
handle_event(~"delete_todo", #{~"id" := IdBin}, Socket) ->
    Id = binary_to_integer(IdBin),
    Todos = arizona_socket:get_binding(todos, Socket),
    UpdatedTodos = lists:filter(fun(#{id := TodoId}) -> TodoId =/= Id end, Todos),
    UpdatedSocket = arizona_socket:put_binding(todos, UpdatedTodos, Socket),
    {noreply, UpdatedSocket};
handle_event(~"set_filter", #{~"filter" := Filter}, Socket) ->
    FilterAtom = binary_to_existing_atom(Filter),
    UpdatedSocket = arizona_socket:put_binding(filter, FilterAtom, Socket),
    {noreply, UpdatedSocket};
handle_event(~"update_new_todo", #{~"value" := Value}, Socket) ->
    UpdatedSocket = arizona_socket:put_binding(new_todo_text, Value, Socket),
    {noreply, UpdatedSocket};
handle_event(~"clear_completed", _Params, Socket) ->
    Todos = arizona_socket:get_binding(todos, Socket),
    UpdatedTodos = lists:filter(fun(#{completed := Completed}) -> not Completed end, Todos),
    UpdatedSocket = arizona_socket:put_binding(todos, UpdatedTodos, Socket),
    {noreply, UpdatedSocket}.

render(Socket) ->
    arizona_html:render_live(~""""
    <div id="root" class="todo-app">
        <header class="header">
            <h1>todos</h1>
            <input 
                class="new-todo" 
                placeholder="What needs to be done?" 
                data-testid="new-todo-input"
                value="{arizona_socket:get_binding(new_todo_text, Socket)}"
                onkeyup="if(event.key === 'Enter') arizona.sendEvent('add_todo'); else arizona.sendEvent('update_new_todo', \{value: event.target.value})"
            />
        </header>
        
        <main class="main" data-testid="main-section">
            {arizona_html:render_list(
                fun(Todo) -> ~"""
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
                """ end,
                arizona_todo_app_live:filter_todos(arizona_socket:get_binding(todos, Socket), arizona_socket:get_binding(filter, Socket)),
                Socket
            )}
        </main>
        
        <footer class="footer" data-testid="footer">
            {arizona_component:call_stateless(arizona_todo_app_live, render_stats, #{}, Socket)}
            {arizona_component:call_stateless(arizona_todo_app_live, render_filters, #{}, Socket)}
            {case length(arizona_socket:get_binding(todos, Socket)) > length(lists:filter(fun(#{completed := Completed}) -> not Completed end, arizona_socket:get_binding(todos, Socket))) of
                true -> arizona_component:call_stateless(arizona_todo_app_live, render_clear_button, #{}, Socket);
                false -> ~""
            end}
        </footer>
    </div>
    """", Socket).

render_stats(Socket) ->
    arizona_html:render_stateless(~"""
    <span class="todo-count" data-testid="todo-count">
        <strong>{length(lists:filter(fun(#{completed := Completed}) -> not Completed end, arizona_socket:get_binding(todos, Socket)))}</strong> {case length(lists:filter(fun(#{completed := Completed}) -> not Completed end, arizona_socket:get_binding(todos, Socket))) of 1 -> ~"item"; _ -> ~"items" end} left
    </span>
    """, Socket).

render_filters(Socket) ->
    arizona_html:render_stateless(~"""
    <ul class="filters" data-testid="filters">
        <li>
            <a class="{case arizona_socket:get_binding(filter, Socket) of all -> ~"selected"; _ -> ~"" end}" 
               data-testid="filter-all"
               onclick="arizona.sendEvent('set_filter', \{filter: 'all'})">All</a>
        </li>
        <li>
            <a class="{case arizona_socket:get_binding(filter, Socket) of active -> ~"selected"; _ -> ~"" end}" 
               data-testid="filter-active"
               onclick="arizona.sendEvent('set_filter', \{filter: 'active'})">Active</a>
        </li>
        <li>
            <a class="{case arizona_socket:get_binding(filter, Socket) of completed -> ~"selected"; _ -> ~"" end}" 
               data-testid="filter-completed"
               onclick="arizona.sendEvent('set_filter', \{filter: 'completed'})">Completed</a>
        </li>
    </ul>
    """, Socket).

render_clear_button(Socket) ->
    arizona_html:render_stateless(~"""
    <button class="clear-completed" 
            data-testid="clear-completed"
            onclick="arizona.sendEvent('clear_completed')">
        Clear completed
    </button>
    """, Socket).

filter_todos(Todos, all) ->
    Todos;
filter_todos(Todos, active) ->
    lists:filter(fun(#{completed := Completed}) -> not Completed end, Todos);
filter_todos(Todos, completed) ->
    lists:filter(fun(#{completed := Completed}) -> Completed end, Todos).

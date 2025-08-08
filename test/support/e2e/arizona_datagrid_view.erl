-module(arizona_datagrid_view).
-behaviour(arizona_view).
-compile({parse_transform, arizona_parse_transform}).

-export([mount/1]).
-export([render/1]).
-export([render_table/1]).

mount(_Req) ->
    Layout = {arizona_datagrid_layout, render, main_content, #{}},
    arizona_view:new(
        ?MODULE,
        #{
            id => ~"datagrid",
            cols => [
                #{name => name, label => ~"Name", sortable => true},
                #{name => email, label => ~"Email", sortable => true},
                #{name => role, label => ~"Role", sortable => false},
                #{name => status, label => ~"Status", sortable => false},
                #{name => created_at, label => ~"Created", sortable => true},
                #{name => actions, label => ~"Actions", sortable => false}
            ],
            rows => [
                #{
                    id => 1,
                    name => ~"John Doe",
                    email => ~"john.doe@example.com",
                    role => ~"admin",
                    status => ~"active",
                    created_at => ~"2024-01-15"
                },
                #{
                    id => 2,
                    name => ~"Jane Smith",
                    email => ~"jane.smith@example.com",
                    role => ~"user",
                    status => ~"active",
                    created_at => ~"2024-02-20"
                },
                #{
                    id => 3,
                    name => ~"Bob Wilson",
                    email => ~"bob.wilson@example.com",
                    role => ~"moderator",
                    status => ~"inactive",
                    created_at => ~"2024-01-30"
                },
                #{
                    id => 4,
                    name => ~"Alice Johnson",
                    email => ~"alice.johnson@example.com",
                    role => ~"user",
                    status => ~"pending",
                    created_at => ~"2024-03-10"
                }
            ],
            callback => fun(Col, Row) ->
                arizona_template:from_string(~""""
                {
                    case Col of
                        #{name := name} ->
                            arizona_template:from_string(~"""
                            <strong>{maps:get(name, Row)}</strong>
                            """);
                        #{name := email} ->
                            arizona_template:from_string(~"""
                            <a href="mailto:{maps:get(email, Row)}">
                                {maps:get(email, Row)}
                            </a>
                            """);
                        #{name := role} ->
                            RoleClass = case maps:get(role, Row) of
                                ~"admin" -> ~"status-badge role-badge-admin";
                                ~"moderator" -> ~"status-badge role-badge-moderator";
                                ~"user" -> ~"status-badge role-badge-user"
                            end,
                            arizona_template:from_string(~"""
                            <span class="{RoleClass}">
                                {maps:get(role, Row)}
                            </span>
                            """);
                        #{name := status} ->
                            {StatusClass, StatusIcon} =
                                case maps:get(status, Row) of
                                    ~"active" -> {~"status-badge status-badge-active", ~"&#10003;"};
                                    ~"inactive" -> {~"status-badge status-badge-inactive", ~"&#9675;"};
                                    ~"pending" -> {~"status-badge status-badge-pending", ~"&#8987;"}
                                end,
                            arizona_template:from_string(~"""
                            <span class="{StatusClass}">
                                {StatusIcon} {maps:get(status, Row)}
                            </span>
                            """);
                        #{name := created_at} ->
                            arizona_template:from_string(~"""
                            <small class="text-muted">
                                {maps:get(created_at, Row)}
                            </small>
                            """);
                        #{name := actions} ->
                            UserId = maps:get(id, Row),
                            arizona_template:from_string(~"""
                            <div class="action-button-group" role="group">
                                <button type="button" class="action-btn action-btn-primary" onclick="editUser({UserId})">
                                    &#9997; Edit
                                </button>
                                <button type="button" class="action-btn action-btn-danger" onclick="deleteUser({UserId})">
                                    &#128465; Delete
                                </button>
                            </div>
                            """)
                    end
                }
                """")
            end
        },
        Layout
    ).

render(Bindings) ->
    arizona_template:from_string(~""""
    <div id="{arizona_template:get_binding(id, Bindings)}">
        {arizona_template:render_stateless(arizona_datagrid_view, render_table, #{
            cols => arizona_template:get_binding(cols, Bindings),
            rows => arizona_template:get_binding(rows, Bindings),
            callback => arizona_template:get_binding(callback, Bindings)
        })}
    </div>
    """").

render_table(Bindings) ->
    arizona_template:from_string(~""""""
    <div class="table-responsive">
        <table class="table table-striped table-hover">
            <thead class="table-dark">
                <tr>
                    {arizona_template:render_list(fun(Col) ->
                        arizona_template:from_string(~""""
                        <th scope="col" class="{
                            case maps:get(sortable, Col, false) of
                                true -> ~"sortable";
                                false -> ~""
                            end
                        }" {
                            case maps:get(sortable, Col, false) of
                                true ->
                                    ColumnName = maps:get(name, Col),
                                    arizona_template:from_string(~"""
                                    style="cursor: pointer;"
                                    onclick="sortTable('{ColumnName}')"
                                    """);
                                false -> ~""
                            end
                        }>
                            {maps:get(label, Col)}
                            {
                                case maps:get(sortable, Col, false) of
                                    true -> ~" <i class=\"fas fa-sort\"></i>";
                                    false -> ~""
                                end
                            }
                        </th>
                        """")
                    end, arizona_template:get_binding(cols, Bindings))}
                </tr>
            </thead>
            <tbody>
                {
                    Rows = arizona_template:get_binding(rows, Bindings),
                    Cols = arizona_template:get_binding(cols, Bindings),
                    Callback = arizona_template:get_binding(callback, Bindings),
                    arizona_template:render_list(fun(Row) ->
                        arizona_template:from_string(~""""
                        <tr class="user-row" data-user-id="{maps:get(id, Row)}">
                            {arizona_template:render_list(fun(Col) ->
                                arizona_template:from_string(~"""
                                <td class="align-middle">
                                    {Callback(Col, Row)}
                                </td>
                                """)
                            end, Cols)}
                        </tr>
                        """")
                    end, Rows)
                }
            </tbody>
        </table>
    </div>
    """""").

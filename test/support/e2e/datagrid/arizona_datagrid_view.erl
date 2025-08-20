-module(arizona_datagrid_view).
-behaviour(arizona_view).
-compile({parse_transform, arizona_parse_transform}).
-export([mount/1]).
-export([render/1]).
-export([render_table/1]).
-export([handle_event/3]).

mount(Req) ->
    Layout =
        {arizona_datagrid_layout, render, main_content, #{
            active_url => arizona_request:get_path(Req)
        }},
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
                    created_at => ~"2025-08-01"
                },
                #{
                    id => 2,
                    name => ~"Jane Smith",
                    email => ~"jane.smith@example.com",
                    role => ~"user",
                    status => ~"active",
                    created_at => ~"2025-08-05"
                },
                #{
                    id => 3,
                    name => ~"Bob Wilson",
                    email => ~"bob.wilson@example.com",
                    role => ~"moderator",
                    status => ~"inactive",
                    created_at => ~"2025-07-28"
                },
                #{
                    id => 4,
                    name => ~"Alice Johnson",
                    email => ~"alice.johnson@example.com",
                    role => ~"user",
                    status => ~"pending",
                    created_at => ~"2025-08-07"
                },
                #{
                    id => 5,
                    name => ~"Mike Davis",
                    email => ~"mike.davis@example.com",
                    role => ~"admin",
                    status => ~"active",
                    created_at => ~"2025-07-25"
                },
                #{
                    id => 6,
                    name => ~"Sarah Brown",
                    email => ~"sarah.brown@example.com",
                    role => ~"moderator",
                    status => ~"active",
                    created_at => ~"2025-08-03"
                },
                #{
                    id => 7,
                    name => ~"David Lee",
                    email => ~"david.lee@example.com",
                    role => ~"user",
                    status => ~"inactive",
                    created_at => ~"2025-07-20"
                },
                #{
                    id => 8,
                    name => ~"Emma Garcia",
                    email => ~"emma.garcia@example.com",
                    role => ~"user",
                    status => ~"active",
                    created_at => ~"2025-08-06"
                },
                #{
                    id => 9,
                    name => ~"Chris Martin",
                    email => ~"chris.martin@example.com",
                    role => ~"moderator",
                    status => ~"pending",
                    created_at => ~"2025-08-08"
                },
                #{
                    id => 10,
                    name => ~"Lisa Thompson",
                    email => ~"lisa.thompson@example.com",
                    role => ~"admin",
                    status => ~"active",
                    created_at => ~"2025-07-30"
                },
                #{
                    id => 11,
                    name => ~"Kevin White",
                    email => ~"kevin.white@example.com",
                    role => ~"user",
                    status => ~"inactive",
                    created_at => ~"2025-07-15"
                },
                #{
                    id => 12,
                    name => ~"Jennifer Taylor",
                    email => ~"jennifer.taylor@example.com",
                    role => ~"user",
                    status => ~"active",
                    created_at => ~"2025-08-02"
                },
                #{
                    id => 13,
                    name => ~"Ryan Anderson",
                    email => ~"ryan.anderson@example.com",
                    role => ~"moderator",
                    status => ~"pending",
                    created_at => ~"2025-08-04"
                },
                #{
                    id => 14,
                    name => ~"Michelle Moore",
                    email => ~"michelle.moore@example.com",
                    role => ~"user",
                    status => ~"active",
                    created_at => ~"2025-07-22"
                },
                #{
                    id => 15,
                    name => ~"Daniel Clark",
                    email => ~"daniel.clark@example.com",
                    role => ~"admin",
                    status => ~"inactive",
                    created_at => ~"2025-07-18"
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
                                <button
                                    type="button"
                                    class="action-btn action-btn-danger"
                                    onclick="arizona.sendEvent('delete_user', \{id: '{UserId}'})"
                                >
                                    &#128465; Delete
                                </button>
                            </div>
                            """)
                    end
                }
                """")
            end,
            sort_column => none,
            sort_direction => asc
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
                                    onclick="arizona.sendEvent('sort_table', \{column: '{ColumnName}'})"
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

handle_event(~"delete_user", #{~"id" := IdBin}, View) ->
    State = arizona_view:get_state(View),
    Id = binary_to_integer(IdBin),
    Rows = arizona_stateful:get_binding(rows, State),
    UpdatedRows = lists:filter(fun(#{id := UserId}) -> UserId =/= Id end, Rows),
    UpdatedState = arizona_stateful:put_binding(rows, UpdatedRows, State),
    UpdatedView = arizona_view:update_state(UpdatedState, View),
    {noreply, UpdatedView};
handle_event(~"sort_table", #{~"column" := ColumnBin}, View) ->
    State = arizona_view:get_state(View),
    Column = binary_to_existing_atom(ColumnBin, utf8),
    CurrentColumn = arizona_stateful:get_binding(sort_column, State),
    CurrentDirection = arizona_stateful:get_binding(sort_direction, State),

    % Toggle direction if same column, otherwise default to ascending
    NewDirection =
        case {CurrentColumn, CurrentDirection} of
            {Column, asc} -> desc;
            {Column, desc} -> asc;
            _ -> asc
        end,

    Rows = arizona_stateful:get_binding(rows, State),
    SortedRows = sort_rows(Rows, Column, NewDirection),

    UpdatedState = arizona_stateful:merge_bindings(
        #{
            rows => SortedRows,
            sort_column => Column,
            sort_direction => NewDirection
        },
        State
    ),
    UpdatedView = arizona_view:update_state(UpdatedState, View),
    {noreply, UpdatedView}.

%% Helper function to sort rows by column and direction
sort_rows(Rows, Column, Direction) ->
    SortFun =
        case Direction of
            asc -> fun(A, B) -> compare_values(maps:get(Column, A), maps:get(Column, B)) end;
            desc -> fun(A, B) -> not compare_values(maps:get(Column, A), maps:get(Column, B)) end
        end,
    lists:sort(SortFun, Rows).

%% Helper function to compare values for sorting (returns true if A should come before B)
compare_values(A, B) when is_binary(A), is_binary(B) ->
    string:casefold(A) =< string:casefold(B);
compare_values(A, B) when is_integer(A), is_integer(B) ->
    A =< B;
compare_values(A, B) ->
    A =< B.

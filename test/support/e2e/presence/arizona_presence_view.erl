-module(arizona_presence_view).
-compile({parse_transform, arizona_parse_transform}).
-behaviour(arizona_view).

-export([mount/2, render/1, handle_event/3, terminate/2]).

mount(_MountArg, Request) ->
    % Subscribe to presence events only if this is a live connection
    case arizona_live:is_connected(self()) of
        true ->
            arizona_pubsub:join(~"presence", self());
        false ->
            ok
    end,

    % Get current online users from server
    OnlineUsers = arizona_presence_server:get_online_users(),
    OnlineUsersCount = arizona_presence_server:get_online_users_count(),

    Bindings = #{
        id => ~"presence-view",
        title => ~"User Presence Demo",
        online_users => OnlineUsers,
        online_users_count => OnlineUsersCount,
        % Will be generated client-side
        current_user_id => ~"",
        % Will be generated client-side
        current_user_name => ~"",
        is_joined => false,
        is_loading => true
    },

    Layout =
        {arizona_presence_layout, render, main_content, #{
            active_url => arizona_request:get_path(Request)
        }},

    arizona_view:new(?MODULE, Bindings, Layout).

render(Bindings) ->
    arizona_template:from_string(~""""
    <div
        id="{arizona_template:get_binding(id, Bindings)}"
        data-testid="presence-view"
        class="container"
    >
        <h1>User Presence Demo</h1>
        <p>This example shows Arizona's hybrid approach: REST API + real-time WebSocket updates</p>

        <div class="user-info">
            <h3>Your Info</h3>
            <p><strong>User ID:</strong><span class="user-id">
                {
                    case arizona_template:get_binding(is_loading, Bindings) of
                        true ->
                            ~"Loading...";
                        false ->
                            arizona_template:get_binding(current_user_id, Bindings)
                    end
                }
            </span></p>
            <p><strong>User Name:</strong><span class="user-name">
                {
                    case arizona_template:get_binding(is_loading, Bindings) of
                        true ->
                            ~"Loading...";
                        false ->
                            arizona_template:get_binding(current_user_name, Bindings)
                    end
                }
            </span></p>
        </div>

        <div class="controls">
            <button id="join-btn" data-testid="join-btn" class="join-btn" onclick="joinPresence()">Join Presence</button>
            <button id="leave-btn" data-testid="leave-btn" class="leave-btn" onclick="leavePresence()" disabled>Leave Presence</button>
        </div>

        <div id="status"></div>

        <div class="online-users">
            <h3>Online Users ({arizona_template:get_binding(online_users_count, Bindings)})</h3>
            <div id="users-list">
                {
                    OnlineUsers = arizona_template:get_binding(online_users, Bindings),
                    arizona_template:render_map(fun({UserId, UserInfo}) ->
                        arizona_template:from_string(~"""
                        <div class="user-item">
                            <strong>{maps:get(user_name, UserInfo)}</strong>
                            (ID: {UserId})
                        </div>
                        """)
                    end, OnlineUsers)
                }
            </div>
        </div>

        <script defer>
            // Generate user ID client-side to avoid server-side mount inconsistencies
            const currentUserId = 'user_' + Date.now() + '_' + Math.floor(Math.random() * 1000);
            const currentUserName = 'USER' + Date.now() + '_' + Math.floor(Math.random() * 1000);
            let isJoined = false;

            // Update the displayed user info
            const userIdSpan = document.querySelector('.user-id');
            const userNameSpan = document.querySelector('.user-name');
            if (userIdSpan) userIdSpan.textContent = currentUserId;
            if (userNameSpan) userNameSpan.textContent = currentUserName;

            document.addEventListener('arizonaEvent', (\{ detail: event }) => \{
                if (event.type !== 'status') return
                if (event.data.status !== 'connected') return;

                // Send user info when WebSocket connects
                arizona.sendEvent('set_user_info', \{
                    user_id: currentUserId,
                    user_name: currentUserName
                });
            });

            function showStatus(message, isError = false) \{
                const status = document.getElementById('status');
                status.className = 'status ' + (isError ? 'error' : 'success');
                status.textContent = message;
                status.style.display = 'block';
                setTimeout(() => \{
                    status.textContent = '';
                    status.style.display = 'none';
                }, 3000);
            }

            function updateButtons() \{
                const joinBtn = document.getElementById('join-btn');
                const leaveBtn = document.getElementById('leave-btn');

                if (isJoined) \{
                    joinBtn.disabled = true;
                    leaveBtn.disabled = false;
                } else \{
                    joinBtn.disabled = false;
                    leaveBtn.disabled = true;
                }
            }

            async function joinPresence() \{
                try \{
                    const response = await fetch('/api/presence/join', \{
                        method: 'POST',
                        headers: \{ 'Content-Type': 'application/json' },
                        body: JSON.stringify(\{
                            user_id: currentUserId,
                            user_name: currentUserName
                        })
                    });

                    const result = await response.json();
                    if (response.ok && result.status === 'success') \{
                        isJoined = true;
                        updateButtons();
                        showStatus('Successfully joined presence!');
                    } else \{
                        showStatus('Failed to join: ' + (result.message || 'Unknown error'), true);
                    }
                } catch (error) \{
                    showStatus('Network error: ' + error.message, true);
                }
            }

            async function leavePresence() \{
                try \{
                    const response = await fetch('/api/presence/leave', \{
                        method: 'POST',
                        headers: \{ 'Content-Type': 'application/json' },
                        body: JSON.stringify(\{
                            user_id: currentUserId,
                            user_name: currentUserName
                        })
                    });

                    const result = await response.json();
                    if (response.ok && result.status === 'success') \{
                        isJoined = false;
                        updateButtons();
                        showStatus('Successfully left presence!');
                    } else \{
                        showStatus('Failed to leave: ' + (result.message || 'Unknown error'), true);
                    }
                } catch (error) \{
                    showStatus('Network error: ' + error.message, true);
                }
            }
        </script>
    </div>
    """").

handle_event(~"set_user_info", Params, View) ->
    % Update view bindings with client-generated user info
    UserId = maps:get(~"user_id", Params),
    UserName = maps:get(~"user_name", Params),

    State = arizona_view:get_state(View),
    UpdatedState = arizona_stateful:merge_bindings(
        #{
            current_user_id => UserId,
            current_user_name => UserName,
            is_loading => false
        },
        State
    ),
    UpdatedView = arizona_view:update_state(UpdatedState, View),
    {[], UpdatedView};
handle_event(~"presence", Event, View) ->
    % The server already includes the updated online_users list and count in events
    OnlineUsers = maps:get(online_users, Event, #{}),
    OnlineUsersCount = maps:get(online_users_count, Event, 0),

    State = arizona_view:get_state(View),
    UpdatedState = arizona_stateful:merge_bindings(
        #{
            online_users => OnlineUsers,
            online_users_count => OnlineUsersCount,
            is_joined => true
        },
        State
    ),
    UpdatedView = arizona_view:update_state(UpdatedState, View),
    {[], UpdatedView}.

terminate(_Reason, View) ->
    State = arizona_view:get_state(View),

    % Only cleanup if user ID was set and user actually joined
    case arizona_stateful:get_binding(is_joined, State) of
        true ->
            CurrentUserId = arizona_stateful:get_binding(current_user_id, State),
            case arizona_presence_server:is_user_joined(CurrentUserId) of
                true ->
                    arizona_presence_server:leave_user(CurrentUserId);
                false ->
                    ok
            end;
        false ->
            ok
    end.

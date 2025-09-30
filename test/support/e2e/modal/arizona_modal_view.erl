-module(arizona_modal_view).
-behaviour(arizona_view).
-compile({parse_transform, arizona_parse_transform}).
-export([mount/2]).
-export([render/1]).
-export([render_modal/1]).
-export([handle_event/3]).

mount(_Arg, Req) ->
    Layout =
        {arizona_modal_layout, render, main_content, #{
            active_url => arizona_request:get_path(Req)
        }},
    arizona_view:new(
        ?MODULE,
        #{
            id => ~"modal",
            user => #{username => ~"Joe"},
            modal_type => success,
            modal_visible => false
        },
        Layout
    ).

render(Bindings) ->
    arizona_template:from_html(~""""
    <div id="{arizona_template:get_binding(id, Bindings)}">
        <!-- Control buttons to test modal functionality -->
        <div class="control-container">
            <button
                class="control-btn control-btn-success"
                onclick="arizona.pushEvent('set_modal_type', \{type: 'success'})"
            >
                Open Success Modal
            </button>
            <button
                class="control-btn control-btn-error"
                onclick="arizona.pushEvent('set_modal_type', \{type: 'error'})"
            >
                Open Error Modal
            </button>
            <button
                class="control-btn control-btn-info"
                onclick="arizona.pushEvent('set_modal_type', \{type: 'info'})"
            >
                Open Info Modal
            </button>
        </div>

        {case arizona_template:get_binding(modal_visible, Bindings) of
            true ->
                User = arizona_template:get_binding(user, Bindings),
                ModalType = arizona_template:get_binding(modal_type, Bindings),
                get_modal_slots(ModalType, User);
            false ->
                ~""
        end}
    </div>
    """").

render_modal(Bindings) ->
    arizona_template:from_html(~""""
    <div class="modal-overlay" onclick="arizona.pushEvent('close_modal')">
        <div class="modal-container" onclick="event.stopPropagation()">
            <div class="modal-header">
                <h1>
                    {arizona_template:render_slot(
                        arizona_template:get_binding(header, Bindings)
                    )}
                </h1>
                <button
                    class="modal-close-btn"
                    onclick="arizona.pushEvent('close_modal')"
                    style="position: absolute; top: 16px; right: 16px; background: none; border: none; font-size: 24px; cursor: pointer; color: #6b7280; padding: 8px;"
                >
                    &times;
                </button>
            </div>

            <div class="modal-body">
                {arizona_template:render_slot(
                   arizona_template:get_binding(inner_block, Bindings)
                )}
            </div>

            <div class="modal-footer">
                {
                    Confirm = arizona_template:get_binding(confirm, Bindings),
                    arizona_template:from_html(~"""
                    <button class="{maps:get(classes, Confirm)}" onclick="arizona.pushEvent('close_modal')">
                        {arizona_template:render_slot(maps:get(content, Confirm))}
                    </button>
                    """)
                }
                {
                    Cancel = arizona_template:get_binding(cancel, Bindings),
                    arizona_template:from_html(~"""
                    <button class="{maps:get(classes, Cancel)}" onclick="arizona.pushEvent('close_modal')">
                        {arizona_template:render_slot(maps:get(content, Cancel))}
                    </button>
                    """)
                }
            </div>
        </div>
    </div>
    """").

%% Helper function to generate modal slots based on type
get_modal_slots(ModalType, User) ->
    arizona_template:render_stateless(arizona_modal_view, render_modal, #{
        header => get_header_slot(ModalType),
        inner_block => get_inner_block_slot(ModalType, User),
        confirm => get_confirm_slot(ModalType),
        cancel => get_cancel_slot(ModalType)
    }).

%% Generate header slot based on modal type
get_header_slot(success) ->
    ~"""
    <span class="header-icon success">&#9989;</span> Success modal
    """;
get_header_slot(error) ->
    ~"""
    <span class="header-icon error">&#10060;</span> Error modal
    """;
get_header_slot(info) ->
    ~"""
    <span class="header-icon info">&#8505;</span> Info modal
    """.

%% Generate inner_block slot based on modal type
get_inner_block_slot(success, User) ->
    arizona_template:from_html(~"""
    <div class="modal-icon modal-icon-success">
        <div class="icon-checkmark"></div>
    </div>
    <div class="greeting text-success">
        Hey, {maps:get(username, User)}!
    </div>
    <div class="message">
        Your settings have been <strong>successfully</strong> saved
    </div>
    """);
get_inner_block_slot(error, User) ->
    arizona_template:from_html(~"""
    <div class="modal-icon modal-icon-error">
        <div class="icon-warning"></div>
    </div>
    <div class="greeting text-error">
        Sorry, {maps:get(username, User)}!
    </div>
    <div class="message">
        There was an <strong>error</strong> processing your request
    </div>
    """);
get_inner_block_slot(info, User) ->
    arizona_template:from_html(~"""
    <div class="modal-icon modal-icon-info">
        <div class="icon-info"></div>
    </div>
    <div class="greeting text-info">
        Hello, {maps:get(username, User)}!
    </div>
    <div class="message">
        Here's some <strong>information</strong> for you
    </div>
    """).

%% Generate confirm button slot based on modal type
get_confirm_slot(success) ->
    #{
        classes => ~"modal-btn modal-btn-success",
        content => ~"Return to profile"
    };
get_confirm_slot(error) ->
    #{
        classes => ~"modal-btn modal-btn-danger",
        content => ~"Retry"
    };
get_confirm_slot(info) ->
    #{
        classes => ~"modal-btn modal-btn-primary",
        content => ~"Got it"
    }.

%% Generate cancel button slot based on modal type
get_cancel_slot(success) ->
    #{
        classes => ~"modal-btn modal-btn-secondary",
        content => ~"Back to index"
    };
get_cancel_slot(error) ->
    #{
        classes => ~"modal-btn modal-btn-secondary",
        content => ~"Cancel"
    };
get_cancel_slot(info) ->
    #{
        classes => ~"modal-btn modal-btn-secondary",
        content => ~"Close"
    }.

%% Event handlers
handle_event(~"set_modal_type", #{~"type" := TypeBin}, View) ->
    State = arizona_view:get_state(View),
    Type = binary_to_existing_atom(TypeBin, utf8),
    UpdatedState = arizona_stateful:merge_bindings(
        #{
            modal_type => Type,
            modal_visible => true
        },
        State
    ),
    UpdatedView = arizona_view:update_state(UpdatedState, View),
    {[], UpdatedView};
handle_event(~"close_modal", _Params, View) ->
    State = arizona_view:get_state(View),
    UpdatedState = arizona_stateful:put_binding(modal_visible, false, State),
    UpdatedView = arizona_view:update_state(UpdatedState, View),
    {[], UpdatedView}.

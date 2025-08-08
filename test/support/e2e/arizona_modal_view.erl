-module(arizona_modal_view).
-behaviour(arizona_view).
-compile({parse_transform, arizona_parse_transform}).

-export([mount/1]).
-export([render/1]).
-export([render_modal/1]).
-export([handle_event/3]).

mount(_Req) ->
    Layout = {arizona_modal_layout, render, main_content, #{}},
    arizona_view:new(
        ?MODULE,
        #{
            id => ~"modal",
            user => #{username => ~"Joe"},
            modal_type => success,
            message_count => 3
        },
        Layout
    ).

%% Ref: https://fly.io/phoenix-files/function-components/
render(Bindings) ->
    arizona_template:from_string(~""""
    <div id="{arizona_template:get_binding(id, Bindings)}">
        <!-- Control buttons to test slot updating -->
        <div class="control-container">
            <button
                class="control-btn control-btn-success"
                onclick="arizona.sendEvent('set_modal_type', \{type: 'success'})"
            >
                Success Modal
            </button>
            <button
                class="control-btn control-btn-error"
                onclick="arizona.sendEvent('set_modal_type', \{type: 'error'})"
            >
                Error Modal
            </button>
            <button
                class="control-btn control-btn-info"
                onclick="arizona.sendEvent('set_modal_type', \{type: 'info'})"
            >
                Info Modal
            </button>
            <button
                class="control-btn control-btn-action"
                onclick="arizona.sendEvent('increment_count')"
            >
                Increment Count
            </button>
        </div>

        {
            User = arizona_template:get_binding(user, Bindings),
            ModalType = arizona_template:get_binding(modal_type, Bindings),
            MessageCount = arizona_template:get_binding(message_count, Bindings),
            get_modal_slots(ModalType, User, MessageCount)
        }
    </div>
    """").

render_modal(Bindings) ->
    arizona_template:from_string(~""""
    <div class="modal-container">
        <div class="header">
            <h1>
            {arizona_template:render_slot(
                arizona_template:get_binding(header, Bindings)
            )}
            </h1>
        </div>

        <div class="modal-body">
            <p class="text-sm text-gray-500">
                {arizona_template:render_slot(
                   arizona_template:get_binding(inner_block, Bindings)
                )}
            </p>
        </div>

        <div class="modal-footer">
            {
                Confirm = arizona_template:get_binding(confirm, Bindings),
                arizona_template:from_string(~"""
                <button class="{maps:get(classes, Confirm)}">
                    {arizona_template:render_slot(maps:get(content, Confirm))}
                </button>
                """)
            }
            {
                Cancel = arizona_template:get_binding(cancel, Bindings),
                arizona_template:from_string(~"""
                <button class="{maps:get(classes, Cancel)}">
                    {arizona_template:render_slot(maps:get(content, Cancel))}
                </button>
                """)
            }
        </div>
    </div>
    """").

%% Helper function to generate modal slots based on type
get_modal_slots(ModalType, User, MessageCount) ->
    arizona_template:render_stateless(arizona_modal_view, render_modal, #{
        header => get_header_slot(ModalType),
        inner_block => get_inner_block_slot(ModalType, User, MessageCount),
        confirm => get_confirm_slot(ModalType),
        cancel => get_cancel_slot(ModalType)
    }).

%% Generate header slot based on modal type
get_header_slot(success) ->
    arizona_template:from_string(~"""
    <div class="border-b-4 border-green-600">
        &#9989; Success modal
    </div>
    """);
get_header_slot(error) ->
    arizona_template:from_string(~"""
    <div class="border-b-4 border-red-600">
        &#10060; Error modal
    </div>
    """);
get_header_slot(info) ->
    arizona_template:from_string(~"""
    <div class="border-b-4 border-blue-600">
        &#8505; Info modal
    </div>
    """).

%% Generate inner_block slot based on modal type
get_inner_block_slot(success, User, MessageCount) ->
    arizona_template:from_string(~"""
    <div class="text-center justify-center items-center">
        <h1 class="text-green-600">
            Hey, {maps:get(username, User)}!
        </h1>
        <p>Your settings have been <strong>successfully</strong> saved</p>
        <p>You have {MessageCount} unread messages</p>
        <div class="flex items-center justify-center">
            <img
                class="h-20 w-20 rounded-full flex items-center"
                src="https://www.erlang.org/assets/img/erlang-logo.svg"
                width="64"
            />
        </div>
    </div>
    """);
get_inner_block_slot(error, User, MessageCount) ->
    arizona_template:from_string(~"""
    <div class="text-center justify-center items-center">
        <h1 class="text-red-600">
            Sorry, {maps:get(username, User)}!
        </h1>
        <p>There was an <strong>error</strong> processing your request</p>
        <p>Failed to update {MessageCount} items</p>
        <div class="flex items-center justify-center">
            <div style="color: red; font-size: 64px;">&#9888;</div>
        </div>
    </div>
    """);
get_inner_block_slot(info, User, MessageCount) ->
    arizona_template:from_string(~"""
    <div class="text-center justify-center items-center">
        <h1 class="text-blue-600">
            Hello, {maps:get(username, User)}!
        </h1>
        <p>Here's some <strong>information</strong> for you</p>
        <p>System has processed {MessageCount} items today</p>
        <div class="flex items-center justify-center">
            <div style="color: blue; font-size: 64px;">&#8505;</div>
        </div>
    </div>
    """).

%% Generate confirm button slot based on modal type
get_confirm_slot(success) ->
    #{
        classes => ~"bg-green-400 rounded-full text-slate-50 text-sm p-2",
        content => arizona_template:from_string(~"Continue")
    };
get_confirm_slot(error) ->
    #{
        classes => ~"bg-red-400 rounded-full text-slate-50 text-sm p-2",
        content => arizona_template:from_string(~"Retry")
    };
get_confirm_slot(info) ->
    #{
        classes => ~"bg-blue-400 rounded-full text-slate-50 text-sm p-2",
        content => arizona_template:from_string(~"Got it")
    }.

%% Generate cancel button slot based on modal type
get_cancel_slot(success) ->
    #{
        classes => ~"bg-gray-400 rounded-full text-slate-50 text-sm p-2",
        content => arizona_template:from_string(~"Back")
    };
get_cancel_slot(error) ->
    #{
        classes => ~"bg-gray-400 rounded-full text-slate-50 text-sm p-2",
        content => arizona_template:from_string(~"Cancel")
    };
get_cancel_slot(info) ->
    #{
        classes => ~"bg-gray-400 rounded-full text-slate-50 text-sm p-2",
        content => arizona_template:from_string(~"Close")
    }.

%% Event handlers
handle_event(~"set_modal_type", #{~"type" := TypeBin}, View) ->
    State = arizona_view:get_state(View),
    Type = binary_to_existing_atom(TypeBin, utf8),
    UpdatedState = arizona_stateful:put_binding(modal_type, Type, State),
    UpdatedView = arizona_view:update_state(UpdatedState, View),
    {noreply, UpdatedView};
handle_event(~"increment_count", _Params, View) ->
    State = arizona_view:get_state(View),
    CurrentCount = arizona_stateful:get_binding(message_count, State),
    NewCount = CurrentCount + 1,
    UpdatedState = arizona_stateful:put_binding(message_count, NewCount, State),
    UpdatedView = arizona_view:update_state(UpdatedState, View),
    {noreply, UpdatedView}.

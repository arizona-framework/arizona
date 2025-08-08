-module(arizona_modal_view).
-behaviour(arizona_view).
-compile({parse_transform, arizona_parse_transform}).

-export([mount/1]).
-export([render/1]).
-export([render_modal/1]).

mount(_Req) ->
    Layout = {arizona_modal_layout, render, main_content, #{}},
    arizona_view:new(
        ?MODULE,
        #{
            id => ~"modal",
            user => #{username => ~"Joe"}
        },
        Layout
    ).

%% Ref: https://fly.io/phoenix-files/function-components/
render(Bindings) ->
    arizona_template:from_string(~""""
    <div id="{arizona_template:get_binding(id, Bindings)}">
        {
            User = arizona_template:get_binding(user, Bindings),
            arizona_template:render_stateless(arizona_modal_view, render_modal, #{
                %% named slot: header
                header => arizona_template:from_string(~"""
                <div class="border-b-4 border-green-600">
                    Success modal
                </div>
                """),

                %% inner_block slot
                inner_block => arizona_template:from_string(~"""
                <div class="text-center justify-center items-center">
                    <h1 class="text-green-600">
                        Hey, {maps:get(username, User)}!
                    </h1>
                    <p>Your settings have been <strong>successfully</strong> saved</p>
                    <div class="flex items-center justify-center">
                        <img
                            class="h-20 w-20 rounded-full flex items-center"
                            src="https://www.erlang.org/assets/img/erlang-logo.svg"
                            width="64"
                        />
                    </div>
                </div>
                """),

                %% footer named slots
                confirm => #{
                    classes => ~"bg-green-400 rounded-full text-slate-50 text-sm p-2",
                    content => arizona_template:from_string(~"""
                    Return to profile
                    """)
                },
                cancel => #{
                    classes => ~"bg-green-400 rounded-full text-slate-50 text-sm p-2",
                    content => arizona_template:from_string(~"""
                    Back to index
                    """)
                }
            })
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
                Cancel = arizona_template:get_binding(confirm, Bindings),
                arizona_template:from_string(~"""
                <button class="{maps:get(classes, Cancel)}">
                    {arizona_template:render_slot(maps:get(content, Cancel))}
                </button>
                """)
            }
        </div>
    </div>
    """").
